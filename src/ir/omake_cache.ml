(*
 * Keep a cache that determines whether we _really_ need to run
 * a command to build a file.
 *
 * The basic idea is this: for each file, remember the command
 * that built it, its Digest, and the Digest of the files it
 * depends on.  If we want to build the node again, and all
 * the parts are the same, then we don't actually need to build
 * it.
 *
 * The same principle works for scanners.
 *)

module I = Lm_instrument

let debug_cache =
   Lm_debug.create_debug (**)
      { debug_name = "cache";
        debug_description = "Display debugging information for the cache";
        debug_value = false
      }

(*
 * An index table.
 * The indexes are small enough that (-) is a correct ordering function.
 *)
module IndexCompare =
struct
   type t = int
   let compare = (-)
end

module IndexTable = Lm_map.LmMakeList (IndexCompare);;

module IndexNodeTable =
struct
   type t = Omake_node.NodeSet.t IndexTable.t

   let empty = IndexTable.empty

   let add table i node =
      IndexTable.filter_add table i (fun nodes ->
            match nodes with
               Some nodes ->
                  Omake_node.NodeSet.add nodes node
             | None ->
                  Omake_node.NodeSet.singleton node)

   let find = IndexTable.find
end;;

(*
 * Directory entry is a directory or node.
 *)
type dir_entry_core =
   LazyEntryCore of Omake_node.Dir.t * string
 | DirEntryCore  of Omake_cache_type.dir_entry

type dir_listing_item = dir_entry_core ref Lm_string_set.StringTable.t

type dir_listing = dir_listing_item list

(*
 * Executable listing.
 *)
type exe_entry_core =
   ExeEntryCore of (Omake_node.Dir.t * string) list
 | ExeEntryNodes of Omake_node.Node.t list

type exe_listing_item = exe_entry_core ref Lm_string_set.StringTable.t

type exe_listing = exe_listing_item list

(*
 * The memo is a record of what we have done during
 * a previous run of this program.
 *
 * The index is a hash over the deps and the commands.
 * The result is mainly used for the scanner, to return
 * the results of the previous scan.
 *
 * We don't really care what the commands are, we just
 * care what their digest is.
 *)
(* %%MAGICBEGIN%% *)
type 'a memo =
   { memo_index    : int;
     memo_targets  : Omake_cache_type.digest Omake_node.NodeTable.t;
     memo_deps     : Omake_cache_type.digest Omake_node.NodeTable.t;
     memo_commands : Omake_cache_type.digest;
     memo_result   : 'a Omake_cache_type.memo_result
   }
(* %%MAGICEND%% *)

type compact_stats = string
  (* File stats as a single string (only for comparison) *)

(*
 * Stats of a file.
 * FreshStats: we have taken the stats during this program run
 * OldStats: we took the stats during a prior run of this program
 *)
(* %%MAGICBEGIN%% *)
type node_stats =
   FreshStats   of compact_stats
 | OldStats     of compact_stats
(* %%MAGICEND%% *)

(*
 * For each file, we keep the stats and the digest.
 *)
(* %%MAGICBEGIN%% *)
type node_memo =
   { nmemo_stats  : node_stats;
     nmemo_digest : Omake_cache_type.digest
   }
(* %%MAGICEND%% *)

(*
 * The memos are classified by functions.
 * Each function has a table of memos, and an index.
 *)
type key = int

type 'a cache_info =
   { mutable cache_memos     : 'a memo Omake_node.NodeTable.t;
     mutable cache_index     : IndexNodeTable.t
   }

(*
 * The cache remembers all the build commands we did last time.
 *)
type stat = Unix.LargeFile.stats

type cache =
   { (* State *)
     mutable cache_nodes             : node_memo Omake_node.NodeTable.t;
     mutable cache_info              : Omake_cache_type.memo_deps cache_info array;
     mutable cache_static_values     : Omake_value_type.obj memo Omake_value_util.ValueTable.t;
     mutable cache_memo_values       : Omake_value_type.obj memo Omake_value_util.ValueTable.t;
     mutable cache_file_stat_count   : int;  (* only succeessful stats are counted *)
     mutable cache_digest_count      : int;

     (* Path lookups *)
     mutable cache_dirs         : (stat option * dir_listing_item) Omake_node.DirTable.t;
     mutable cache_path         : (stat option list * dir_listing_item) Omake_node.DirListTable.t;
     mutable cache_exe_path     : (stat option list * exe_listing_item) Omake_node.DirListTable.t
   }

(*
 * The version of the cache that is saved in the file also
 * contains a table of all nodes.
 *)
(* %%MAGICBEGIN%% *)
type cache_save =
   { save_cache_nodes        : node_memo Omake_node.NodeTable.t;
     save_cache_info         : Omake_cache_type.memo_deps memo Omake_node.NodeTable.t array;
     save_cache_value        : Omake_value_type.obj memo Omake_value_util.ValueTable.t
   }
(* %%MAGICEND%% *)

(*
 * Squash stat code. (Returned for directories, non-regular files, and nodes
 * where contents do not matter.)
 *)
let squash_stat = Some "squash"

(*
 * The cache type.
 *)
type t = cache

(*
 * Memo functions for the cache.
 *)
let scanner_fun         = 0
let rule_fun            = 1
let env_fun             = 2
let include_fun         = 3

let env_target  = Omake_node.Node.create_phony_global ".ENV"

(************************************************************************
 * Printing.
 *)

(*
 * Print a digest.
 *)
let pp_print_digest buf digest =
   let s =
      match digest with
         Some digest ->
            Lm_string_util.hexify digest
       | None ->
            "<none>"
   in
      Lm_printf.pp_print_string buf s

(*
 * Print a digest table.
 *)
let pp_print_node_digest_table buf deps =
   Omake_node.NodeTable.iter (fun node digest ->
         Format.fprintf buf "@ %a = %a" (**)
            Omake_node.pp_print_node node
            pp_print_digest digest) deps

(*
 * Print a result.
 *)
let pp_print_memo_result _pp_print_result buf result =
   match result with
      Omake_cache_type.MemoSuccess _ ->
         Format.fprintf buf "@ success"
    | MemoFailure code ->
         Format.fprintf buf "@ failed(%d)" code

(*
 * Print a memo.
 *)
(* let pp_print_memo buf memo = *)
(*    let { memo_index    = index; *)
(*          memo_targets  = targets; *)
(*          memo_deps     = deps; *)
(*          memo_result   = _result; *)
(*          memo_commands = commands *)
(*        } = memo *)
(*    in *)
(*       Format.fprintf buf "@[<hv 0>@[<hv 3>memo {@ index = %d;@ @[<b 3>targets =%a@];@ @[<b 3>deps =%a@]@ @[<hv 3>commands =%a@]@]@ }@]" (\**\) *)
(*          index *)
(*          pp_print_node_digest_table targets *)
(*          pp_print_node_digest_table deps *)
(*          pp_print_digest commands *)

(************************************************************************
 * Persistence.
 *)

(*
 * Create the cache.  We save the cache in a file, so try to
 * load it from that file.
 *)
let magic_number = Omake_magic.cache_magic
let input_magic  = Omake_magic.input_magic
let output_magic = Omake_magic.output_magic

(*
 * Create a new cache.
 *)
let create () =
   { cache_nodes           = Omake_node.NodeTable.empty;
     cache_info            = [||];
     cache_static_values   = Omake_value_util.ValueTable.empty;
     cache_memo_values     = Omake_value_util.ValueTable.empty;
     cache_file_stat_count = 0;
     cache_digest_count    = 0;
     cache_dirs            = Omake_node.DirTable.empty;
     cache_path            = Omake_node.DirListTable.empty;
     cache_exe_path        = Omake_node.DirListTable.empty
   }

let rehash cache =
   cache.cache_dirs <- Omake_node.DirTable.empty;
   cache.cache_path <- Omake_node.DirListTable.empty;
   cache.cache_exe_path <- Omake_node.DirListTable.empty

let stats { cache_file_stat_count = stat_count;
            cache_digest_count = digest_count;
            _
    } =
   stat_count, digest_count

(*
 * When the file is saved, remove all stat information,
 * as well as any files that don't exist.
 *)
let save_of_cache cache =
   let { cache_nodes  = cache_nodes;
         cache_info   = cache_info;
         cache_static_values = cache_values;
         _
       } = cache
   in
   let cache_nodes =
      Omake_node.NodeTable.fold (fun nodes target nmemo ->
            match nmemo.nmemo_stats with
               FreshStats stats
             | OldStats stats ->
                  let nmemo = { nmemo with nmemo_stats = OldStats stats } in
                     Omake_node.NodeTable.add nodes target nmemo
      ) Omake_node.NodeTable.empty cache_nodes
   in
   let cache_info = Array.map (fun info -> info.cache_memos) cache_info in
      { save_cache_nodes = cache_nodes;
        save_cache_info  = cache_info;
        save_cache_value = cache_values
      }

(*
 * Rebuild the index from the memos.
 *)
let create_index memos =
  Omake_node.NodeTable.fold (fun (memos, index) target memo ->
    let { memo_result = result;
          memo_index = hash;
          _
        } = memo
    in
    match result with
      Omake_cache_type.MemoSuccess _ ->
      let index = IndexNodeTable.add index hash target in
      memos, index
    | MemoFailure _ ->
      let memos = Omake_node.NodeTable.remove memos target in
      memos, index) (memos, IndexNodeTable.empty) memos

(*
 * Rebuild the cache from the saved version.
 *)
let cache_of_save options save =
   let { save_cache_nodes = cache_nodes;
         save_cache_info  = cache_info;
         _
       } = save
   in
   let flush_scanner = Omake_options.opt_flush_dependencies options in
   let cache_info =
      Array.mapi (fun idx memos ->
            if flush_scanner && idx = scanner_fun then
               { cache_memos = Omake_node.NodeTable.empty;
                 cache_index = IndexNodeTable.empty
               }
            else
               let memos, index = create_index memos in
                  { cache_memos = memos;
                    cache_index = index
                  }) cache_info
   in
   let cache_values =
      if Omake_options.opt_flush_static options then
         Omake_value_util.ValueTable.empty
      else
         save.save_cache_value
   in
      { (create ()) with cache_nodes  = cache_nodes;
                         cache_info   = cache_info;
                         cache_static_values = cache_values
      }

(*
 * Load the old cache from a file.
 *)
let from_channel options inx =
   if Omake_options.opt_flush_cache options then
      create ()
   else
      let magic = input_magic inx in
      let _ =
         if magic <> magic_number then
            raise (Sys_error "bad magic number")
      in
      let save = Marshal.from_channel inx in
         cache_of_save options save

(*
 * Save the cache to the file.
 *)
let to_channel outx cache =
   output_magic outx magic_number;
   Marshal.to_channel outx (save_of_cache cache) []

(************************************************************************
 * Stat.
 *)

(*
 * Reset the information about a node (probably because
 * we ran a command that changed it).
 *)
let reset cache node =
   let nodes = cache.cache_nodes in
   let nodes =
      try
         Omake_node.NodeTable.filter_remove nodes node (fun nmemo ->
               match nmemo.nmemo_stats with
                  FreshStats stats
                | OldStats stats ->
                     Some { nmemo with nmemo_stats = OldStats stats })
      with
         Not_found ->
            nodes
   in
      cache.cache_nodes <- nodes

let reset_set cache nodes =
   Omake_node.NodeSet.iter (reset cache) nodes

let reset_table cache nodes =
   Omake_node.NodeTable.iter (fun node _ -> reset cache node) nodes

let compact_stats s =
  let open Unix.LargeFile in
  Printf.sprintf "%x:%x:%Lx:%Lx"
    s.st_dev
    s.st_ino
    s.st_size
    (Int64.bits_of_float s.st_mtime)

(*
 * Test whether stats are equal enough that we think the
 * file is up-to-date.
 *)
let stats_equal stat1 stat2 =
   stat1 = stat2

(* let pp_print_stat buf (stat : Unix.LargeFile.stats) = *)
(*   match stat with  *)
(*     { st_ino  = ino; *)
(*       st_kind = _kind; *)
(*       st_size = size; *)
(*       st_mtime = mtime; *)
(*       _ *)
(*     } -> *)
(*     Format.fprintf buf "ino = %d, size = %Ld, mtime = %g" ino size mtime *)

(*
 * Sloppy digests on files larger than 16MB.
 *)
let large_file_size = 8_000_000L
let sample_size     = 0x10000
let sample_count    = 16

let rec really_read_exn fd buf off len =
   if len > 0 then
      let amount = Unix.read fd buf off len in
         really_read_exn fd buf (off + amount) (len - amount)

let digest_sample_exn fd file_size =
   let sample = String.create (8 + (sample_count + 1) * sample_size) in
      (* Add the file length to the string *)
      sample.[0] <- Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical file_size 56));
      sample.[1] <- Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical file_size 48));
      sample.[2] <- Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical file_size 40));
      sample.[3] <- Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical file_size 32));
      sample.[4] <- Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical file_size 24));
      sample.[5] <- Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical file_size 16));
      sample.[6] <- Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical file_size 8));
      sample.[7] <- Char.unsafe_chr (Int64.to_int file_size);

      (* Sample the file *)
      for i = 0 to sample_count do
         let off =
            if i = sample_count then
               Int64.sub file_size (Int64.of_int sample_size)
            else
               Int64.div (Int64.mul file_size (Int64.of_int i)) (Int64.of_int sample_count)
         in
         let _ = Unix.LargeFile.lseek fd off Unix.SEEK_SET in
            really_read_exn fd sample (8 + i * sample_size) sample_size
      done;

      (* Take the digest of the sample *)
      Digest.string sample

let digest_large name file_size =
   try
      let fd = Unix.openfile name [Unix.O_RDONLY] 0o000 in
         try
            let digest = digest_sample_exn fd file_size in
               Unix.close fd;
               digest
         with
            Unix.Unix_error _
          | End_of_file ->
               Unix.close fd;
               raise (Sys_error name)
   with
      Unix.Unix_error _ ->
         raise (Sys_error name)

let probe_digest_file =
  I.create "Omake_cache.digest_file"

let digest_file name = 
  I.instrument probe_digest_file
  (fun file_size ->
   if file_size > large_file_size then
      digest_large name file_size
   else
      Digest.file name
  )


let probe_stat_file = 
  I.create "Omake_cache.stat_file"

(*
 * Stat a file. (Returns digest.)
 *)
let stat_file cache =
  I.instrument probe_stat_file
  (fun node ->
   let nodes = cache.cache_nodes in
   let stats =
      try Some (Omake_node.NodeTable.find nodes node) with
         Not_found ->
            None
   in
      match stats with
         Some { nmemo_stats = FreshStats _;
                nmemo_digest = digest
         } ->
            (*
             * We've recently computed the digest for this file.
             *)
            digest
       | Some ({ nmemo_stats = OldStats cstats ; _} as nmemo) ->
            (*
             * We have no recent record of this file.
             * Get current stats.  If they match, then
             * use current digest; otherwise recompute.
             *)
            let name = Omake_node.Node.fullname node in
            ( try
                let stats' = Unix.LargeFile.stat name in
                if stats'.Unix.LargeFile.st_kind <> Unix.S_REG then (
                  cache.cache_nodes <- Omake_node.NodeTable.remove nodes node;
                  squash_stat
                )
                else (
                  cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
                  let cstats' = compact_stats stats' in
                  let nmemo =
                    if stats_equal cstats' cstats then
                      { nmemo with nmemo_stats = FreshStats cstats' }
                    else
                      let digest = digest_file name stats'.Unix.LargeFile.st_size in
                      cache.cache_digest_count <- succ cache.cache_digest_count;
                      { nmemo_stats = FreshStats cstats';
                        nmemo_digest = Some digest
                      } in
                  cache.cache_nodes <- Omake_node.NodeTable.add nodes node nmemo;
                  nmemo.nmemo_digest
                )
              with
                  Unix.Unix_error _
                | Sys_error _ ->
                     cache.cache_nodes <- Omake_node.NodeTable.remove nodes node;
                     None
            )

       | None ->
            (*
             * We've never seen this file before.
             * Get complete stats, including a digest.
             *)
            let name = Omake_node.Node.fullname node in
            ( try
                let stats = Unix.LargeFile.stat name in
                if stats.Unix.LargeFile.st_kind <> Unix.S_REG then
                  squash_stat
                else (
                  cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
                  let digest = digest_file name stats.Unix.LargeFile.st_size in
                  let cstats = compact_stats stats in
                  cache.cache_digest_count <- succ cache.cache_digest_count;
                  let nmemo =
                    { nmemo_stats = FreshStats cstats;
                      nmemo_digest = Some digest
                    } in
                  cache.cache_nodes <- Omake_node.NodeTable.add nodes node nmemo;
                  Some digest
                )
              with
                  Unix.Unix_error _
                | Sys_error _ ->
                     None
            )
  )

let stat_unix_node cache ~force node =
  (* We used to cache this information, but don't do this anymore. Hence,
     the [force] flag is ignored: we always re-stat
   *)
  ignore(force);
  let name = Omake_node.Node.fullname node in
  try
    cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
    Unix.LargeFile.stat name
  with
      Unix.Unix_error _
    | Sys_error _ ->
         raise Not_found

(*
 * lstat versions, for not following symlinks.
 *)

let lstat_unix_node cache ~force node =
  (* We used to cache this information, but don't do this anymore. Hence,
     the [force] flag is ignored: we always re-stat
   *)
  ignore(force);
  let name = Omake_node.Node.fullname node in
  try
    cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
    Unix.LargeFile.lstat name
  with
      Unix.Unix_error _
    | Sys_error _ ->
         raise Not_found

(*
 * Cached stat.
 *)
let stat_unix cache ?(force=false) ?(follow_symlinks=true) node =
   let node =
      match Omake_node.Node.kind node with
         NodePhony
       | NodeScanner ->
            raise Not_found
       | NodeNormal ->
            node
       | NodeOptional
       | NodeSquashed
       | NodeExists ->
            Omake_node.Node.core node
   in
      if follow_symlinks then
         stat_unix_node cache ~force node
      else
         lstat_unix_node cache ~force node

(*
 * Check if a file is a directory.
 *)
let is_dir cache ?(force=false) ?(follow_symlinks=true) node =
   try (stat_unix cache ~force ~follow_symlinks node).Unix.LargeFile.st_kind = Unix.S_DIR with
      Not_found ->
         false

(*
 * Tests for whether a file is executable.
 * This really only works on Unix.
 *)
let euid =
   try Unix.geteuid () with
      Unix.Unix_error _ ->
         0

let groups =
   try Array.to_list (Unix.getgroups ()) with
      Unix.Unix_error _ ->
         []

(*
 * Check if the node is phony first.
 *)
let stat cache node =
   let core = Omake_node.Node.core node in
      match Omake_node.Node.kind node with
         NodePhony
       | NodeScanner ->
            None
       | NodeOptional
       | NodeNormal ->
            stat_file cache core
       | NodeSquashed
       | NodeExists ->
            (match stat_file cache core with
                Some _ ->
                   squash_stat
              | None ->
                   None)

(*
 * Turn a set into a table of stat info.
 *)
let stat_set cache nodes =
   Omake_node.NodeSet.fold (fun table node ->
         Omake_node.NodeTable.add table node (stat cache node)) 
    Omake_node.NodeTable.empty nodes

let stat_table cache nodes =
   Omake_node.NodeTable.mapi (fun node _ -> stat cache node) nodes

(*
 * Force a stat.
 *)
let force_stat cache node =
   reset cache node;
   stat cache node

let force_stat_set cache nodes =
   Omake_node.NodeSet.fold (fun table node ->
         Omake_node.NodeTable.add table node (force_stat cache node)) 
    Omake_node.NodeTable.empty nodes

let force_stat_table cache nodes =
   Omake_node.NodeTable.mapi (fun node _ -> force_stat cache node) nodes

(*
 * Check if the stat changed.
 *)
let stat_changed cache node =
   let old_digest =
      try (Omake_node.NodeTable.find cache.cache_nodes node).nmemo_digest with
         Not_found ->
            None
   in
   let new_digest = force_stat cache node in
      new_digest <> old_digest

(*
 * Check if a file exists.
 *)
let exists cache ?(force=false) node =
   try
      ignore (stat_unix cache ~force node); true
   with
      Not_found -> Omake_node.Node.always_exists node

let exists_dir cache ?(force=false) dir =
   exists cache ~force (Omake_node.Node.node_of_dir dir)

(************************************************************************
 * Adding to the cache.
 *)

(*
 * Hash a set of deps and commands.
 * The commands is a digest.
 *)
let hash_index deps commands =
   let index =
      Omake_node.NodeSet.fold (fun index node ->
            index lxor (index lsl 4) lxor (index lsr 4) lxor (Omake_node.Node.hash node)) 0 deps
   in
   let index = index lxor (index lsl 4) lxor (index lsr 4) lxor (Hashtbl.hash commands) in
      index land 0x3fffffff

(*
 * Expand the cache_info if necessary.
 *)
let get_info cache key =
   let cache_info = cache.cache_info in
   let len = Array.length cache_info in
   let cache_info =
      if key >= Array.length cache_info then
         begin
            let cache_info' =
               Array.init (succ key) (fun _ ->
                     { cache_memos = Omake_node.NodeTable.empty;
                       cache_index = IndexNodeTable.empty
                     })
            in
               Array.blit cache_info 0 cache_info' 0 len;
               cache.cache_info <- cache_info';
               cache_info'
         end
      else
         cache_info
   in
      cache_info.(key)

(*
 * Add a command.
 *)
let add cache key target targets deps commands result =
   let index = hash_index deps commands in
   let memo =
      { memo_index    = index;
        memo_targets  = stat_set cache targets;
        memo_deps     = stat_set cache deps;
        memo_result   = result;
        memo_commands = commands
      }
   in
   let info = get_info cache key in
      info.cache_memos <- Omake_node.NodeTable.add info.cache_memos target memo;
      info.cache_index <- IndexNodeTable.add info.cache_index index target

(*
 * Check the target digest.
 *)
let targets_equal cache targets =
   Omake_node.NodeTable.forall (fun target digest ->
         stat cache target = digest) targets

(*
 * Check if deps are the same.
 * This returns true if the deps are equal.
 * If not, it either returns false, or raises Not_found.
 *)
let deps_equal cache deps1 deps2 =
   let count1 = Omake_node.NodeSet.cardinal deps1 in
   let count2 = Omake_node.NodeTable.cardinal deps2 in
      (count1 = count2) && Omake_node.NodeSet.for_all (fun dep1 ->
            let digest1 = stat cache dep1 in
            let digest2 = Omake_node.NodeTable.find deps2 dep1 in
               digest1 = digest2) deps1

(*
 * Find a memo from the deps and commands.
 *)
let find_memo cache key deps commands =
   let { cache_memos = memos;
         cache_index = index
       } = get_info cache key
   in
   let hash = hash_index deps commands in
   let rec search = function
      target :: targets ->
         let memo = Omake_node.NodeTable.find memos target in
         let { memo_index = hash';
               memo_deps  = deps';
               memo_commands = commands';
               _
             } = memo
         in
            if hash' = hash && deps_equal cache deps deps' && commands = commands' then
               memo
            else
               search targets
    | [] ->
         raise Not_found
   in
   let targets = Omake_node.NodeSet.to_list (IndexNodeTable.find index hash) in
      (* eprintf "Targets: %d@." (List.length targets); *)
      search targets

(*
 * A memo has not changed if all the deps and the target
 * have the same digests.
 *)
let up_to_date cache key deps commands =
   try
      let memo = find_memo cache key deps commands in
         if targets_equal cache memo.memo_targets then
            match memo.memo_result with
               MemoSuccess _ ->
                  true
             | MemoFailure _ ->
                  false
         else
            false
   with
      Not_found ->
         false

let up_to_date_status cache key deps commands =
  try
    let memo = find_memo cache key deps commands in
    if targets_equal cache memo.memo_targets then
      match memo.memo_result with
        Omake_cache_type.MemoSuccess _ ->
        Omake_cache_type.StatusSuccess
      | MemoFailure code ->
        StatusFailure code
    else
      StatusUnknown
  with
    Not_found ->
    StatusUnknown

(*
 * A memo has not changed if all the deps and the target
 * have the same digests.
 *)
let target_results results =
  match results with
    Omake_cache_type.MemoFailure _ ->
    raise Not_found
  | MemoSuccess deps ->
    deps

let find_result cache key deps commands =
   let memo = find_memo cache key deps commands in
   let { memo_targets = targets;
         memo_result  = result;
         _
       } = memo
   in
      if targets_equal cache targets then
         target_results result
      else
         raise Not_found

(*
 * Find the result of a run, without the commands.
 *)
let find_result_sloppy cache key target =
   let { cache_memos = memos ; _} = get_info cache key in
   let memo = Omake_node.NodeTable.find memos target in
      match memo.memo_result with
         MemoFailure _ ->
            raise Not_found
       | MemoSuccess deps ->
            deps

(************************************************************************
 * Values.  In this case, we use the key to find the memo.
 *)

(*
 * Get the memo entry from the key.
 *)
let get_value cache key is_static =
   Omake_value_util.ValueTable.find (if is_static then cache.cache_static_values else cache.cache_memo_values) key

(*
 * Find a memo from the deps and commands.
 *)
let find_value_memo cache key is_static deps1 commands1 =
   let memo = get_value cache key is_static in
   let hash1 = hash_index deps1 commands1 in
   let { memo_index = hash2;
         memo_deps  = deps2;
         memo_commands = commands2;
         _
       } = memo
   in
      if hash1 = hash2 && deps_equal cache deps1 deps2 && commands1 = commands2 then
         memo
      else
         raise Not_found

let find_value cache key is_static deps commands =
   let memo = find_value_memo cache key is_static deps commands in
   let { memo_targets = targets;
         memo_result  = result;
         _
       } = memo
   in
      if targets_equal cache targets then
         target_results result
      else
         raise Not_found

let add_value cache key is_static deps commands result =
   let index = hash_index deps commands in
   let memo =
      { memo_index    = index;
        memo_targets  = Omake_node.NodeTable.empty;
        memo_deps     = stat_set cache deps;
        memo_result   = result;
        memo_commands = commands
      }
   in
      if is_static then
         cache.cache_static_values <- Omake_value_util.ValueTable.add cache.cache_static_values key memo
      else
         cache.cache_memo_values <- Omake_value_util.ValueTable.add cache.cache_memo_values key memo

(************************************************************************
 * Directory listings.
 *)

(*
 * When auto-rehash is in effect, we need to stat the directories
 * on every lookup.
 *)
let stat_dir cache dir =
   let name = Omake_node.Dir.fullname dir in
      try
         let stat = Unix.LargeFile.stat name in
            cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
            Some stat
      with
         Unix.Unix_error _ ->
            None

let stat_dirs cache dirs =
   List.map (stat_dir cache) dirs

let stats_equal_opt stat1 stat2 =
   match stat1, stat2 with
      Some stat1, Some stat2 ->
         stats_equal stat1 stat2
    | None, None ->
         true
    | None, Some _
    | Some _, None ->
         false

let rec stats_equal_opt_list stats1 stats2 =
   match stats1, stats2 with
      stat1 :: stats1, stat2 :: stats2 ->
         stats_equal_opt stat1 stat2 && stats_equal_opt_list stats1 stats2
    | [], [] ->
         true
    | _ :: _, []
    | [], _ :: _ ->
         false

let check_stat auto_rehash (stat_old, entries) stat_new =
   if auto_rehash && not (stats_equal_opt stat_old (Lazy.force stat_new)) then
      raise Not_found
   else
      entries

let check_stats auto_rehash (stats_old, entries) stats_new =
   if auto_rehash && not (stats_equal_opt_list stats_old (Lazy.force stats_new)) then
      raise Not_found
   else
      entries

(*
 * List a directory.
 *)
let  list_directory _cache dir =
   let dirx =
      try Unix.opendir (Omake_node.Dir.fullname dir) with
         Unix.Unix_error _ ->
            raise Not_found
   in
   let rec list entries =
      let name =
         try Some (Unix.readdir dirx) with
            Unix.Unix_error _
          | End_of_file ->
               None
      in
         match name with
            Some "."
          | Some ".." ->
               list entries
          | Some name ->
               let entry = ref (LazyEntryCore (dir, name)) in
               let entries = Lm_string_set.StringTable.add entries name entry in
                  list entries
          | None ->
               entries
   in
   let entries = list Lm_string_set.StringTable.empty in
      Unix.closedir dirx;
      entries

(*
 * Get the directory listing as a Lm_string_set.StringTable.
 *)
let ls_dir cache auto_rehash dir =
   let stat = lazy (stat_dir cache dir) in
      try check_stat auto_rehash (Omake_node.DirTable.find cache.cache_dirs dir) stat with
         Not_found ->
            let entries = list_directory cache dir in
            let stat = Lazy.force stat in
               cache.cache_dirs <- Omake_node.DirTable.add cache.cache_dirs dir (stat, entries);
               entries

(*
 * Path version.
 *)
let ls_path cache auto_rehash dirs =
   let key = Omake_node.DirListHash.create dirs in
   let stats = lazy (stat_dirs cache dirs) in
      try check_stats auto_rehash (Omake_node.DirListTable.find cache.cache_path key) stats with
         Not_found ->
            (* Fold together the tables *)
            let stats = Lazy.force stats in
            let entries =
               List.fold_left (fun entries1 dir ->
                     try
                        let entries2 = ls_dir cache auto_rehash dir in
                           Lm_string_set.StringTable.fold Lm_string_set.StringTable.add entries1 entries2
                     with
                        Not_found ->
                           entries1) Lm_string_set.StringTable.empty (List.rev dirs)
            in
               cache.cache_path <- Omake_node.DirListTable.add cache.cache_path key (stats, entries);
               entries

(*
 * Resolve an entry in the listing.
 *)
let listing_find_item cache listing s =
  let entry_ref = Lm_string_set.StringTable.find listing s in
  match !entry_ref with
    DirEntryCore entry ->
    entry
  | LazyEntryCore (dir, s) ->
    let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
    let entry =
      if is_dir cache node then
        Omake_cache_type.DirEntry (Omake_node.Dir.chdir dir s)
      else
        NodeEntry node
    in
    entry_ref := DirEntryCore entry;
    entry

(*
 * The execution path is a little harder, and it is quite different
 * on Win32 and Unix.
 *
 * On Win32:
 *    - File permission doesn't matter
 *    - Files without suffix, and with .com, .exe, .bat and .cmd suffixes are executable
 *
 * On Cygwin:
 *    - Files without suffix must be executable
 *    - Files with .com, .exe, .bat and .cmd suffixes are executable
 *
 * On Unix:
 *    - There is no .exe suffix
 *    - Only files that are executable count
 *)
let win32_suffixes = [".com"; ".exe"; ".bat"; ".cmd"]

let ls_exe_path_win32 cache auto_rehash dirs =
   let key = Omake_node.DirListHash.create dirs in
   let stats = lazy (stat_dirs cache dirs) in
      try check_stats auto_rehash (Omake_node.DirListTable.find cache.cache_exe_path key) stats with
         Not_found ->
            let entries =
               List.fold_left (fun entries1 dir ->
                     try
                        let entries2 = ls_dir cache auto_rehash dir in
                           Lm_string_set.StringTable.fold (fun entries name _ ->
                                 let info = dir, name in
                                 let name = String.lowercase name in
                                 let entries = Lm_string_set.StringMTable.add entries name info in
                                    if List.exists (Filename.check_suffix name) win32_suffixes then
                                       let name = Filename.chop_extension name in
                                          Lm_string_set.StringMTable.add entries name info
                                    else
                                       entries) entries1 entries2
                     with
                        Not_found ->
                           entries1) Lm_string_set.StringMTable.empty (List.rev dirs)
            in
            let entries =
               Lm_string_set.StringMTable.fold_all (fun entries name info ->
                     Lm_string_set.StringTable.add entries name (ref (ExeEntryCore info))) 
                Lm_string_set.StringTable.empty entries
            in
            let stats = Lazy.force stats in
               cache.cache_exe_path <- Omake_node.DirListTable.add cache.cache_exe_path key (stats, entries);
               entries

let ls_exe_path_unix cache auto_rehash dirs =
  let key = Omake_node.DirListHash.create dirs in
  let stats = lazy (stat_dirs cache dirs) in
  try check_stats auto_rehash (Omake_node.DirListTable.find cache.cache_exe_path key) stats with
    Not_found ->
    let entries =
      List.fold_left (fun entries1 dir ->
        try
          let entries2 = ls_dir cache auto_rehash dir in
          Lm_string_set.StringTable.fold (fun entries name _ ->
            Lm_string_set.StringMTable.add entries name (dir, name)) entries1 entries2
        with
          Not_found ->
          entries1) Lm_string_set.StringMTable.empty (List.rev dirs)
    in
    let entries =
      Lm_string_set.StringMTable.fold_all (fun entries name info ->
        Lm_string_set.StringTable.add entries name (ref (ExeEntryCore info))) 
        Lm_string_set.StringTable.empty entries
    in
    let stats = Lazy.force stats in
    cache.cache_exe_path <- Omake_node.DirListTable.add cache.cache_exe_path key (stats, entries);
    entries

(*
 * Find the first entry that is executable.
 *)
let is_exe_file cache node =
   try
      let { Unix.LargeFile.st_kind = kind;
            Unix.LargeFile.st_perm = perm;
            Unix.LargeFile.st_uid = uid;
            Unix.LargeFile.st_gid = gid;
            _
          } = stat_unix cache node
      in
         (kind = Unix.S_REG)
         && ((perm land 0o001) <> 0
             || (List.mem gid groups && (perm land 0o010) <> 0)
             || (uid = euid && (perm land 0o100) <> 0))
   with
      Unix.Unix_error _
    | Not_found ->
         false

let is_exe_win32 cache dir s =
  let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
  if is_dir cache node then
    None
  else
    Some node

let is_exe_unix cache dir s =
  let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
  if is_exe_file cache node then
    Some node
  else
    None

let is_exe_cygwin cache dir s =
  let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
  if List.exists (Filename.check_suffix s) win32_suffixes || is_exe_file cache node then
    Some node
  else
    None

let ls_exe_path, is_exe, name_exe, exe_suffixes =
   if Sys.os_type = "Win32" then
      ls_exe_path_win32, is_exe_win32, String.lowercase, "" :: win32_suffixes
   else if Sys.os_type = "Cygwin" then
      ls_exe_path_win32, is_exe_cygwin, String.lowercase, "" :: win32_suffixes
   else
      ls_exe_path_unix, is_exe_unix, (fun s -> s), [""]

let  search_exe cache entries =
   Lm_list_util.some_map (fun (dir, s) ->
         is_exe cache dir s) entries

let exe_find_nodes cache listing s =
   let entry_ref = Lm_string_set.StringTable.find listing (name_exe s) in
      match !entry_ref with
         ExeEntryCore entries ->
            let nodes = search_exe cache entries in
               entry_ref := ExeEntryNodes nodes;
               nodes
       | ExeEntryNodes nodes ->
            nodes

let exe_find_nodes_all cache listing s =
   try exe_find_nodes cache listing s with
      Not_found ->
         []

let exe_find_item cache listing s =
   match exe_find_nodes cache listing s with
      node :: _ ->
         node
    | [] ->
         raise Not_found

(*
 * Find all entries with the given prefix.
 *)
let exe_complete_prefix cache s items listing =
   Lm_string_set.StringTable.fold (fun items s2 entry_ref ->
         if Lm_string_util.is_string_prefix s s2 then
            let nodes =
               match !entry_ref with
                  ExeEntryCore entries ->
                     let nodes = search_exe cache entries in
                        entry_ref := ExeEntryNodes nodes;
                        nodes
                | ExeEntryNodes nodes ->
                     nodes
            in
               if nodes = [] then
                  items
               else
                  Lm_string_set.StringSet.add items s2
         else
            items) items listing

(************************************************************************
 * Redefine the functions to work on directory groups, where each group may
 * specify auto-rehashing.
 *)
let rec listing_find cache listings s =
   match listings with
      [listing] ->
         listing_find_item cache listing s
    | listing :: listings ->
         (try listing_find_item cache listing s with
             Not_found ->
                listing_find cache listings s)
    | [] ->
         raise Not_found

let rec exe_find cache listings s =
   match listings with
      [listing] ->
         exe_find_item cache listing s
    | listing :: listings ->
         (try exe_find_item cache listing s with
             Not_found ->
                exe_find cache listings s)
    | [] ->
         raise Not_found

let exe_find_all cache listings s =
   List.flatten (List.map (fun listing -> exe_find_nodes_all cache listing s) listings)

let exe_complete cache listings s =
   List.fold_left (exe_complete_prefix cache s) Lm_string_set.StringSet.empty listings

let ls_dir cache auto_rehash dir =
   [ls_dir cache auto_rehash dir]

let ls_path cache groups =
   List.map (fun (auto_rehash, dirs) ->
         ls_path cache auto_rehash dirs) groups

let ls_exe_path cache groups =
   List.map (fun (auto_rehash, dirs) ->
         ls_exe_path cache auto_rehash dirs) groups
