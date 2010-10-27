(*
 * This is the base part of the build system.
 * Each file in the system is represented as a node.
 * Node may be virtual: the node may exist before the file
 * does.  For each file, we maintain stat and MD5 information
 * (if they exist).
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech and HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>, <anogin@hrl.com>
 * @end[license]
 *)
open Lm_hash
open Lm_printf
open Lm_symbol
open Lm_hash_sig
open Lm_location
open Lm_string_util
open Lm_filename_util

open Fmarshal

open Omake_state
open Omake_marshal
open Omake_node_sig
open Omake_print_util

(************************************************************************
 * This case [in-]sensitivity of file names is a complex issue.
 * We make the type abstract so we don't make a mistake.
 *)
module type FileCaseSig =
sig
   type t
   type dir

   val create              : dir -> string -> t
   val compare             : t -> t -> int
   val equal               : t -> t -> bool
   val add_filename        : HashCode.t -> t -> unit
   val add_filename_string : Buffer.t -> t -> unit
   val marshal             : t -> msg
   val unmarshal           : msg -> t
end;;

module rec FileCase : FileCaseSig with type dir = DirHash.t =
struct
   (* %%MAGICBEGIN%% *)
   type t = string
   (* %%MAGICEND%% *)

   type dir = DirHash.t

   open DirElt
   open Unix.LargeFile

   (*
    * Check whether a directory is case-sensitive.
    *)
   let case_table = ref DirTable.empty

   (* We'll use randomly generated names *)
   let fs_random = Random.State.make_self_init ()

   (*
    * Test whether stats are equal enough that we think that it's the same file.
    *)
   let stats_equal stat1 stat2 =
      stat1.st_dev = stat2.st_dev
         && stat1.st_ino = stat2.st_ino
         && stat1.st_kind = stat2.st_kind
         && stat1.st_rdev = stat2.st_rdev
         && stat1.st_nlink = stat2.st_nlink
         && stat1.st_size = stat2.st_size
         && stat1.st_mtime = stat2.st_mtime
         && stat1.st_ctime = stat2.st_ctime

   (*
    * Toggle the case of the name.
    * Raises Not_found if the name contains no alphabetic letters.
    *)
   let rec toggle_name_case name len i =
      if i = len then
         raise Not_found
      else
         match name.[i] with
            'A'..'Z'
          | '\192' .. '\214'
          | '\216' .. '\222' ->
               String.lowercase name
          | 'a'..'z'
          | '\224' .. '\246'
          | '\248' .. '\254' ->
               String.uppercase name
          | _ ->
               toggle_name_case name len (succ i)

   (*
    * Stat, does not fail.
    *)
   let do_stat absname =
      try Some (Unix.LargeFile.lstat absname) with
         Unix.Unix_error _ ->
            None

   (*
    * Unlink, does not fail.
    *)
   let do_unlink absname =
      try Unix.unlink absname with
         Unix.Unix_error _ ->
            ()

   (*
    * Create a file, raising Unix_error if the file can't be created.
    *)
   let do_create absname =
      Unix.close (Unix.openfile absname [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600)

   (*
    * Given two filenames that differ only in case,
    * stat them both.  If the stats are different,
    * the directory is on a case-sensitive fs.
    *
    * XXX: try to detect race conditions by performing
    * a stat on the first file before and after.
    * Raise Not_found if a race condition is detected.
    *)
   let stats_not_equal name1 name2 =
      let stat1 = do_stat name1 in
      let stat2 = do_stat name2 in
      let stat3 = do_stat name1 in
         match stat1, stat3 with
            Some s1, Some s3 when stats_equal s1 s3 ->
               (match stat2 with
                   Some s2 when stats_equal s2 s1 -> false
                 | _ -> true)
          | _ ->
               raise Not_found

   (*
    * If we have an alphabetic name, just toggle the case.
    *)
   let stat_with_toggle_case absdir name =
      let alternate_name = toggle_name_case name (String.length name) 0 in
         stats_not_equal (Filename.concat absdir name) (Filename.concat absdir alternate_name)

   (*
    * Look through the entire directory for a name with alphabetic characters.
    * A check for case-sensitivity base on that.
    *
    * Raises Not_found if there are no such filenames.
    *)
   let rec dir_test_all_entries_exn absdir dir_handle =
      let name =
         try Unix.readdir dir_handle
         with Unix.Unix_error _ | End_of_file as exn ->
            Unix.closedir dir_handle;
            raise exn
      in
         match name with
            "."
          | ".." ->
               dir_test_all_entries_exn absdir dir_handle
          | _ ->
               try
                  let result = stat_with_toggle_case absdir name in
                     Unix.closedir dir_handle;
                     result
               with Not_found ->
                     dir_test_all_entries_exn absdir dir_handle

   (*
    * Check for sensativity by creating a dummy file.
    *)
   let dir_test_new_entry_exn absdir =
      Unix.access absdir [Unix.W_OK];
      let name = sprintf "OM%06x.tmp" (Random.State.bits fs_random land 0xFFFFFF) in
      let absname = Filename.concat absdir name in
      let () = do_create absname in
         try
            let flag = stat_with_toggle_case absdir name in
               do_unlink absname;
               flag
         with Not_found as exn ->
            do_unlink absname;
            raise exn

   (*
    * To check for case sensitivity, try these tests in order,
    * stopping when one is successful.
    *    1. Try the name being created itself.
    *    2. Try looking at the directory entries.
    *    3. Create a dummy file, and test (unless the attept to do step 2 made
    *       us realize there is no such directory).
    *    4. Test the parent.
    *
    * Steps 2-4 will only be performed when we really need them (the name contains
    * uppercase letters),
    *)
   exception Not_a_usable_directory
   exception Already_lowercase

   let rec check_already_lowercase name len i =
      if i = len then
         raise Already_lowercase
      else
         match name.[i] with
            'A'..'Z'
          | '\192' .. '\214'
          | '\216' .. '\222' -> ()
          | _ -> check_already_lowercase name len (i+1)

   let rec dir_test_sensitivity shortcircuit dir absdir name =
      try stat_with_toggle_case absdir name
      with Not_found ->
         if shortcircuit then
            check_already_lowercase name (String.length name) 0;
         try
            let dir_handle =
               try Unix.opendir absdir with
                  Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR | Unix.ELOOP | Unix.ENAMETOOLONG), _, _) ->
                     raise Not_a_usable_directory
            in
               try dir_test_all_entries_exn absdir dir_handle
               with Unix.Unix_error _ | Not_found | End_of_file ->
                     dir_test_new_entry_exn absdir
         with Unix.Unix_error _ | Not_found | End_of_file | Not_a_usable_directory ->
            match DirHash.get dir with
               DirRoot _ ->
                  (* Nothing else we can do, assume sensitive *)
                  true
             | DirSub (_, name, parent) ->
                  dir_is_sensitive false parent name

   (*
    * This is the caching version of the case-sensitivity test.
    *)
   and dir_is_sensitive shortcircuit dir name =
      try DirTable.find !case_table dir with
         Not_found ->
            let absdir = abs_dir_name dir in
            let sensitive =
               if Lm_fs_case_sensitive.available then
                  try
                     Lm_fs_case_sensitive.case_sensitive absdir
                  with
                     Failure _ ->
                        dir_test_sensitivity shortcircuit dir absdir name
               else
                  dir_test_sensitivity shortcircuit dir absdir name
            in
               case_table := DirTable.add !case_table dir sensitive;
               sensitive

   (*
    * On Unix-like OS (especially on Mac OS X), create needs to check the fs of the node's parent
    * directory for case sensitivity.
    * See also http://bugzilla.metaprl.org/show_bug.cgi?id=657
    *)
   let create =
      match Sys.os_type with
         "Win32"
       | "Cygwin" ->
            (fun _ name -> String.lowercase name)
       | _ ->
            (fun dir name ->
               try
                  if dir_is_sensitive true dir name then name else String.lowercase name
               with Already_lowercase ->
                  name)

   let compare = Lm_string_util.string_compare
   let equal (s1: string) s2 = (s1 = s2)
   let add_filename = HashCode.add_string
   let add_filename_string = Buffer.add_string

   let marshal s =
      String s

   let unmarshal = function
      String s ->
         s
    | _ ->
         raise MarshalError
end

(************************************************************************
 * Directories.
 *)

(*
 * Internally, we represent pathnames as absolute paths.
 * We keep a hashed integer for quick equality testing.
 *    dir_root : the root of this name
 *    dir_key  : the path in canonical form (lowercase on Windows)
 *    dir_name : the actual path will full capitalization
 *)
(* %%MAGICBEGIN%% *)
and DirElt :
sig
   type t =
      DirRoot of Lm_filename_util.root
    | DirSub of FileCase.t * string * DirHash.t

   val abs_dir_name: DirHash.t -> string
end
=
struct
   type t =
      DirRoot of Lm_filename_util.root
    | DirSub of FileCase.t * string * t hash_marshal_eq_item

   let abs_dir_name =
      let rec name buf dir =
         match DirHash.get dir with
            DirRoot root ->
               Buffer.add_string buf (Lm_filename_util.string_of_root root)
          | DirSub (key, _, parent) ->
               name buf parent;
               begin match DirHash.get parent with
                  DirRoot _ ->
                     ()
                | _ ->
                     Buffer.add_char buf Lm_filename_util.separator_char
               end;
               FileCase.add_filename_string buf key
      in
      fun dir ->
         let buf = Buffer.create 17 in
         let () = name buf dir in
            Buffer.contents buf

end
(* %%MAGICEND%% *)

(*
 * Sets and tables.
 *)
and DirCompare : HashMarshalEqArgSig with type t = DirElt.t =
struct
   open DirElt
   type t = DirElt.t

   let debug = "Dir"

   let fine_hash = function
      DirRoot root ->
         Hashtbl.hash root
    | DirSub (_, raw_name, parent) ->
         let buf = HashCode.create () in
            HashCode.add_int buf (DirHash.hash parent);
            HashCode.add_string buf raw_name;
            HashCode.code buf

   let coarse_hash = function
      DirRoot root ->
         Hashtbl.hash root
    | DirSub (name, _, parent) ->
         let buf = HashCode.create () in
            HashCode.add_int buf (DirHash.hash parent);
            FileCase.add_filename buf name;
            HashCode.code buf

   let rec fine_compare dir1 dir2 =
      match dir1, dir2 with
         DirRoot root1, DirRoot root2 ->
            Pervasives.compare root1 root2
       | DirSub (_, name1, parent1), DirSub (_, name2, parent2) ->
            let cmp = Lm_string_util.string_compare name1 name2 in
               if cmp = 0 then
                  DirHash.fine_compare parent1 parent2
               else
                  cmp
       | DirRoot _, DirSub _ ->
            -1
       | DirSub _, DirRoot _ ->
            1

   let rec coarse_compare dir1 dir2 =
      match dir1, dir2 with
         DirRoot root1, DirRoot root2 ->
            Pervasives.compare root1 root2
       | DirSub (name1, _, parent1), DirSub (name2, _, parent2) ->
            let cmp = FileCase.compare name1 name2 in
               if cmp = 0 then
                  DirHash.compare parent1 parent2
               else
                  cmp
       | DirRoot _, DirSub _ ->
            -1
       | DirSub _, DirRoot _ ->
            1

   let reintern dir =
      match dir with
         DirRoot _ ->
            dir
       | DirSub (name1, name2, parent1) ->
            let parent2 = DirHash.reintern parent1 in
               if parent2 == parent1 then
                  dir
               else
                  DirSub (name1, name2, parent2)
end

(* %%MAGICBEGIN%% *)
and DirHash :
   HashMarshalEqSig
   with type elt = DirElt.t
   with type t = DirElt.t hash_marshal_eq_item
=
   MakeHashMarshalEq (DirCompare)

and DirSet   : Lm_set_sig.LmSet with type elt = DirHash.t = Lm_set.LmMake (DirHash)
and DirTable : Lm_map_sig.LmMap with type key = DirHash.t = Lm_map.LmMake (DirHash)

type dir = DirHash.t
(* %%MAGICEND%% *)

(*
 * Lists of directories.
 *)
module rec DirListCompare : HashMarshalEqArgSig with type t = dir list =
struct
   type t = dir list

   let debug = "DirList"

   let hash f l =
      let buf = HashCode.create () in
         List.iter (fun dir -> HashCode.add_int buf (f dir)) l;
         HashCode.code buf

   let fine_hash = hash DirHash.fine_hash
   let coarse_hash = hash DirHash.hash

   let rec compare f l1 l2 =
      match l1, l2 with
         d1 :: l1, d2 :: l2 ->
            let cmp = f d1 d2 in
               if cmp = 0 then
                  compare f l1 l2
               else
                  cmp
       | [], [] ->
            0
       | [], _ :: _ ->
            -1
       | _ :: _, [] ->
            1

   let fine_compare = compare DirHash.fine_compare
   let coarse_compare = compare DirHash.compare

   let reintern l =
      Lm_list_util.smap DirHash.reintern l
end

and DirListHash : HashMarshalEqSig with type elt = dir list =
   MakeHashMarshalEq (DirListCompare);;

module DirListSet = Lm_set.LmMake (DirListHash);;
module DirListTable = Lm_map.LmMake (DirListHash);;

open DirElt

(************************************************************************
 * Nodes.
 *)

(*
 * Possible node flags.
 *)
(* %%MAGICBEGIN%% *)
type node_flag =
   NodeIsOptional
 | NodeIsExisting
 | NodeIsSquashed
 | NodeIsScanner

(*
 * A node is a phony, or it is a filename.
 *)
type node_elt =
   NodeFile        of dir * FileCase.t * string
 | NodePhonyGlobal of string
 | NodePhonyDir    of dir * FileCase.t * string
 | NodePhonyFile   of dir * FileCase.t * string * string
 | NodeFlagged     of node_flag * node_elt hash_marshal_eq_item
(* %%MAGICEND%% *)

module rec NodeCompare :
sig
   include HashMarshalEqArgSig with type t = node_elt
   (* Include the default "compare" for the PreNodeSet *)
   val compare : t -> t -> int
end
=
struct
   type t = node_elt;;

   let debug = "Node"

   type code =
      CodeSpace
    | CodeEnd
    | CodeNodeFile
    | CodeNodePhonyGlobal
    | CodeNodePhonyDir
    | CodeNodePhonyFile
    | CodeNodeFlagged
    | CodeNodeIsOptional
    | CodeNodeIsExisting
    | CodeNodeIsSquashed
    | CodeNodeIsScanner

   let add_code buf (code : code) =
      HashCode.add_bits buf (Obj.magic code)

   let add_flag_code buf code =
      let code =
         match code with
            NodeIsOptional ->
               CodeNodeIsOptional
          | NodeIsExisting ->
               CodeNodeIsExisting
          | NodeIsSquashed ->
               CodeNodeIsSquashed
          | NodeIsScanner ->
               CodeNodeIsScanner
      in
         add_code buf code

   module MakeNodeOps (Arg : sig
      val add_dir : HashCode.t -> DirHash.t -> unit
      val add_node : HashCode.t -> NodeHash.t -> unit
      val add_filename : HashCode.t -> FileCase.t -> string -> unit
      val filename_compare : FileCase.t -> string -> FileCase.t -> string -> int
      val node_compare : NodeHash.t -> NodeHash.t -> int
      val dir_compare : DirHash.t -> DirHash.t -> int
   end) = struct
      open Arg

      let add_node buf node =
         match node with
            NodeFile (dir, name, raw_name) ->
               add_code buf CodeNodeFile;
               add_dir buf dir;
               add_code buf CodeSpace;
               add_filename buf name raw_name;
               add_code buf CodeEnd
          | NodePhonyGlobal name ->
               add_code buf CodeNodePhonyGlobal;
               HashCode.add_string buf name;
               add_code buf CodeEnd
          | NodePhonyDir (dir, name, raw_name) ->
               add_code buf CodeNodePhonyDir;
               add_dir buf dir;
               add_code buf CodeSpace;
               add_filename buf name raw_name;
               add_code buf CodeEnd
          | NodePhonyFile (dir, key, raw_name, name) ->
               add_code buf CodeNodePhonyFile;
               add_dir buf dir;
               add_code buf CodeSpace;
               add_filename buf key raw_name;
               add_code buf CodeSpace;
               HashCode.add_string buf name;
               add_code buf CodeEnd
          | NodeFlagged (flag, node) ->
               add_code buf CodeNodeFlagged;
               add_flag_code buf flag;
               add_code buf CodeSpace;
               add_node buf node;
               add_code buf CodeEnd

      let hash node =
         let buf = HashCode.create () in
            add_node buf node;
            HashCode.code buf

      let compare_flags flag1 flag2 =
         match flag1, flag2 with
            NodeIsOptional, NodeIsOptional
          | NodeIsExisting, NodeIsExisting
          | NodeIsSquashed, NodeIsSquashed
          | NodeIsScanner,  NodeIsScanner ->
               0
          | NodeIsOptional, NodeIsExisting
          | NodeIsOptional, NodeIsSquashed
          | NodeIsOptional, NodeIsScanner
          | NodeIsExisting, NodeIsSquashed
          | NodeIsExisting, NodeIsScanner
          | NodeIsSquashed, NodeIsScanner ->
               -1
          | NodeIsExisting, NodeIsOptional
          | NodeIsSquashed, NodeIsOptional
          | NodeIsScanner,  NodeIsOptional
          | NodeIsSquashed, NodeIsExisting
          | NodeIsScanner,  NodeIsExisting
          | NodeIsScanner,  NodeIsSquashed ->
               1

      let compare node1 node2 =
         match node1, node2 with
            NodeFile (dir1, key1, name1), NodeFile (dir2, key2, name2)
          | NodePhonyDir (dir1, key1, name1), NodePhonyDir (dir2, key2, name2) ->
               let cmp = filename_compare key1 name1 key2 name2 in
                  if cmp = 0 then
                     dir_compare dir1 dir2
                  else
                     cmp
          | NodePhonyGlobal name1, NodePhonyGlobal name2 ->
               Lm_string_util.string_compare name1 name2
          | NodePhonyFile (dir1, key1, name1, exname1), NodePhonyFile (dir2, key2, name2, exname2) ->
               let cmp = Lm_string_util.string_compare exname1 exname2 in
                  if cmp = 0 then
                     let cmp = filename_compare key1 name1 key2 name2 in
                        if cmp = 0 then
                           dir_compare dir1 dir2
                        else
                           cmp
                  else
                     cmp
          | NodeFlagged (flag1, node1), NodeFlagged (flag2, node2) ->
               let cmp = compare_flags flag1 flag2 in
                  if cmp = 0 then
                     node_compare node1 node1
                  else
                     cmp
          | NodeFile _,        NodePhonyGlobal _
          | NodeFile _,        NodePhonyDir _
          | NodeFile _,        NodePhonyFile _
          | NodeFile _,        NodeFlagged _
          | NodePhonyGlobal _, NodePhonyDir _
          | NodePhonyGlobal _, NodePhonyFile _
          | NodePhonyGlobal _, NodeFlagged _
          | NodePhonyDir _,    NodePhonyFile _
          | NodePhonyDir _,    NodeFlagged _
          | NodePhonyFile _,   NodeFlagged _ ->
               -1
          | NodeFlagged _,      NodeFile _
          | NodePhonyGlobal _,  NodeFile _
          | NodePhonyDir _,     NodeFile _
          | NodePhonyFile _,    NodeFile _
          | NodeFlagged _,      NodePhonyGlobal _
          | NodePhonyDir _,     NodePhonyGlobal _
          | NodePhonyFile _,    NodePhonyGlobal _
          | NodeFlagged _,      NodePhonyDir _
          | NodePhonyFile _,    NodePhonyDir _
          | NodeFlagged _,      NodePhonyFile _ ->
               1
   end;;

   (*
    * These operations are case insensitive on case-insensitive
    * filesystems.  They use the canonical FileCase.t name.
    *)
   module Ops =
      MakeNodeOps (struct
         let add_dir buf dir =
            HashCode.add_int buf (DirHash.hash dir )

         let add_node buf node =
            HashCode.add_int buf (NodeHash.hash node)

         let add_filename buf name raw_name =
            FileCase.add_filename buf name

         let filename_compare name1 _raw_name1 name2 _raw_name2 =
            FileCase.compare name1 name2

         let node_compare = NodeHash.compare

         let dir_compare = DirHash.compare
      end);;

   (*
    * These operations are always case-sensitive.
    *)
   module FineOps =
      MakeNodeOps (struct
         let add_dir buf dir =
            HashCode.add_int buf (DirHash.fine_hash dir )

         let add_node buf node =
            HashCode.add_int buf (NodeHash.fine_hash node)

         let add_filename buf name raw_name =
            HashCode.add_string buf raw_name

         let filename_compare _name1 raw_name1 _name2 raw_name2 =
            String.compare raw_name1 raw_name2

         let node_compare = NodeHash.fine_compare

         let dir_compare = DirHash.fine_compare
      end);;

   let coarse_compare = Ops.compare
   let coarse_hash = Ops.hash
   let fine_compare = FineOps.compare
   let fine_hash = FineOps.hash
   let compare = Ops.compare (* for the PreNodeSet *)

   let reintern node =
      match node with
         NodeFile (dir1, key, name) ->
            let dir2 = DirHash.reintern dir1 in
               if dir2 == dir1 then
                  node
               else
                  NodeFile (dir2, key, name)
       | NodePhonyDir (dir1, key, name) ->
            let dir2 = DirHash.reintern dir1 in
               if dir2 == dir1 then
                  node
               else
                  NodePhonyDir (dir2, key, name)
       | NodePhonyFile (dir1, key, name1, name2) ->
            let dir2 = DirHash.reintern dir1 in
               if dir2 == dir1 then
                  node
               else
                  NodePhonyFile (dir2, key, name1, name2)
       | NodePhonyGlobal _ ->
            node
       | NodeFlagged (flag, node1) ->
            let node2 = NodeHash.reintern node1 in
               if node2 == node1 then
                  node
               else
                  NodeFlagged (flag, node2)
end

(* %%MAGICBEGIN%% *)
and NodeHash :
   HashMarshalEqSig
   with type elt = node_elt
   with type t = node_elt hash_marshal_eq_item
=
   MakeHashMarshalEq (NodeCompare);;

type node = NodeHash.t
(* %%MAGICEND%% *)

module NodeSet = Lm_set.LmMake (NodeHash);;
module NodeTable = Lm_map.LmMake (NodeHash);;
module NodeMTable = Lm_map.LmMakeList (NodeHash);;

module PreNodeSet = Lm_set.LmMake (NodeCompare);;

(************************************************************************
 * Implementation.
 *)

(*
 * Get a pathname from a directory.
 * The name must be reversed.
 *)
let rec path_of_dir_aux keypath path dir =
   match DirHash.get dir with
      DirRoot root ->
         root, keypath, path
    | DirSub (key, name, parent) ->
         path_of_dir_aux (key :: keypath) (name :: path) parent

let path_of_dir dir =
   path_of_dir_aux [] [] dir

(*
 * Build a list of the directories, in reverse order.
 *)
let dir_list_of_dir dir =
   let rec dir_list_of_dir path dir =
      match DirHash.get dir with
         DirRoot _ ->
            dir :: path
       | DirSub (_, _, parent) ->
            dir_list_of_dir (dir :: path) parent
   in
      dir_list_of_dir [] dir

(*
 * Produce a path (a string list) from the dir list.
 *)
let rec path_of_dir_list dirs =
   match dirs with
      [] ->
         []
    | dir :: dirs ->
         match DirHash.get dir with
            DirSub (_, name, _) ->
               name :: path_of_dir_list dirs
          | DirRoot _ ->
               raise (Invalid_argument "path_of_dir_list")

(*
 * Make a directory node from the pathname.
 *)
let make_subdir parent name =
   DirHash.create (DirSub (FileCase.create parent name, name, parent))

let make_dir root path =
   List.fold_left make_subdir (DirHash.create (DirRoot root)) path

(*
 * Get the current absolute name of the working directory.
 *)
let getcwd () =
   let cwd = Unix.getcwd () in
      match Lm_filename_util.filename_path cwd with
         AbsolutePath (root, dir) ->
            make_dir root dir
       | RelativePath dir ->
            raise (Invalid_argument "Unix.getcwd returned a relative path")

(*
 * A null root directory for globals.
 *)
let null_root =
   make_dir null_root []

(*
 * Fake root for "absname" computations
 *)
let impossible_root =
   make_dir (DriveRoot '$') []

(*
 * Split the directory name into a path.
 *)
let rec path_simplify dir = function
   "" :: path
 | "." :: path ->
      path_simplify dir path
 | ".." :: path ->
      let dir =
         match DirHash.get dir with
            DirSub (_, _, parent) ->
               parent
          | DirRoot _ ->
               dir
      in
         path_simplify dir path
 | [name] ->
      dir, Some name
 | name :: path ->
      path_simplify (make_subdir dir name) path
 | [] ->
      dir, None

let new_path dir path =
   match Lm_filename_util.filename_path path with
      AbsolutePath (root, path) ->
         (* This is an absolute path, so ignore the directory *)
         path_simplify (make_dir root []) path
    | RelativePath path ->
         (* This is relative to the directory *)
         path_simplify dir path

let new_dir dir path =
   match new_path dir path with
      dir, None ->
         dir
    | dir, Some name ->
         make_subdir dir name

let new_file dir path =
   let dir, name = new_path dir path in
      match name with
         Some name ->
            let key = FileCase.create dir name in
               dir, key, name
       | None ->
            begin match DirHash.get dir with
               DirSub (key, name, dir) ->
                  dir, key, name
             | DirRoot _ ->
                  let name = "." in
                  let key = FileCase.create dir name in
                     dir, key, name
            end

(*
 * Check if .. works in a directory.
 *)
let dotdot_table = ref DirTable.empty

(*
 * Force dotdot to fail.
 *)
let make_dotdot_fail dir =
   dotdot_table := DirTable.add !dotdot_table dir true

let dotdot_fails dir =
   let table = !dotdot_table in
      try DirTable.find table dir with
         Not_found ->
            let name = DirElt.abs_dir_name dir in
            let islink =
               try (Unix.lstat name).Unix.st_kind = Unix.S_LNK with
                  Unix.Unix_error _ ->
                     false
            in
            let table = DirTable.add table dir islink in
               dotdot_table := table;
               islink

(*
 * Produce string filename for the path,
 * relative to a particular directory.
 *
 * Algorithm:
 *    1. Compute the common prefix between the directory and file
 *    2. Add as many ".." as there are remaining names in the directory
 *       and concatenate the rest of the path.
 *
 * Example:
 *       dir = /a/b/c/d
 *       path = /a/b/e/f/g
 *       result = ../../e/f/g
 *)

(*
 * Create a string from the list of strings.
 *)
let rec flatten_generic (add_string : 'a -> string -> 'a) (contents : 'a -> string) (buf : 'a) (path : string list) =
   match path with
      [path] ->
         let buf = add_string buf path in
            contents buf
    | [] ->
         contents buf
    | name :: path ->
         let buf = add_string buf name in
         let buf = add_string buf Lm_filename_util.separator_string in
            flatten_generic add_string contents buf path

(*
 * Add .. to get out of the directory and down into the path.
 *)
let updirs_generic add_string contents buf dirs1 dirs2 =
   (* Abort if any of the dotdots fail *)
   if List.exists dotdot_fails dirs1 then
      None
   else
      (* Prepend the .. sequence *)
      let rec updirs dirs path =
         match dirs with
            _ :: dirs ->
               updirs dirs (".." :: path)
          | [] ->
               path
      in
      let path = path_of_dir_list dirs2 in
      let path = updirs dirs1 path in
         Some (flatten_generic add_string contents buf path)

(*
 * Compute the path of dir2 relative to dir1.
 *)
let rec relocate_generic add_string contents buf (dirs1 : dir list) (dirs2 : dir list) =
   match dirs1, dirs2 with
    | [], _ ->
         Some (flatten_generic add_string contents buf (path_of_dir_list dirs2))
    | _, [] ->
         updirs_generic add_string contents buf dirs1 dirs2
    | dir1 :: dirs1', dir2 :: dirs2'  ->
         if DirHash.equal dir1 dir2 then
            relocate_generic add_string contents buf dirs1' dirs2'
         else
            updirs_generic add_string contents buf dirs1 dirs2

(*
 * If the files differ in the root directory, just use the absolute path.
 *)
let relocate_generic add_string contents buf dir1 dir2 =
   let dirs1 = dir_list_of_dir dir1 in
   let dirs2 = dir_list_of_dir dir2 in
      match dirs1, dirs2 with
         dir1 :: dirs1, dir2 :: dirs2 ->
            (match DirHash.get dir1, DirHash.get dir2 with
                DirRoot root1, DirRoot root2 ->
                   (let s =
                       if dirs1 = [] || root1 <> root2 then
                          None
                       else
                          relocate_generic add_string contents buf dirs1 dirs2
                    in
                       match s with
                          Some s ->
                             s
                        | None ->
                             let buf = add_string buf (Lm_filename_util.string_of_root root2) in
                             let path2 = path_of_dir_list dirs2 in
                                flatten_generic add_string contents buf path2)
              | _ ->
                   raise (Invalid_argument "relocate_generic"))
       | _ ->
            raise (Invalid_argument "relocate_generic")

(*
 * Directory versions.
 *)
let dir_buffer = Buffer.create 17

let dir_add_string buf s =
   Buffer.add_string buf s;
   buf

let dir_contents buf =
   let s = Buffer.contents buf in
      Buffer.clear buf;
      s

let flatten_dir dir =
   flatten_generic dir_add_string dir_contents dir_buffer dir

let relocate_dir dir1 dir2 =
   relocate_generic dir_add_string dir_contents dir_buffer dir1 dir2

(*
 * File version.
 *)
let file_contents name buf =
   let len = Buffer.length buf in
   let buf =
      if len = 0 || Buffer.nth buf (len - 1) = Lm_filename_util.separator_char then
         buf
      else
         dir_add_string buf Lm_filename_util.separator_string
   in
   let buf = dir_add_string buf name in
      dir_contents buf

let flatten_file dir name =
   let buf = dir_add_string dir_buffer Lm_filename_util.separator_string in
      flatten_generic dir_add_string (file_contents name) buf dir

let relocate_file dir1 dir2 name =
   if DirHash.equal dir1 dir2 then
      name
   else
      relocate_generic dir_add_string (file_contents name) dir_buffer dir1 dir2

(*
 * Apply a mount point.
 *)
let rec resolve_mount_dir dir_dst dir_src dir =
   if DirHash.compare dir dir_dst = 0 then
      dir_src
   else
      match DirHash.get dir with
         DirRoot _ ->
            raise Not_found
       | DirSub (key, name, parent) ->
            let parent = resolve_mount_dir dir_dst dir_src parent in
               DirHash.create (DirSub (key, name, parent))

let rec resolve_mount_node dir_dst dir_src node =
   let node =
      match NodeHash.get node with
         NodeFile (dir, key, name) ->
            let dir = resolve_mount_dir dir_dst dir_src dir in
               NodeFile (dir, key, name)
       | NodePhonyDir (dir, key, name) ->
            let dir = resolve_mount_dir dir_dst dir_src dir in
               NodePhonyDir (dir, key, name)
       | NodePhonyFile (dir, key1, name1, name) ->
            let dir = resolve_mount_dir dir_dst dir_src dir in
               NodePhonyFile (dir, key1, name1, name)
       | NodePhonyGlobal _ ->
            raise Not_found
       | NodeFlagged (flag, node) ->
            NodeFlagged (flag, resolve_mount_node dir_dst dir_src node)
   in
      NodeHash.create node

(*
 * A name can stand for a global phony only if it has no slashes,
 * or it only leads with a slash.  Raises Not_found if the name
 * contains any non-leading slashes.
 *)
type phony_name =
   PhonyGlobalString of string
 | PhonyDirString of string
 | PhonySimpleString
 | PhonyPathString

(* Starting at position i, s begins with ".PHONY/" *)
let string_prefix_phony s i len =
   len >= i + 7 &&
      s.[i  ] = '.' &&
      s.[i+1] = 'P' &&
      s.[i+2] = 'H' &&
      s.[i+3] = 'O' &&
      s.[i+4] = 'N' &&
      s.[i+5] = 'Y' &&
      (s.[i+6] = '/' || s.[i+6] = '\\')

let rec is_simple_string s len i =
   (i = len) ||
      match s.[i] with
         '/'
       | '\\' ->
            false
       | _ ->
            is_simple_string s len (succ i)

let parse_phony_name s =
   let len = String.length s in
      if len = 0 then
         PhonySimpleString
      else match s.[0] with
         '/'
       | '\\' ->
         if string_prefix_phony s 1 len && is_simple_string s len 8 then
            (* /.PHONY/foo *)
            PhonyGlobalString (String.sub s 8 (len - 8))
         else
            PhonyPathString
       | '.' when string_prefix_phony s 0 len ->
            (* .PHONY/foo/bar *)
            PhonyDirString (String.sub s 7 (len - 7))
       | _ ->
            if is_simple_string s len 1 then
               PhonySimpleString
            else
               PhonyPathString

(************************************************************************
 * Modules.
 *)
module Dir =
struct
   type t = dir

   (*
    * We assume the cwd does not change
    * once we first get it.
    *)
   let cwd_ref =
      let dir =
         try
            getcwd ()
         with Unix.Unix_error (err, _, _) ->
            eprintf "@[<v3>*** omake: warning:@ Can not find out the current directory:@ %s;@ Using the root directory instead.@]@." (Unix.error_message err);
            null_root
      in
         ref dir

   (*
    * Default is current working directory.
    *)
   let cwd () = !cwd_ref

   let reset_cwd () =
      let cwd = getcwd () in
         cwd_ref := getcwd ();
         make_dotdot_fail cwd

   (*
    * Building a new path.
    *)
   let chdir = new_dir

   (*
    * Name, relative to the cwd.
    *)
   let name dir1 dir2 =
      if DirHash.equal dir1 dir2 then
         "."
      else
         relocate_dir dir1 dir2

   (*
    * Name relative to the root.
    *)
   let fullname dir =
      name !cwd_ref dir

   (*
    * Absolute name.
    *)
   let root = null_root

   let absname dir =
      name impossible_root dir

   (*
    * Library directory is relative to the root.
    *)
   let lib =
      match Lm_filename_util.filename_path Omake_state.lib_dir with
         AbsolutePath (root, dir) ->
            make_dir root dir
       | RelativePath _ ->
            raise (Invalid_argument ("Omake_node.lib_dir specified as relative path: " ^ Omake_state.lib_dir))

   (*
    * home directory is also relative to the root.
    *)
   let home =
      match Lm_filename_util.filename_path home_dir with
         AbsolutePath (root, dir) ->
            make_dir root dir
       | RelativePath _ ->
            raise (Invalid_argument ("Omake_node.home_dir specified as relative path: " ^ home_dir))

   let () =
      make_dotdot_fail root;
      make_dotdot_fail home

   (*
    * Equality.
    *)
   let compare = DirHash.compare
   let equal = DirHash.equal

   (*
    * Marshaling.
    *)
   let marshal_root root =
      match root with
         NullRoot ->
            Magic NullRootMagic
       | DriveRoot c ->
            List [Magic DriveRootMagic; Char c]

   let unmarshal_root l =
      match l with
         Magic NullRootMagic ->
            NullRoot
       | List [Magic DriveRootMagic; Char c] ->
            DriveRoot c
       | _ ->
            raise MarshalError

   let rec marshal dir =
      match DirHash.get dir with
         DirRoot root ->
            List [Magic DirRootMagic; marshal_root root]
       | DirSub (key, name, parent) ->
            List [Magic DirSubMagic; FileCase.marshal key; String name; marshal parent]

   let rec unmarshal l =
      let dir =
         match l with
            List [Magic DirRootMagic; root] ->
               DirRoot (unmarshal_root root)
          | List [Magic DirSubMagic; key; String name; parent] ->
               DirSub (FileCase.unmarshal key, name, unmarshal parent)
          | _ ->
               raise MarshalError
      in
         DirHash.create dir
end;;

(*
 * Virtual mounts.
 * We need a function that checks if a file exists.
 *)
module Mount =
struct
   type t = (Dir.t * Dir.t * mount_option list) list

   type dir_tmp = dir
   type node_tmp = node
   type dir = dir_tmp
   type node = node_tmp

   (*
    * Create a new mount state.
    *)
   let empty = []

   (*
    * Add a mount point.
    *)
   let mount info options dir_src dir_dst =
      (dir_dst, dir_src, options) :: info
end;;

type mount_info = node poly_mount_info

let no_mount_info =
   { mount_file_exists = (fun _ -> false);
     mount_file_reset  = (fun _ -> ());
     mount_is_dir      = (fun _ -> false);
     mount_stat        = (fun _ -> raise (Invalid_argument "no_mount_info"));
     mount_digest      = (fun _ -> None)
   }

(*
 * Nodes.
 *)
module Node =
struct
   type pre   = node_elt
   type t     = node
   type dir   = Dir.t
   type mount = Mount.t

   open Mount;;

   let dest = NodeHash.get

   (*
    * Get the name.
    *)
   let phony_name name =
      "<" ^ name ^ ">"

   (*
    * Name of the node.
    *)
   let rec name dir1 node =
      match NodeHash.get node with
         NodePhonyGlobal name ->
            phony_name name
       | NodePhonyDir (dir2, _, name) ->
            phony_name (relocate_file dir1 dir2 name)
       | NodePhonyFile (dir2, _, name1, name2) ->
            phony_name (relocate_file dir1 dir2 name1 ^ ":" ^ name2)
       | NodeFile (dir2, _, name) ->
            relocate_file dir1 dir2 name
       | NodeFlagged (_, node) ->
            name dir1 node

   (*
    * Create a phony name.
    *)
   let create_phony_global name =
      NodeHash.create (NodePhonyGlobal name)

   (*
    * Create a phony from a dir.
    *)
   let create_phony_dir dir name =
      let key = FileCase.create dir name in
         NodeHash.create (NodePhonyDir (dir, key, name))

   (*
    * Create a phony with a new directory.
    *)
   let create_phony_chdir node dir =
      match NodeHash.get node with
         NodePhonyDir (_, _, name) ->
            create_phony_dir dir name
       | _ ->
            node

   (*
    * Create a new phony node from a previous node.
    * These are not interned.
    *)
   let rec create_phony_node node name =
      match NodeHash.get node with
         NodeFile (dir, key1, name1) ->
            NodeHash.create (NodePhonyFile (dir, key1, name1, name))
       | NodePhonyGlobal name1 ->
            let key1 = FileCase.create null_root name1 in
               NodeHash.create (NodePhonyFile (null_root, key1, name1, name))
       | NodePhonyDir (dir, key1, name1)
       | NodePhonyFile (dir, key1, name1, _) ->
            NodeHash.create (NodePhonyFile (dir, key1, name1, name))
       | NodeFlagged (_, node) ->
            create_phony_node node name

   (*
    * Get the core node.
    *)
   let rec core node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFile _ ->
            node
       | NodeFlagged (_, node) ->
            core node

   (*
    * Escape a node.
    *)
   let create_escape kind node =
      let node = core node in
         match kind with
            NodeNormal ->
               node
          | NodePhony ->
               raise (Invalid_argument "Omake_node.Node.escape: NodePhony is not allowed")
          | NodeOptional ->
               NodeHash.create (NodeFlagged (NodeIsOptional, node))
          | NodeExists ->
               NodeHash.create (NodeFlagged (NodeIsExisting, node))
          | NodeSquashed ->
               NodeHash.create (NodeFlagged (NodeIsSquashed, node))
          | NodeScanner ->
               NodeHash.create (NodeFlagged (NodeIsScanner, node))

   (*
    * Hash code for a node.
    *)
   let hash = NodeHash.hash
   let reintern = NodeHash.reintern

   (*
    * For building targets, we sometimes want to know the
    * original node.
    *)
   let rec unsquash node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFile _
       | NodeFlagged (NodeIsOptional, _)
       | NodeFlagged (NodeIsScanner, _)
       | NodeFlagged (NodeIsExisting, _) ->
            node

       | NodeFlagged (NodeIsSquashed, node) ->
            unsquash node

   (*
    * Kind of the node.
    *)
   let kind node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _ ->
            NodePhony
       | NodeFile _ ->
            NodeNormal
       | NodeFlagged (NodeIsOptional, _) ->
            NodeOptional
       | NodeFlagged (NodeIsExisting, _) ->
            NodeExists
       | NodeFlagged (NodeIsSquashed, _) ->
            NodeSquashed
       | NodeFlagged (NodeIsScanner, _) ->
            NodeScanner

   (*
    * Phony nodes.
    *)
   let rec is_phony node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _ ->
            true
       | NodeFile _
       | NodeFlagged _ ->
            false

   let rec phony_name node =
      match NodeHash.get node with
         NodePhonyGlobal name
       | NodePhonyDir (_, _, name)
       | NodePhonyFile (_, _, _, name) ->
            name
       | NodeFile _ ->
            raise (Invalid_argument "phony_name")
       | NodeFlagged (_, node) ->
            phony_name node

   let rec is_real node =
      match NodeHash.get node with
         NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFlagged (NodeIsOptional, _)
       | NodeFlagged (NodeIsExisting, _) ->
            false
       | NodeFile _ ->
            true
       | NodeFlagged (NodeIsSquashed, node)
       | NodeFlagged (NodeIsScanner, node) ->
            is_real node

   (*
    * Existential flag.
    *)
   let always_exists node =
      match NodeHash.get node with
         NodeFlagged (NodeIsOptional, _)
       | NodeFlagged (NodeIsExisting, _) ->
            true
       | NodeFlagged (NodeIsSquashed, _)
       | NodeFlagged (NodeIsScanner, _)
       | NodePhonyGlobal _
       | NodePhonyDir _
       | NodePhonyFile _
       | NodeFile _ ->
            false

   (*
    * Just the tail name.
    *)
   let rec tail node =
      match NodeHash.get node with
         NodePhonyGlobal name
       | NodePhonyDir (_, _, name)
       | NodePhonyFile (_, _, _, name)
       | NodeFile (_, _, name) ->
            name
       | NodeFlagged (_, node) ->
            tail node

   (*
    * Get the name of the directory.
    *)
   let rec dir node =
      match NodeHash.get node with
         NodePhonyGlobal _ ->
            null_root
       | NodePhonyDir (dir, _, _)
       | NodePhonyFile (dir, _, _, _)
       | NodeFile (dir, _, _) ->
            dir
       | NodeFlagged (_, node) ->
            dir node

   (*
    * Equality testing.
    *)
   let compare = NodeHash.compare
   let equal = NodeHash.equal

   (*
    * Flags.
    *)
   let marshal_flag = function
      NodeIsOptional ->
         Magic NodeIsOptionalMagic
    | NodeIsExisting ->
         Magic NodeIsExistingMagic
    | NodeIsSquashed ->
         Magic NodeIsSquashedMagic
    | NodeIsScanner ->
         Magic NodeIsScannerMagic

   let unmarshal_flag flag =
      match flag with
         Magic NodeIsOptionalMagic ->
            NodeIsOptional
       | Magic NodeIsExistingMagic ->
            NodeIsExisting
       | Magic NodeIsSquashedMagic ->
            NodeIsSquashed
       | Magic NodeIsScannerMagic ->
            NodeIsScanner
       | _ ->
            raise MarshalError

   (*
    * Marshaling.
    *)
   let rec marshal node =
      match NodeHash.get node with
         NodeFile (dir, name1, name2) ->
            List [Magic NodeFileMagic; Dir.marshal dir; FileCase.marshal name1; String name2]
       | NodePhonyGlobal s ->
            List [Magic NodePhonyGlobalMagic; String s]
       | NodePhonyDir (dir, name1, name2) ->
            List [Magic NodePhonyDirMagic; Dir.marshal dir; FileCase.marshal name1; String name2]
       | NodePhonyFile (dir, name1, name2, name3) ->
            List [Magic NodePhonyFileMagic; Dir.marshal dir; FileCase.marshal name1; String name2; String name3]
       | NodeFlagged (flag, node) ->
            List [Magic NodeFlaggedMagic; marshal_flag flag; marshal node]

   let rec unmarshal l =
      let node =
         match l with
            List [Magic NodeFileMagic; dir; name1; String name2] ->
               NodeFile (Dir.unmarshal dir, FileCase.unmarshal name1, name2)
          | List [Magic NodePhonyGlobalMagic; String s] ->
               NodePhonyGlobal s
          | List [Magic NodePhonyDirMagic; dir; name1; String name2] ->
               NodePhonyDir (Dir.unmarshal dir, FileCase.unmarshal name1, name2)
          | List [Magic NodePhonyFileMagic; dir; name1; String name2; String name3] ->
               NodePhonyFile (Dir.unmarshal dir, FileCase.unmarshal name1, name2, name3)
          | List [Magic NodeFlaggedMagic; flag; node] ->
               NodeFlagged (unmarshal_flag flag, unmarshal node)
          | _ ->
               raise MarshalError
      in
         NodeHash.create node

   (*
    * This is a hack to allow Omake_cache to take stats of directories.
    *)
   let node_of_dir dir =
      let name = "." in
         NodeHash.create (NodeFile (dir, FileCase.create dir name, name))

   (*
    * Full name is relative to the cwd.
    *)
   let fullname node =
      name (Dir.cwd ()) node

   let absname node =
      name impossible_root node

   (************************************************************************
    * Mount point handling.
    *)
   let unlink_file filename =
      try Unix.unlink filename with
         Unix.Unix_error _ ->
            ()

   let copy_file mount_info src dst =
      if mount_info.mount_is_dir src then begin
         if not (mount_info.mount_is_dir dst) then begin
            Lm_filename_util.mkdirhier (fullname dst) 0o777;
            mount_info.mount_file_reset dst
         end
      end
      else
         let src_digest = mount_info.mount_digest src in
         let dst_digest = mount_info.mount_digest dst in
            if src_digest <> dst_digest then
               let dir = dir dst in
               let mode = (mount_info.mount_stat src).Unix.LargeFile.st_perm in
                  Lm_filename_util.mkdirhier (Dir.fullname dir) 0o777;
                  Lm_unix_util.copy_file (absname src) (absname dst) mode;
                  mount_info.mount_file_reset dst

   let symlink_file_unix mount_info src dst =
      if mount_info.mount_is_dir src then begin
         if not (mount_info.mount_is_dir dst) then begin
            Lm_filename_util.mkdirhier (fullname dst) 0o777;
            mount_info.mount_file_reset dst
         end
      end
      else
         let src_digest = mount_info.mount_digest src in
         let dst_digest = mount_info.mount_digest dst in
            if src_digest <> dst_digest then
               let dir = dir dst in
               let src_name = name dir src in
               let dst_name = fullname dst in
                  Lm_filename_util.mkdirhier (Dir.fullname dir) 0o777;
                  unlink_file dst_name;
                  Unix.symlink src_name dst_name;
                  mount_info.mount_file_reset dst

   let symlink_file =
      if Sys.os_type = "Win32" then
         copy_file
      else
         symlink_file_unix

   let create_node mount_info mounts dir name =
      let { mount_file_exists = file_exists;
            mount_file_reset = reset_file
          } = mount_info
      in
      let dir, key, name = new_file dir name in
      let node = NodeHash.create (NodeFile (dir, key, name)) in
      let rec search mounts =
         match mounts with
            (dir_dst, dir_src, options) :: mounts ->
               (try
                   let node' = resolve_mount_node dir_dst dir_src node in
                      if file_exists node' then
                         if List.mem MountCopy options then begin
                            copy_file mount_info node' node;
                            node
                         end
                         else if List.mem MountLink options then begin
                            symlink_file mount_info node' node;
                            node
                         end
                         else
                            node'
                      else
                         raise Not_found
                with
                   Not_found ->
                      search mounts)
          | [] ->
               node
      in
         search mounts
end

(*
 * Intern a string with no escapes.
 * This version ignores mount points.
 * Check for existing phonies first.
 * NOTE: NodeHash.intern will not create the
 * node if it does not already exist.
 *)
let create_node_or_phony phonies mount_info mount phony_ok dir name =
   match parse_phony_name name, phony_ok with
      PhonyDirString name, PhonyOK
    | PhonyDirString name, PhonyExplicit ->
         let dir, key, name = new_file dir name in
            NodeHash.create (NodePhonyDir (dir, key, name))
    | PhonyGlobalString name, PhonyOK
    | PhonyGlobalString name, PhonyExplicit ->
         NodeHash.create (NodePhonyGlobal name)
    | PhonyDirString _, PhonyProhibited
    | PhonyGlobalString _, PhonyProhibited ->
         raise (Invalid_argument "Omake_node.Node.intern: NodePhony is not allowed");
    | PhonySimpleString, PhonyOK ->
         (* Try PhonyDir first *)
         let node = NodePhonyDir (dir, FileCase.create dir name, name) in
            if PreNodeSet.mem phonies node then
               NodeHash.create node
            else
               (* Try PhonyGlobal next *)
               let node = NodePhonyGlobal name in
                  if PreNodeSet.mem phonies node then
                     NodeHash.create node
                  else
                     Node.create_node mount_info mount dir name
    | PhonySimpleString, PhonyExplicit
    | PhonySimpleString, PhonyProhibited
    | PhonyPathString, _ ->
         Node.create_node mount_info mount dir name

(*
 * Print the directory, for debugging.
 *)
let pp_print_dir buf dir =
   let root, _, path = path_of_dir dir in
      fprintf buf "%s%s" (**)
         (Lm_filename_util.string_of_root root)
         (flatten_dir path)

(*
 * Print the kind.
 *)
let pp_print_node_kind buf kind =
   let s =
      match kind with
         NodePhony     -> "phony"
       | NodeOptional  -> "optional"
       | NodeExists    -> "exists"
       | NodeSquashed  -> "squashed"
       | NodeScanner   -> "scanner"
       | NodeNormal    -> "normal"
   in
      pp_print_string buf s

(*
 * Print the node, for debugging.
 *)
let pp_print_node buf node =
   match Node.kind node with
      NodePhony ->
         fprintf buf "<phony %s>" (Node.fullname node)
    | NodeOptional ->
         fprintf buf "<optional %s>" (Node.fullname node)
    | NodeExists ->
         fprintf buf "<exists %s>" (Node.fullname node)
    | NodeSquashed ->
         fprintf buf "<squash %s>" (Node.fullname node)
    | NodeScanner ->
         fprintf buf "<scanner %s>" (Node.fullname node)
    | NodeNormal ->
         pp_print_string buf (Node.fullname node)

let pp_print_string_list buf sources =
   List.iter (fun s -> fprintf buf "@ %s" s) (List.sort String.compare sources)

let pp_compare_nodes n1 n2 =
   let cmp = Pervasives.compare (Node.kind n1) (Node.kind n2) in
      if cmp = 0 then
         let cmp = String.compare (Node.fullname n1) (Node.fullname n2) in
            if cmp = 0 then
               NodeHash.compare n1 n2
            else
               cmp
      else
         cmp

let pp_print_node_sorted buf nodes =
   let nodes = List.sort pp_compare_nodes nodes in
      List.iter (fun node -> fprintf buf "@ %a" pp_print_node node) nodes

let pp_print_node_list buf nodes =
   List.iter (fun node -> fprintf buf "@ %a" pp_print_node node) nodes

let pp_print_node_set buf set =
   pp_print_node_list buf (NodeSet.elements set)

let pp_print_node_table buf table =
   pp_print_node_list buf (NodeTable.keys table)

let pp_print_node_set_table buf table =
   NodeTable.iter (fun node set ->
         fprintf buf "@ @[<b 3>%a:%a@]" (**)
            pp_print_node node
            pp_print_node_set set) table

let pp_print_node_set_table_opt buf table_opt =
   match table_opt with
      Some table ->
         pp_print_node_set_table buf table
    | None ->
         pp_print_string buf "<none>"


(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
