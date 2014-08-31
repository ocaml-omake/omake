(*  Cache information about past runs. *)


(* Debugging *)
val debug_cache : bool ref

(* The abstract type for the cache database *)
type t

(* Keys for various functions *)
type key

(* Directory listing *)
type dir_listing
type exe_listing

(* Manifest keys *)
val scanner_fun         : key
val rule_fun            : key
val env_fun             : key
val include_fun         : key

val env_target  : Omake_node.Node.t

(* Fetch the cache *)
val create       : unit -> t
val from_channel : Omake_options.t -> in_channel -> t
val to_channel   : Pervasives.out_channel -> t -> unit

(*
 * Stats.
 *)
val stats : t -> int * int

(*
 * File digest information.
 * The reset functions indicate that stat info may need to be recomputed.
 * The stat functions get the digest.
 * The ?force flag, if set, forces the restat in case we cached a _negative_
 * information on the file (file did not exist before).
 *)
val stat             : t -> Omake_node.Node.t -> Omake_cache_type.digest
val stat_set         : t -> Omake_node.NodeSet.t -> Omake_cache_type.digest Omake_node.NodeTable.t
val stat_table       : t -> 'b Omake_node.NodeTable.t -> Omake_cache_type.digest Omake_node.NodeTable.t
val stat_unix        : t -> ?force:bool -> ?follow_symlinks:bool -> Omake_node.Node.t -> Unix.LargeFile.stats
val is_dir           : t -> ?force:bool -> ?follow_symlinks:bool -> Omake_node.Node.t -> bool

val reset            : t -> Omake_node.Node.t -> unit
val reset_set        : t -> Omake_node.NodeSet.t -> unit
val reset_table      : t -> 'b Omake_node.NodeTable.t -> unit

val force_stat       : t -> Omake_node.Node.t -> Omake_cache_type.digest
val force_stat_set   : t -> Omake_node.NodeSet.t -> Omake_cache_type.digest Omake_node.NodeTable.t
val force_stat_table : t -> 'b Omake_node.NodeTable.t -> Omake_cache_type.digest Omake_node.NodeTable.t

val stat_changed     : t -> Omake_node.Node.t -> bool

(*
 * Check if a file exists.
 *)
val exists           : t -> ?force:bool -> Omake_node.Node.t -> bool
val exists_dir       : t -> ?force:bool -> Omake_node.Dir.t -> bool

(*
 * Directory listings.  The Boolean in ls_path and ls_exe_path should
 * be true for those directory collections where auto-rehashing is to
 * be performed.
 *)
val rehash           : t -> unit

val ls_dir           : t -> bool -> Omake_node.Dir.t -> dir_listing
val ls_path          : t -> (bool * Omake_node.Dir.t list) list -> dir_listing
val listing_find     : t -> dir_listing -> string -> Omake_cache_type.dir_entry

val ls_exe_path      : t -> (bool * Omake_node.Dir.t list) list -> exe_listing
val exe_find         : t -> exe_listing -> string -> Omake_node.Node.t
val exe_find_all     : t -> exe_listing -> string -> Omake_node.Node.t list
val exe_complete     : t -> exe_listing -> string -> Lm_string_set.StringSet.t

val exe_suffixes     : string list

(*
 * Memoizing commands.
 *
 * add                : key targets sources commands result
 * up_to_date         : key sources commands
 * find_result        : key sources commands
 * find_result_sloppy : key target
 *
 * The sloppy function just returns the results for the target
 * without even considering the command and dependencies.
 *)
val add                : 
  t -> key -> Omake_node.Node.t -> Omake_node.NodeSet.t -> 
  Omake_node.NodeSet.t -> Omake_command_type.command_digest -> Omake_cache_type.memo_deps_result -> unit
val up_to_date         : t -> key -> Omake_node.NodeSet.t -> Omake_command_type.command_digest -> bool
val up_to_date_status  : t -> key -> Omake_node.NodeSet.t -> Omake_command_type.command_digest -> Omake_cache_type.memo_status
val find_result        : t -> key -> Omake_node.NodeSet.t -> Omake_command_type.command_digest -> Omake_cache_type.memo_deps
val find_result_sloppy : t -> key -> Omake_node.Node.t -> Omake_cache_type.memo_deps

(*
 * Similar functions for values. The bool flag indicates whether we want a static value.
 *)
val find_value : 
  t -> Omake_value_type.value -> bool -> Omake_node.NodeSet.t -> 
  Omake_command_type.command_digest -> Omake_value_type.obj

val add_value : 
  t -> Omake_value_type.value -> bool -> Omake_node.NodeSet.t -> Omake_command_type.command_digest -> Omake_cache_type.memo_obj_result -> unit

(*
 * Printing.
 *)
val pp_print_digest            : Omake_cache_type.digest Lm_printf.t 
val pp_print_node_digest_table : Omake_cache_type.digest Omake_node.NodeTable.t Lm_printf.t

val pp_print_memo_result       : 
  Omake_node.NodeSet.t Omake_node.NodeTable.t Lm_printf.t ->  Omake_cache_type.memo_deps_result Lm_printf.t
