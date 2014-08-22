(*
 * Abstract representation of files.
 *)

open! Lm_hash
open Lm_printf
open! Lm_hash_sig

open Omake_node_sig

module Dir : DirSig
module DirSet : Lm_set_sig.LmSet with type elt = Dir.t
module DirTable : Lm_map_sig.LmMap with type key = Dir.t

module DirListHash : HashMarshalSig with type elt = Dir.t list
module DirListSet   : Lm_set_sig.LmSet with type elt = DirListHash.t
module DirListTable : Lm_map_sig.LmMap with type key = DirListHash.t

module Node : NodeSig with type dir = Dir.t
module NodeSet : Lm_set_sig.LmSet with type elt = Node.t
module NodeTable : Lm_map_sig.LmMap with type key = Node.t
module NodeMTable : Lm_map_sig.LmMapList with type key = Node.t

module PreNodeSet : Lm_set_sig.LmSet with type elt = Node.pre

module Mount
: MountSig
  with type dir = Dir.t
  with type node = Node.t
  with type t = Node.mount;;

type mount_info = Node.t poly_mount_info

val no_mount_info : mount_info

(*
 * Handle known phonies.
 *)
val create_node_or_phony : PreNodeSet.t -> mount_info -> Mount.t -> phony_ok -> Dir.t -> string -> Node.t

(*
 * For debugging.
 *)
val pp_print_dir  : formatter -> Dir.t -> unit
val pp_print_node : formatter -> Node.t -> unit
val pp_print_node_kind : formatter -> node_kind -> unit

val pp_print_string_list : formatter -> string list -> unit
val pp_print_node_set : formatter -> NodeSet.t -> unit
val pp_print_node_list : formatter -> Node.t list -> unit
val pp_print_node_table : formatter -> 'a NodeTable.t -> unit
val pp_print_node_set_table : formatter -> NodeSet.t NodeTable.t -> unit
val pp_print_node_set_table_opt : formatter -> NodeSet.t NodeTable.t option -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
