(** This is the base part of the build system.
   Each file in the system is represented as a node.
   Node may be virtual: the node may exist before the file
   does.  For each file, we maintain stat and MD5 information
   (if they exist).
    This case [in-]sensitivity of file names is a complex issue.
    We make the type abstract so we don't make a mistake.
 *)

module Dir : Omake_node_sig.DirSig

module DirSet : Lm_set_sig.LmSet with type elt = Dir.t

module DirTable : Lm_map_sig.LmMap with type key = Dir.t

module DirListHash : Lm_hash.HashMarshalSig with type elt = Dir.t list
module DirListSet   : Lm_set_sig.LmSet with type elt = DirListHash.t
module DirListTable : Lm_map_sig.LmMap with type key = DirListHash.t

module Node : Omake_node_sig.NodeSig with type dir = Dir.t
module NodeSet : Lm_set_sig.LmSet with type elt = Node.t
module NodeTable : Lm_map_sig.LmMap with type key = Node.t
module NodeMTable : Lm_map_sig.LmMapList with type key = Node.t

module PreNodeSet : Lm_set_sig.LmSet with type elt = Node.pre

module Mount : Omake_node_sig.MountSig
  with type dir = Dir.t
  with type node = Node.t
  with type t = Node.mount;;

type mount_info = Node.t Omake_node_sig.poly_mount_info

val no_mount_info : mount_info


(*
 * Handle known phonies.
 *)
val create_node_or_phony : PreNodeSet.t -> mount_info -> Mount.t -> 
  Omake_node_sig.phony_ok -> Dir.t -> string -> Node.t

(* Same, but factor out the sometimes slow parser for phony names: *)
type phony_name
val parse_phony_name : string -> phony_name  (* failsafe *)
val create_node_or_phony_1 : PreNodeSet.t -> mount_info -> Mount.t -> 
  Omake_node_sig.phony_ok -> Dir.t -> phony_name -> Node.t

val node_will_be_phony : PreNodeSet.t -> Omake_node_sig.phony_ok -> Dir.t -> phony_name -> bool 


(*
 * For debugging.
 *)
val pp_print_dir  : Dir.t Lm_printf.t 
val pp_print_node : Node.t Lm_printf.t 
val pp_print_node_kind : Omake_node_sig.node_kind Lm_printf.t 

val pp_print_string_list : string list Lm_printf.t 
val pp_print_node_set : NodeSet.t Lm_printf.t 
val pp_print_node_list : Node.t list Lm_printf.t 
val pp_print_node_table : 'a NodeTable.t Lm_printf.t 
val pp_print_node_set_table : NodeSet.t NodeTable.t Lm_printf.t
val pp_print_node_set_table_opt : NodeSet.t NodeTable.t option Lm_printf.t

