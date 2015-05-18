(*  Types used to represent commands and the cache. *)

(* %%MAGICBEGIN%% *)
(*
 * File digest is an option, in case the file does not exist.
 * The left string is a compactified version of Unix.stat. The right
 * half is the MD5 digest.
 *)
type digest = (string * Digest.t) option

(*
 * The memo result is used only for the scanner,
 * whice produces a table of dependencies.
 *)
type 'a memo_result =
   MemoFailure of int
 | MemoSuccess of 'a

type memo_deps = Omake_node.NodeSet.t Omake_node.NodeTable.t

type memo_deps_result = memo_deps memo_result

type memo_obj_result = Omake_value_type.obj memo_result

(*
 * Status query.
 *)
type memo_status =
   StatusSuccess
 | StatusFailure of int
 | StatusUnknown

(*
 * A directory entry is a node or directory.
 *)
type dir_entry =
   NodeEntry of Omake_node.Node.t
 | DirEntry of Omake_node.Dir.t
(* %%MAGICEND%% *)

