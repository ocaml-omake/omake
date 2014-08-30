(*  Types used to represent commands and the cache. *)




open Omake_node
open Omake_value_type

(* %%MAGICBEGIN%% *)
(*
 * File digest is an option, in case the file does not exist.
 *)
type digest = Digest.t option

(*
 * The memo result is used only for the scanner,
 * whice produces a table of dependencies.
 *)
type 'a memo_result =
   MemoFailure of int
 | MemoSuccess of 'a

type memo_deps = NodeSet.t NodeTable.t

type memo_deps_result = memo_deps memo_result

type memo_obj_result = obj memo_result

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
   NodeEntry of Node.t
 | DirEntry of Dir.t
(* %%MAGICEND%% *)

