(*
 * Types of internal representations of nodes.
 *)


(*
 * Internally, we represent pathnames as absolute paths.
 * We keep a hashed integer for quick equality testing.
 *    dir_hash : the quick hash
 *    dir_root : the root of this name
 *    dir_key  : the path in canonical form (lowercase on Windows)
 *    dir_name : the actual path will full capitalization
 *)
type dir =
  |  DirRoot of int * Lm_filename_util.root
  | DirSub of int * string * string * dir

(*
 * Possible node flags.
 *)
type node_flag =
   NodeIsOptional
 | NodeIsExisting
 | NodeIsSquashed
 | NodeIsScanner

(*
 * A node is a phony, or it is a filename.
 *)
type node =
   NodeFile        of int * dir * string * string
 | NodePhonyGlobal of int * string
 | NodePhonyDir    of int * dir * string * string
 | NodePhonyFile   of int * dir * string * string * string
 | NodeFlagged     of node_flag * node
