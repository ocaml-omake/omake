(*
 * Utilities for process execution on any platform.
 *
 *)





(*
 * Debugging.
 *)
val debug_exec : bool ref

(*
 * File descriptor table.
 *)
module IntTable : Lm_map_sig.LmMap with type key = int;;
module FdTable : Lm_map_sig.LmMap with type key = Unix.file_descr;;

(*
 * Open a pipe.  Close automatically on exceptions.
 *)
val with_pipe : (Unix.file_descr -> Unix.file_descr -> 'a) -> 'a


(*
 * Copy data to a file.
 *)
val copy_file : string -> (Omake_exec_id.t -> string -> int -> int -> unit)

(*
 * Tee to a file.
 *)
type tee

val tee_none        : tee
val tee_create      : bool -> tee
val tee_close       : tee -> unit
val tee_file        : tee -> string option
val tee_stdout      : tee -> bool -> Omake_exec_id.t -> string -> int -> int -> unit
val tee_stderr      : tee -> bool -> Omake_exec_id.t -> string -> int -> int -> unit
val tee_file_descr  : tee -> Unix.file_descr option


