(* Utilities for program execution. *)


val debug_remote : bool ref

(* The main function, if invoked on a remote machine. *)
val main : ('exp, 'pid, 'value) Omake_exec_type.shell -> Omake_options.t -> unit

(* The interface to the remote server. *)
include Omake_exec_type.ExecServer

