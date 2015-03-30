(* Utilities for program execution. *)


include Omake_exec_type.ExecServer

val likely_blocking : ('exp, 'pid, 'value) t -> bool
