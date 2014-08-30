open Lm_printf

(*
 * Exceptions that should be treated as command failures.
 *)
val is_shell_exn : exn -> bool

(*
 * Print an exception.
 *)
val pp_print_exn : formatter -> exn -> unit

(*
 * Generic catcher.
 *)
val catch : ('a -> 'b) -> 'a -> 'b

