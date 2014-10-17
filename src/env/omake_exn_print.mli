

(*
 * Exceptions that should be treated as command failures.
 *)
val is_shell_exn : exn -> bool

(*
 * Print an exception.
 *)
val pp_print_exn : exn Lm_printf.t 
val pp_print_exn_with_backtrace : backtrace:string -> exn Lm_printf.t 


(*
 * Generic catcher.
 *)
val catch : ('a -> 'b) -> 'a -> 'b

