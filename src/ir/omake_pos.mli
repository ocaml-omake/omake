

open Omake_value_type

(* type pos = Omake_value_type.pos *)

val pp_print_exn : omake_error Lm_printf.t 

module MakePos (Name : sig val name : string end) : PosSig

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
