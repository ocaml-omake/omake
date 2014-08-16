

open Omake_ast

val loc_of_exp : exp -> Lm_location.loc
val key_of_exp : exp -> string
val scan_body_flag : body_flag -> exp -> body_flag
val update_body : exp -> body_flag -> exp list -> exp
val can_continue : exp -> string option
val flatten_sequence_prog : prog -> prog
val flatten_string_prog   : prog -> prog

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
