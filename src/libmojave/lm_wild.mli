(*
 * Wildcard string.
 *)
val wild_string : string

(*
 * Wildcard matching. "Incoming" patterns must have exactly one instance
 * of the pattern symbol %. "Outgoing" patterns may have any number.
 *)
type in_patt = private int * string * int * string
type out_patt =  private string list
type subst = private int * string


val pp_print_wild_in :  in_patt Lm_printf.t
val pp_print_wild_out :  out_patt Lm_printf.t

val is_wild : string -> bool
val compile_in : string -> in_patt
val compile_out : string -> out_patt


(**
   {[
     wild_match  (compile_in "xx%yyzu") "xx1234yyzu";;
     Some (4, "1234")
   ]}
*)
val wild_match : in_patt -> string -> subst option
val core : subst -> string
val of_core : string -> subst

val subst_in : subst -> in_patt -> string
val subst : subst -> out_patt -> string

