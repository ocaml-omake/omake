(*  Calculate the static free variables of an expression. *)


open Omake_ir

type free_vars

val free_vars_exp      : exp -> free_vars
val free_vars_exp_list : exp list -> free_vars

(*
 * Operations on free var sets.
 *)
val free_vars_empty : free_vars
val free_vars_union : free_vars -> free_vars -> free_vars
val free_vars_set   : free_vars -> VarInfoSet.t
