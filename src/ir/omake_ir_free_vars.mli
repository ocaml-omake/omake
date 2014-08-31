(*  Calculate the static free variables of an expression. *)


type free_vars

val free_vars_exp      : Omake_ir.exp -> free_vars
val free_vars_exp_list : Omake_ir.exp list -> free_vars

(*
 * Operations on free var sets.
 *)
val free_vars_empty : free_vars
val free_vars_union : free_vars -> free_vars -> free_vars
val free_vars_set   : free_vars -> Omake_ir_util.VarInfoSet.t
