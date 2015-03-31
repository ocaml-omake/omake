(*
 * Compute the free variables of an expression.
 * NOTE: this is a little sloppy.
 *    1. The language is dynamically scoped;
 *       we don't catch variables not mentioned statically
 *    2. We take the presence of a definition anywhere
 *       as an indication that the variable is not free.
 *)



(*
 * Tables of free variables.
 *)
type free_vars = Omake_ir_util.VarInfoSet.t

let free_vars_empty = Omake_ir_util.VarInfoSet.empty

(*
 * Free variable operations.
 *)
let free_vars_add = Omake_ir_util.VarInfoSet.add
let free_vars_remove = Omake_ir_util.VarInfoSet.remove

let free_vars_remove_param_list fv params =
   List.fold_left Omake_ir_util.VarInfoSet.remove fv params

let free_vars_remove_opt_param_list fv keywords =
   List.fold_left (fun fv (_, v, _) -> Omake_ir_util.VarInfoSet.remove fv v) fv keywords
(*
 * Union of two free variable sets.
 *)
let free_vars_union fv1 fv2 =
   Omake_ir_util.VarInfoSet.fold Omake_ir_util.VarInfoSet.add fv1 fv2

(*
 * Free vars of the export.
 *)
let free_vars_export_info fv info =
  match info with
    Omake_ir.ExportNone
  | Omake_ir.ExportAll ->
    fv
  | Omake_ir.ExportList items ->
    List.fold_left (fun fv item ->
        match item with
          Omake_ir.ExportRules
        | ExportPhonies ->
          fv
        | ExportVar v ->
          Omake_ir_util.VarInfoSet.add fv v) fv items

(*
 * Free vars in optional args.
 *)
let rec free_vars_opt_params fv opt_params =
  match opt_params with
    (_, _, Some s) :: opt_params ->
    free_vars_opt_params (free_vars_string_exp fv s) opt_params
  | (_, _, None) :: opt_params ->
    free_vars_opt_params fv opt_params
  | [] ->
    fv

(*
 * Calculate free vars.
 * NOTE: this only calculates the static free variables.
 * Since the language is dynamically scoped, this will miss
 * the dynamic free variables.
 *)
and free_vars_string_exp fv s =
  match s with
  |Omake_ir.NoneString _
  | IntString _
  | FloatString _
  | WhiteString _
  | ConstString _
  | ConstStringNoMeta _
  | ThisString _
  | KeyApplyString _
  | VarString _ ->
    fv
  | FunString (_, opt_params, vars, s, export) ->
    let fv_body = free_vars_export_info free_vars_empty export in
    let fv_body = free_vars_exp_list fv_body s in
    let fv_body = free_vars_remove_param_list fv_body vars in
    let fv_body = free_vars_remove_opt_param_list fv_body opt_params in
    let fv = free_vars_union fv fv_body in
    free_vars_opt_params fv opt_params
  | ApplyString (_, v, args, kargs)
  | MethodApplyString (_, v, _, args, kargs) ->
    let fv = free_vars_string_exp_list fv args in
    let fv = free_vars_keyword_exp_list fv kargs in
    free_vars_add fv v
  | SuperApplyString (_, _, _, args, kargs) ->
    let fv = free_vars_string_exp_list fv args in
    let fv = free_vars_keyword_exp_list fv kargs in
    fv
  | SequenceString (_, sl)
  | ArrayString (_, sl)
  | QuoteString (_, sl)
  | QuoteStringString (_, _, sl) ->
    free_vars_string_exp_list fv sl
  | ArrayOfString (_, s)
  | LazyString (_, s) ->
    free_vars_string_exp fv s
  | ObjectString (_, e, export)
  | BodyString (_, e, export)
  | ExpString (_, e, export) ->
    free_vars_exp_list (free_vars_export_info fv export) e
  | CasesString (_loc, cases) ->
    free_vars_cases fv cases
  | LetVarString (_, v, e1, _e2) ->
    let fv = free_vars_string_exp fv e1 in
    let fv = free_vars_remove fv v in
    free_vars_string_exp fv e1

and free_vars_string_exp_list fv sl =
  match sl with
    s :: sl ->
    free_vars_string_exp_list (free_vars_string_exp fv s) sl
  | [] ->
    fv

and free_vars_keyword_exp_list fv sl =
  match sl with
    (_, s) :: sl ->
    free_vars_keyword_exp_list (free_vars_string_exp fv s) sl
  | [] ->
    fv

and free_vars_cases fv cases =
  match cases with
    (_, s, e, export) :: cases ->
    free_vars_cases (free_vars_string_exp (free_vars_exp_list (free_vars_export_info fv export) e) s) cases
  | [] ->
    fv

and free_vars_exp_list fv el =
  match el with
    e :: el ->
    free_vars_exp (free_vars_exp_list fv el) e
  | [] ->
    fv

and free_vars_exp fv e =
  match e with
    LetVarExp (_, v, _, _, s) ->
    let fv = free_vars_remove fv v in
    free_vars_string_exp fv s
  | LetFunExp (_, v, _, _, opt_params, vars, el, export) ->
    let fv_body = free_vars_export_info free_vars_empty export in
    let fv_body = free_vars_exp_list fv_body el in
    let fv_body = free_vars_remove_param_list fv_body vars in
    let fv_body = free_vars_remove_opt_param_list fv_body opt_params in
    let fv = free_vars_union fv fv_body in
    let fv = free_vars_remove fv v in
    free_vars_opt_params fv opt_params
  | LetObjectExp (_, v, _, s, el, export) ->
    let fv = free_vars_export_info fv export in
    let fv = free_vars_exp_list fv el in
    let fv = free_vars_remove fv v in
    let fv = free_vars_string_exp fv s in
    fv
  | IfExp (_, cases) ->
    free_vars_if_cases fv cases
  | SequenceExp (_, el) ->
    free_vars_exp_list fv el
  | SectionExp (_, s, el, export) ->
    free_vars_string_exp (free_vars_exp_list (free_vars_export_info fv export) el) s
  | StaticExp (_, _, _, el) ->
    free_vars_exp_list fv el
  | IncludeExp (_, s, sl) ->
    free_vars_string_exp (free_vars_string_exp_list fv sl) s
  | ApplyExp (_, v, args, kargs)
  | MethodApplyExp (_, v, _, args, kargs) ->
    free_vars_keyword_exp_list (free_vars_string_exp_list (free_vars_add fv v) args) kargs
  | SuperApplyExp (_, _, _, args, kargs) ->
    free_vars_keyword_exp_list (free_vars_string_exp_list fv args) kargs
  | ReturnBodyExp (_, el, _) ->
    free_vars_exp_list fv el
  | LetKeyExp (_, _, _, s)
  | LetThisExp (_, s)
  | ShellExp (_, s)
  | StringExp (_, s)
  | ReturnExp (_, s, _) ->
    free_vars_string_exp fv s
  | OpenExp _
  | KeyExp _
  | ReturnObjectExp _
  | ReturnSaveExp _ ->
    fv

and free_vars_if_cases fv cases =
  match cases with
  |      (s, e, export) :: cases ->
    free_vars_if_cases (free_vars_string_exp (free_vars_exp_list (free_vars_export_info fv export) e) s) cases
  | [] ->
    fv

(*
 * Wrapper.
 *)
let free_vars_exp e =  free_vars_exp free_vars_empty e

let free_vars_exp_list el =   free_vars_exp_list free_vars_empty el

let free_vars_set fv =   fv
