(*
 * Compute the free variables of an expression.
 * NOTE: this is a little sloppy.
 *    1. The language is dynamically scoped;
 *       we don't catch variables not mentioned statically
 *    2. We take the presence of a definition anywhere
 *       as an indication that the variable is not free.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2007 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol

open Omake_ir

(*
 * Tables of free variables.
 *)
type free_vars = VarInfoSet.t

let free_vars_empty = VarInfoSet.empty

(*
 * Free variable operations.
 *)
let free_vars_add = VarInfoSet.add
let free_vars_remove = VarInfoSet.remove

let free_vars_remove_param_list fv params =
   List.fold_left VarInfoSet.remove fv params

let free_vars_remove_opt_param_list fv keywords =
   List.fold_left (fun fv (_, v, _) -> VarInfoSet.remove fv v) fv keywords
(*
 * Union of two free variable sets.
 *)
let free_vars_union fv1 fv2 =
   VarInfoSet.fold VarInfoSet.add fv1 fv2

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
                  ExportRules
                | ExportPhonies ->
                     fv
                | ExportVar v ->
                     VarInfoSet.add fv v) fv items

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
      NoneString _
    | IntString _
    | FloatString _
    | WhiteString _
    | ConstString _
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
    | CasesString (loc, cases) ->
         free_vars_cases fv cases
    | LetVarString (_, v, e1, e2) ->
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
      (s, e, export) :: cases ->
         free_vars_if_cases (free_vars_string_exp (free_vars_exp_list (free_vars_export_info fv export) e) s) cases
    | [] ->
         fv

(*
 * Wrapper.
 *)
let free_vars_exp e =
   free_vars_exp free_vars_empty e

let free_vars_exp_list el =
   free_vars_exp_list free_vars_empty el

let free_vars_set fv =
   fv

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
