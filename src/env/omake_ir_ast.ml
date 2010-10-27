(*
 * Convert the AST to an IR representation.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology and
 * HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_location
open Lm_symbol
open Lm_lexer

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_node
open Omake_symbol
open Omake_options
open Omake_ir_util
open Omake_node_sig
open Omake_ir_print
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_ir_ast" end)
open Pos;;

(************************************************************************
 * Variable checking.
 *)
let raise_var_def_error pos info1 info2 =
   let print_error buf =
      let loc, _ = var_of_var_info info2 in
         fprintf buf "@[<v 3>Variable declaration mismatch:@ variable@    %a@ is already defined as@    %a@    %a@]" (**)
            pp_print_var_info info1
            pp_print_var_info info2
            pp_print_location loc
   in
      raise (OmakeException (pos, LazyError print_error))

let check_vars pos info1 info2 =
   match info1, info2 with
      VarPrivate (_, v1), VarPrivate (_, v2) ->
         if not (Lm_symbol.eq v1 v2) then
            raise_var_def_error pos info1 info2
    | VarThis (_, v1), VarThis (_, v2) ->
         if not (Lm_symbol.eq v1 v2) then
            raise_var_def_error pos info1 info2
    | VarVirtual (_, v1), VarVirtual (_, v2) ->
         if not (Lm_symbol.eq v1 v2) then
            raise_var_def_error pos info1 info2
    | VarGlobal (_, v1), VarGlobal (_, v2) ->
         if not (Lm_symbol.eq v1 v2) then
            raise_var_def_error pos info1 info2
    | _ ->
         ()

(************************************************************************
 * Declaration checking.
 *)

(*
 * Forced variables.
 *)
module type ForcedVarsSig =
sig
   type t

   val empty         : t
   val mem           : t -> var -> bool
   val add_var       : t -> pos -> var -> var_info -> t
   val add_param     : t -> pos -> var -> var_info -> t
   val add_extern    : t -> var -> var_info -> t
   val find_var      : t -> var -> var_info
   val fold_var      : ('a -> var -> var_info -> 'a) -> 'a -> t -> 'a
   val to_vars       : t -> senv
end;;

module ForcedVars : ForcedVarsSig =
struct
   type forced_info =
      (* Variable in this file *)
      ForcedVar of var_info

      (* Variable defined outside this file (usually builtin) *)
    | ForcedExtern of var_info

   type t = forced_info SymbolTable.t

   let empty = SymbolTable.empty

   let mem = SymbolTable.mem

   let find_var env v =
      match SymbolTable.find env v with
         ForcedVar info
       | ForcedExtern info ->
            info

   let find_local_var env v =
      match SymbolTable.find env v with
         ForcedVar info ->
            info
       | ForcedExtern info ->
            raise Not_found

   let add_param env pos v info =
      SymbolTable.add env v (ForcedVar info)

   let add_var env pos v info =
      SymbolTable.add env v (ForcedVar info)

   let add_extern env v info =
      SymbolTable.add env v (ForcedExtern info)

   let fold_var f x env =
      SymbolTable.fold (fun x v info ->
            match info with
             | ForcedExtern _ ->
                  x
             | ForcedVar info ->
                  f x v info) x env

   (*
    * Extract the set of vars so they can be re-used.
    *)
   let to_vars env =
      fold_var SymbolTable.add SymbolTable.empty env
end;;

(*
 * Some values get checked against corresponding definitions.
 *)
module type AllVarsSig =
sig
   type t

   val empty  : t
   val add    : t -> simple_var_info -> var_info -> t
   val find   : t -> simple_var_info -> var_info
   val iter   : (simple_var_info -> var_info -> unit) -> t -> unit
end;;

module AllVars : AllVarsSig =
struct
   type t = var_info SimpleVarTable.t

   (* Inherit *)
   let empty = SimpleVarTable.empty
   let find = SimpleVarTable.find
   let iter = SimpleVarTable.iter
   let add  = SimpleVarTable.add
end;;

(************************************************************************
 * Types.
 *)
type ast_exp = Omake_ast.exp

(*
 * Environment for parsing AST files.
 *)
type senv_open_file  = string -> pos -> loc -> Node.t * senv

(*
 * Are we toplevel, or in an object, or a function.
 *)
type context =
   ContextTop
 | ContextFunction of return_id
 | ContextRule
 | ContextObject

(*
 * What to export from the current section.
 *)
type export_mode =
   ExportNoneMode
 | ExportAllMode  of loc
 | ExportListMode of loc * export_item list

(*
 * Context environment.  This is strictly scoped,
 * and not affected by exports.
 *)
type cenv =
   { (* The forcing mode, if there is one *)
     cenv_scope          : var_scope option;

     (* Are we in an object, or a function, or toplevel? *)
     cenv_context        : context;
   }

(*
 * Scoping environment.
 * The values here are affected by exports, but otherwise
 * respect scoping/indentation.
 *)
type senv =
   { (* Forced, non-private definitions in the current object/file *)
     senv_object_senv    : Omake_ir.senv;

     (* The variables that we know about *)
     senv_update_vars    : Omake_ir.senv;
     senv_forced_vars    : ForcedVars.t;
     senv_all_vars       : AllVars.t;

     (* What sections are exporting *)
     senv_export_mode    : export_mode;

     (* The current environment *)
     senv_venv           : venv
   }

(*
 * "Object" environment.
 *
 * This is an environment that is global to each object.
 * That is, each object gets a fresh environment.
 *)
type oenv =
   { (* Class names for the current object *)
     oenv_class_names     : SymbolSet.t;
   }

(*
 * "Lazy" environment.
 *
 * This environment is for collecting eager expressions
 * inside a lazy block.
 *)
type genv_lazy =
   { genv_lazy_mode       : bool;
     genv_lazy_values     : (loc * string_exp) SymbolTable.t;
   }

(*
 * Global environment.
 *)
type genv =
   { (* How to open a file *)
     genv_open_file         : senv_open_file;

     (* The name of this file *)
     genv_file              : Node.t;

     (* Index of the next static section *)
     genv_static_index      : int;

     (* Count up the number of warnings *)
     genv_warning_count     : int;

     (* Lazy mode *)
     genv_lazy              : genv_lazy
   }

(*
 * Parsing environment has a set of environments.
 *)
type penv = genv * oenv * senv * cenv

(*
 * Static results.  We use this to keep track of what is being
 * exported.
 *
 *    ValValue: a value, or something besides an export
 *    ValExport: exporting some variables
 *    ValNotReached: the statement is never executed
 *       (because it is after a return statement)
 *)
type value =
   ValValue
 | ValNotReached
 | ValExport of var_info SymbolTable.t

(*
 * Parameters.
 *)
type param_info =
   RequiredParam
 | NormalParam
 | OptionalParam of string_exp

type param = var * param_info * loc

(************************************************************************
 * Name info.
 *)
let empty_name_info =
   { name_static     = false;
     name_scope      = None;
     name_curry      = false
   }

let is_nonempty_name_info info =
   info <> empty_name_info

(*
 * Empty object environment.
 *)
let oenv_empty =
   { oenv_class_names   = SymbolSet.empty;
   }

(*
 * Empty lazy environment.
 *)
let lazy_empty =
   { genv_lazy_mode   = false;
     genv_lazy_values = SymbolTable.empty
   }

let lazy_env =
   { genv_lazy_mode   = true;
     genv_lazy_values = SymbolTable.empty
   }

(************************************************************************
 * Utilities.
 *)

(*
 * Add a warning.
 *)
let genv_add_warning genv =
   { genv with genv_warning_count = succ genv.genv_warning_count }

let genv_warn_error genv senv =
   let { genv_file = file;
         genv_warning_count = count
       } = genv
   in
      if count <> 0 && opt_warn_error (venv_options senv.senv_venv) then
         let filename = Node.absname file in
         let loc = Lm_location.bogus_loc filename in
            raise (OmakeException (loc_exp_pos loc, StringIntError ("warnings treated as errors", count)))

(************************************************************************
 * Utilities.
 *)

(*
 * In a nested object, all currently protected vars become private.
 *)
let nested_object_vars vars =
   SymbolTable.map (function
      VarThis (loc, v) ->
         VarPrivate (loc, v)
    | x ->
         x) vars

(*
 * Collect the cases in a conditional.
 *)
let rec collect_if cases el =
   match el with
      Omake_ast.CommandExp (v, e, body, loc) :: el when Lm_symbol.eq v elseif_sym ->
         collect_if ((e, body) :: cases) el
    | Omake_ast.CommandExp (v, e, body, loc) :: el when Lm_symbol.eq v else_sym ->
         let cases = (Omake_ast.StringOtherExp ("true", loc), body) :: cases in
            List.rev cases, el
    | _ ->
         List.rev cases, el

let is_true_string s =
   match s with
      Omake_ast.StringOtherExp ("true", _) ->
         true
    | _ ->
         false

(*
 * Generic case collection.
 *)
let rec collect_cases cases el =
   match el with
      (Omake_ast.CommandExp (v, e, body, _) :: el) when SymbolSet.mem clauses_set v ->
         collect_cases ((v, e, body) :: cases) el
    | (Omake_ast.CatchExp (v1, v2, body, loc) :: el) ->
         collect_cases ((v1, Omake_ast.StringOtherExp (Lm_symbol.to_string v2, loc), body) :: cases) el
    | _ ->
         List.rev cases, el

(*
 * Extract an option.
 *)
let extract_option loc map key =
   try
      let x = SymbolTable.find map key in
      let map = SymbolTable.remove map key in
         x, map
   with
      Not_found ->
         Omake_ast.NullExp loc, map

let build_bool_exp loc b =
   ConstString (loc, if b then "true" else "false")

(************************************************************************
 * Environments.
 *)

(*
 * Simple variables.
 *)
let var_scope_of_var_info = function
   VarPrivate _ ->
      VarScopePrivate
 | VarThis _ ->
      VarScopeThis
 | VarVirtual _ ->
      VarScopeVirtual
 | VarGlobal _ ->
      VarScopeGlobal

(*
 * What is the actual mode of a variable in a context?
 *)
let cenv_var_scope cenv info =
   let scope =
      match cenv.cenv_context, info.name_scope with
         ContextTop, None
       | ContextFunction _, None
       | ContextRule, None
       | _, Some VarScopeVirtual ->
            VarScopeVirtual
       | ContextObject, None
       | _, Some VarScopeThis ->
            VarScopeThis
       | _, Some VarScopePrivate ->
            VarScopePrivate
       | _, Some VarScopeGlobal ->
            VarScopeGlobal
   in
      scope

(*
 * Force the scope.
 *)
let cenv_update_scope cenv info =
   let scope =
      match info.name_scope with
         None -> cenv.cenv_scope
       | scope -> scope
   in
      { info with name_scope = scope;
      }

let cenv_scope cenv =
   let { cenv_scope = scope;
       } = cenv
   in
      { name_scope  = scope;
        name_curry  = false;
        name_static = false
      }

let cenv_force_scope cenv info =
   let info = cenv_update_scope cenv info in
      { cenv with cenv_scope = info.name_scope }

let cenv_fun_scope cenv id =
   { (* cenv with *) cenv_scope        = None;
                     cenv_context      = ContextFunction id
   }

let cenv_rule_scope cenv =
   { (* cenv with *) cenv_scope        = None;
                     cenv_context      = ContextRule
   }

let cenv_sequence_scope cenv el =
   cenv

let cenv_return_id cenv pos loc =
   match cenv.cenv_context with
      ContextFunction id ->
         id
    | ContextTop
    | ContextRule
    | ContextObject ->
         raise (OmakeException (loc_pos loc pos, StringError "misplaced return statement"))

let new_return_id loc v =
   let v = Lm_list_util.last v in
      loc, Lm_symbol.to_string v

(*
 * Get a new static symbol.
 *)
let genv_new_index genv =
   let index = genv.genv_static_index in
   let genv = { genv with genv_static_index = succ index } in
      genv, index

let genv_new_symbol_string name genv =
   let genv, index = genv_new_index genv in
   let v = Lm_symbol.make name index in
      genv, v

let genv_new_static_id = genv_new_symbol_string "static"

(*
 * Create the IR, and reset the env.
 *)
let genv_close genv oenv senv e =
   let vars = ForcedVars.to_vars senv.senv_forced_vars in
   let ir =
      { ir_classnames   = SymbolSet.to_list oenv.oenv_class_names;
        ir_vars         = vars;
        ir_exp          = e
      }
   in
      genv, oenv, senv, ir

(*
 * Create the environment when we enter a new object from a virtual object.
 *)
let senv_object_body senv cenv v =
   let senv =
      { senv with senv_object_senv  = SymbolTable.empty;
                  senv_update_vars  = SymbolTable.empty
      }
   in
   let cenv =
      { (* cenv with *) cenv_scope   = Some VarScopeThis;
                        cenv_context = ContextObject
      }
   in
      senv, cenv

let senv_static_body = senv_object_body

(************************************************************************
 * Variable exporting.
 *)

(*
 * Add all the vars to the parent environment.
 * Add only those vars we don't already know about.
 *)
let senv_add_export_all pos senv1 forced_vars2 =
   let pos = string_pos "senv_add_export_all" pos in
   let { senv_object_senv = object_senv1;
         senv_update_vars = update_vars1;
         senv_forced_vars = forced_vars1;
         senv_all_vars    = all_vars1
       } = senv1
   in

   (* Don't export the private forced vars *)
   let object_senv, update_vars, forced_vars, all_vars =
      SymbolTable.fold (fun (object_senv, update_vars, forced_vars, all_vars) v info2 ->
            try
               let info1 = ForcedVars.find_var forced_vars v in
                  check_vars pos info2 info1;
                  object_senv, update_vars, forced_vars, all_vars
            with
               Not_found ->
                  match info2 with
                     VarPrivate _
                   | VarThis _
                   | VarVirtual _
                   | VarGlobal _ ->
                        let object_senv = SymbolTable.add object_senv v info2 in
                        let update_vars = SymbolTable.add update_vars v info2 in
                        let forced_vars = ForcedVars.add_var forced_vars pos v info2 in
                        let all_vars    = AllVars.add all_vars (var_scope_of_var_info info2, v) info2 in
                           object_senv, update_vars, forced_vars, all_vars) (**)
         (object_senv1, update_vars1, forced_vars1, all_vars1) forced_vars2
   in
      { senv1 with senv_object_senv = object_senv;
                   senv_update_vars = update_vars;
                   senv_forced_vars = forced_vars;
                   senv_all_vars    = all_vars
      }

(*
 * Merge the exports from several cases.
 * Export all the vars, but issue warnings if the exports exist in
 * some, but not the others.
 *)
let var_union _ _ _ =
   raise (Invalid_argument "Omake_ir_ast.senv_merge_forced_vars: internal error")

let rec senv_merge_forced_vars pos export1 exports errors =
   let pos = string_pos "senv_merge_forced_vars" pos in
   match exports with
      export2 :: exports ->
         (* Check that all exports from export1 match up in export2 *)
         let export2, errors =
            SymbolTable.fold (fun (export2, errors) v info1 ->
                  try
                     let info2 = SymbolTable.find export2 v in
                        check_vars pos info1 info2;
                        SymbolTable.remove export2 v, errors
                  with
                     Not_found ->
                        export2, SymbolTable.add errors v info1) (export2, errors) export1
         in

         (* All remaining variables in export2 are errors *)
         let errors = SymbolTable.union var_union errors export2 in
         let export1 = SymbolTable.union var_union export1 export2 in
            senv_merge_forced_vars pos export1 exports errors
    | [] ->
         export1, errors

(*
 * Print the error messages for any variables that are not
 * already defined.
 *)
let warned_solution = ref false

let senv_warn_merge_errors genv senv loc errors =
   let forced_vars = senv.senv_forced_vars in
   let errors =
      SymbolTable.fold (fun errors v info ->
            match info with
               VarPrivate _ ->
                  errors
             | _ ->
                  if ForcedVars.mem forced_vars v then
                     errors
                  else
                     info :: errors) [] errors
   in
      if errors <> [] && opt_warn_declare (venv_options senv.senv_venv) then begin
         eprintf "@[<v 3>%a" pp_print_location loc;
         if not !warned_solution then begin
            eprintf "@ The following variables are exported in some cases, but not others.";
            eprintf "@ @[<v 3>Declare or define these variables if you want to avoid this warning.";
            warned_solution := true
         end
         else
            eprintf "@ @[<v 3>The following variables are exported in some cases, but not others.";
         List.iter (fun info -> eprintf "@ %a" pp_print_var_info info) errors;
         eprintf "@]@]@."
      end

(*
 * Merge the exports from the cases.
 * At least one of the cases is an export.
 * If they are not all exports, then check that the variables
 * being exported are already declared.
 *)
let senv_merge_exports genv senv1 cenv pos loc results is_complete =
   let pos = string_pos "senv_merge_exports" pos in
   let is_complete, exports =
      List.fold_left (fun (is_complete, exports) result ->
            match result with
               ValValue
             | ValNotReached ->
                  false, exports
             | ValExport forced_vars ->
                  is_complete, forced_vars :: exports) (is_complete, []) results
   in
   let export1, exports =
      if is_complete then
         match exports with
            export1 :: exports ->
               export1, exports
          | [] ->
               raise (Invalid_argument "senv_merge_exports: empty exports")
      else
         SymbolTable.empty, exports
   in
   let exports, errors = senv_merge_forced_vars pos export1 exports SymbolTable.empty in
   let () = senv_warn_merge_errors genv senv1 loc errors in
   let senv = senv_add_export_all pos senv1 exports in
      senv, ValValue

(*
 * Merge the exports only if the results contain an export.
 *)
let all_results_are_not_reached results =
   List.for_all (fun result -> result = ValNotReached) results

let all_results_are_values_or_not_reached results =
   List.for_all (fun result ->
         match result with
            ValValue
          | ValNotReached ->
               true
          | ValExport _ ->
               false) results

let senv_merge_results genv senv1 cenv pos loc results is_complete =
   let pos = string_pos "senv_merge_results" pos in
      if is_complete && all_results_are_not_reached results then
         senv1, ValNotReached
      else if all_results_are_values_or_not_reached results then
         senv1, ValValue
      else
         senv_merge_exports genv senv1 cenv pos loc results is_complete

(*
 * Merge the exports from an inner section.
 *)
let senv_export_section senv pos result =
   let pos = string_pos "senv_export_section" pos in
      match result with
         ValExport exports ->
            senv_add_export_all pos senv exports, ValValue
       | ValValue
       | ValNotReached ->
            senv, result

(*
 * Warn if a statement is not reached.
 *)
let senv_warn_not_reached genv e result =
   match result with
      ValNotReached ->
         let loc = Omake_ast_util.loc_of_exp e in
            eprintf "@[<v 3>*** omake warning: %a@ statement not reached@]@." pp_print_location loc;
            genv_add_warning genv
    | ValValue
    | ValExport _ ->
         genv

(*
 * Get the export vars.
 *)
let senv_export_all_vars senv =
   ForcedVars.fold_var (fun forced_vars v info ->
         match info with
          | VarPrivate _
          | VarThis _
          | VarVirtual _
          | VarGlobal _ ->
               SymbolTable.add forced_vars v info) SymbolTable.empty senv.senv_forced_vars

let senv_export_var_list items =
   List.fold_left (fun vars item ->
         match item with
            ExportRules
          | ExportPhonies ->
               vars
          | ExportVar info ->
               let _, v = var_of_var_info info in
                  SymbolTable.add vars v info) SymbolTable.empty items

let senv_export_value senv info =
   match info with
      Omake_ir.ExportNone ->
         SymbolTable.empty
    | Omake_ir.ExportAll ->
         senv_export_all_vars senv
    | Omake_ir.ExportList items ->
         senv_export_var_list items

(*
 * Items from the export mode.
 *)
let items_of_export_mode = function
   ExportNoneMode
 | ExportAllMode _ ->
      []
 | ExportListMode (_, items) ->
      items

(*
 * Compute the exports in the current environment.
 *)
let senv_add_exports senv result =
   match result, senv.senv_export_mode with
      ValNotReached, _
    | _, ExportNoneMode ->
         Omake_ir.ExportNone, ValValue
    | _, ExportAllMode _ ->
         Omake_ir.ExportAll, ValExport (senv_export_all_vars senv)
    | _, ExportListMode (_, items) ->
         Omake_ir.ExportList items, ValExport (senv_export_var_list items)

(************************************************************************
 * Variables.
 *)

(*
 * Create a variable that refers to the Pervasives module.
 *)
let create_pervasives_var loc v =
   VarVirtual (loc, v)

(*
 * Create a variable for the given scope.
 * If we are not in an object, then this is actually a file variable.
 *)
let create_var genv oenv senv cenv loc scope v =
   match scope with
      VarScopePrivate ->
         VarPrivate (loc, v)
    | VarScopeThis ->
         VarThis (loc, v)
    | VarScopeVirtual ->
         VarVirtual (loc, v)
    | VarScopeGlobal ->
         VarGlobal (loc, v)

(*
 * Strip the leading qualifiers.
 *)
let parse_declaration senv pos loc vl =
   (* Check scoping *)
   let make_forced_scope info scope2 =
      match info.name_scope with
         Some scope1 ->
            let print_error buf =
               fprintf buf "multiple declaration modes: %a and %a" (**)
                  pp_print_var_scope scope1
                  pp_print_var_scope scope2
            in
               raise (OmakeException (loc_pos loc pos, LazyError print_error))
       | None ->
            { info with name_scope = Some scope2 }
   in

   (* Read all the qualifiers *)
   let rec parse info vl =
      match vl with
         [] ->
            NameEmpty info
       | scope_var :: vl ->
            if Lm_symbol.eq scope_var private_sym then
               parse (make_forced_scope info VarScopePrivate) vl
            else if Lm_symbol.eq scope_var this_sym || Lm_symbol.eq scope_var protected_sym then
               parse (make_forced_scope info VarScopeThis) vl
            else if Lm_symbol.eq scope_var public_sym || Lm_symbol.eq scope_var global_sym then
               parse (make_forced_scope info VarScopeVirtual) vl
            else if Lm_symbol.eq scope_var static_sym then
               parse { info with name_static = true } vl
            else if Lm_symbol.eq scope_var curry_sym then
               parse { info with name_curry = true } vl
            (* ZZZ: Ignore the const modifier in 0.9.8 *)
            else if Lm_symbol.eq scope_var const_sym then
               parse info vl
            else
               NameMethod (info, scope_var, vl)
   in
      parse empty_name_info vl

(************************************************************************
 * Scoping.
 *)

(*
 * Builtin-vars.
 *)
let builtin_vars =
   SymbolSet.of_list [star_sym; gt_sym; at_sym; plus_sym; hat_sym; lt_sym; amp_sym; nf_sym]

(*
 * Get the scope for a variable.
 * Numeric symbols are global by default.
 *)
let senv_find_var genv oenv senv cenv pos loc v =
   if is_numeric_symbol v || SymbolSet.mem builtin_vars v then
      oenv, create_pervasives_var loc v
   else
      try
         let info = ForcedVars.find_var senv.senv_forced_vars v in
            oenv, info
      with
         Not_found ->
            let info =
               match cenv.cenv_context with
                  ContextTop
                | ContextRule
                | ContextFunction _ ->
                     VarGlobal (loc, v)
                | ContextObject ->
                     VarThis (loc, v)
            in
               oenv, info

(*
 * A path expression.
 *)
let senv_find_scoped_var genv oenv senv cenv pos loc info v =
   match info.name_scope with
      Some scope ->
         let info = create_var genv oenv senv cenv loc scope v in
            oenv, info
    | None ->
         senv_find_var genv oenv senv cenv pos loc v

let senv_find_method_var genv oenv senv cenv pos loc vl =
   match parse_declaration senv pos loc vl with
      NameEmpty _ ->
         raise (OmakeException (pos, StringError "empty method name"))
    | NameMethod (info, v, vl) ->
         let curry = info.name_curry in
         let oenv, info = senv_find_scoped_var genv oenv senv cenv pos loc info v in
            oenv, curry, info, vl

let senv_find_method_nocurry_var genv oenv senv cenv pos loc vl =
   let oenv, curry, info, vl = senv_find_method_var genv oenv senv cenv pos loc vl in
      if curry then
         raise (OmakeException (pos, StringError "curry qualifier not allowed"));
      oenv, info, vl

(************************************************************************
 * Variable definitions.
 *)

(*
 * Open a file and include all the symbols.
 *)
let senv_open_file genv senv pos loc filename =
   let node, vars = genv.genv_open_file filename pos loc in
   let vars =
      SymbolTable.fold (fun forced_vars v info ->
            ForcedVars.add_var forced_vars pos v info) senv.senv_forced_vars vars
   in
      { senv with senv_forced_vars = vars }, node

(* ZZZ: in 0.9.8.x:
 * If the scope is specified explicitly,
 * do not add it as a definition to senv.
 *
 * This should be uncommented in 0.9.9.
 *)
let senv_define_var_info_bogus senv pos loc scope v info =
   let { senv_object_senv = object_vars;
         senv_update_vars = update_vars;
         senv_forced_vars = forced_vars;
         senv_all_vars    = all_vars
       } = senv
   in

   (* They appear in the object only if not private *)
   let object_vars =
      match info with
         VarPrivate _ ->
            object_vars
       | _ ->
            SymbolTable.add object_vars v info
   in
   let update_vars = SymbolTable.add update_vars v info in
   let all_vars    = AllVars.add all_vars (scope, v) info in
      { senv with senv_object_senv = object_vars;
                  senv_update_vars = update_vars;
                  senv_forced_vars = forced_vars;
                  senv_all_vars    = all_vars
      }

(*
 * Low-level variable definition.
 *)
let senv_define_var_info senv pos loc scope v info =
   let { senv_object_senv = object_vars;
         senv_update_vars = update_vars;
         senv_forced_vars = forced_vars;
         senv_all_vars    = all_vars
       } = senv
   in

   (* They appear in the object only if not private *)
   let object_vars =
      match info with
         VarPrivate _ ->
            object_vars
       | _ ->
            SymbolTable.add object_vars v info
   in
   let forced_vars = ForcedVars.add_var forced_vars pos v info in
   let update_vars = SymbolTable.add update_vars v info in
   let all_vars    = AllVars.add all_vars (scope, v) info in
      { senv with senv_object_senv = object_vars;
                  senv_update_vars = update_vars;
                  senv_forced_vars = forced_vars;
                  senv_all_vars    = all_vars
      }

let senv_define_var scope genv oenv senv cenv pos loc v =
   let info = create_var genv oenv senv cenv loc scope v in
   let senv = senv_define_var_info senv pos loc scope v info in
      senv, info

(*
 * Parameter sorting.
 *)
let check_duplicate_keyword pos keywords v =
   if SymbolTable.mem keywords v then
      raise (OmakeException (pos, StringVarError ("duplicate keyword parameter", v)))

let senv_add_params genv oenv senv cenv pos params =
   let senv, keywords, params =
      List.fold_left (fun (senv, keywords, params) (v, info, loc) ->
            let senv, v_info = senv_define_var VarScopeVirtual genv oenv senv cenv pos loc v in
               match info with
                  NormalParam ->
                     senv, keywords, v_info :: params
                | RequiredParam ->
                     check_duplicate_keyword pos keywords v;
                     senv, SymbolTable.add keywords v (v_info, None), params
                | OptionalParam s ->
                     check_duplicate_keyword pos keywords v;
                     senv, SymbolTable.add keywords v (v_info, Some s), params) (senv, SymbolTable.empty, []) params
   in
   let keywords = SymbolTable.fold (fun keywords v (v_info, x) -> (v, v_info, x) :: keywords) [] keywords in
   let keywords = List.rev keywords in
   let params = List.rev params in
      senv, keywords, params

let senv_add_var_aux genv oenv senv cenv pos loc name_info v =
   if is_nonempty_name_info name_info then
      let scope = cenv_var_scope cenv name_info in
      let info =
         try
            let info = AllVars.find senv.senv_all_vars (scope, v) in
            (* ZZZ: check_var_info pos info name_info; *)
               info
         with
            Not_found ->
               create_var genv oenv senv cenv loc scope v
      in
      let senv = senv_define_var_info senv pos loc scope v info in
         genv, oenv, senv, info
   else
      (* ZZZ: in 0.9.8.x:
       * If the current scope is forced, add the variable in that mode.
       * Otherwise, if the variable is already defined, use that.
       * Otherwise, force the variable in global mode.
       * In all cases, add to senv.
       *
       * This should be valid until var3, where the VarScopeGlobal becomes a link var *)
      let senv, info =
         match cenv.cenv_scope with
            Some scope ->
               senv_define_var scope genv oenv senv cenv pos loc v
          | None ->
               try senv, ForcedVars.find_var senv.senv_forced_vars v with
                  Not_found ->
                     senv_define_var VarScopeGlobal genv oenv senv cenv pos loc v
      in
         genv, oenv, senv, info

let senv_add_var genv oenv senv cenv pos loc v =
   senv_add_var_aux genv oenv senv cenv pos loc (cenv_scope cenv) v

let senv_add_scoped_var genv oenv senv cenv pos loc info v =
   senv_add_var_aux genv oenv senv cenv pos loc (cenv_update_scope cenv info) v

let senv_add_method_def_var genv oenv senv cenv pos loc vl =
   match parse_declaration senv pos loc vl with
      NameEmpty _ ->
         raise (OmakeException (pos, StringError "empty method name"))

    | NameMethod (info, v, vl) ->
         let curry = info.name_curry in
         let genv, oenv, senv, info = senv_add_scoped_var genv oenv senv cenv pos loc info v in
            genv, oenv, senv, curry, info, vl

let senv_add_method_var genv oenv senv cenv pos loc kind vl =
   match kind with
      VarDefNormal ->
         senv_add_method_def_var genv oenv senv cenv pos loc vl
    | VarDefAppend ->
         (* ZZZ: we _should_ preserve the scope of the variable.
          * However, 0.9.8 chooses the forced mode over the
          * previous mode. *)
         let oenv, curry, info, vl = senv_find_method_var genv oenv senv cenv pos loc vl in
            genv, oenv, senv, curry, info, vl

let senv_add_method_nocurry_var genv oenv senv cenv pos loc kind vl =
   let genv, oenv, senv, curry, info, vl = senv_add_method_var genv oenv senv cenv pos loc kind vl in
      if curry then
         raise (OmakeException (pos, StringError "curry qualifier not allowed"));
      genv, oenv, senv, info, vl

(************************************************************************
 * Declarations.
 *)

(*
 * This is slightly different.  If the mode is not specified, then:
 *    top-level variables are public
 *    object variables are protected
 *    function variables are private
 *)
let senv_declare_static_var genv oenv senv cenv pos loc v =
   let pos = string_pos "senv_declare_static_var" pos in
   let scope = cenv_var_scope cenv (cenv_scope cenv) in
      senv_define_var scope genv oenv senv cenv pos loc v

let senv_declare_normal_var genv oenv senv cenv pos loc info v =
   let scope = cenv_var_scope cenv (cenv_update_scope cenv info) in
      senv_define_var scope genv oenv senv cenv pos loc v

(************************************************************************
 * Lazy mode.
 *
 * Strategy handling.  When we enter a lazy mode, we collect any eager parts
 * in the oenv.
 *)
type lazy_state =
   NormalState
 | EagerState
 | NestedState
 | LazyState of genv_lazy

let lazy_push_strategy genv strategy =
   match strategy with
      Omake_ast.NormalApply
    | Omake_ast.CommandApply ->
         genv, NormalState
    | Omake_ast.EagerApply ->
         let state = if genv.genv_lazy.genv_lazy_mode then EagerState else NormalState in
            genv, state
    | Omake_ast.LazyApply ->
         if genv.genv_lazy.genv_lazy_mode then
            genv, NestedState
         else
            (* Push a new lazy state when not already lazy *)
            let state = LazyState genv.genv_lazy in
            let genv = { genv with genv_lazy = lazy_env } in
               genv, state

let lazy_pop_strategy genv state loc e =
   match state with
      NormalState ->
         genv, e
    | EagerState ->
         (* Expression was eager *)
         let i = genv.genv_static_index in
         let v = Lm_symbol.make "eager.x" i in
         let lenv = genv.genv_lazy in
         let lenv = { lenv with genv_lazy_values = SymbolTable.add lenv.genv_lazy_values v (loc, e) } in
         let genv = { genv with genv_static_index = i + 1; genv_lazy = lenv } in
         let e = ApplyString (loc, VarPrivate (loc, v), [], []) in
            genv, e
    | NestedState ->
         (* Expression was lazy, but nested *)
         genv, LazyString (loc, e)
    | LazyState lenv_old ->
         (* Expression was lazy, so pre-evaluate all the eager parts *)
         let e = LazyString (loc, e) in
         let e =
            SymbolTable.fold (fun e1 v (loc, e2) ->
                  let v = VarPrivate (loc, v) in
                     LetVarString (loc, v, e2, e1)) e genv.genv_lazy.genv_lazy_values
         in
         let genv = { genv with genv_lazy = lenv_old } in
            genv, e

(************************************************************************
 * Conversion
 *)

(*
 * Literal string.
 *)
let build_literal_string e =
   let buf = Buffer.create 32 in
   let rec collect_exp e =
      match e with
         Omake_ast.NullExp _ ->
            ()
       | Omake_ast.StringOpExp (s, _)
       | Omake_ast.StringIdExp (s, _)
       | Omake_ast.StringIntExp (s, _)
       | Omake_ast.StringFloatExp (s, _)
       | Omake_ast.StringWhiteExp (s, _)
       | Omake_ast.StringOtherExp (s, _)
       | Omake_ast.StringKeywordExp (s, _) ->
            Buffer.add_string buf s
       | Omake_ast.QuoteExp (el, _)
       | Omake_ast.QuoteStringExp (_, el, _)
       | Omake_ast.SequenceExp (el, _) ->
            collect_exp_list el
       | Omake_ast.IntExp (_, loc)
       | Omake_ast.FloatExp (_, loc)
       | Omake_ast.ArrayExp (_, loc)
       | Omake_ast.ApplyExp (_, _, _, loc)
       | Omake_ast.SuperApplyExp (_, _, _, _, loc)
       | Omake_ast.MethodApplyExp (_, _, _, loc)
       | Omake_ast.BodyExp (_, loc)
       | Omake_ast.KeyExp (_, _, loc)
       | Omake_ast.CommandExp (_, _, _, loc)
       | Omake_ast.VarDefExp (_, _, _, _, loc)
       | Omake_ast.VarDefBodyExp (_, _, _, _, loc)
       | Omake_ast.KeyDefExp (_, _, _, _, loc)
       | Omake_ast.KeyDefBodyExp (_, _, _, _, loc)
       | Omake_ast.ObjectDefExp (_, _, _, loc)
       | Omake_ast.FunDefExp (_, _, _, loc)
       | Omake_ast.RuleExp (_, _, _, _, _, loc)
       | Omake_ast.ShellExp (_, loc)
       | Omake_ast.CatchExp (_, _, _, loc)
       | Omake_ast.ClassExp (_, loc) ->
            raise (OmakeException (loc_exp_pos loc, SyntaxError "misplaced expression"))
   and collect_exp_list el =
      List.iter collect_exp el
   in
      collect_exp e;
      Buffer.contents buf

let build_literal_argv e pos =
   let s = build_literal_string e in
      try Lm_string_util.parse_args s with
         Failure _
       | Invalid_argument _ ->
            raise (OmakeException (pos, StringStringError ("syntax error", s)))

let build_literal_string_opt e =
   try Some (build_literal_string e) with
      OmakeException _ ->
         None

let literal_string_equal e s =
   try build_literal_string e = s with
      OmakeException _ ->
         false

let build_literal_argv_list el =
   List.map build_literal_string el

let is_empty_string e =
   try build_literal_string e = "" with
      OmakeException _ ->
         false

(* Some is_static - it's a memo |  None - it's not *)
let get_memo_target e =
   try
      match build_literal_string e with
         ".STATIC" -> Some true
       | ".MEMO" -> Some false
       | _ -> None
   with OmakeException _ ->
      None

(*
 * Conversion.
 *)
let rec build_string genv oenv senv cenv e pos =
   let pos = string_pos "build_string" pos in
      match e with
         Omake_ast.NullExp loc ->
            genv, oenv, NoneString loc
       | Omake_ast.IntExp (i, loc) ->
            genv, oenv, IntString (loc, i)
       | Omake_ast.FloatExp (x, loc) ->
            genv, oenv, FloatString (loc, x)
       | Omake_ast.StringOpExp (s, loc)
       | Omake_ast.StringIdExp (s, loc)
       | Omake_ast.StringIntExp (s, loc)
       | Omake_ast.StringFloatExp (s, loc)
       | Omake_ast.StringOtherExp (s, loc)
       | Omake_ast.StringKeywordExp (s, loc) ->
            genv, oenv, ConstString (loc, s)
       | Omake_ast.StringWhiteExp (s, loc) ->
            genv, oenv, WhiteString (loc, s)
       | Omake_ast.QuoteExp (el, loc) ->
            build_quote_string genv oenv senv cenv el pos loc
       | Omake_ast.QuoteStringExp (c, el, loc) ->
            build_quote_string_string genv oenv senv cenv c el pos loc
       | Omake_ast.SequenceExp ([e], _) ->
            build_string genv oenv senv cenv e pos
       | Omake_ast.SequenceExp (el, loc) ->
            build_sequence_string genv oenv senv cenv el pos loc
       | Omake_ast.ArrayExp (e, loc) ->
            build_array_string genv oenv senv cenv e pos loc
       | Omake_ast.ApplyExp (strategy, v, args, loc) ->
            build_apply_string genv oenv senv cenv strategy v args pos loc
       | Omake_ast.SuperApplyExp (strategy, super, v, args, loc) ->
            build_super_apply_string genv oenv senv cenv strategy super v args pos loc
       | Omake_ast.MethodApplyExp (strategy, vl, args, loc) ->
            build_method_apply_string genv oenv senv cenv strategy vl args pos loc
       | Omake_ast.BodyExp (el, loc) ->
            build_body_string genv oenv senv cenv el pos loc
       | Omake_ast.KeyExp (strategy, v, loc) ->
            build_key_apply_string genv oenv senv cenv strategy v pos loc
       | Omake_ast.CommandExp (_, _, _, loc)
       | Omake_ast.VarDefExp (_, _, _, _, loc)
       | Omake_ast.VarDefBodyExp (_, _, _, _, loc)
       | Omake_ast.KeyDefExp (_, _, _, _, loc)
       | Omake_ast.KeyDefBodyExp (_, _, _, _, loc)
       | Omake_ast.ObjectDefExp (_, _, _, loc)
       | Omake_ast.FunDefExp (_, _, _, loc)
       | Omake_ast.RuleExp (_, _, _, _, _, loc)
       | Omake_ast.ShellExp (_, loc)
       | Omake_ast.CatchExp (_, _, _, loc)
       | Omake_ast.ClassExp (_, loc) ->
            raise (OmakeException (loc_pos loc pos, SyntaxError "misplaced expression"))

and build_string_list genv oenv senv cenv el pos =
   let pos = string_pos "build_string_list" pos in
   let genv, oenv, el =
      List.fold_left (fun (genv, oenv, el) e ->
            let genv, oenv, e = build_string genv oenv senv cenv e pos in
               genv, oenv, e :: el) (genv, oenv, []) el
   in
      genv, oenv, List.rev el

and build_string_opt genv oenv senv cenv sl pos =
   let pos = string_pos "build_string_opt" pos in
      match sl with
         Some s ->
            Some (build_string genv oenv senv cenv s pos)
       | None ->
            None

(*
 * Parameter lists.
 *)
and build_params genv oenv senv cenv params pos loc =
   let pos = string_pos "build_params" pos in
   let genv, oenv, params =
      List.fold_left (fun (genv, oenv, params) param ->
            let genv, oenv, param =
               match param with
                  Omake_ast.RequiredParam (v, _) ->
                     genv, oenv, (v, RequiredParam, loc)
                | Omake_ast.NormalParam (v, loc) ->
                     genv, oenv, (v, NormalParam, loc)
                | Omake_ast.OptionalParam (v, e, loc) ->
                     let genv, oenv, s = build_string genv oenv senv cenv e (loc_pos loc pos) in
                        genv, oenv, (v, OptionalParam s, loc)
            in
               genv, oenv, param :: params) (genv, oenv, []) params
   in
   let params = List.rev params in
   let senv, keywords, params = senv_add_params genv oenv senv cenv pos params in
      genv, oenv, senv, keywords, params

(*
 * When building a sequence, try to collapse adjacent constant strings.
 *)
and build_sequence_string genv oenv senv cenv el pos loc =
   let pos = string_pos "build_sequence_string" pos in
   let genv, oenv, args = build_sequence_string_aux genv oenv senv cenv el pos loc in
      genv, oenv, SequenceString (loc, args)

and build_quote_string genv oenv senv cenv el pos loc =
   let pos = string_pos "build_quote_string" pos in
   let genv, oenv, args = build_sequence_string_aux genv oenv senv cenv el pos loc in
      genv, oenv, QuoteString (loc, args)

and build_quote_string_string genv oenv senv cenv c el pos loc =
   let pos = string_pos "build_quote_string_string" pos in
   let genv, oenv, args = build_sequence_string_aux genv oenv senv cenv el pos loc in
      genv, oenv, QuoteStringString (loc, c, args)

and build_sequence_string_aux genv oenv senv cenv el pos loc =
   let pos = string_pos "build_sequence_string_aux" pos in
   let buf = Buffer.create 32 in

   (* Flush the buffer *)
   let flush_buffer buf_opt args =
      match buf_opt with
         Some loc ->
            let args = ConstString (loc, Buffer.contents buf) :: args in
               Buffer.clear buf;
               args
       | None ->
            args
   in

   (* Add a constant string to the buffer *)
   let add_string buf_opt s loc =
      Buffer.add_string buf s;
      match buf_opt with
         Some loc' ->
            let loc = union_loc loc' loc in
               Some loc
       | None ->
            Some loc
   in

   (* Collect all the strings in the sequence *)
   let rec collect genv oenv buf_opt args el =
      match el with
         [] ->
            let args = flush_buffer buf_opt args in
               genv, oenv, List.rev args
       | e :: el ->
            let genv, oenv, e = build_string genv oenv senv cenv e pos in
               match e with
                  NoneString _ ->
                     collect genv oenv buf_opt args el
                | ConstString (loc, s) ->
                     let buf_opt = add_string buf_opt s loc in
                        collect genv oenv buf_opt args el
                | IntString _
                | FloatString _
                | WhiteString _
                | FunString _
                | KeyApplyString _
                | ApplyString _
                | SuperApplyString _
                | MethodApplyString _
                | SequenceString _
                | ObjectString _
                | BodyString _
                | ArrayString _
                | ArrayOfString _
                | QuoteString _
                | QuoteStringString _
                | ExpString _
                | CasesString _
                | VarString _
                | ThisString _
                | LazyString _
                | LetVarString _ ->
                     let args = flush_buffer buf_opt args in
                     let args = e :: args in
                        collect genv oenv None args el
   in
      collect genv oenv None [] el

(*
 * Compatibility with old binding forms.
 *)
and foreach_warning loc =
   eprintf "@[<v 3>%a:@ Warning: old-style foreach expression.@ \
This expression should use a => binding.@]@." (**)
      pp_print_location loc

and fun_warning loc =
   eprintf "@[<v 3>%a:@ Warning: old-style fun expression.@ \
This expression should use a => binding.@]@." (**)
      pp_print_location loc

and build_compat_args genv oenv senv cenv v args pos loc =
   match args with
      [Omake_ast.ExpArg body;
       Omake_ast.ExpArg x;
       Omake_ast.ExpArg e]
      when Lm_symbol.eq v foreach_sym ->
         (match build_literal_string_opt x with
             Some x ->
                foreach_warning loc;
                [Omake_ast.ArrowArg ([Omake_ast.NormalParam (Lm_symbol.add x, loc)], body); Omake_ast.ExpArg e]
           | None ->
                args)
    | [Omake_ast.ExpArg x;
       Omake_ast.ExpArg body]
      when Lm_symbol.eq v fun_sym ->
         (match build_literal_string_opt x with
             Some x ->
                fun_warning loc;
                [Omake_ast.ArrowArg ([Omake_ast.NormalParam (Lm_symbol.add x, loc)], body)]
           | None ->
                args)
    | _ ->
         args

(*
 * New-style foreach methods have a single argument: the function.
 * Multi-argument foreach should be converted.
 *)
and build_method_compat_args genv oenv senv cenv vl args pos loc =
   if Lm_symbol.eq (Lm_list_util.last vl) foreach_sym then
      (* New-style foreach methods have a single argument *)
      match args with
         [Omake_ast.ExpArg body;
          Omake_ast.ExpArg x] ->
            (match build_literal_string_opt x with
                Some x ->
                   foreach_warning loc;
                   [Omake_ast.ArrowArg ([Omake_ast.NormalParam (Lm_symbol.add x, loc)], body)]
           | None ->
                args)
       | [Omake_ast.ExpArg body;
          Omake_ast.ExpArg x;
          Omake_ast.ExpArg y] ->
            (match build_literal_string_opt x, build_literal_string_opt y with
                Some x, Some y ->
                   foreach_warning loc;
                   [Omake_ast.ArrowArg ([Omake_ast.NormalParam (Lm_symbol.add x, loc);
                                         Omake_ast.NormalParam (Lm_symbol.add y, loc)], body)]
              | _ ->
                   args)
       | _ ->
            args
   else
      args

(*
 * Applications might have parameters.
 * If they do, then add a function value to the arguments.
 *)
and build_arg senv cenv pos loc (genv, oenv, args, kargs) arg =
   match arg with
      Omake_ast.ExpArg e ->
         let genv, oenv, s = build_string genv oenv senv cenv e pos in
            genv, oenv, s :: args, kargs
    | Omake_ast.KeyArg (v, e) ->
         let genv, oenv, s = build_string genv oenv senv cenv e pos in
            genv, oenv, args, (v, s) :: kargs
    | Omake_ast.ArrowArg (params, e) ->
         let genv, oenv, senv, keywords, params =
            build_params genv oenv senv cenv params pos loc
         in
         let genv, oenv, e, export =
            match e with
               Omake_ast.BodyExp (el, _) ->
                  let genv, oenv, e, export, _ = build_body genv oenv senv cenv el pos loc in
                     genv, oenv, e, export
             | e ->
                  let genv, oenv, s = build_string genv oenv senv cenv e pos in
                     genv, oenv, [StringExp (loc, s)], ExportNone
         in
         let e = FunString (loc, keywords, params, e, export) in
            genv, oenv, e :: args, kargs

and build_arg_list genv oenv senv cenv args pos loc =
   let pos = string_pos "build_arg_list" pos in
   let genv, oenv, args, kargs =
      List.fold_left (build_arg senv cenv pos loc) (genv, oenv, [], []) args
   in
   let args = List.rev args in
   let kargs = List.sort (fun (v1, _) (v2, _) -> Lm_symbol.compare v1 v2) kargs in
      genv, oenv, args, kargs

and build_apply_args genv oenv senv cenv v args pos loc =
   let pos = string_pos "build_apply_body" pos in
   let args = build_compat_args genv oenv senv cenv v args pos loc in
      build_arg_list genv oenv senv cenv args pos loc

and build_method_apply_args genv oenv senv cenv vl args pos loc =
   let pos = string_pos "build_apply_body" pos in
   let args = build_method_compat_args genv oenv senv cenv vl args pos loc in
      build_arg_list genv oenv senv cenv args pos loc

(*
 * Build an array of strings.
 *)
and build_array_string genv oenv senv cenv args pos loc =
   let pos = string_pos "build_array_string" pos in
   let genv, oenv, args = build_string_list genv oenv senv cenv args pos in
      genv, oenv, ArrayString (loc, args)

(*
 * Build an application.
 *)
and build_apply_string genv oenv senv cenv strategy v args pos loc =
   let pos = string_pos "build_apply_string" pos in
      if Lm_symbol.eq v this_sym then begin
         if args <> [] then
            raise (OmakeException (loc_pos loc pos, StringError "illegal arguments"));
         genv, oenv, ThisString loc
      end
      else
         let genv, lazy_state = lazy_push_strategy genv strategy in
         let genv, oenv, args, kargs = build_apply_args genv oenv senv cenv v args pos loc in
         let oenv, v = senv_find_var genv oenv senv cenv pos loc v in
         let e = ApplyString (loc, v, args, kargs) in
         let genv, e = lazy_pop_strategy genv lazy_state loc e in
            genv, oenv, e

(*
 * Super call.
 *)
and build_super_apply_string genv oenv senv cenv strategy super v args pos loc =
   let pos = string_pos "build_super_apply_string" pos in
   let genv, lazy_state = lazy_push_strategy genv strategy in
   let genv, oenv, args, kargs = build_apply_args genv oenv senv cenv v args pos loc in
   let e = SuperApplyString (loc, super, v, args, kargs) in
   let genv, e = lazy_pop_strategy genv lazy_state loc e in
      genv, oenv, e

(*
 * Build a method application.
 *)
and build_method_apply_string genv oenv senv cenv strategy vars args pos loc =
   let pos = string_pos "build_method_apply_string" pos in
   let genv, lazy_state = lazy_push_strategy genv strategy in
   let genv, oenv, args, kargs = build_method_apply_args genv oenv senv cenv vars args pos loc in
   let oenv, v, vl = senv_find_method_nocurry_var genv oenv senv cenv pos loc vars in
   let e =
      match vl with
         [] ->
            ApplyString (loc, v, args, kargs)
       | _ ->
            MethodApplyString (loc, v, vl, args, kargs)
   in
   let genv, e = lazy_pop_strategy genv lazy_state loc e in
      genv, oenv, e

(*
 * Key application $|xxx|
 *)
and build_key_apply_string genv oenv senv cenv strategy v pos loc =
   let genv, lazy_state = lazy_push_strategy genv strategy in
   let e = KeyApplyString (loc, v) in
   let genv, e = lazy_pop_strategy genv lazy_state loc e in
      genv, oenv, e

(*
 * Build a body expression.
 *)
and build_body_string genv oenv senv cenv el pos loc =
   let pos = string_pos "build_body_string" pos in
   let genv, oenv, body, export, _ = build_body genv oenv senv cenv el pos loc in
      genv, oenv, BodyString (loc, body, export)

(*
 * Build an expression.
 *)
and build_exp genv oenv senv cenv result e =
   let pos = string_pos "build_exp" (ast_exp_pos e) in
      match e with
         Omake_ast.NullExp loc
       | Omake_ast.IntExp (_, loc)
       | Omake_ast.FloatExp (_, loc)
       | Omake_ast.StringOpExp (_, loc)
       | Omake_ast.StringIdExp (_, loc)
       | Omake_ast.StringIntExp (_, loc)
       | Omake_ast.StringFloatExp (_, loc)
       | Omake_ast.StringWhiteExp (_, loc)
       | Omake_ast.StringOtherExp (_, loc)
       | Omake_ast.StringKeywordExp (_, loc)
       | Omake_ast.QuoteExp (_, loc)
       | Omake_ast.QuoteStringExp (_, _, loc) ->
            genv, oenv, senv, SequenceExp (loc, []), ValValue
       | Omake_ast.SequenceExp ([e], _)
       | Omake_ast.BodyExp ([e], _) ->
            build_exp genv oenv senv cenv result e
       | Omake_ast.SequenceExp (el, loc)
       | Omake_ast.BodyExp (el, loc)
       | Omake_ast.ArrayExp (el, loc) ->
            build_sequence_exp genv oenv senv cenv result el pos loc
       | Omake_ast.ApplyExp (_, v, args, loc) ->
            build_apply_exp genv oenv senv cenv v args pos loc
       | Omake_ast.SuperApplyExp (_, super, v, args, loc) ->
            build_super_apply_exp genv oenv senv cenv super v args pos loc
       | Omake_ast.MethodApplyExp (_, vl, args, loc) ->
            build_method_apply_exp genv oenv senv cenv vl args pos loc
       | Omake_ast.CommandExp (v, arg, commands, loc) ->
            build_command_exp genv oenv senv cenv v arg commands pos loc
       | Omake_ast.VarDefExp (v, kind, flag, e, loc) ->
            build_var_def_exp genv oenv senv cenv v kind flag e pos loc
       | Omake_ast.VarDefBodyExp (v, kind, flag, [], loc) ->
            build_var_def_exp genv oenv senv cenv v kind flag (Omake_ast.SequenceExp ([], loc)) pos loc
       | Omake_ast.VarDefBodyExp (v, kind, flag, el, loc) ->
            build_var_def_body_exp genv oenv senv cenv v kind flag el pos loc
       | Omake_ast.KeyExp (_, v, loc) ->
            genv, oenv, senv, KeyExp (loc, v), ValValue
       | Omake_ast.KeyDefExp (v, kind, flag, e, loc) ->
            build_key_def_exp genv oenv senv cenv v kind flag e pos loc
       | Omake_ast.KeyDefBodyExp (v, kind, flag, el, loc) ->
            build_key_def_body_exp genv oenv senv cenv v kind flag el pos loc
       | Omake_ast.ObjectDefExp (v, flag, el, loc) ->
            build_object_def_exp genv oenv senv cenv v flag el pos loc
       | Omake_ast.FunDefExp (v, params, e, loc) ->
            build_fun_def_exp genv oenv senv cenv v params e pos loc
       | Omake_ast.RuleExp (multiple, target, pattern, source, commands, loc) ->
            build_rule_exp genv oenv senv cenv multiple target pattern source commands pos loc
       | Omake_ast.ShellExp (e, loc) ->
            build_shell_exp genv oenv senv cenv e pos loc
       | Omake_ast.CatchExp (_, _, _, loc) ->
            raise (OmakeException (pos, StringError "misplaced catch clause"))
       | Omake_ast.ClassExp (names, loc) ->
            build_class_exp genv oenv senv cenv loc names

(*
 * Add the class names.
 *)
and build_class_exp genv oenv senv cenv loc names =
   let oenv = { (* oenv with *) oenv_class_names = SymbolSet.add_list oenv.oenv_class_names names } in
      genv, oenv, senv, SequenceExp (loc, []), ValValue

(*
 * Sequence exp.  Build the expression one at a time.
 *)
and build_sequence genv oenv senv cenv result pos rval el =
   match el with
      Omake_ast.CommandExp (v, e, body, loc)
      :: el when Lm_symbol.eq v if_sym ->
         let cases, el = collect_if [e, body] el in
         let pos = loc_pos loc pos in
         let cenv_body = cenv_sequence_scope cenv el in
         let genv, oenv, senv, e, result = build_if_exp genv oenv senv cenv_body cases pos loc in
         let genv, oenv, senv, el, result = build_sequence genv oenv senv cenv result pos rval el in
            genv, oenv, senv, e :: el, result

    | Omake_ast.CommandExp (v, e, body, loc)
      :: el when Lm_symbol.eq v while_sym ->
         let cases, el = collect_cases [] el in
         let pos = loc_pos loc pos in
         let cenv_body = cenv_sequence_scope cenv el in
         let genv, oenv, senv, e, result = build_opt_cases_command_exp genv oenv senv cenv_body v e cases body pos loc in
         let genv, oenv, senv, el, result = build_sequence genv oenv senv cenv result pos rval el in
            genv, oenv, senv, e :: el, result

    | Omake_ast.CommandExp (v, e, body, loc) :: el ->
         let cases, el = collect_cases [] el in
         let pos = loc_pos loc pos in
            if Lm_symbol.eq v export_sym then
               let oenv, senv = build_export_command genv oenv senv cenv e cases body pos loc in
                  build_sequence genv oenv senv cenv result pos rval el
            else
               let cenv_body = cenv_sequence_scope cenv el in
               let genv, oenv, senv, e, result = build_cases_command_exp genv oenv senv cenv_body v e cases body pos loc in
               let genv, oenv, senv, el, result = build_sequence genv oenv senv cenv result pos rval el in
                  genv, oenv, senv, e :: el, result

    | Omake_ast.ApplyExp (_, v, args, loc) :: el ->
         let cases, el = collect_cases [] el in
         let pos = loc_pos loc pos in
         let genv, oenv, senv, e, result = build_cases_apply_exp genv oenv senv cenv v args cases pos loc in
         let genv, oenv, senv, el, result = build_sequence genv oenv senv cenv result pos rval el in
            genv, oenv, senv, e :: el, result

      (* ZZZ: preserve the old behavior of changing the mode to protected
       * after a class definition. *)
    | Omake_ast.ClassExp (names, loc) :: el ->
         let genv, oenv, senv, e, result = build_class_exp genv oenv senv cenv loc names in
         let cenv = { cenv with cenv_scope = Some VarScopeThis } in
         let genv, oenv, senv, el, result = build_sequence genv oenv senv cenv result pos rval el in
            genv, oenv, senv, e :: el, result

    | e :: el ->
         let genv, oenv, senv, e, result = build_exp genv oenv senv cenv result e in
         let genv, oenv, senv, el, result = build_sequence genv oenv senv cenv result pos rval el in
            genv, oenv, senv, e :: el, result

    | [] ->
         rval genv oenv senv cenv result

(*
 * Normal sequences are always in global mode.
 *)
and build_body genv oenv senv cenv el pos loc =
   let pos = string_pos "build_body" pos in
   let genv, oenv, senv, el, result =
      build_sequence genv oenv senv cenv ValValue pos (fun genv oenv senv cenv result ->
            genv, oenv, senv, [], result) el
   in
   let export, result = senv_add_exports senv result in
      genv, oenv, el, export, result

(*
 * Export the environment.
 *)
and build_export_command genv oenv senv cenv e cases body pos loc =
   let pos = string_pos "build_export_command" pos in
   let () =
      if cases <> [] then
         raise (OmakeException (pos, StringError "illegal cases"));
      if body <> [] then
         raise (OmakeException (pos, StringError "illegal body"))
   in

   (* Merge the export set *)
   let argv = build_literal_argv e pos in
   let oenv, mode =
      match senv.senv_export_mode, argv with
         _, ["all"] ->
            eprintf "@[<hv3>*** omake WARNING: %a:\
@ @[<hov0>\"export all\" syntax is deprecated;\
@ use an empty \"export\" instead.@]@]@." pp_print_pos pos;
            oenv, ExportAllMode loc
       | _, []
       | ExportAllMode _, _ ->
            oenv, ExportAllMode loc
       | mode, argv ->
            let items = items_of_export_mode mode in
            let oenv, items =
               List.fold_left (fun (oenv, items) v ->
                     let oenv, item =
                        match v with
                           "rules" ->
                              eprintf "@[<hv3>*** omake WARNING: %a:\
@ @[<hov0>\"export rules\" syntax is deprecated;\
@ use \"export .RULE .PHONY\" instead.@]@]@." pp_print_pos pos;
                              oenv, Omake_ir.ExportRules
                         | ".RULE" ->
                              oenv, Omake_ir.ExportRules
                         | ".PHONY" ->
                              oenv, Omake_ir.ExportPhonies
                         | v ->
                              let v = Lm_symbol.add v in
                              let oenv, info = senv_find_var genv oenv senv cenv pos loc v in
                                 oenv, Omake_ir.ExportVar info
                     in
                        oenv, item :: items) (oenv, items) argv
            in
               oenv, ExportListMode (loc, List.rev items)
   in
   let senv = { senv with senv_export_mode = mode } in
      oenv, senv

(*
 * In an application, turn the arguments into strings.
 *)
and build_apply_exp genv oenv senv cenv v args pos loc =
   let pos = string_pos "build_apply_exp" pos in
   let genv, oenv, args, kargs = build_apply_args genv oenv senv cenv v args pos loc in
      match args with
         [arg] when Lm_symbol.eq v return_sym ->
            let id = cenv_return_id cenv pos loc in
               genv, oenv, senv, ReturnExp (loc, arg, id), ValNotReached
       | args ->
            let oenv, info = senv_find_var genv oenv senv cenv pos loc v in
               genv, oenv, senv, ApplyExp (loc, info, args, kargs), ValValue

and build_super_apply_exp genv oenv senv cenv super v args pos loc =
   let pos = string_pos "build_super_apply_exp" pos in
   let genv, oenv, args, kargs = build_apply_args genv oenv senv cenv v args pos loc in
      genv, oenv, senv, SuperApplyExp (loc, super, v, args, kargs), ValValue

and build_method_apply_exp genv oenv senv cenv vl args pos loc =
   let pos = string_pos "build_method_apply_exp" pos in
   let genv, oenv, args, kargs = build_method_apply_args genv oenv senv cenv vl args pos loc in
   let oenv, v, vl = senv_find_method_nocurry_var genv oenv senv cenv pos loc vl in
   let e =
      match vl with
         [] ->
            ApplyExp (loc, v, args, kargs)
       | _ ->
            MethodApplyExp (loc, v, vl, args, kargs)
   in
      genv, oenv, senv, e, ValValue

and build_cases_apply_exp genv oenv senv cenv v args cases pos loc =
   let pos = string_pos "build_cases_apply_exp" pos in
   let genv, oenv, args, kargs = build_apply_args genv oenv senv cenv v args pos loc in
      match args, cases with
         [arg], [] when Lm_symbol.eq v return_sym ->
            let id = cenv_return_id cenv pos loc in
               genv, oenv, senv, ReturnExp (loc, arg, id), ValNotReached
       | _, [] ->
            let oenv, v = senv_find_var genv oenv senv cenv pos loc v in
               genv, oenv, senv, ApplyExp (loc, v, args, kargs), ValValue
       | _ ->
            let oenv, v = senv_find_var genv oenv senv cenv pos loc v in
            let genv, oenv, cases =
               List.fold_left (fun (genv, oenv, cases) (v, e, body) ->
                     let genv, oenv, body, export, _ = build_body genv oenv senv cenv body pos loc in
                     let genv, oenv, s = build_string genv oenv senv cenv e pos in
                     let case = v, s, body, export in
                        genv, oenv, case :: cases) (genv, oenv, []) cases
            in
            let args = CasesString (loc, List.rev cases) :: args in
               genv, oenv, senv, ApplyExp (loc, v, args, kargs), ValValue

and build_cases_command_exp genv oenv senv cenv v arg cases commands pos loc =
   let pos = string_pos "build_cases_command_exp" pos in
      match cases with
         [] ->
            build_command_exp genv oenv senv cenv v arg commands pos loc
       | _ ->
            let arg =
               match commands with
                  [] ->
                     arg
                | _ ->
                     Omake_ast.BodyExp (commands, loc)
            in
               build_cases_apply_exp genv oenv senv cenv v [Omake_ast.ExpArg arg] cases pos loc

and build_opt_cases_command_exp genv oenv senv cenv v arg cases commands pos loc =
   let pos = string_pos "build_opt_cases_command_exp" pos in
   let cases =
      if commands = [] then
         cases
      else
         let default = default_sym, Omake_ast.NullExp loc, commands in
            cases @ [default]
   in
      build_cases_apply_exp genv oenv senv cenv v [Omake_ast.ExpArg arg] cases pos loc

(*
 * The command line is handled at parse time as well as
 * at evaluation time.
 *)
and build_set_exp genv oenv senv cenv e pos loc =
   let _pos = string_pos "build_set_exp" pos in
   let argv = build_literal_argv e pos in
   let senv = { senv with senv_venv = venv_set_options senv.senv_venv loc pos argv } in
   let argv = List.map (fun s -> ConstString (loc, s)) argv in
   let argv = ArrayString (loc, argv) in
   let e = ApplyExp (loc, omakeflags_var, [argv], []) in
      genv, oenv, senv, e, ValValue

(*
 * Commands.
 *)
and build_command_exp genv oenv senv cenv v arg commands pos loc =
   let pos = string_pos "build_command_exp" pos in
      if Lm_symbol.eq v include_sym then
         build_include_exp genv oenv senv cenv arg commands pos loc
      else if Lm_symbol.eq v if_sym then
         build_if_exp genv oenv senv cenv [arg, commands] pos loc
      else if Lm_symbol.eq v section_sym then
         build_section_exp genv oenv senv cenv arg commands pos loc
      else if Lm_symbol.eq v value_sym then
         build_value_exp genv oenv senv cenv arg commands pos loc
      else if Lm_symbol.eq v declare_sym then
         build_declare_exp genv oenv senv cenv arg commands pos loc
      else if commands <> [] then
         raise (OmakeException (loc_pos loc pos, StringVarError ("illegal body for", v)))
      else if Lm_symbol.eq v return_sym then
         build_return_exp genv oenv senv cenv arg pos loc
      else if Lm_symbol.eq v open_sym then
         build_open_exp genv oenv senv cenv arg pos loc
      else if Lm_symbol.eq v autoload_sym then
         genv, oenv, senv, SequenceExp (loc, []), ValValue
      else if Lm_symbol.eq v set_sym then
         build_set_exp genv oenv senv cenv arg pos loc
      else
         build_apply_exp genv oenv senv cenv v [Omake_ast.ExpArg arg] pos loc

(*
 * Include a file.
 *)
and build_include_exp genv oenv senv cenv e commands pos loc =
   let pos = string_pos "build_include_exp" pos in
   let genv, oenv, s = build_string genv oenv senv cenv e pos in
   let genv, oenv, commands = build_string_list genv oenv senv cenv commands pos in
      genv, oenv, senv, IncludeExp (loc, s, commands), ValValue

(*
 * Conditionals.
 *)
and build_if_exp genv oenv senv cenv cases pos loc =
   let pos = string_pos "build_if_exp" pos in
   let genv, oenv, cases, results, is_complete =
      List.fold_left (fun (genv, oenv, cases, results, is_complete) (e1, e2) ->
            let is_complete = is_complete || is_true_string e1 in
            let genv, oenv, s = build_string genv oenv senv cenv e1 pos in
            let genv, oenv, e2, export, result = build_body genv oenv senv cenv e2 pos loc in
            let cases = (s, e2, export) :: cases in
            let results = result :: results in
               genv, oenv, cases, results, is_complete) (genv, oenv, [], [], false) cases
   in
   let senv, result = senv_merge_results genv senv cenv pos loc results is_complete in
      genv, oenv, senv, IfExp (loc, List.rev cases), result

(*
 * A normal sequence, not a new scope.
 *)
and build_sequence_exp genv oenv senv cenv result el pos loc =
   let pos = string_pos "build_sequence_exp" pos in
   let genv, oenv, senv, el, result =
      build_sequence genv oenv senv cenv result pos (fun genv oenv senv cenv result ->
            genv, oenv, senv, [], result) el
   in
      genv, oenv, senv, SequenceExp (loc, el), result

(*
 * A section is just an "if true" command.
 *)
and build_section_exp genv oenv senv cenv e1 e2 pos loc =
   let pos = string_pos "build_section_exp" pos in
   let genv, oenv, s = build_string genv oenv senv cenv e1 pos in
   let genv, oenv, body, export, result = build_body genv oenv senv cenv e2 pos loc in
   let senv, result = senv_export_section senv pos result in
      genv, oenv, senv, SectionExp (loc, s, body, export), result

(*
 * Return a value.
 *)
and build_value_exp genv oenv senv cenv e commands pos loc =
   let pos = string_pos "build_value_exp" pos in
   let genv, oenv, s =
      match e, commands with
         e, [] ->
            build_string genv oenv senv cenv e pos
       | Omake_ast.NullExp _, el ->
            let genv, oenv, el, export, _ = build_body genv oenv senv cenv el pos loc in
               genv, oenv, ExpString (loc, el, export)
       | _, _ :: _  ->
            raise (OmakeException (loc_pos loc pos, StringError ("Value can have an argument or a body, but not both")))
   in
      genv, oenv, senv, StringExp (loc, s), ValValue

and build_return_exp genv oenv senv cenv e pos loc =
   let pos = string_pos "build_return_exp" pos in
   let genv, oenv, s = build_string genv oenv senv cenv e pos in
   let id = cenv_return_id cenv pos loc in
      genv, oenv, senv, ReturnExp (loc, s, id), ValNotReached

(*
 * Open the namespace from another file.
 *)
and build_open_exp genv oenv senv cenv arg pos loc =
   let pos = string_pos "build_open_exp" pos in
   let argv = build_literal_argv arg pos in
   let senv, nodes =
      List.fold_left (fun (senv, nodes) filename ->
            let senv, node = senv_open_file genv senv pos loc filename in
            let nodes = node :: nodes in
               senv, nodes) (senv, []) argv
   in
      genv, oenv, senv, OpenExp (loc, List.rev nodes), ValValue

(*
 * Declare a variable, but dont worry about its definition.
 *)
and build_declare_exp genv oenv senv cenv arg commands pos loc =
   let pos = string_pos "build_declare_exp" pos in
   let argv1 = build_literal_argv arg pos in
   let argv2 = build_literal_argv_list commands in
   let argv = argv1 @ argv2 in
   let genv, oenv, senv =
      List.fold_left (fun (genv, oenv, senv) name ->
            let name = List.map Lm_symbol.add (Lm_string_util.split "." name) in
               match parse_declaration senv pos loc name with
                  NameEmpty _ ->
                     raise (OmakeException (pos, StringError "illegal name"))

                | NameMethod (info, v, []) ->
                     let senv, _ = senv_declare_normal_var genv oenv senv cenv pos loc info v in
                        genv, oenv, senv

                | NameMethod (_, _, _ :: _) ->
                     raise (OmakeException (pos, StringError "name has too many components"))) (genv, oenv, senv) argv
   in
      genv, oenv, senv, SequenceExp (loc, []), ValValue

(*
 * The public/protected/private are not really objects.
 * They are scope definitions.
 *)
and build_object_def_exp genv oenv senv cenv vl flag body pos loc =
   match parse_declaration senv pos loc vl with
      NameEmpty { name_static = true; name_scope = None } ->
         build_static_object_exp genv oenv senv cenv body pos loc
    | NameEmpty { name_static = true } ->
         raise (OmakeException (loc_pos loc pos, StringError "static objects cannot be qualified"))
    | NameEmpty info ->
         build_scope_exp genv oenv senv cenv info body pos loc

    | NameMethod ({ name_static = true }, _, _) ->
         raise (OmakeException (loc_pos loc pos, StringError "static named objects are not allowed"))
    | NameMethod (info, v, vl) ->
         build_normal_object_exp genv oenv senv cenv info v vl flag body pos loc

(*
 * Qualified definitions, but not a new scope in the
 * current environment.
 *)
and build_scope_exp genv oenv senv cenv info el pos loc =
   let pos = string_pos "build_scope_exp" pos in
   let cenv = cenv_force_scope cenv info in
   let genv, oenv, senv, el, result =
      build_sequence genv oenv senv cenv ValValue pos (fun genv oenv senv cenv result ->
            genv, oenv, senv, [], result) el
   in
      genv, oenv, senv, SequenceExp (loc, el), ValValue

(*
 * A static object is evaluated just once.
 *
 * ZZZ: this is pretty different in 0.9.9.
 *)
and build_static_object_exp genv oenv senv cenv el pos loc =
   let pos = string_pos "build_scope_exp" pos in
   let genv, id = genv_new_static_id genv in

   let senv_body, cenv_body = senv_static_body senv cenv (Lm_symbol.new_symbol static_sym) in
   let genv, oenv, senv_body, el, _ =
      build_sequence genv oenv senv_body cenv_body ValValue pos (fun genv oenv senv cenv _ ->
            genv, oenv, senv, [ReturnObjectExp (loc, [])], ValValue) el
   in
   let senv, vars =
      SymbolTable.fold (fun (senv, vars) _ info ->
            match info with
               VarThis (loc, v) ->
                  let senv, info = senv_declare_static_var genv oenv senv cenv pos loc v in
                     senv, info :: vars
             | VarPrivate _
             | VarVirtual _
             | VarGlobal _ ->
                  senv, vars) (senv, []) senv_body.senv_object_senv
   in
   let file = genv.genv_file in
      genv, oenv, senv, StaticExp (loc, file, id, el), ValValue

(*
 * An object is a collection of definitions.
 *)
and build_normal_object_exp genv oenv senv cenv info v vl flag body pos loc =
   let pos = string_pos "build_normal_object_exp" pos in
   let genv, oenv, senv, parent_string, info =
      match flag with
         Omake_ast.DefineNormal ->
            let genv, oenv, senv, info = senv_add_scoped_var genv oenv senv cenv pos loc info v in
            let oenv, parent_var = senv_find_var genv oenv senv cenv pos loc object_sym in
            let parent_string = ApplyString (loc, parent_var, [], []) in
               genv, oenv, senv, parent_string, info
       | Omake_ast.DefineAppend ->
            let oenv, parent_var = senv_find_scoped_var genv oenv senv cenv pos loc info v in
            let parent_string =
               match vl with
                  [] -> ApplyString (loc, parent_var, [], [])
                | _ -> MethodApplyString (loc, parent_var, vl, [], [])
            in

            (* ZZZ: We should just use the previous info.
             * However, in 0.9.8 the current forced mode overrides
             * any previous definition. *)
            let genv, oenv, senv, var_info = senv_add_scoped_var genv oenv senv cenv pos loc info v in
               genv, oenv, senv, parent_string, var_info
   in

   (* Compile the body *)
   let rval genv oenv senv cenv result =
      let class_names = SymbolSet.to_list oenv.oenv_class_names in
         genv, oenv, senv, [ReturnObjectExp (loc, class_names)], ValValue
   in

   let senv_body, cenv_body = senv_object_body senv cenv v in
   let oenv_body = oenv_empty in
   let genv, oenv_body, senv_body, body, result =
      build_sequence genv oenv_body senv_body cenv_body ValValue pos rval body
   in
   let export, _ = senv_add_exports senv_body result in

   (* Add the extends directive to the object body *)
   let e = LetObjectExp (loc, info, vl, parent_string, body, export) in
      genv, oenv, senv, e, ValValue

(*
 * Variable definition.
 *)
and build_var_def_kind flag =
   match flag with
      Omake_ast.DefineNormal ->
         VarDefNormal
    | Omake_ast.DefineAppend ->
         VarDefAppend

(*
 * Variable definitions.
 *)
and build_var_def_exp genv oenv senv cenv v kind flag e pos loc =
   let pos = string_pos "build_var_def_exp" pos in
   let genv, oenv, s = build_string genv oenv senv cenv e pos in
   let s =
      match kind with
         Omake_ast.DefineString ->
            s
       | Omake_ast.DefineArray ->
            ArrayOfString (loc, s)
   in
   let kind = build_var_def_kind flag in
      match v with
         [v] when Lm_symbol.eq v this_sym ->
            genv, oenv, senv, LetThisExp (loc, s), ValValue
       | _ ->
            let genv, oenv, senv, v, vl = senv_add_method_nocurry_var genv oenv senv cenv pos loc kind v in
               genv, oenv, senv, LetVarExp (loc, v, vl, kind, s), ValValue

and build_var_def_body_exp genv oenv senv cenv v kind flag body pos loc =
   let pos = string_pos "build_var_def_body_exp" pos in
   let genv, oenv, e =
      match kind with
         Omake_ast.DefineString ->
            let genv, oenv, el, export, _ = build_body genv oenv senv cenv body pos loc in
               genv, oenv, ExpString (loc, el, export)
       | Omake_ast.DefineArray ->
            let genv, oenv, sl = build_string_list genv oenv senv cenv body pos in
               genv, oenv, ArrayString (loc, sl)
   in
   let kind = build_var_def_kind flag in
   let genv, oenv, senv, v, vl = senv_add_method_nocurry_var genv oenv senv cenv pos loc kind v in
      genv, oenv, senv, LetVarExp (loc, v, vl, kind, e), ValValue

(*
 * Key definitions (for object properties.
 *)
and build_key_def_exp genv oenv senv cenv v kind flag e pos loc =
   let pos = string_pos "build_key_def_exp" pos in
   let genv, oenv, s = build_string genv oenv senv cenv e pos in
   let s =
      match kind with
         Omake_ast.DefineString ->
            s
       | Omake_ast.DefineArray ->
            ArrayOfString (loc, s)
   in
   let kind = build_var_def_kind flag in
      genv, oenv, senv, LetKeyExp (loc, v, kind, s), ValValue

and build_key_def_body_exp genv oenv senv cenv v kind flag body pos loc =
   let pos = string_pos "build_key_def_body_exp" pos in
   let genv, oenv, e =
      match kind with
         Omake_ast.DefineString ->
            let genv, oenv, el, export, _ = build_body genv oenv senv cenv body pos loc in
               genv, oenv, ExpString (loc, el, export)
       | Omake_ast.DefineArray ->
            let genv, oenv, sl =  build_string_list genv oenv senv cenv body pos in
               genv, oenv, ArrayString (loc, sl)
   in
   let kind = build_var_def_kind flag in
      genv, oenv, senv, LetKeyExp (loc, v, kind, e), ValValue

(*
 * Function definition.
 *)
and build_fun_def_exp genv oenv senv cenv v params el pos loc =
   let pos = string_pos "build_fun_def_exp" pos in
   let cenv_body = cenv_fun_scope cenv (new_return_id loc v) in
   let genv, oenv, senv_body, opt_params, params = build_params genv oenv senv cenv_body params pos loc in
   let genv, oenv, body, export, _ = build_body genv oenv senv_body cenv_body el pos loc in
   let genv, oenv, senv, curry, v, vl = senv_add_method_def_var genv oenv senv cenv pos loc v in
      genv, oenv, senv, LetFunExp (loc, v, vl, curry, opt_params, params, body, export), ValValue

(*
 * Special rule expressions.
 *)
and build_options_exp genv oenv senv cenv pos loc sources =
   let genv, oenv, options =
      SymbolTable.fold (fun (genv, oenv, options) v source ->
            if Lm_symbol.eq v normal_sym then
               genv, oenv, options
            else
               let key = ConstString (loc, Lm_symbol.to_string v) in
               let genv, oenv, value = build_string genv oenv senv cenv source pos in
                  genv, oenv, key :: value :: options) (genv, oenv, []) sources
   in
   let create_map_sym =
      match options with
         [] -> empty_map_sym
       | _ -> create_lazy_map_sym
   in
   let oenv, create_map_var = senv_find_var genv oenv senv cenv pos loc create_map_sym in
   let options = ApplyString (loc, create_map_var, options, []) in
      genv, oenv, options

and build_rule_exp genv oenv senv cenv multiple target pattern sources body pos loc =
   let pos = string_pos "build_rule_exp" pos in
   let multiple = build_bool_exp loc multiple in
      match get_memo_target target with
         Some is_static ->
            let is_static = build_bool_exp loc is_static in
               build_memo_rule_exp genv oenv senv cenv multiple is_static pattern sources body pos loc
       | None ->
            build_normal_rule_exp genv oenv senv cenv multiple target pattern sources body pos loc

and build_normal_rule_exp genv oenv senv cenv multiple target pattern sources body pos loc =
   let pos = string_pos "build_normal_rule_exp" pos in

   (* Get the sources *)
   let source, sources = extract_option loc sources normal_sym in
   let genv, oenv, source = build_string genv oenv senv cenv source pos in
   let genv, oenv, options = build_options_exp genv oenv senv cenv pos loc sources in

   (* Get the body *)
   let genv, oenv, body =
      let original_class_names = oenv.oenv_class_names in
      let oenv = { (* oenv with *) oenv_class_names = SymbolSet.empty } in
      let cenv = cenv_rule_scope cenv in
      let genv, oenv, body, export, _ = build_body genv oenv senv cenv body pos loc in
      let oenv = { (* oenv with *) oenv_class_names = original_class_names } in
         genv, oenv, BodyString (loc, body, export)
   in

   let genv, oenv, target  = build_string genv oenv senv cenv target pos in
   let genv, oenv, pattern = build_string genv oenv senv cenv pattern pos in
   let args = [multiple; target; pattern; source; options; body] in
   (*
    * XXX: until var3, assume that it is written this.rule
   let oenv, rule_var = senv_find_var genv oenv senv cenv pos loc rule_sym in
   let e = ApplyExp (loc, rule_var, args, []) in
    *)
   let e = ApplyExp (loc, VarThis (loc, rule_sym), args, []) in
      genv, oenv, senv, e, ValValue

and build_memo_rule_exp genv oenv senv cenv multiple is_static names sources body pos loc =
   let pos = string_pos "build_memo_rule_exp" pos in

   (* Extract the special keys *)
   let source, sources = extract_option loc sources normal_sym in
   let key, sources = extract_option loc sources key_sym in
   let genv, oenv, source = build_string genv oenv senv cenv source pos in
   let genv, oenv, key = build_string genv oenv senv cenv key pos in
   let genv, oenv, options = build_options_exp genv oenv senv cenv pos loc sources in

   (* Build the body object expression *)
   let senv_body, cenv_body = senv_static_body senv cenv (Lm_symbol.new_symbol static_sym) in
   let genv, oenv, senv_body, el, _ =
      build_sequence genv oenv senv_body cenv_body ValValue pos (fun genv oenv senv cenv _ ->
            genv, oenv, senv, [ReturnObjectExp (loc, [])], ValValue) body
   in
   let body = ObjectString (loc, el, ExportNone) in

   (* Add the variables to the outer environment *)
   let names = build_literal_argv names pos in
   let senv, vars =
      if names = [] then
         (* Export all the object variables *)
         SymbolTable.fold (fun (senv, vars) _ info ->
               match info with
                  VarThis (loc, v) ->
                     let senv, info = senv_declare_static_var genv oenv senv cenv pos loc v in
                        senv, info :: vars
                | VarPrivate _
                | VarVirtual _
                | VarGlobal _ ->
                     senv, vars) (senv, []) senv_body.senv_object_senv
      else
         (* Export only the ones that are named *)
         List.fold_left (fun (senv, vars) name ->
               let v = Lm_symbol.add name in
               let info =
                  try
                     SymbolTable.find senv_body.senv_object_senv v
                  with Not_found ->
                     raise (OmakeException (pos, UnboundVar v))
               in
               let v =
                  match info with
                     VarThis (loc, v) ->
                        v
                   | VarPrivate _
                   | VarVirtual _
                   | VarGlobal _ ->
                        raise (OmakeException (pos, UnboundVarInfo info))
               in
               let senv, info = senv_declare_static_var genv oenv senv cenv pos loc v in
                  senv, info :: vars) (senv, []) names
   in
   let vars = ArrayString (loc, List.map (fun v -> VarString (loc, v)) vars) in

   (* The name has three parts: (file, index, key) *)
   let file = ApplyString (loc, file_var, [], []) in
   let genv, index = genv_new_index genv in
   let index = ConstString (loc, string_of_int index) in
   let args = [multiple; is_static; file; index; key; vars; source; options; body] in
   let oenv, rule_var = senv_find_var genv oenv senv cenv pos loc memo_rule_sym in
   let e = ApplyExp (loc, rule_var, args, []) in
      genv, oenv, senv, e, ValValue

(*
 * Shell command.
 *)
and build_shell_exp genv oenv senv cenv e pos loc =
   let pos = string_pos "build_shell_exp" pos in
   let genv, oenv, s = build_string genv oenv senv cenv e pos in
      genv, oenv, senv, ShellExp (loc, s), ValValue

(*
 * A ReturnFile expression.
 * Place the initialization function right before.
 *)
let build_return_file_exp genv oenv senv cenv loc =
   ReturnSaveExp loc

(*
 * Build the IR from the AST program.
 *)
let compile_string (genv, oenv, senv, cenv) e pos =
   let pos = string_pos "build_string" pos in
   let genv, oenv, s = build_string genv oenv senv cenv e pos in
      genv_warn_error genv senv;
      (genv, oenv, senv, cenv), s

let compile_exp (genv, oenv, senv, cenv) e =
   let genv, oenv, senv, e, _ = build_exp genv oenv senv cenv ValValue e in
   let genv, oenv, senv, ir = genv_close genv oenv senv e in
      genv_warn_error genv senv;
      (genv, oenv, senv, cenv), ir

let compile_prog (genv, oenv, senv, cenv) el =
   let loc = bogus_loc "Omake_ir_ast" in
   let pos = string_pos "compile_prog" (loc_exp_pos loc) in
   let genv, oenv, senv, el, _ =
      build_sequence genv oenv senv cenv ValValue pos (fun genv oenv senv cenv result ->
            let e = build_return_file_exp genv oenv senv cenv loc in
               genv, oenv, senv, [e], ValValue) el
   in
   let genv, oenv, senv, ir = genv_close genv oenv senv (SequenceExp (loc, el)) in
      genv_warn_error genv senv;
      (genv, oenv, senv, cenv), ir

let compile_exp_list (genv, oenv, senv, cenv) el =
   let loc = bogus_loc "Omake_ir_ast" in
   let pos = string_pos "compile_exp_list" (loc_exp_pos loc) in
   let genv, oenv, senv, el, _ =
      build_sequence genv oenv senv cenv ValValue pos (fun genv oenv senv cenv result ->
            genv, oenv, senv, [], result) el
   in
   let genv, oenv, senv, ir = genv_close genv oenv senv (SequenceExp (loc, el)) in
      genv_warn_error genv senv;
      (genv, oenv, senv, cenv), ir

let build_string (genv, oenv, senv, cenv) e pos =
   let genv, oenv, e = build_string genv oenv senv cenv e pos in
      genv_warn_error genv senv;
      (genv, oenv, senv, cenv), e

(*
 * Create the environment.
 *)
let cenv_empty =
   { cenv_scope          = None;
     cenv_context        = ContextTop;
   }

let senv_create venv =
   { senv_object_senv    = SymbolTable.empty;
     senv_update_vars    = SymbolTable.empty;
     senv_forced_vars    = ForcedVars.empty;
     senv_all_vars       = AllVars.empty;
     senv_export_mode    = ExportNoneMode;
     senv_venv           = venv
   }

let genv_create open_file node =
   { genv_open_file      = open_file;
     genv_file           = node;
     genv_static_index   = 0;
     genv_warning_count  = 0;
     genv_lazy           = lazy_empty
   }

let penv_create open_file venv node =
   let cenv = cenv_empty in
   let senv = senv_create venv in
   let oenv = oenv_empty in
   let genv = genv_create open_file node in
      genv, oenv, senv, cenv

let penv_class_names (_, oenv, senv, _) =
   let class_names = SymbolSet.to_list oenv.oenv_class_names in
   let vars = ForcedVars.to_vars senv.senv_forced_vars in
      class_names, vars

let penv_of_vars open_file venv node vars =
   let genv, oenv, senv, cenv = penv_create open_file venv node in
   let vars = SymbolTable.add vars file_sym file_var in
   let vars = SymbolTable.add vars file_id_sym file_id_var in
   let forced_vars, all_vars =
      SymbolTable.fold (fun (forced_vars, all_vars) v info ->
            let forced_vars = ForcedVars.add_extern forced_vars v info in
            let all_vars = AllVars.add all_vars (var_scope_of_var_info info, v) info in
               forced_vars, all_vars) (senv.senv_forced_vars, senv.senv_all_vars) vars
   in
   let senv =
      { senv with senv_forced_vars = forced_vars;
                  senv_all_vars = all_vars
      }
   in
      genv, oenv, senv, cenv

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
