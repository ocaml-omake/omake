(*
 * Some builtin functions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
open Lm_printf

open Lm_symbol
open Lm_location

open Omake_ir
open Omake_env
open Omake_pos
open Omake_symbol
open Omake_node_sig
open Omake_exn_print
open Omake_value_type
open Omake_build_type
open Omake_builtin_util
open Omake_builtin_type

module Pos = MakePos (struct let name = "Omake_builtin" end)
open Pos

let object_sym = Lm_symbol.add "Object"

(*
 * Add a command line variable definition.
 *)
let add_command_def = Omake_builtin_util.add_command_def
let command_defs_are_nonempty = Omake_builtin_util.command_defs_are_nonempty
let venv_add_command_defs = Omake_builtin_util.venv_add_command_defs

(*
 * Register some builtin info.
 *)
let builtin_info = ref builtin_empty

let register_builtin info =
   let { builtin_vars       = builtin_vars1;
         builtin_funs       = builtin_funs1;
         builtin_kfuns      = builtin_kfuns1;
         builtin_objects    = builtin_objects1;
         pervasives_objects = pervasives_objects1;
         phony_targets      = phony_targets1;
         builtin_rules      = builtin_rules1
       } = !builtin_info
   in
   let { builtin_vars       = builtin_vars2;
         builtin_funs       = builtin_funs2;
         builtin_kfuns      = builtin_kfuns2;
         builtin_objects    = builtin_objects2;
         pervasives_objects = pervasives_objects2;
         phony_targets      = phony_targets2;
         builtin_rules      = builtin_rules2
       } = info
   in
   let info =
      { builtin_vars       = builtin_vars1 @ builtin_vars2;
        builtin_funs       = builtin_funs1 @ builtin_funs2;
        builtin_kfuns      = builtin_kfuns1 @ builtin_kfuns2;
        builtin_objects    = builtin_objects1 @ builtin_objects2;
        pervasives_objects = pervasives_objects1 @ pervasives_objects2;
        phony_targets      = phony_targets1 @ phony_targets2;
        builtin_rules      = builtin_rules1 @ builtin_rules2
       }
   in
      builtin_info := info

let get_registered_builtins () =
   !builtin_info

(*
 * Check that there are no keyword arguments.
 *)
let wrap_normal_prim_fun f venv pos loc args kargs =
   match kargs with
      [] ->
         venv, f venv pos loc args
    | (v, _) :: _ ->
         raise (OmakeException (loc_pos loc pos, StringVarError ("no such parameter", v)))

(*
 * Add all the functions to the environment.
 *)
let venv_add_builtins venv =
   let loc = bogus_loc "<builtins>" in
   let pos = string_pos "venv_add_builtins" (loc_exp_pos loc) in
   let { builtin_vars       = builtin_vars;
         builtin_funs       = builtin_funs;
         builtin_kfuns      = builtin_kfuns;
         builtin_objects    = builtin_objects;
         pervasives_objects = pervasives_objects;
         phony_targets      = phony_targets;
         builtin_rules      = builtin_rules
       } = get_registered_builtins ()
   in

   (* Add only to the protected (current object) environment *)
   let venv = venv_add_phony venv loc (List.map (fun s -> TargetString s) phony_targets) in
   let venv =
      List.fold_left (fun venv (special, s, f, arity) ->
            let name = Lm_symbol.add s in
            let v = VarGlobal (loc, name) in
            let p = venv_add_prim_fun venv name (wrap_normal_prim_fun f) in
            let no_args =
               match arity with
                  ArityExact 0 -> ApplyEmpty
                | _ -> ApplyNonEmpty
            in
               venv_add_var venv v (ValPrim (arity, special,no_args, p))) venv builtin_funs
   in
   let venv =
      List.fold_left (fun venv (special, s, f, arity) ->
            let name = Lm_symbol.add s in
            let v = VarGlobal (loc, name) in
            let p = venv_add_prim_fun venv name f in
            let no_args =
               match arity with
                  ArityExact 0 -> ApplyEmpty
                | _ -> ApplyNonEmpty
            in
               venv_add_var venv v (ValPrim (arity, special,no_args,  p))) venv builtin_kfuns
   in
   let venv =
      List.fold_left (fun venv (multiple, targets, sources) ->
            let targets = List.map (fun name -> TargetString name) targets in
            let sources = List.map (fun source -> NodeNormal, TargetString source) sources in
            let multiple =
               if multiple then
                  RuleMultiple
               else
                  RuleSingle
            in
            let venv, _ = venv_add_rule venv pos loc multiple targets [] [] sources [] [] [] in
               venv) venv builtin_rules
   in

   (* Add the Object object *)
   let obj = venv_empty_object in

   (* Add values to each of the primitive objects *)
   let venv =
      List.fold_left (fun venv (s, v, x) ->
            let obj = venv_add_field_internal obj v x in
            let v = VarGlobal (loc, Lm_symbol.add s) in
               venv_add_var venv v (ValObject obj)) venv builtin_objects
   in
   let venv =
      List.fold_left (fun venv s ->
            let v = VarGlobal (loc, Lm_symbol.add s) in
               venv_add_var venv v (ValObject obj)) venv pervasives_objects
   in

   (* Add the variables last *)
   let venv =
      List.fold_left (fun venv (s, v) ->
            let x = VarGlobal (loc, Lm_symbol.add s) in
               venv_add_var venv x (v venv)) venv builtin_vars
   in
      venv

(*
 * Add the Pervasives module.
 *)
let venv_add_pervasives venv =
   let loc = bogus_loc "Omake_builtin" in
   let pos = string_pos "venv_add_pervasives" (loc_exp_pos loc) in
   let () = venv_set_pervasives venv in
   let obj = object_of_file venv pos loc "Pervasives" in
   let venv = venv_flatten_object venv obj in
      venv_set_pervasives venv;
      venv

(*
 * Load a file.
 *)
let venv_include_rc_file venv name =
   if Sys.file_exists name then
      let node = venv_intern venv PhonyProhibited name in
         try
            let loc = bogus_loc (Filename.basename name) in
            let pos = string_pos "create_venv" (loc_exp_pos loc) in
               Omake_eval.include_file venv IncludePervasives pos loc node
         with
            exn ->
               eprintf "%a@." pp_print_exn exn;
               venv
   else
      venv

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
