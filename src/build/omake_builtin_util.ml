1(*
 * Some builtin functions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 Mojave Group, Caltech
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
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_pos
open Omake_eval
open Omake_wild
open Omake_node
open Omake_value
open Omake_state
open Omake_node_sig
open Omake_value_type
open Omake_build_type
open Omake_symbol

module Pos = MakePos (struct let name = "Omake_builtin" end)
open Pos

(************************************************************************
 * Run-time variables.
 *
 * Strip the leading qualifiers.
 * This is a big hack, repeating Omake_ir_ast.
 * We may want to move this into there.
 *)
let parse_path unlinked venv pos loc s =
   let vl = List.map Lm_symbol.add (Lm_string_util.split "." s) in
      match Omake_ir_ast.parse_declaration venv pos loc vl with
         NameEmpty _ ->
            raise (OmakeException (pos, StringError "empty name"))

       | NameMethod (_, v, _ :: _) ->
            raise (OmakeException (pos, StringVarError ("name has too many components", v)))

       | NameMethod (info, v, vl) ->
            let info =
               match info.name_scope with
                  Some VarScopePrivate ->
                     VarPrivate (loc, v)
                | Some VarScopeThis ->
                     VarThis (loc, v)
                | Some VarScopeVirtual
                | None ->
                     VarVirtual (loc, v)
                | Some VarScopeGlobal ->
                     VarGlobal (loc, v)
            in
               info, vl

let parse_sym =
   parse_path (fun loc v -> VarThis (loc, v))

let parse_def venv pos loc s =
   let v, vl =
      parse_path (fun loc v ->
            VarVirtual (loc, v)) venv pos loc s
   in
      if vl <> [] then
         raise (OmakeException (pos, StringError "name has too many components"));
      v

(*
 * Variable manipulations.
 *)
let defined_sym venv pos loc s =
   let pos = string_pos "defined_sym" pos in
   let v, vl = parse_sym venv pos loc s in
      match vl with
         [] ->
            venv_defined venv v
       | _ ->
            eval_defined_field venv pos loc v vl

let get_sym venv pos loc s =
   let pos = string_pos "get_sym" pos in
   let v, vl = parse_sym venv pos loc s in
      match vl with
         [] ->
            venv_find_var venv pos loc v
       | _ ->
            snd (eval_find_method venv pos loc v vl)

let add_sym venv pos loc s x =
   let pos = string_pos "add_sym" pos in
   let v, vl = parse_sym venv pos loc s in
      if vl <> [] then
         raise (OmakeException (loc_pos loc pos, StringError "name has too many components"));
      venv_add_var venv v x

(*
 * Command-line definitions.
 *)
let command_defs = ref []

let add_command_def v s =
   command_defs := (v, s) :: !command_defs

let command_defs_are_nonempty () =
   !command_defs <> []

let venv_add_command_defs venv =
   let loc = bogus_loc "<command-line>" in
   let pos = string_pos "venv_add_command_defs" (loc_exp_pos loc) in
      List.fold_left (fun venv (v, s) ->
            let v = parse_def venv pos loc v in
               venv_add_var venv v (ValString s)) venv !command_defs

(*
 * Fold its in a sequence, and place separators between them.
 *)
let sequence_map f sl =
   let white = ValString " " in
   let rec collect seq sl =
      match sl with
         s :: sl ->
            let s = f s in
            let seq =
               if seq = [] then
                  [s]
            else
                  s :: white :: seq
            in
               collect seq sl
       | [] ->
            List.rev seq
   in
      collect [] sl

(*
 * Add separators to a list.
 *)
let sequence_list sl =
   let white = ValString " " in
   let rec collect sl =
      match sl with
         [s] ->
            [s]
       | s :: sl ->
            s :: white :: collect sl
       | [] ->
            []
   in
      collect sl

(*
 * Default Boolean values.
 *)
let val_true  = ValData "true"
let val_false = ValData "false"

let val_of_bool b =
   if b then val_true else val_false

(*
 * Maintaining the environment.
 *)
let saved_env = ref None

let set_env env =
   saved_env := Some env

let get_env pos loc =
   match !saved_env with
      Some env ->
         env
    | None ->
         raise (OmakeException (loc_pos loc pos, StringError "this function can be called only in rule bodies"))

let is_build_phase () =
   !saved_env <> None

(*
 * Check whether a node is a leaf node.
 *)
let is_leaf_command command =
   let { command_scanner_deps = scanner_deps;
         command_static_deps  = static_deps;
         command_build_deps   = build_deps;
         command_lines        = lines
       } = command
   in
      NodeSet.is_empty scanner_deps
      && NodeSet.is_empty static_deps
      && NodeSet.is_empty build_deps
      && (lines = CommandNone)

let is_leaf_node env node =
   try is_leaf_command (NodeTable.find env.env_commands node) with
      Not_found ->
         false

(*
 * Extend an object with another.
 * The argument may be a file or an object.
 *)
let object_of_file venv pos loc s =
   let pos  = string_pos "extends" pos in
   let node = find_include_file venv pos loc s in
      try venv_find_object_file_exn venv node with
         Not_found ->
            let obj = eval_object_file venv pos loc node in
               venv_add_object_file venv node obj;
               obj

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
