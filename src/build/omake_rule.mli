(*
 * Rule expansion.
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
open Lm_glob
open Lm_location

open Omake_ir
open Omake_env
open Omake_pos
open Omake_node
open Omake_exec_type
open Omake_cache_type
open Omake_value_type

(*
 * Debugging.
 *)
val debug_active_rules : bool ref

(*
 * Expand rules so that the rule body is not a function.
 *)
val expand_rule : erule -> erule

(*
 * Glob options.
 *)
val glob_options_of_string : glob_option list -> string -> glob_option list
val glob_options_of_env : venv -> pos -> glob_option list

(*
 * Evaluators for the Exec module.
 *)
val eval_shell   : venv -> pos -> (arg_command_line, pid, value) shell

(*
 * Create the command lines.
 *   eval_commands venv loc target sloppy_deps commands
 *
 * The sloppy deps are used for scanner commands to represent the
 * results of the previous scan.
 *)
val eval_commands : venv -> loc -> Node.t -> NodeSet.t -> command_info list -> arg_command_line list

(*
 * Rules and shell expressions.
 *)
val eval_rule_exp :
   venv -> pos -> loc ->
   bool ->                      (* multiple (whether the rule was defined with a ::) *)
   value ->                     (* targets *)
   value ->                     (* patterns *)
   value ->                     (* sources *)
   value ->                     (* options *)
   value ->                     (* commands *)
   venv * value

val eval_memo_rule_exp :
   venv -> pos -> loc ->
   bool ->                      (* multiple (whether the rule was defined with a ::) *)
   bool ->                      (* static (whether the results should be cached in .omakedb) *)
   value ->                     (* key *)
   var_info list ->             (* variables to be defined *)
   Node.t ->                    (* Target *)
   value ->                     (* sources *)
   value ->                     (* options *)
   value ->                     (* commands *)
   venv

val eval_shell_exp : venv -> pos -> loc -> value -> venv * value
val eval_shell_output : venv -> pos -> loc -> value -> string

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
