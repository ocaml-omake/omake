(*
 * Operations on targets.
 *
 * \begin{doc}
 * \section{Examining the dependency graph}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2007 Mojave Group, California Institute of Technology and
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

open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_pos
open Omake_eval
open Omake_node
open Omake_rule
open Omake_exec
open Omake_value
open Omake_state
open Omake_symbol
open Omake_builtin
open Omake_node_sig
open Omake_exec_util
open Omake_build_type
open Omake_cache_type
open Omake_builtin_type
open Omake_builtin_util
open Omake_value_type
open Omake_build
open Omake_var

module Pos = MakePos (struct let name = "Omake_builtin_target" end)
open Pos

(************************************************************************
 * Targets.
 *)

(*
 * Find dependencies of a target.
 *
 * \begin{doc}
 * \threefuns{dependencies}{dependencies-all}{dependencies-proper}
 *
 * \begin{verbatim}
 *    $(dependencies targets) : File Array
 *    $(dependencies-all targets) : File Array
 *    $(dependencies-proper targets) : File Array
 *       targets : File Array
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+dependencies+ function returns the set of immediate dependencies of
 * the given targets. This function can only be used within a rule body and
 * all the arguments to the \verb+dependency+ function must also be dependencies of
 * this rule. This restriction ensures that all the dependencies are known when
 * this function is executed.
 *
 * The \verb+dependencies-all+ function is similar, but it expands the dependencies
 * recursively, returning all of the dependencies of a target, not just the immediate
 * ones.
 *
 * The \verb+dependencies-proper+ function returns all recursive dependencies, except
 * the dependencies that are leaf targets.  A leaf target is a target that has no
 * dependencies and no build commands; a leaf target corresponds to a source file
 * in the current project.
 *
 * In all three functions, files that are not part of the current project are silently
 * discarded. All three functions will return phony and scanner targets along with the
 * ``real'' ones.
 *
 * One purpose of the \verb+dependencies-proper+ function is for ``clean'' targets.
 * For example, one way to delete all intermediate files in a build is with a rule
 * that uses the \verb+dependencies-proper+.  Note however, that the rule requires
 * building the project before it can be deleted.
 *
 * \begin{verbatim}
 *     .PHONY: clean
 *
 *     APP = ...     # the name of the target application
 *     clean: $(APP)
 *        rm -f $(dependencies-proper $(APP))
 * \end{verbatim}
 *
 * Also note that the \verb+dependencies-proper+ function will return the phony and scanner
 * targets in addition to real one.
 *
 * For other (possibly better) alternatives, see Section~\ref{section:distclean} and
 * \hyperfun{filter-proper-targets}.
 * \end{doc}
 *)
let dependencies venv pos loc args =
   let pos = string_pos "dependencies" pos in
   let nodes =
      match args with
         [arg] ->
            let args  = values_of_value venv pos arg in
            let nodes = List.map (file_of_value venv pos) args in
            let rec find_deps deps node =
               try
                  let env = get_env pos loc in
                  let command = NodeTable.find env.env_commands node in
                     NodeSet.union deps command.command_build_deps
               with
                  Not_found ->
                     raise (OmakeException (pos, StringNodeError ("file is not buildable", node)))
            in
               List.fold_left find_deps NodeSet.empty nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
      ValArray (List.map (fun v -> ValNode v) (NodeSet.to_list nodes))

let dependencies_all_core test venv pos loc args =
   let pos = string_pos "dependencies-all" pos in
      match args with
         [arg] ->
            let env =
               try get_env pos loc with
                  Not_found ->
                     raise (OmakeException (pos, StringError "this command can only be executed in a rule body"))
            in
            let commands = env.env_commands in
            let args  = values_of_value venv pos arg in
            let nodes =
               List.fold_left (fun nodes v ->
                     NodeSet.add nodes (file_of_value venv pos v)) NodeSet.empty args
            in
            let rec find_deps found examined unexamined =
               if NodeSet.is_empty unexamined then
                  found
               else
                  let node = NodeSet.choose unexamined in
                  let unexamined = NodeSet.remove unexamined node in
                     if NodeSet.mem examined node then
                        find_deps found examined unexamined
                     else
                        let examined = NodeSet.add examined node in
                        let found, deps =
                           try
                              let command = NodeTable.find commands node in
                              let deps = command.command_build_deps in
                              let found =
                                 if test command then
                                    NodeSet.add found node
                                 else
                                    found
                              in
                                 found, deps
                           with
                              Not_found ->
                                 found, NodeSet.empty
                        in
                        let unexamined = NodeSet.union unexamined deps in
                           find_deps found examined unexamined
            in
            let nodes = find_deps NodeSet.empty NodeSet.empty nodes in
               ValArray (List.map (fun v -> ValNode v) (NodeSet.to_list nodes))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let dependencies_all = dependencies_all_core (fun _ -> true)
let dependencies_proper = dependencies_all_core (fun command -> not (is_leaf_command command))

(*
 * \begin{doc}
 * \fun{target}
 * \begin{verbatim}
 *    $(target targets) : Target Array
 *       targets : File Sequence
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+target+ function returns the Target object associated with each
 * of the targets.  See the \verb+Target+ object for more information.
 * \end{doc}
 *)
let array_of_node_set nodes =
   ValArray (List.map (fun v -> ValNode v) (NodeSet.to_list nodes))

let split_command venv (values1, lines1) command =
   let { command_values = values2;
         command_body = lines2
       } = command
   in
   let values = List.rev_append values2 values1 in
   let lines =
      List.fold_left (fun lines line ->
            let v =
               match line with
                  CommandSection (_, _, e) ->
                     ValBody (e, ExportNone)
                | CommandValue (_, exp, v) ->
                     ValStringExp(exp,v)
            in
               v :: lines) lines1 lines2
   in
      values, lines

let split_commands venv commands =
   match commands with
      CommandNone ->
         [], []
    | CommandInfo info
    | CommandLines (info, _, _)
    | CommandScanner (info, _, _, _) ->
         List.fold_left (split_command venv) ([], []) info

let target_of_command venv pos loc command =
   let { command_target       = target;
         command_effects      = effects;
         command_scanner_deps = scanner_deps;
         command_static_deps  = static_deps;
         command_build_deps   = build_deps;
         command_tee          = tee;
         command_lines        = commands
       } = command
   in

   (* Dependency lists *)
   let effects      = array_of_node_set effects in
   let scanner_deps = array_of_node_set scanner_deps in
   let static_deps  = array_of_node_set static_deps in
   let build_deps   = array_of_node_set build_deps in

   (* Command lists *)
   let command_values, command_lines = split_commands venv commands in

   (* Get the default target object *)
   let obj = venv_find_var_exn venv target_object_var in
   let obj =
      match obj with
         ValObject obj ->
            obj
       | _ ->
            raise (OmakeException (loc_pos loc pos, StringVarError ("object not defined", target_sym)))
   in

   (* Add the fields *)
   let obj = venv_add_field_internal obj target_sym (ValNode target) in
   let obj = venv_add_field_internal obj target_effects_sym effects in
   let obj = venv_add_field_internal obj scanner_deps_sym scanner_deps in
   let obj = venv_add_field_internal obj static_deps_sym static_deps in
   let obj = venv_add_field_internal obj build_deps_sym build_deps in
   let obj = venv_add_field_internal obj build_values_sym (ValArray command_values) in
   let obj = venv_add_field_internal obj build_commands_sym (ValArray command_lines) in

   (* Add the tee *)
   let obj =
      match tee_file tee with
         Some name ->
            let node = venv_intern venv PhonyProhibited name in
               venv_add_field_internal obj output_file_sym (ValNode node)
       | None ->
            obj
   in
      ValObject obj

let target_core optional_flag venv pos loc args =
   let pos = string_pos "target" pos in
      match args with
         [arg] ->
            let args     = values_of_value venv pos arg in
            let env      = get_env pos loc in
            let commands = env.env_commands in
            let targets =
               List.fold_left (fun targets v ->
                     let node = file_of_value venv pos v in
                     let target =
                        try
                           let command = NodeTable.find commands node in
                              target_of_command venv pos loc command
                        with
                           Not_found ->
                              if optional_flag then
                                 val_false
                              else
                                 raise (OmakeException (pos, StringNodeError ("file is not buildable", node)))
                     in
                        target :: targets) [] args
            in
               concat_array (List.rev targets)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let target = target_core false
let target_optional = target_core true

(*
 * \begin{doc}
 * \fun{find-build-targets}
 *
 * \begin{verbatim}
 *     $(find-build-targets tag) : Target Array
 *        tag : Succeeded | Failed
 * \end{verbatim}
 *
 * The \verb+find-build-targets+ allow the results
 * of the build to be examined.  The \verb+tag+ must
 * specifies which targets are to be returned; the comparison
 * is case-insensitive.
 *
 * \begin{description}
 * \item[Succeeded] The list of targets that were built successfully.
 * \item[Failed] The list of targets that could not be built.
 * \end{description}
 *
 * These are used mainly in conjuction with the
 * \verb+.BUILD_SUCCESS+ (Section~\ref{target:.BUILD_SUCCESS}) and
 * \verb+.BUILD_FAILURE+ (Section~\ref{target:.BUILD_FAILURE}) phony targets.
 * For example, adding the following to your project \verb+OMakefile+
 * will print the number of targets that failed (if the build failed).
 *
 * \begin{verbatim}
 *     .BUILD_FAILURE:
 *         echo "Failed target count: $(length $(find-build-targets Failed))"
 * \end{verbatim}
 * \end{doc}
 *)
let find_build_targets venv pos loc args =
   let pos = string_pos "find-build-targets" pos in
      match args with
         [tag] ->
            let tag =
               match String.lowercase (string_of_value venv pos tag) with
                  "succeeded" -> CommandSucceededTag
                | "failed" -> CommandFailedTag
                | tag ->
                     raise (OmakeException (loc_pos loc pos, StringStringError ("find-build-targets: unknown option", tag)))
            in
            let env = get_env pos loc in
            let targets =
               command_fold env tag (fun targets command ->
                     target_of_command venv pos loc command :: targets) []
            in
               concat_array targets
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{project-directories}
 *
 * \begin{verbatim}
 *    $(project-directories) : Dir Array
 * \end{verbatim}
 *
 * The \verb+project-directories+ function returns the list of all directories
 * that are considered to be part of the project.
 *
 * To get the complete directory list, this function should be called
 * from within a rule body.
 * \end{doc}
 *)
let project_directories venv pos loc args =
   let pos = string_pos "project-directories" pos in
      match args with
         [] ->
            let dirs =
               DirTable.fold (fun dirs dir _ ->
                     ValDir dir :: dirs) [] (venv_directories venv)
            in
               ValArray (List.rev dirs)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(*
 * \begin{doc}
 * \fun{rule}
 *
 * The \verb+rule+ function is called whenever a build rule is defined.
 * It is unlikely that you will need to redefine this function, except in
 * very exceptional cases.
 *
 * \begin{verbatim}
 *    rule(multiple, target, pattern, sources, options, body) : Rule
 *       multiple : String
 *       target   : Sequence
 *       pattern  : Sequence
 *       sources  : Sequence
 *       options  : Array
 *       body     : Body
 * \end{verbatim}
 *
 * The \verb+rule+ function is called when a rule is evaluated.
 *
 * \begin{description}
 * \item[multiple] A Boolean value indicating whether the rule was defined
 *   with a double colon \verb+::+.
 * \item[target] The sequence of target names.
 * \item[pattern] The sequence of patterns.  This sequence will be empty
 *   for two-part rules.
 * \item[sources] The sequence of dependencies.
 * \item[options] An array of options.  Each option is represented
 *   as a \hyperobj{Map} associating each specified option with
 *   a value.
 * \item[body] The body expression of the rule.
 * \end{description}
 *
 * Consider the following rule.
 *
 * \begin{verbatim}
 *    target: pattern: sources :name1: option1 :name2: option2
 *       expr1
 *       expr2
 * \end{verbatim}
 *
 * This expression represents the following function call, where
 * square brackets are used to indicate arrays, and the curly
 * brackets represent a \hyperobj{Map}.
 *
 * \begin{verbatim}
 *    rule(false, target, pattern, sources,
 *         { $|:name1:| = option1; $|:name2:| = option2 }
 *         [expr1; expr2])
 * \end{verbatim}
 * \end{doc}
 *)
let rule_fun venv pos loc args kargs =
   let pos = string_pos "rule_fun" pos in
      match args, kargs with
         [multiple; target; pattern; source; options; body], [] ->
            let multiple = bool_of_value venv pos multiple in
               eval_rule_exp venv pos loc multiple target pattern source options body
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 6, List.length args)))

(*
 * This is called whenever a .STATIC: ... or a .MEMO: ... rule is evaluated.
 * It isn't clear whether we want to document this.
 *)
let memo_rule_fun venv pos loc args kargs =
   let pos = string_pos "memo_rule_fun" pos in
      match args, kargs with
         [multiple; is_static; node; index; key; vars; source; options; body], [] ->
            let multiple = bool_of_value venv pos multiple in
            let is_static = bool_of_value venv pos is_static in
            let key = values_of_value venv pos key in
            let key = key_of_value venv pos (ValArray (node :: index ::key)) in
            let vars = vars_of_value venv pos vars in
            let target = file_of_value venv pos node in
            let venv = eval_memo_rule_exp venv pos loc multiple is_static key vars target source options body in
               venv, ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 9, List.length args)))

(*
 * \begin{doc}
 * \fun{build}
 *
 * \begin{verbatim}
 *     build(targets : File Array) : bool
 * \end{verbatim}
 *
 * Build the given targets.  The value is true iff the build was successful.
 * This function can be used only in \verb+osh+.
 * \end{doc}
 *)
let build venv pos loc args =
   let pos = string_pos "build" pos in
      if not (Omake_options.opt_osh (venv_options venv)) then
         raise (OmakeException (pos, StringError "build can be called only from osh"));
      match args with
         [arg] ->
            let targets = strings_of_value venv pos arg in
            let b =
               try Omake_build.build_fun venv targets with
                  exn ->
                     raise_uncaught_exception pos exn
            in
               if b then val_true else val_false
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(************************************************************************
 * Hooks.
 *)

let () =
   let builtin_funs =
      [true,  "target",               target,              ArityExact 1;
       true,  "target-optional",      target_optional,     ArityExact 1;
       true,  "dependencies",         dependencies,        ArityExact 1;
       true,  "dependencies-all",     dependencies_all,    ArityExact 1;
       true,  "dependencies-proper",  dependencies_proper, ArityExact 1;
       true,  "project-directories",  project_directories, ArityExact 0;
       true,  "find-build-targets",   find_build_targets,  ArityExact 1;
       true,  "build",                build,               ArityExact 1;
      ]
   in
   let builtin_kfuns =
      [true,  "rule",                 rule_fun,            ArityExact 6;
       true,  "memo-rule",            memo_rule_fun,       ArityExact 9;
      ]
   in
   let pervasives_objects =
      ["Target"]
   in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs;
                           builtin_kfuns = builtin_kfuns;
                           pervasives_objects = pervasives_objects
      }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
