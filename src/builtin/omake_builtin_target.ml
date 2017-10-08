(*
 * Operations on targets.
 *
 * \begin{doc}
 * \section{Examining the dependency graph}
 * \end{doc}
 *
 *)


include  Omake_pos.Make (struct let name = "Omake_builtin_target" end)


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
      let args  = Omake_eval.values_of_value venv pos arg in
      let nodes = List.map (Omake_eval.file_of_value venv pos) args in
      let  find_deps deps node =
        try
          let env = Omake_build_util.get_env pos loc in
          let command = Omake_node.NodeTable.find env.env_commands node in
          Omake_node.NodeSet.union deps command.command_build_deps
        with
          Not_found ->
          raise (Omake_value_type.OmakeException (pos, StringNodeError ("file is not buildable", node)))
      in
      List.fold_left find_deps Omake_node.NodeSet.empty nodes
    | _ ->
      raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
  in
  Omake_value_type.ValArray (List.map (fun v -> Omake_value_type.ValNode v) (Omake_node.NodeSet.to_list nodes))

let dependencies_all_core test venv pos loc args =
   let pos = string_pos "dependencies-all" pos in
      match args with
         [arg] ->
            let env =
               try Omake_build_util.get_env pos loc with
                  Not_found ->
                     raise (Omake_value_type.OmakeException (pos, StringError "this command can only be executed in a rule body"))
            in
            let commands = env.env_commands in
            let args  = Omake_eval.values_of_value venv pos arg in
            let nodes =
               List.fold_left (fun nodes v ->
                     Omake_node.NodeSet.add nodes (Omake_eval.file_of_value venv pos v)) Omake_node.NodeSet.empty args
            in
            let rec find_deps found examined unexamined =
               if Omake_node.NodeSet.is_empty unexamined then
                  found
               else
                  let node = Omake_node.NodeSet.choose unexamined in
                  let unexamined = Omake_node.NodeSet.remove unexamined node in
                     if Omake_node.NodeSet.mem examined node then
                        find_deps found examined unexamined
                     else
                        let examined = Omake_node.NodeSet.add examined node in
                        let found, deps =
                           try
                              let command = Omake_node.NodeTable.find commands node in
                              let deps = command.command_build_deps in
                              let found =
                                 if test command then
                                    Omake_node.NodeSet.add found node
                                 else
                                    found
                              in
                                 found, deps
                           with
                              Not_found ->
                                 found, Omake_node.NodeSet.empty
                        in
                        let unexamined = Omake_node.NodeSet.union unexamined deps in
                           find_deps found examined unexamined
            in
            let nodes = find_deps Omake_node.NodeSet.empty Omake_node.NodeSet.empty nodes in
            Omake_value_type.ValArray 
              (List.map (fun v -> Omake_value_type.ValNode v) 
                 (Omake_node.NodeSet.to_list nodes))
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let dependencies_all = dependencies_all_core (fun _ -> true)
let dependencies_proper = 
  dependencies_all_core 
    (fun command -> not (Omake_build_util.is_leaf_command command))

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
   Omake_value_type.ValArray (List.map (fun v -> Omake_value_type.ValNode v) (Omake_node.NodeSet.to_list nodes))

let split_command _ (values1, lines1) command =
  let {Omake_env. command_values = values2;
       command_body = lines2;
       command_env = venv;
        _
      } = command
  in
  let values = List.rev_append values2 values1 in
  let lines =
    List.fold_left (fun lines line ->
      let v =
        match line with
          | Omake_value_type.CommandSection (_, _, e) ->
              let env = Omake_env.venv_get_env venv in
              Omake_value_type.ValBody (env, [], [], e, ExportNone)
          | CommandValue (_, exp, v) ->
              ValStringExp(exp,v)
      in
      v :: lines) lines1 lines2
  in
  values, lines

let split_commands venv (commands : Omake_build_type.command_body) =
  match commands with
  | CommandNone ->
    [], []
  | CommandInfo info
  | CommandLines (info, _, _)
  | CommandScanner (info, _, _, _) ->
    List.fold_left (split_command venv) ([], []) info

let target_of_command venv pos loc command =
  let { Omake_build_type.command_target       = target;
        command_effects      = effects;
        command_scanner_deps = scanner_deps;
        command_static_deps  = static_deps;
        command_build_deps   = build_deps;
        command_tee          = tee;
        command_lines        = commands;
        _
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
  let obj = Omake_env.venv_find_var_exn venv Omake_var.target_object_var in
  let obj =
    match obj with
      ValObject obj ->
      obj
    | _ ->
      raise (Omake_value_type.OmakeException (loc_pos loc pos, StringVarError ("object not defined", Omake_symbol.target_sym)))
  in

  (* Add the fields *)
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.target_sym (ValNode target) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.target_effects_sym effects in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.scanner_deps_sym scanner_deps in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.static_deps_sym static_deps in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.build_deps_sym build_deps in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.build_values_sym (ValArray command_values) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.build_commands_sym (ValArray command_lines) in

  (* Add the tee *)
  let obj =
    match Omake_exec_util.tee_file tee with
      Some name ->
      let node = Omake_env.venv_intern venv PhonyProhibited name in
      Omake_env.venv_add_field_internal obj Omake_symbol.output_file_sym (ValNode node)
    | None ->
      obj
  in
  Omake_value_type.ValObject obj

let target_core optional_flag venv pos loc args =
   let pos = string_pos "target" pos in
      match args with
         [arg] ->
            let args     = Omake_eval.values_of_value venv pos arg in
            let env      = Omake_build_util.get_env pos loc in
            let commands = env.env_commands in
            let targets =
               List.fold_left (fun targets v ->
                     let node = Omake_eval.file_of_value venv pos v in
                     let target =
                        try
                           let command = Omake_node.NodeTable.find commands node in
                              target_of_command venv pos loc command
                        with
                           Not_found ->
                              if optional_flag then
                                 Omake_builtin_util.val_false
                              else
                                 raise (Omake_value_type.OmakeException (pos, StringNodeError ("file is not buildable", node)))
                     in
                        target :: targets) [] args
            in
               Omake_value.concat_array (List.rev targets)
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

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
      match String.lowercase_ascii (Omake_eval.string_of_value venv pos tag) with
      | "succeeded" -> Omake_build_type.CommandSucceededTag
      | "failed" -> CommandFailedTag
      | tag ->
        raise (Omake_value_type.OmakeException (loc_pos loc pos, StringStringError ("find-build-targets: unknown option", tag)))
    in
    let env = Omake_build_util.get_env pos loc in
    let targets =
      Omake_build_util.command_fold env tag (fun targets command ->
        target_of_command venv pos loc command :: targets) [] in
    Omake_value.concat_array targets
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

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
      Omake_node.DirTable.fold (fun dirs dir _ ->
        Omake_value_type.ValDir dir :: dirs) [] (Omake_env.venv_directories venv)
    in
    Omake_value_type.ValArray (List.rev dirs)
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

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
  |[multiple; target; pattern; source; options; body], [] ->
    let multiple = Omake_eval.bool_of_value venv pos multiple in
    Omake_rule.eval_rule_exp venv pos loc multiple target pattern source options body
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 6, List.length args)))

(*
 * This is called whenever a .STATIC: ... or a .MEMO: ... rule is evaluated.
 * It isn't clear whether we want to document this.
 *)
let memo_rule_fun venv pos loc args kargs =
  let pos = string_pos "memo_rule_fun" pos in
  match args, kargs with
    [multiple; is_static; node; index; key; vars; source; options; body], [] ->
    let multiple = Omake_eval.bool_of_value venv pos multiple in
    let is_static = Omake_eval.bool_of_value venv pos is_static in
    let key = Omake_eval.values_of_value venv pos key in
    let key = Omake_value.key_of_value venv pos (ValArray (node :: index ::key)) in
    let vars = Omake_value.vars_of_value venv pos vars in
    let target = Omake_value.file_of_value venv pos node in
    let venv = Omake_rule.eval_memo_rule_exp venv pos loc multiple is_static key vars target source options body in
    venv, Omake_value_type.ValNone
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 9, List.length args)))

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
  if not (Omake_env.venv_options venv).osh then
    raise (Omake_value_type.OmakeException (pos, StringError "build can be called only from osh"));
  match args with
    [arg] ->
    let targets = Omake_eval.strings_of_value venv pos arg in
    let b =
      try Omake_build.build_fun venv targets with
        exn ->
        Omake_eval.raise_uncaught_exception pos exn
    in
    if b then Omake_builtin_util.val_true else Omake_builtin_util.val_false
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(************************************************************************
 * Hooks.
*)

let () =
  let builtin_funs =
    [true,  "target",               target,              Omake_ir.ArityExact 1;
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
    [true,  "rule",                 rule_fun,            Omake_ir.ArityExact 6;
     true,  "memo-rule",            memo_rule_fun,       ArityExact 9;
    ]
  in
  let pervasives_objects =
    ["Target"]
  in
  let builtin_info =
    { Omake_builtin_type.builtin_empty with builtin_funs = builtin_funs;
      builtin_kfuns = builtin_kfuns;
      pervasives_objects = pervasives_objects
    }
  in
  Omake_builtin.register_builtin builtin_info
