(*
 * Rule expansion.
 *)


(*  Debugging. *)
val debug_active_rules : bool ref

(*
 * Expand rules so that the rule body is not a function.
 *)
val expand_rule : Omake_env.erule -> Omake_env.erule

(*
 * Glob options.
 *)
val glob_options_of_string : Lm_glob.glob_option list -> string -> Lm_glob.glob_option list
val glob_options_of_env : Omake_env.venv -> Omake_value_type.pos -> Lm_glob.glob_option list

(*
 * Evaluators for the Exec module.
 *)
val eval_shell   : Omake_env.venv -> Omake_value_type.pos -> (Omake_env.arg_command_line, Omake_env.pid, Omake_value_type.value) Omake_exec_type.shell

(*
 * Create the command lines.
 *   eval_commands venv loc target sloppy_deps commands
 *
 * The sloppy deps are used for scanner commands to represent the
 * results of the previous scan.
 *)
val eval_commands : Omake_env.venv -> Lm_location.loc -> Omake_node.Node.t -> Omake_node.NodeSet.t -> Omake_env.command_info list -> Omake_env.arg_command_line list

(*
 * Rules and shell expressions.
 *)
val eval_rule_exp :
   Omake_env.venv -> Omake_value_type.pos -> Lm_location.loc ->
   bool ->                      (* multiple (whether the rule was defined with a ::) *)
   Omake_value_type.value ->                     (* targets *)
   Omake_value_type.value ->                     (* patterns *)
   Omake_value_type.value ->                     (* sources *)
   Omake_value_type.value ->                     (* options *)
   Omake_value_type.value ->                     (* commands *)
   Omake_env.venv * Omake_value_type.value

val eval_memo_rule_exp :
   Omake_env.venv -> Omake_value_type.pos -> Lm_location.loc ->
   bool ->                      (* multiple (whether the rule was defined with a ::) *)
   bool ->                      (* static (whether the results should be cached in .omakedb) *)
   Omake_value_type.value ->                     (* key *)
   Omake_ir.var_info list ->             (* variables to be defined *)
   Omake_node.Node.t ->                    (* Target *)
   Omake_value_type.value ->                     (* sources *)
   Omake_value_type.value ->                     (* options *)
   Omake_value_type.value ->                     (* commands *)
   Omake_env.venv

val eval_shell_exp : Omake_env.venv -> Omake_value_type.pos -> Lm_location.loc -> Omake_value_type.value -> Omake_env.venv * Omake_value_type.value
val eval_shell_output : Omake_env.venv -> Omake_value_type.pos -> Lm_location.loc -> Omake_value_type.value -> string
