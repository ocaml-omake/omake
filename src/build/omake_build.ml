module I = Lm_instrument

module Pos = Omake_pos.Make (struct let name = "Omake_build" end);;

exception BuildExit of int

type prompt_state =
  {
    count : int; (* success count *)
    save : float; (* next .omakedb save time *)
    progress : float; (* next progress bar update time *)
  }

let save_interval = ref Omake_magic.default_save_interval

(*
 * XXX: Should this be an option as well?
 *)
let prompt_interval = 0.5

(*
 * Maximum number of events that can be queued during
 * the .BUILD_* phases.
 *)
let max_pending_events = 256

(*
 * Build debugging.
 *)
let debug_rule =
  Lm_debug.create_debug (**)
    { debug_name = "rule";
      debug_description = "Display debugging information for rule execution";
      debug_value = false
    }

let debug_build =
  Lm_debug.create_debug (**)
    { debug_name = "build";
      debug_description = "Display debugging information during the build process";
      debug_value = false
    }

let debug_deps =
  Lm_debug.create_debug (**)
    { debug_name = "deps";
      debug_description = "Display dependency information as it is read";
      debug_value = false
    }

let scanner_fun = Omake_cache.scanner_fun
let rule_fun    = Omake_cache.rule_fun
let env_fun     = Omake_cache.env_fun
let env_target  = Omake_cache.env_target

(*
 * The argument to "Restart" is the reason for restarting.
 * The default reason is a change in one of the OMakefiles or included files.
 *)
exception Restart of string option
exception UnknownTarget of Omake_node.Node.t


type default_scanner_mode =
  | IsEnabled
  | IsDisabled
  | IsWarning
  | IsError

(** Special nodes. *)
let build_begin   = ".BUILD_BEGIN"
let build_success = ".BUILD_SUCCESS"
let build_failure = ".BUILD_FAILURE"
let build_begin_target   = Omake_node.Node.create_phony_global build_begin
let build_success_target = Omake_node.Node.create_phony_global build_success
let build_failure_target = Omake_node.Node.create_phony_global build_failure


(*
 * Check if a command list contains value dependencies.
 *)
let commands_have_value_dependencies commands =
  List.exists 
    (fun (command : Omake_env.command_info) -> command.command_values <> []) 
    commands


let flatten_deps table =
  Omake_node.NodeTable.fold (fun deps1 _ deps2 ->
    Omake_node.NodeSet.union deps1 deps2) Omake_node.NodeSet.empty table

(*
 * Get the scanner mode.
 *)
let venv_find_scanner_mode venv pos =
  try
    let v = Omake_env.venv_find_var_exn venv Omake_var.scanner_mode_var in
    match Omake_eval.string_of_value venv pos v with
    | "enabled" ->
      IsEnabled
    | "disabled" ->
      IsDisabled
    | "warning" ->
      IsWarning
    | "error" ->
      IsError
    | s ->
      raise (Omake_value_type.OmakeException (pos, StringStringError ("bad scanner mode (should be enabled, disabled, error, or warning)", s)))
  with
    Not_found ->
    IsError

let restartable_exn = function
  | Omake_value_type.OmakeException _
  | Omake_value_type.UncaughtException _
  | Omake_value_type.RaiseException _ ->
    true
  | _ ->
    false

let probe_process_changes = I.create "process_changes"

(*
 * JYH: the overhead of scanning directories every time
 * it changes is pretty high.  We may want to think of
 * other ways of doing this.
 *
 * Intercept directory change events and pretend that every file
 * in the directory has changed.
 *)
let process_changes is_node_relevant process_node venv cwd cache =
  I.instrument probe_process_changes (fun (event : Lm_notify.event) ->
  let process_node name =
    let node = Omake_env.venv_intern_cd venv PhonyProhibited cwd name in
    let changed = is_node_relevant node && Omake_cache.stat_changed cache node in
    if !Lm_notify.debug_notify then
      Format.eprintf "Omake_build.process_changes: received %s event for node: %a@."
        (if changed then "relevant" else "ignored") Omake_node.pp_print_node node;
    if changed then
      process_node node;
    changed
  in
  match event  with
    { notify_code = DirectoryChanged; notify_name = name } ->
    List.fold_left (fun changed name -> process_node name || changed) false 
      (Lm_unix_util.list_directory name)
  | { notify_code = (Changed | Created); notify_name = name } ->
    process_node name
  | _ ->
    false
  )

(*
 * Find a command from a target.
 * May raise Not_found.
 *)
let find_command (env : Omake_build_type.t) target =
  Omake_node.NodeTable.find env.env_commands target

(*
 * Find the immediate parents of a node in the dependency DAG
 *)
let find_parents (env : Omake_build_type.t) node =
  try
    let inverse = Omake_node.NodeTable.find env.env_inverse node in
    Omake_node.NodeTable.fold (fun nodes node _ -> Omake_node.NodeSet.add nodes node) Omake_node.NodeSet.empty inverse
  with
    Not_found ->
    Omake_node.NodeSet.empty



let probe_all_dependencies = I.create "all_dependencies"


(*
 * Compute all of the dependencies.
 *)
let all_dependencies dependencies_of (env : Omake_build_type.t) =
  I.instrument probe_all_dependencies (fun nodes ->
  let commands = env.env_commands in
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
            let deps = dependencies_of command in
            let found = Omake_node.NodeSet.add found node in
            found, deps
          with
            Not_found ->
            found, Omake_node.NodeSet.empty
        in
        let unexamined = Omake_node.NodeSet.union unexamined deps in
        find_deps found examined unexamined
  in
  find_deps Omake_node.NodeSet.empty Omake_node.NodeSet.empty nodes
  )

let all_build_dependencies =
  all_dependencies (fun command -> command.command_build_deps)

let all_scanner_dependencies =
  all_dependencies (fun command -> command.command_scanner_deps)

(*
 * Print the dependency information.
 *)
let rec pp_print_dependencies_aux show_all env buf (command : Omake_build_type.command) =
  match command with 
    { command_target       = target;
      command_effects      = effects;
      command_scanner_deps = scanner_deps;
      command_static_deps  = static_deps;
      command_build_deps   = build_deps;
      _} -> 
    let inverse = find_parents env target in
    let options = Omake_env.venv_options env.env_venv in
    let total, build_deps, scanner_deps =
      if show_all && Omake_options.opt_all_dependencies options then
        "all transitive ", all_build_dependencies env build_deps, all_scanner_dependencies env scanner_deps
      else
        "", build_deps, scanner_deps
    in
    Format.fprintf buf "@[<v 3>target: %a@ @[<b 3>%sscanner dependencies:%a@]@ @[<b 3>static dependencies:%a@]@ @[<b 3>%sbuild dependencies:%a@]@ @[<b 3>dependencies are merged from:%a@]@ @[<b 3>targets that depend on this node at this point:%a@]@]" (**)
      Omake_node.pp_print_node target
      total
      Omake_node.pp_print_node_set scanner_deps
      Omake_node.pp_print_node_set static_deps
      total
      Omake_node.pp_print_node_set build_deps
      Omake_node.pp_print_node_set effects
      Omake_node.pp_print_node_set inverse;

    if show_all && Omake_options.opt_verbose_dependencies options then
      let nodes = Omake_node.NodeSet.union scanner_deps build_deps in
      Format.fprintf buf "@ @ --- Complete dependency listing ---@ ";
      Omake_node.NodeSet.iter (fun node ->
        let command = find_command env node in
        Format.fprintf buf "@ %a" (pp_print_dependencies_aux false env) command) nodes

let pp_print_dependencies =
  pp_print_dependencies_aux true


let probe_reclassify_command = I.create "reclassify_command"


(*
 * Reclassify the commands.
 *)
let reclassify_command 
    (env : Omake_build_type.t) (command : Omake_build_type.command) =
  I.instrument probe_reclassify_command (fun (state : Omake_build_type.command_state) ->
  (* Unlink the node from its current list *)
  let pred = command.command_pred in
  let succ = !(command.command_succ) in
  let _ =
    pred := succ;
    match succ with
      Some next -> next.Omake_build_type.command_pred <- pred
    | None -> ()
  in

  (* Update the job counter *)
  let incr =
    match command.command_state, state with
      CommandSucceeded _, CommandSucceeded _ -> 0
    | CommandSucceeded _, _                  -> -1
    | _,                  CommandSucceeded _ -> 1
    | _                                      -> 0
  in
  let () = env.env_succeeded_count <- env.env_succeeded_count + incr in

  (* Add to the new list *)
  let l = Omake_build_util.command_worklist env (Omake_build_util.command_tag state) in
  let next = !l in
  l := Some command;
  command.command_state <- state;
  command.command_pred <- l;
  command.command_succ := next;
  match next with
    Some next -> next.command_pred <- command.command_succ
  | None -> ()
  )


(************************************************************************
 * Other target utilities.
*)

let target_is_phony = Omake_node.Node.is_phony

let target_exists (env : Omake_build_type.t) node =
  Omake_cache.exists env.env_cache node

let target_is_buildable (env : Omake_build_type.t) venv node =
  Omake_target.target_is_buildable env.env_cache venv node



let env_options (env : Omake_build_type.t) =
  Omake_env.venv_options env.env_venv

(*
 * Add a target to the print-dependency list.
 * The target dependencies will be printed just before the
 * build rule is executed.
 *)
let print_node_dependencies (env : Omake_build_type.t ) target =
  env.env_print_dependencies <- Omake_node.NodeSet.add env.env_print_dependencies target

(*
 * Start command if it is idle.
 *)
let start_command env (command : Omake_build_type.command) =
  if command.command_state = CommandIdle then
    reclassify_command env command CommandInitial

(*
 * Find a process by pid.
 *)
let find_pid env pid =
  Omake_build_util.command_find env CommandRunningTag (fun command ->
    match command.command_state with
      CommandRunning (pid', _) ->
      pid' = pid
    | _ ->
      false)

(*
 * Get the command lines.
 *)
let command_lines (command : Omake_build_type.command) =
  match command.command_lines with
  | CommandNone ->
    [], None
  | CommandScanner (_, _, lines, digest)
  | CommandLines (_, lines, digest) ->
    lines, digest
  | CommandInfo _ ->
    raise (Invalid_argument "build_lines")

(*
 * See if this is a scanner command.
 *)
let command_is_scanner (command : Omake_build_type.command) =
  Omake_node.Node.kind command.command_target = NodeScanner

let set_tee env (command : Omake_build_type.command) tee =
  Omake_node.NodeSet.iter (fun target -> Omake_build_tee.unlink_tee (find_command env target)) command.command_effects;
  Omake_build_tee.unlink_tee command;
  command.command_tee <- tee

(************************************************************************
 * Command creation.
*)

(*
 * Create a command for a target that always exists.
 *)
let create_exists_command env _ loc target =
  (* Create the command, and link it to the worklist *)
  let l = Omake_build_util.command_worklist env CommandSucceededTag in
  let next = !l in
  let succ = ref next in
  let effects = Omake_node.NodeSet.singleton target in
  let command : Omake_build_type.command =
    { command_venv               = env.env_venv;
      command_state              = CommandSucceeded Omake_node.NodeTable.empty;
      command_target             = target;
      command_locks              = effects;
      command_effects            = effects;
      command_scanner_deps       = Omake_node.NodeSet.empty;
      command_static_deps        = Omake_node.NodeSet.empty;
      command_build_deps         = Omake_node.NodeSet.empty;
      command_blocked            = [];
      command_loc                = loc;
      command_lines              = CommandNone;
      command_tee                = Omake_exec_util.tee_none;
      command_pred               = l;
      command_succ               = succ
    }
  in
  (* Link it into the list *)
  l := Some command;
  (match next with
    Some next -> next.command_pred <- succ
  | None -> ());

  (* Add to the command table *)
  env.env_optional_count <- env.env_optional_count + 1;
  env.env_commands <- Omake_node.NodeTable.add env.env_commands target command

(*
 * Create a command for a squashed target,
 * where the digest value is ignored, but the
 * target should be built.
 *)
let create_squashed_command env _ loc target =
  (* Create the command, and link it to the worklist *)
  let l = Omake_build_util.command_worklist env CommandInitialTag in
  let next = !l in
  let succ = ref next in
  let effects = Omake_node.NodeSet.singleton target in
  let static_deps = Omake_node.NodeSet.singleton (Omake_node.Node.core target) in
  let command : Omake_build_type.command =
    { command_venv               = env.env_venv;
      command_state              = CommandInitial;
      command_target             = target;
      command_effects            = effects;
      command_locks              = static_deps;
      command_static_deps        = static_deps;
      command_scanner_deps       = Omake_node.NodeSet.empty;
      command_build_deps         = Omake_node.NodeSet.empty;
      command_blocked            = [];
      command_loc                = loc;
      command_lines              = CommandNone;
      command_tee                = Omake_exec_util.tee_none;
      command_pred               = l;
      command_succ               = succ
    }
  in
  (* Link it into the list *)
  l := Some command;
  (match next with
    Some next -> next.command_pred <- succ
  | None -> ());

  (* Add to the command table *)
  env.env_commands <- Omake_node.NodeTable.add env.env_commands target command

(*
 * Create a command in a state.
 *)
let create_command env venv target effects lock_deps static_deps scanner_deps loc _ commands =
  (* Create the command, and link it to the worklist *)
  let l = Omake_build_util.command_worklist env CommandInitialTag in
  let next = !l in
  let succ = ref next in
  let () =
    if not (Omake_node.NodeSet.for_all (fun node -> Omake_node.Node.kind node = NodeScanner) scanner_deps) then
      let print_error buf =
        Format.fprintf buf "@[<v 3>Malformed scanner dependencies:";
        Format.fprintf buf "@ target: %a" Omake_node.pp_print_node target;
        Format.fprintf buf "@ @[<b 3>lock_deps:%a@]" Omake_node.pp_print_node_set lock_deps;
        Format.fprintf buf "@ @[<b 3>static_deps:%a@]" Omake_node.pp_print_node_set static_deps;
        Format.fprintf buf "@ @[<b 3>scanner_deps:%a@]" Omake_node.pp_print_node_set scanner_deps;
        Format.fprintf buf "@]"
      in
      raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, LazyError print_error))
  in
  let effects = Omake_node.NodeSet.add effects target in
  let locks = Omake_node.NodeSet.union lock_deps effects in
  let command : Omake_build_type.command =
    { command_venv               = venv;
      command_state              = CommandInitial;
      command_target             = target;
      command_effects            = effects;
      command_locks              = locks;
      command_static_deps        = static_deps;
      command_scanner_deps       = scanner_deps;
      command_build_deps         = Omake_node.NodeSet.empty;
      command_blocked            = [];
      command_loc                = loc;
      command_lines              = commands;
      command_tee                = Omake_exec_util.tee_none;
      command_pred               = l;
      command_succ               = succ
    }
  in
  (* Link it into the list *)
  l := Some command;
  (match next with
    Some next -> next.command_pred <- succ
  | None -> ());

  (* Add to the command table *)
  env.env_commands <- Omake_node.NodeTable.add env.env_commands target command


let probe_build_any_command = I.create "build_any_command"

(*
 * Build a command given a directory and a command list.
 *)
let build_any_command (env : Omake_build_type.t)
    pos loc venv target effects locks sources scanners =
  I.instrument probe_build_any_command (fun commands ->
  let pos = Pos.string_pos "build_any_command" (Pos.loc_pos loc pos) in

  (* Directory for this target *)
  let dir = Omake_env.venv_dir venv in

  (* Get all the extra dependencies that are statically defined *)
  let lock_deps, static_deps, scanner_deps =
    try Omake_node.NodeTable.find env.env_explicit_deps target with
      Not_found ->
      Omake_node.NodeSet.empty, Omake_node.NodeSet.empty, Omake_node.NodeSet.empty
  in
  let lock_deps = Omake_node.NodeSet.union lock_deps locks in
  let static_deps = Omake_node.NodeSet.union static_deps sources in
  let scanner_deps = Omake_node.NodeSet.union scanner_deps scanners in
  let implicit_lock_deps, implicit_static_deps, implicit_scanner_deps, implicit_values = 
    Omake_env.venv_find_implicit_deps venv target in
  let lock_deps = Omake_node.NodeSet.union lock_deps implicit_lock_deps in
  let static_deps = Omake_node.NodeSet.union static_deps implicit_static_deps in
  let scanner_deps = Omake_node.NodeSet.union scanner_deps implicit_scanner_deps in
  let scanner_deps =
    if Omake_node.Node.kind target = NodeScanner || not (Omake_node.NodeSet.is_empty scanner_deps) then
      scanner_deps
    else
      let scanner_mode = venv_find_scanner_mode venv pos in
      if scanner_mode = IsDisabled then
        scanner_deps
      else
        let scanner_target = Omake_node.Node.create_escape NodeScanner target in
        if target_is_buildable env venv pos scanner_target then
          match scanner_mode with
            IsWarning ->
            Format.eprintf "*** omake: warning: using default scanner %a@." Omake_node.pp_print_node scanner_target;
            Omake_node.NodeSet.add scanner_deps scanner_target
          | IsError ->
            raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringNodeError ("default scanners are not allowed", scanner_target)))
          | IsEnabled ->
            Omake_node.NodeSet.add scanner_deps scanner_target
          | IsDisabled ->
            scanner_deps
        else
          scanner_deps
  in
  let () =
    if Lm_debug.debug debug_build then
      Format.eprintf "@[<hv 3>Building new rule: %s@ @[<b 3>lock deps:%a@]@ @[<b 3>static deps:%a@]@ @[<b 3>scanner deps:%a@]@]@." (**)
        (Omake_node.Node.fullname target)
        Omake_node.pp_print_node_set lock_deps
        Omake_node.pp_print_node_set static_deps
        Omake_node.pp_print_node_set scanner_deps
  in
  let commands : Omake_build_type.command_body =
    match implicit_values, commands with
    | [], [] ->
      CommandNone
    | [], _ :: _ ->
      CommandInfo commands
    | _ :: _, _ ->
      let (command : Omake_env.command_info) =
        { command_env = venv;
          command_sources = Omake_node.NodeSet.to_list sources;
          command_values = implicit_values;
          command_body = []
        }
      in
      CommandInfo (command :: commands)
  in
  create_command env venv target effects lock_deps static_deps scanner_deps loc dir commands
  )


(*
 * Build a null command for a file that exists but has no
 * build rules.
 *)
let build_null_command env pos loc venv target =
  let pos = Pos.string_pos "build_null_command" pos in
  if Lm_debug.debug Omake_env.debug_implicit then
    Format.eprintf "build_null_command: %a@." Omake_node.pp_print_node target;
  if target_is_phony target || target_exists env target then begin
    build_any_command env pos loc venv target Omake_node.NodeSet.empty Omake_node.NodeSet.empty Omake_node.NodeSet.empty Omake_node.NodeSet.empty [];
    if Omake_options.opt_poll (env_options env) && not (target_is_phony target) then
      Omake_exec.Exec.monitor env.env_exec target
  end
  else
    raise (UnknownTarget target)

(*
 * Build a command from an environment, a set of sources, and
 * a list of commands.
 *)
let build_explicit_command
    (env : Omake_build_type.t)
    pos loc target effects locks venv sources scanners commands =
  let pos = Pos.string_pos "build_explicit_command" pos in
  let () =
    if Lm_debug.debug Omake_env.debug_implicit then
      Format.eprintf "@[<hv 3>build_explicit_command: explicit rule %a:@ @[<b 3>effects =%a@]@ @[<b 3>sources =%a@]@ @[<b 3>scanners =%a@]@]@." (**)
        Omake_node.pp_print_node target
        Omake_node.pp_print_node_set effects
        Omake_node.pp_print_node_set sources
        Omake_node.pp_print_node_set scanners
  in

  (* Check that all the effects have the same environment *)
  let bogus =
    Omake_node.NodeSet.fold (fun bogus effect ->
      try
        let erule = Omake_node.NodeTable.find env.env_explicit_targets effect in
        if erule.rule_env != venv then
          Omake_node.NodeSet.add bogus effect
        else
          bogus
      with
        Not_found ->
        bogus) Omake_node.NodeSet.empty effects
  in
  let _ =
    if not (Omake_node.NodeSet.is_empty bogus) then
      let pp_print_target_loc buf (target, loc) =
        Format.fprintf buf "@ @[<hv3>%a@ (%a)@]" Omake_node.pp_print_node target Lm_location.pp_print_location loc
      in
      let rec pp_print_bogus_set buf bogus =
        if not (Omake_node.NodeSet.is_empty bogus) then begin
          let effect = Omake_node.NodeSet.choose bogus in
          pp_print_target_loc buf (effect, (Omake_node.NodeTable.find env.env_explicit_targets effect).rule_loc);
          pp_print_bogus_set buf (Omake_node.NodeSet.remove bogus effect)
        end
      in
      Format.eprintf "@[<v 3>*** omake:@ These file are targeted separately, but appear as effects of a single rule.@ This is likely to lead to unpredictable behavior.@ @[<v 3>targets:%a%a@]@]@." (**)
        pp_print_target_loc (target, loc)
        pp_print_bogus_set bogus
  in
  build_any_command env pos loc venv target effects locks sources scanners commands

(*
 * Build a command from a set of implicit rules.
 * We choose the first rule where the dependencies can be satisfied.
 *)
let build_implicit_command 
    (env : Omake_build_type.t) pos loc target venv =
  let pos = Pos.string_pos "build_implicit_command" pos in
  match Omake_target.venv_find_buildable_implicit_rule env.env_cache venv pos target with
    Some { rule_loc      = loc;
           rule_effects  = effects;
           rule_locks    = locks;
           rule_sources  = sources;
           rule_scanners = scanners;
           rule_commands = commands;
           _
         } ->
    build_explicit_command env pos loc target effects locks venv sources scanners commands
  | None ->
    build_null_command env pos loc venv target

(*
 * Build a command from an explicit rule.
 * The rule defines an environment that we can use
 * to find the scanner, and extra dependencies.
 *
 * If the erule specifies some commands, use them.
 * Otherwise, find an implicit rule to use.
 *)
let build_explicit_target env pos _ target erule =
  let pos = Pos.string_pos "build_explicit_target" pos in
  match Omake_rule.expand_rule erule with 
    { rule_loc         = loc;
      rule_env         = venv;
      rule_effects     = effects;
      rule_locks       = locks;
      rule_sources     = sources;
      rule_scanners    = scanners;
      rule_commands    = commands;
      _
    } -> 
    if commands = [] then
      build_implicit_command env pos loc target venv
    else
      build_explicit_command env pos loc target effects locks venv sources scanners commands

(*
 * Create a new command for the target.
 *)
let build_command_non_escaped 
    (env : Omake_build_type.t) pos loc target =
  let pos = Pos.string_pos "build_command_non_escaped" pos in

  (*
   * If the target has an explicit rule, use it.
   * Otherwise, this is a leaf in the dependency tree.
   *)
  let erule =
    try Omake_build_type.ExplicitTarget (Omake_node.NodeTable.find env.env_explicit_targets target) with
      Not_found ->
      let target_dir = Omake_node.Node.dir target in
      try ExplicitDirectory (Omake_node.DirTable.find env.env_explicit_directories target_dir) with
        Not_found ->
        ExplicitNone
  in
  match erule with
    ExplicitTarget erule ->
    build_explicit_target env pos loc target erule
  | ExplicitDirectory venv ->
    build_implicit_command env pos loc target venv
  | ExplicitNone ->
    build_null_command env pos loc env.env_venv target

(*
 * Create a new command for the scanner target.  The difference is
 * that the scanner uses the dependent environment unless it is explicit.
 *)
let build_scanner_command 
    (env : Omake_build_type.t) pos loc target venv =
  let pos = Pos.string_pos "build_scanner_command" pos in
  (*
   * If the target has an explicit rule, use it.
   * Otherwise, this is a leaf in the dependency tree.
   *)
  match  (Omake_node.NodeTable.find env.env_explicit_targets target) with
  | erule -> build_explicit_target env pos loc target erule
  | exception Not_found -> build_implicit_command env pos loc target venv

(*
 * If the node is escaped, just create it as succeeded.
 * If it is squashed, create a fake node that depends
 * on the original file.
 *)
let build_command env pos loc target =
  let pos = Pos.string_pos "build_command" pos in
  let () =
    if Lm_debug.debug debug_build then
      Format.eprintf "@[<hv 3>Building command for: %s@]@." (Omake_node.Node.fullname target)
  in
  match Omake_node.Node.kind target with
    NodeOptional
  | NodeExists ->
    create_exists_command env pos loc target
  | NodeSquashed ->
    create_squashed_command env pos loc target
  | NodePhony
  | NodeNormal
  | NodeScanner ->
    build_command_non_escaped env pos loc target


let probe_start_or_build_commands = I.create "start_or_build_commands"

(*
 * Start commands, or build them.
 *)
let start_or_build_commands env pos loc parent =
  I.instrument probe_start_or_build_commands (fun targets ->
  let pos = Pos.string_pos "start_or_build_commands" pos in
  Omake_node.NodeSet.iter (fun target ->
    try start_command env (find_command env target) with
      Not_found ->
      (try build_command env pos loc target with
        UnknownTarget target ->
        let print_error buf =
          Format.fprintf buf "Do not know how to build \"%a\" required for \"%a\"" Omake_node.pp_print_node target Omake_node.pp_print_node parent
        in
        raise (Omake_value_type.OmakeException (pos, LazyError print_error)))) targets
  )


let probe_start_or_build_scanners = I.create "start_or_build_scanners"

(*
 * Start scanners.  The difference is that a scanner inherits
 * the environment of the parent, unless the scanner
 * target is explicit.
 *)
let start_or_build_scanners env pos loc parent targets =
  I.instrument probe_start_or_build_scanners (fun venv ->
  let pos = Pos.string_pos "start_or_build_scanners" pos in
  Omake_node.NodeSet.iter (fun target ->
    try
      let command = find_command env target in
      (*
  if command.command_venv != venv then
  Format.eprintf "@[<hv 3>*** omake warning:@ @[<hv3>scanner uses a different environment than the target@ %a:@]@ @[<hv3>scanner definition:@ %a;@]@ @[<hv 3>current location:@ %a@]@]@." (**)
  Omake_node.pp_print_node target pp_print_location command.command_loc pp_print_location loc;
 *)
      start_command env command
    with
      Not_found ->
      (try build_scanner_command env pos loc target venv with
        UnknownTarget target ->
        let print_error buf =
          Format.fprintf buf "Do not know how to build \"%a\" required for \"%a\"" Omake_node.pp_print_node target Omake_node.pp_print_node parent
        in
        raise (Omake_value_type.OmakeException (pos, LazyError print_error)))) targets
  )

let probe_start_or_build_effects = I.create "start_or_build_effects"


(*
 * Make sure the effect sets form equivalence classes.
 * Every command in the effect set should have the
 * same effects.
 *)
let start_or_build_effects env pos loc target =
  I.instrument probe_start_or_build_effects (fun effects ->
  let pos = Pos.string_pos "start_or_build_effects" pos in
  let step effects =
    start_or_build_commands env pos loc target effects;
    Omake_node.NodeSet.fold (fun (changed, effects) effect ->
      let command = find_command env effect in
      let effects' = command.command_effects in
      if effects' == effects then
        changed, effects
      else
        let effects = Omake_node.NodeSet.union effects effects' in
        command.command_effects <- effects;
        true, effects) (false, effects) effects
  in
  let rec fixpoint effects =
    let changed, effects = step effects in
    if changed then
      fixpoint effects
  in
  fixpoint effects
 )

(*
 * Catch errors.
 *)
let build_command env pos loc target =
  try build_command env pos loc target with
    UnknownTarget _ ->
    raise (Omake_value_type.OmakeException (pos, StringNodeError ("Do not know how to build", target)))

(************************************************************************
 * Dependency management
*)

let probe_command_set_blocked = I.create "command_set_blocked"

(*
 * Add inverse entries from the command,
 * and set the blocked queue.
 *)
let command_set_blocked (env : Omake_build_type.t)
    (command : Omake_build_type.command) =
  I.instrument probe_command_set_blocked (fun deps ->
  match command with 
    { command_target = target ; _} -> 

    let inverse =
      Omake_node.NodeSet.fold (fun inverse dep ->
        Omake_node.NodeTable.filter_add inverse dep (fun commands ->
          let commands =
            match commands with
              Some commands ->
              commands
            | None ->
              Omake_node.NodeTable.empty
          in
          Omake_node.NodeTable.add commands target command)) env.env_inverse deps
    in
    env.env_inverse <- inverse;
    command.command_blocked <- Omake_node.NodeSet.to_list deps
  )

(*
 * Add the build deps.
 *)
let command_set_build_deps env command deps =
  command_set_blocked env command deps;
  command.command_build_deps <- deps

(*
 * Check if the command overlaps with a running process.
 *)
let command_conflicts_with_running (env : Omake_build_type.t) 
    (command : Omake_build_type.command) =
  let locks = command.command_locks in
  Omake_build_util.command_exists env CommandRunningTag (fun command ->
    Omake_node.NodeSet.exists (Omake_node.NodeSet.mem locks) command.command_locks) false

(*
 * Check if a command succeeded.
 *)
let command_succeeded (command : Omake_build_type.command) =
  match command with
  | { command_state = CommandSucceeded _; _ } ->
    true
  | _ ->
    false

(*
 * Command is blocked until all dependencies have been built.
 *)
let command_is_blocked (env : Omake_build_type.t)
    (command : Omake_build_type.command)  =
  match command with 
    { command_blocked = blocked ; _} -> 
    if blocked = [] then
      false
    else
      let rec process_blocked env deps =
        match deps with
          dep :: deps' ->
          if command_succeeded (find_command env dep) then
            process_blocked env deps'
          else
            deps
        | [] ->
          []
      in
      let blocked = process_blocked env blocked in
      command.command_blocked <- blocked;
      blocked <> []

(*
 * Check if all effects that are not the target have been scanned.
 *)
let command_effects_are_scanned 
    (env : Omake_build_type.t)
    (command : Omake_build_type.command) =
  match command with 
    { command_target = target;
      command_effects = effects;
      _
    } -> 
    Omake_node.NodeSet.for_all (fun effect ->
        if Omake_node.Node.equal effect target then
          true
        else
          match (find_command env effect).command_state with
          | CommandScannedPending
          | CommandSucceeded _ ->
            true
          | _ ->
            false) effects

let probe_enable_parents = I.create "enable_parents"


(*
 * Reclassify dependent rules.
 *)
let enable_parents 
    (env : Omake_build_type.t) =
  I.instrument probe_enable_parents
  (fun (command : Omake_build_type.command) ->
  let enable_parent _ command =
    if not (command_is_blocked env command) then
      let state : Omake_build_type.command_state =
        match command.command_state with
        | CommandScanBlocked ->
          if command_effects_are_scanned env command then
            CommandScanned
          else
            CommandScannedPending
        | CommandBlocked ->
          CommandReady
        | state ->
          state
      in
      reclassify_command env command state
  in
  let parents =
    try Omake_node.NodeTable.find env.env_inverse command.command_target with
      Not_found ->
      Omake_node.NodeTable.empty
  in
  Omake_node.NodeTable.iter enable_parent parents
  )

(************************************************************************
 * Generic execution.
*)

let probe_parse_deps = I.create "parse_deps"

(*
 * Parse the dependency list.
 *)
let parse_deps _ venv target =
  I.instrument probe_parse_deps
  (fun file ->
  let deps = Omake_eval.compile_deps venv target file in
  if !debug_deps then
    begin
      Format.eprintf "@[<v 3>Scanner: %s" file;
      List.iter (fun (targets, sources) ->
        Format.eprintf "@ @[<hv 0>@[<b 3>targets =";
        List.iter (fun target -> Format.eprintf "@ %s" target) targets;
        Format.eprintf "@]@ @[<b 3>sources =";
        List.iter (fun source -> Format.eprintf "@ %s" source) sources;
        Format.eprintf "@]@]") deps;
      Format.eprintf "@]@."
    end;
  List.fold_left (fun table (targets, sources) ->
    let sources =
      List.fold_left (fun set s ->
        let node = Omake_env.venv_intern venv PhonyOK s in
        Omake_node.NodeSet.add set node) Omake_node.NodeSet.empty sources
    in
    List.fold_left (fun table target ->
      let target = Omake_env.venv_intern venv PhonyOK target in
      Omake_node.NodeTable.filter_add table target (fun set ->
        match set with
          Some set ->
          Omake_node.NodeSet.union set sources
        | None ->
          sources)) table targets) Omake_node.NodeTable.empty deps
  )

(*
 * A command finished with an error.
 *)
let abort_command env command code =
  if Omake_options.opt_terminate_on_error (env_options env) then
    env.env_error_code <- code;
  Omake_build_tee.env_close_failed_tee env command;
  reclassify_command env command (CommandFailed code)

let abort_commands env targets code =
  if Omake_options.opt_terminate_on_error (env_options env) then
    env.env_error_code <- code;
  Omake_node.NodeSet.iter (fun target ->
    reclassify_command env (find_command env target) (CommandFailed code)) targets

(************************************************************************
 * Scanner execution.
*)

let probe_finish_scanned = I.create "finish_scanned"


(*
 * All scanner subgoals have finished, and all effects
 * have been scanned too.  Take the union of all the dependencies.
 *)
let finish_scanned (env : Omake_build_type.t) =
  I.instrument probe_finish_scanned
  (fun (command : Omake_build_type.command) ->
  match command with 
    { command_loc          = loc;
      command_target       = target;
      command_effects      = effects;
      _
    } -> 
    let pos = Pos.loc_exp_pos loc in

    (* Get the command for each of the effects *)
    let effects_commands =
      Omake_node.NodeSet.fold (fun commands command ->
        find_command env command :: commands) [] effects
    in

    (* Find all the scanner results *)
    let scanner_deps =
      List.fold_left 
        (fun scanner_deps (command : Omake_build_type.command) ->
          Omake_node.NodeSet.union scanner_deps command.command_scanner_deps)
        Omake_node.NodeSet.empty effects_commands
    in
    let dep_tables =
      Omake_node.NodeSet.fold (fun dep_tables scanner ->
        let scan_command = find_command env scanner in
        match scan_command.command_state with
          CommandSucceeded table ->
          table :: dep_tables
        | _ ->
          let print_error buf =
            Format.fprintf buf "@[<hv 3>Internal error in Omake_build.finish_scanned:@ %a@ %a@ @[<v 3>Effects:%a@]@]" (**)
              Omake_build_util.pp_print_command command
              Omake_build_util.pp_print_command scan_command
              (Omake_build_util.pp_print_node_states env) effects
          in
          raise (Omake_value_type.OmakeFatalErr (pos, LazyError print_error))) [] scanner_deps
    in

    (* Now collect all the deps *)
    let deps =
      List.fold_left (fun deps (command : Omake_build_type.command) ->
        match command with 
          { command_target = target;
            command_static_deps = static_deps;
            command_scanner_deps = scanner_deps;
            _} -> 

          let deps = Omake_node.NodeSet.union deps static_deps in
          let deps = Omake_node.NodeSet.union deps scanner_deps in
          List.fold_left (fun deps table ->
            try Omake_node.NodeSet.union deps (Omake_node.NodeTable.find table target) with
              Not_found ->
              deps) deps dep_tables) Omake_node.NodeSet.empty effects_commands
    in
    (* Make sure all the newly discovered dependencies have commands *)
    start_or_build_commands env pos loc target deps;

    (* Set the state of all the effects *)
    List.iter (fun (command : Omake_build_type.command) ->
      let target = command.command_target in
      (* Set the dependencies *)
      command_set_build_deps env command deps;

      (* Dependencies are final at this point *)
      if Omake_node.NodeSet.mem env.env_print_dependencies target then
        Format.eprintf "@[<v 3>dependencies:@ %a@]@." (pp_print_dependencies env) command;

      (* Set the command state *)
      let state =
        if command_succeeded command then
          command.command_state
        else if command_is_blocked env command then
          CommandBlocked
        else
          CommandReady
      in
      reclassify_command env command state) effects_commands
  )

let probe_finish_scanner = I.create "finish_scanner"

(*
 * A scanner has finished successfully.
 * Notify the parents.
 *)
let finish_scanner env (command : Omake_build_type.command) =
  I.instrument probe_finish_scanner
  (fun scanned_deps ->
  match command with
    {command_target = target; _} -> 
    if Lm_debug.debug Omake_env.debug_scanner then
      Format.eprintf "@[<hv 3>finish_scanner %a:%a@]@." (**)
        Omake_node.pp_print_node target
        Omake_node.pp_print_node_set_table scanned_deps;

    (* This command has been scanned *)
    reclassify_command env command (CommandSucceeded scanned_deps);

    (* Notify parents that something is done *)
    enable_parents env command
  )

let probe_save_and_finish_scanner_results =
  I.create "save_and_finish_scanner_results"

(*
 * A scanner command finished successfully.
 *
 * XXX: HACK: Recompute the command digest if the scanner dependencies
 * have changed.
 *
 * This is probably a reasonable thing to do, but it means that the
 * rule text may be computed twice for .SCANNER rules.
 *
 * This could be wrong in two cases:
 *    1. If the .SCANNER body performs a side-effect while computing
 *       the rule text.
 *    2. If the .SCANNER body depends non-trivially on the scanner
 *       dependencies $&.
 *)
let save_and_finish_scanner_results 
    (env : Omake_build_type.t) 
    (command : Omake_build_type.command) =
  I.instrument probe_save_and_finish_scanner_results
  (fun scanned_deps ->
  (* Add the run to the cache *)
  match command with 
    { command_loc            = loc;
      command_venv           = venv;
      command_target         = target;
      command_lines          = scanner;
      command_locks          = locks;
      command_build_deps     = build_deps;
      _
    } -> 
    (* Save in cache *)
    let cache = env.env_cache in
    let targets = Omake_node.NodeSet.singleton target in

    (* Re-stat the locks *)
    let () = Omake_node.NodeSet.iter (fun lock -> ignore (Omake_cache.force_stat cache lock)) locks in

    (* Recompute the scanner digest *)
    let digest =
      match scanner with
        CommandNone ->
        None
      | CommandLines (_, _, digest) ->
        digest
      | CommandScanner (info, sloppy_deps, _, digest) ->
        let deps = flatten_deps scanned_deps in
        if Omake_node.NodeSet.equal deps sloppy_deps then
          digest
        else
          let pos = Pos.loc_exp_pos loc in
          let scanner_commands = Omake_rule.eval_commands venv loc target deps info in
          let scanner_commands = List.map Omake_command.command_allow_output scanner_commands in
          Omake_command_digest.digest_of_commands pos scanner_commands
      | CommandInfo _ ->
        raise (Invalid_argument "scanner_lines")
    in
    Omake_cache.add cache scanner_fun target targets build_deps digest (MemoSuccess scanned_deps);
    finish_scanner env command scanned_deps
  )

let probe_save_and_finish_scanner_success =
  I.create "save_and_finish_scanner_success"

(*
 * Add the run to the cache.
 *)
let save_and_finish_scanner_success 
    (env : Omake_build_type.t) (command : Omake_build_type.command) =
  I.instrument probe_save_and_finish_scanner_success
  (fun filename ->
  match command with 
    { command_loc = loc;
      command_venv = venv;
      command_target  = target;
      _
    } -> 


    (*
   * Get the result.
   * The parser may still fail.
   *)
    let result =
      try
        let result = parse_deps env venv target filename in
        (* Remove the file as early as possible *)
        Lm_unix_util.try_unlink_file filename;
        Some result
      with
        Omake_value_type.OmakeException _
      | Omake_value_type.UncaughtException _
      | Failure _
      | Not_found
      | Parsing.Parse_error
      | Sys_error _ ->
        None
    in
    match result with
      Some result ->
      if Lm_debug.debug Omake_env.debug_scanner then
        Format.eprintf "@[<v 3>Saving dependencies: %a@ @[<b 3>scanned deps:%a@]@]@." (**)
          Omake_node.pp_print_node target
          Omake_node.pp_print_node_set_table result;
      Omake_build_tee.env_close_success_tee env command;
      save_and_finish_scanner_results env command result

    | None ->
      (* Don't remove the file, in case the user wants to look at it *)
      let pos = Pos.string_pos "save_and_finish_scanner" (Pos.loc_exp_pos loc) in
      let lines, _ = command_lines command in
      let shell = Omake_rule.eval_shell venv pos in
      let options = env_options env in
      let divert_only = not (Omake_options.opt_output options OutputNormal) in
      let handle_err = Omake_exec_util.tee_stderr command.command_tee divert_only Omake_exec_id.null_id in
      let out = Lm_printf.make_formatter handle_err (fun () -> handle_err "" 0 0) in
      Format.fprintf out "@?*** omake: scanner produced ill-formed output@.";
      Omake_exec_print.pp_status_lines out options shell "scan" lines;
      Format.fprintf out "*** omake: @[<hv0>scanner output is saved in@ %s@]@." filename;
      abort_command env command Omake_state.scanner_error_code
  )

(*
 * Failed run.
 *)
let save_and_finish_scanner_failed env command filename code =
  Lm_unix_util.try_unlink_file filename;
  abort_command env command code

let probe_execute_scanner =
  I.create "execute_scanner"

(*
 * Run the command.
 *)
let execute_scanner (env : Omake_build_type.t) =
  I.instrument probe_execute_scanner
  (fun  (command : Omake_build_type.command) ->
  match command with 
    { command_target = target;
      command_loc = loc;
      command_venv = venv;
      _
    } -> 
    let pos = Pos.string_pos "execute_scanner" (Pos.loc_exp_pos loc) in
    let scanner, _ = command_lines command in

    (* Save errors to the tee *)
    let options = Omake_env.venv_options venv in
    let tee = Omake_exec_util.tee_create (Omake_options.opt_divert options) in
    let divert_only = not (Omake_options.opt_output options OutputNormal) in
    let copy_stdout = Omake_exec_util.tee_stdout tee divert_only in
    let copy_stderr = Omake_exec_util.tee_stderr tee divert_only in

    (* Save output into a temporary file *)
    let tmpfile = Filename.temp_file "omake" ".deps" in
    let handle_out = Omake_exec_util.copy_file tmpfile in

    (* Debugging *)
    let () =
      if Lm_debug.debug Omake_env.debug_scanner then
        Format.eprintf "@[<v 3>run_scanner %a@ to tmp file %s:%a@]@." (**)
          Omake_node.pp_print_node target
          tmpfile
          Omake_env.pp_print_arg_command_lines scanner

    in
    let shell = Omake_rule.eval_shell venv pos in
    set_tee env command tee;
    env.env_scan_exec_count <- succ env.env_scan_exec_count;
    match Omake_exec.Exec.spawn env.env_exec shell 
        (Omake_env.venv_options venv) copy_stdout handle_out copy_stderr "scan" target scanner with
      ProcessFailed ->
      (* The fork failed *)
      abort_command env command Omake_state.fork_error_code
    | ProcessStarted pid ->
      (* Process was started *)
      env.env_idle_count <- pred env.env_idle_count;
      reclassify_command env command (CommandRunning (pid, Some tmpfile))
  )

let probe_start_scanner = I.create "start_scanner"

(*
 * Execute a command.
 * Check with the cache to see if this command is already
 * up-to-date.
 *)
let start_scanner 
    (env : Omake_build_type.t) =
  I.instrument probe_start_scanner
  (fun (command : Omake_build_type.command) ->
  match command with 
    { command_venv           = venv;
      command_loc            = loc;
      command_target         = target;
      command_lines          = scanner;
      command_build_deps     = build_deps;
      _
    } ->

    let pos = Pos.string_pos "start_scanner" (Pos.loc_exp_pos loc) in
    let sloppy_deps =
      try flatten_deps (Omake_cache.find_result_sloppy env.env_cache scanner_fun target) with
        Not_found ->
        Omake_node.NodeSet.empty
    in
    let scanner, scanner_digest =
      match scanner with
        CommandNone ->
        [], None
      | CommandInfo info ->
        assert (info <> []);
        let scanner_commands = Omake_rule.eval_commands venv loc target sloppy_deps info in
        let scanner_commands = List.map Omake_command.command_allow_output scanner_commands in
        let scanner_digest = Omake_command_digest.digest_of_commands pos scanner_commands in
        let info : Omake_build_type.command_body =
          if commands_have_value_dependencies info then
            CommandScanner (info, sloppy_deps, scanner_commands, scanner_digest)
          else
            CommandLines (info, scanner_commands, scanner_digest)
        in
        command.command_lines <- info;
        scanner_commands, scanner_digest
      | CommandScanner (_, _, lines, digest)
      | CommandLines (_, lines, digest) ->
        lines, digest
    in
    if scanner = [] then begin
      (* If scanner is empty, don't do anything *)
      if Lm_debug.debug Omake_env.debug_scanner then
        Format.eprintf "@[<hv 3>start_scanner: target has no scanner: %a@]@." Omake_node.pp_print_node target;
      save_and_finish_scanner_results env command Omake_node.NodeTable.empty
    end
    else begin
      (* Look up previous results from the cache *)
      env.env_scan_count <- succ env.env_scan_count;
      try
        let scanned_deps = Omake_cache.find_result env.env_cache scanner_fun build_deps scanner_digest in
        if Lm_debug.debug Omake_env.debug_scanner then
          Format.eprintf "@[<hv 3>start_scanner: target dependencies are accurate %a:@ @[<b 3>scanner's build deps:%a@]@ @[<b 3>scanned deps:%a@]@]@." (**)
            Omake_node.pp_print_node target
            Omake_node.pp_print_node_set build_deps
            Omake_node.pp_print_node_set_table scanned_deps;
        finish_scanner env command scanned_deps
      with
        Not_found ->
        execute_scanner env command
    end
  )

(************************************************************************
 * Rule execution.
*)


let probe_finish_rule_success = I.create "finish_rule_success"

(*
 * A command finished successfuly.
 *)
let finish_rule_success env =
  I.instrument probe_finish_rule_success
  (fun command ->
  (* Get a list of all commands that might be updated *)
  reclassify_command env command (CommandSucceeded Omake_node.NodeTable.empty);
  enable_parents env command
  )

(*
 * A command remains failed.
 *)
let finish_rule_failed env (command : Omake_build_type.command) code =
  match command with 
    { command_effects = effects ; _} -> 
    abort_commands env effects code

(*
 * A command finished successfuly.
 *)
let hexify_digest = function
    Some digest ->
    Lm_string_util.hexify digest
  | None ->
    "none"

let probe_save_and_finish_rule_success =
  I.create "save_and_finish_rule_success"

let probe_safrs_effects =
  I.create "safrs_effects"

let save_and_finish_rule_success 
    (env : Omake_build_type.t) =
  I.instrument probe_save_and_finish_rule_success
  (fun (command : Omake_build_type.command) ->
  (* Add the run to the cache *)
  match command with 
    { command_loc        = loc;
      command_target     = target;
      command_effects    = effects;
      command_locks      = locks;
      command_build_deps = build_deps;
      _
    } ->  
    let cache = env.env_cache in
    let _, commands_digest = command_lines command in

    (* Collect the effects that are not phony, check that they were created *)
    let effects =
      I.instrument probe_safrs_effects
      (fun () ->
      Omake_node.NodeSet.fold (fun effects effect ->
        let digest = Omake_cache.force_stat cache effect in
        if Omake_node.Node.is_phony effect then
          effects
        else if digest = None then begin
          abort_command env command Omake_state.exn_error_code;
          raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, StringNodeError ("rule failed to build its target", effect)))
        end else
          Omake_node.NodeSet.add effects effect) Omake_node.NodeSet.empty effects
      )
      ()
    in

    (* Re-stat the locks *)
    Omake_node.NodeSet.iter (fun lock ->
      ignore (Omake_cache.force_stat cache lock)) locks;

    (* Add a memo for a specific target *)
    if Lm_debug.debug debug_rule then
      Format.eprintf "@[<v 3>saving %a:@ @[<hv 3>build-deps:%a@]@ @[<hv 3>effects:%a@]@ digest: %s@]@." (**)
        Omake_node.pp_print_node target
        Omake_node.pp_print_node_set build_deps
        Omake_node.pp_print_node_set effects
        (hexify_digest commands_digest);

    (* Add the memo only if the target is not phony *)
    if not (Omake_node.NodeSet.is_empty effects) then
      Omake_cache.add cache rule_fun target effects build_deps commands_digest (MemoSuccess Omake_node.NodeTable.empty);

    (* Remove the tees *)
    Omake_build_tee.env_close_success_tee env command;

    (* Now tell parents that this job succeeded *)
    finish_rule_success env command
  )

(*
 * A command failed.
 *)
let save_and_finish_rule_failed 
    (env : Omake_build_type.t)
    (command : Omake_build_type.command) code =
  (* Add the run to the cache *)
  match command with 
    { command_target     = target;
      command_effects    = effects;
      command_build_deps = build_deps;
      _
    } -> 
    let cache = env.env_cache in
    let _, commands_digest = command_lines command in
    Omake_build_tee.env_close_failed_tee env command;
    Omake_cache.add cache rule_fun target effects build_deps commands_digest (MemoFailure code);
    abort_commands env effects code

let probe_run_rule = I.create "run_rule"

(*
 * Run the command.
 *)
let run_rule (env : Omake_build_type.t) =
  I.instrument probe_run_rule
  (fun (command : Omake_build_type.command) ->
  match command with 
    { command_loc     = loc;
      command_target  = target;

      command_venv    = venv;
      _
    } -> 

    let pos = Pos.string_pos "run_rule" (Pos.loc_exp_pos loc) in
    let commands, _ = command_lines command in
    let shell = Omake_rule.eval_shell venv pos in

    (* Set up the tee *)
    let options = Omake_env.venv_options venv in
    let tee = Omake_exec_util.tee_create (Omake_options.opt_divert options) in
    let divert_only = not (Omake_options.opt_output options OutputNormal) in
    let copy_stdout = Omake_exec_util.tee_stdout tee divert_only in
    let copy_stderr = Omake_exec_util.tee_stderr tee divert_only in
    set_tee env command tee;
    env.env_rule_exec_count <- succ env.env_rule_exec_count;
    match Omake_exec.Exec.spawn env.env_exec shell 
        (Omake_env.venv_options venv) copy_stdout copy_stdout copy_stderr "build" target commands with
      ProcessFailed ->
      (* The fork failed *)
      abort_command env command Omake_state.fork_error_code
    | ProcessStarted pid ->
      (* The process was started *)
      env.env_idle_count <- pred env.env_idle_count;
      reclassify_command env command (CommandRunning (pid, None))
  )

let probe_execute_rule = I.create "execute_rule"

(*
 * Execute a command.
 * Check with the cache to see if this command is already
 * up-to-date.
 *)
let execute_rule 
    (env : Omake_build_type.t) =
  I.instrument probe_execute_rule
  (fun (command : Omake_build_type.command) ->
  match command with 
    { command_loc          = loc;

      command_target       = target;
      command_effects      = effects;
      command_lines        = commands;
      command_build_deps   = build_deps;
      command_venv         = venv;
      _
    } -> 
    let pos = Pos.string_pos "execute_rule" (Pos.loc_exp_pos loc) in
    let options = Omake_env.venv_options venv in
    let commands, commands_digest =
      match commands with
      | CommandNone ->
        [], None
      | CommandScanner (_, _, lines, digest)
      | CommandLines (_, lines, digest) ->
        lines, digest
      | CommandInfo info ->
        assert (info <> []);
        let commands = Omake_rule.eval_commands venv loc target build_deps info in
        let digest = Omake_command_digest.digest_of_commands pos commands in
        command.command_lines <- CommandLines (info, commands, digest);
        commands, digest
    in
    if Lm_debug.debug debug_rule then
      Format.eprintf "@[<v 3>building %a:@ @[<hv 3>build-deps:%a@]@ @[<hv 3>effects:%a@]@ digest: %s@]@." (**)
        Omake_node.pp_print_node target
        Omake_node.pp_print_node_set build_deps
        Omake_node.pp_print_node_set effects
        (hexify_digest commands_digest);
    if commands = [] then
      save_and_finish_rule_success env command
    else begin
      env.env_rule_count <- succ env.env_rule_count;
      match Omake_cache.up_to_date_status env.env_cache rule_fun build_deps commands_digest with
        StatusSuccess ->
        if Lm_debug.debug debug_rule then
          Format.eprintf "@[<hv 3>target %a is up to date:%a@]@." (**)
            Omake_node.pp_print_node target
            Omake_node.pp_print_node_set build_deps;
        finish_rule_success env command
      | StatusFailure code ->
        if Lm_debug.debug debug_rule then
          Format.eprintf "@[<hv 3>target %a failure:%a@]@." (**)
            Omake_node.pp_print_node target
            Omake_node.pp_print_node_set build_deps;
        finish_rule_failed env command code
      | StatusUnknown ->
        if Omake_options.opt_touch_only options then begin
          if Omake_options.opt_print_file options then
            Lm_printf.printf "updating %a@." Omake_node.pp_print_node target;
          save_and_finish_rule_success env command
        end
        else if  options.dry_run then begin
          if Omake_options.opt_print_command options <> EvalNever then
            List.iter (fun command ->
              if not (List.mem Omake_command_type.QuietFlag command.Omake_command_type.command_flags) then
                Lm_printf.printf "+ %a@." Omake_env.pp_print_arg_command_inst command.Omake_command_type.command_inst)
              commands;
          save_and_finish_rule_success env command
        end
        else begin
          if Lm_debug.debug debug_rule then
            Format.eprintf "@[<v 3>running %a:%a@]@." (**)
              Omake_node.pp_print_node target
              Omake_node.pp_print_node_set build_deps;
          run_rule env command
        end
    end
  )

(************************************************************************
 * Saved versions.
*)

(*
 * Create a new, empty environment.
 *)
let empty_env venv cache exec ~summary deps targets dirs includes : Omake_build_type.t=
  let cwd = Omake_node.Dir.cwd () in
  let options = Omake_env.venv_options venv in
  let wl = Omake_build_util.create_wl () in
  { env_venv                 = venv;
    env_cwd                  = cwd;
    env_cache                = cache;
    env_exec                 = exec;
    env_explicit_deps        = deps;
    env_explicit_targets     = targets;
    env_explicit_directories = dirs;
    env_includes             = includes;

    env_commands           = Omake_node.NodeTable.empty;
    env_inverse            = Omake_node.NodeTable.empty;
    env_error_code         = 0;
    env_idle_count         = Omake_options.opt_job_count options;
    env_print_dependencies = Omake_node.NodeSet.empty;

    env_current_wl         = wl;
    env_main_wl            = wl;

    env_pending_events     = Queue.create ();

    env_summary            = summary;

    env_optional_count   = 0;
    env_succeeded_count  = 0;
    env_scan_count       = 0;
    env_scan_exec_count  = 0;
    env_rule_count       = 0;
    env_rule_exec_count  = 0
  }

let create exec venv cache summary =
  match Omake_env.venv_explicit_rules venv with 
    { explicit_targets     = target_table;
      explicit_directories = dir_table;
      explicit_deps        = dep_table;
      _
    }  -> 

    let includes = Omake_env.venv_files venv in
    let includes = Omake_cache.stat_set cache includes in
    empty_env venv cache exec ~summary dep_table target_table dir_table includes

(************************************************************************
 * Saving state to .omakedb
*)

let pid = Unix.getpid () (* this is the PID of the main thread *)

(*
 * Save the cache and environment to a file.
 *)
let save_aux (env : Omake_build_type.t) =
  (* Only the "master" thread should be saving the DB *)
  if (pid <> Unix.getpid ()) then begin
    Format.eprintf "@[<hv3>*** OMake Internal ERROR:@ Slave thread %i trying to save db opened by the master thread %i@]@." (Unix.getpid ()) pid;
    raise (Invalid_argument "Internal error: Slave thread trying to save the OMake DB")
  end;

  (* Save the static values *)
  let () = Omake_env.venv_save_static_values env.env_venv in

  (* Save the .omakedb *)
  let cache = env.env_cache in

  (* We want the name to be fairly unique in case locking had failed us. *)
  let db_tmp = Lm_printf.sprintf ".#%s.%s.%i" Omake_state.db_name (Unix.gethostname ()) pid in

  (* Marshal the state to the output file *)
  let outx = Pervasives.open_out_bin db_tmp in
  let includes =
    Omake_node.NodeTable.fold (fun includes node _ ->
      Omake_node.NodeSet.add includes node) Omake_node.NodeSet.empty env.env_includes
  in
  let targets = Omake_node.NodeSet.singleton env_target in
  try
    Omake_cache.add cache env_fun env_target targets includes None (MemoSuccess Omake_node.NodeTable.empty);
    Omake_cache.to_channel outx cache;
    close_out outx;
    Unix.rename db_tmp Omake_state.db_name
  with
    Unix.Unix_error (errno, name, arg) ->
    Format.eprintf "*** omake: failure during saving: %s: %s(%s)@." (Unix.error_message errno) name arg;
    close_out outx;
    Lm_unix_util.try_unlink_file db_tmp
  | Sys_error _
  | Failure _ as exn ->
    Format.eprintf "*** omake: failure during saving: %s@." (Printexc.to_string exn);
    close_out outx;
    Lm_unix_util.try_unlink_file db_tmp

(*
 * Save to the .omakedb.
 *)
let save env =
  if not (env_options env).dry_run then
    try save_aux env with
      Sys_error _ as exn ->
      Format.eprintf "*** omake: failure during saving: %s@." (Printexc.to_string exn)


(************************************************************************
 * Invalidation.
*)

(*
 * Forms for walking up and down the tree.
 *)
let invalidate_parents env (command : Omake_build_type.command) =
  find_parents env command.command_target

let invalidate_children _ (command : Omake_build_type.command) =
  command.command_build_deps

let probe_invalidate_aux = I.create "invalidate_aux"

(*
 * General invalidation function.
 *
 * The invalidate_next function determines how to walk the tree.
 *)
let rec invalidate_aux invalidate_next env =
  I.instrument probe_invalidate_aux 
  (fun nodes ->
  if not (Omake_node.NodeSet.is_empty nodes) then
    let node = Omake_node.NodeSet.choose nodes in
    let command = find_command env node in
    let nodes =
      if command.command_state <> CommandInitial then
        let nodes = Omake_node.NodeSet.union nodes command.command_effects in
        let nodes = Omake_node.NodeSet.union nodes (invalidate_next env command) in

        (* Recompute the commands if they have value dependencies *)
        let () =
          match command.command_lines with
            CommandScanner (info, _, _, _)
          | CommandLines (info, _, _) ->
            command.command_lines <- CommandInfo info
          | CommandInfo _
          | CommandNone ->
            ()
        in
        (* Move the command back to the initial state *)
        reclassify_command env command CommandInitial;
        nodes
      else
        nodes
    in
    invalidate_aux invalidate_next env (Omake_node.NodeSet.remove nodes node)
  )

let invalidate_ancestors = invalidate_aux invalidate_parents
let invalidate_children  = invalidate_aux invalidate_children

(************************************************************************
 * Command management.
*)

let probe_process_initial = I.create "process_initial"

(*
 * Process a command in the Initial state.
 * Check the dependencies.  For each dependency,
 * make sure there is a command to build it.
 * If the dependencies are all finished, then
 * schedule this command for scanning.
 *)
let process_initial =
  I.instrument probe_process_initial
  (fun env ->
  let command = Omake_build_util.command_list_head env CommandInitialTag in
  match command with  { command_loc = loc;
                        command_venv = venv;
                        command_target = target;
                        command_effects = effects;
                        command_scanner_deps = scanner_deps;
                        command_static_deps = static_deps;
                        _
                      } -> 
    let pos = Pos.string_pos "process_initial" (Pos.loc_exp_pos loc) in
    let _ =
      if Lm_debug.debug debug_build then
        Format.eprintf "@[<hv 3>Process initial: %a@ @[<b 3>scanner deps:%a@]@ @[<b 3>static deps:%a@]@]@." (**)
          Omake_node.pp_print_node target
          Omake_node.pp_print_node_set scanner_deps
          Omake_node.pp_print_node_set static_deps
    in
    (* Add commands for all the dependencies *)
    start_or_build_commands env pos loc target static_deps;
    start_or_build_scanners env pos loc target scanner_deps venv;

    (* Take the union of all the effects *)
    if Omake_node.NodeSet.cardinal effects > 1 then
      start_or_build_effects env pos loc target effects;

    (* Initially, we enter scanning mode *)
    command_set_blocked env command scanner_deps;
    let state : Omake_build_type.command_state =
      if command_is_blocked env command then
        CommandScanBlocked
      else if command_effects_are_scanned env command then
        CommandScanned
      else
        CommandScannedPending
    in
    reclassify_command env command state
  )

(*
 * A command has been scanned successfully.
 *)
let process_scanned env =
  let command = Omake_build_util.command_list_head env CommandScannedTag in
  finish_scanned env command

(*
 * Process a command in the Ready state.
 * Start it and place it on the run queue.
 *)
let process_ready env =
  let command = Omake_build_util.command_list_head env CommandReadyTag in
  if command_conflicts_with_running env command then
    reclassify_command env command CommandPending
  else if command_is_scanner command then
    start_scanner env command
  else
    execute_rule env command

(*
 * A command has just finished, so check each pending
 * process and move it to the ready queue if it no
 * longer conflicts with a running process.
 *)
let process_pending env =
  Omake_build_util.command_iter env CommandPendingTag (fun command ->
    if not (command_conflicts_with_running env command) then
      reclassify_command env command CommandReady)

(*
 * Leaf dependency - a leaf node, or a node that appears as optional/exists node
 *)
let is_leaf_file (env : Omake_build_type.t) node =
  if Omake_node.NodeTable.mem env.env_commands node then
    Omake_build_util.is_leaf_node env node
  else
    (Omake_node.NodeTable.mem env.env_commands (Omake_node.Node.create_escape NodeOptional node) ||
     Omake_node.NodeTable.mem env.env_commands (Omake_node.Node.create_escape NodeExists node))

(*
 * Process the running queue.
 * Wait until a process exits.
 *)
let rec process_running (env : Omake_build_type.t) notify =
  match Omake_exec.Exec.wait env.env_exec (env_options env) with
    WaitExited (pid, code, _) ->
    begin
      env.env_idle_count <- succ env.env_idle_count;
      try
        let command = find_pid env pid in
        let () =
          match code, command with
            0, { command_state = CommandRunning (_, None) ; _} ->
            save_and_finish_rule_success env command
          | _, { command_state = CommandRunning (_, None) ; _} ->
            save_and_finish_rule_failed env command code
          | 0, { command_state = CommandRunning (_, Some filename) ; _} ->
            save_and_finish_scanner_success env command filename
          | _, { command_state = CommandRunning (_, Some filename) ; _} ->
            save_and_finish_scanner_failed env command filename code
          | _ ->
            raise (Invalid_argument "process_running")
        in
        process_pending env
      with
        Not_found ->
        ()
    end
  | WaitServer additional_jobs ->
    if !Omake_exec_remote.debug_remote then
      Format.eprintf "# new idle count: %d + %d@." env.env_idle_count additional_jobs;
    env.env_idle_count <- env.env_idle_count + additional_jobs
  | WaitNotify event ->
    if notify then
      ignore (invalidate_event env event)
  | WaitNone ->
    ()

(*
 * Wait for all jobs to finish.
 *)
and wait_all env verbose =
  if not (Omake_build_util.command_list_is_empty env CommandRunningTag) then begin
    if verbose then
      Format.eprintf "*** omake: waiting for all jobs to finish@.";
    ignore (process_running env false);
    wait_all env false
  end

(************************************************************************
 * Invalidation when a file is updated.
*)

(*
 * If the event is really a file change and it refers to a leaf
 * node, reset the command and all its ancestors to the initial state.
 *)
and invalidate_event_core env node =
  let verbose = Omake_options.opt_print_status (env_options env) in
  if verbose then begin
    Omake_exec_print.progress_flush ();
    Format.eprintf "*** omake: file %s changed@." (Omake_node.Node.fullname node)
  end;

  (* If this is an OMakefile, abort and restart *)
  if Omake_node.NodeTable.mem env.env_includes node then begin
    wait_all env verbose;
    raise (Restart None)
  end else
    let nodes = if Omake_build_util.is_leaf_node env node then Omake_node.NodeSet.singleton node else Omake_node.NodeSet.empty in
    let nodes = 
      Omake_node.NodeSet.union nodes 
        (find_parents env (Omake_node.Node.create_escape NodeOptional node)) in
    let nodes = Omake_node.NodeSet.union nodes (find_parents env (Omake_node.Node.create_escape NodeExists node)) in
    invalidate_ancestors env nodes

and do_invalidate_event env event =
  process_changes (is_leaf_file env) (invalidate_event_core env) env.env_venv env.env_cwd env.env_cache event

(*
 * Block FAM events during when performing a build phase
 * (like .BUILD_BEGIN, .BUILD_SUCCESS, etc.).
 *)
and invalidate_event env event =
  if env.env_current_wl != env.env_main_wl then begin
    (* Don't let the queue get too large *)
    if Queue.length env.env_pending_events < max_pending_events then
      Queue.add event env.env_pending_events;
    false
  end
  else
    do_invalidate_event env event


(************************************************************************
 * Loading state from .omakedb
*)
let notify_wait_simple venv cwd exec cache =
  Format.eprintf "*** omake: polling for filesystem changes (OMakefiles only)@.";
  let files = Omake_env.venv_files venv in
  let () =
    Omake_node.NodeSet.iter (fun node ->
      ignore (Omake_cache.stat cache node);
      Omake_exec.Exec.monitor exec node) files
  in
  let print_msg =
    if Omake_options.opt_print_status (Omake_env.venv_options venv) then
      fun node -> Lm_printf.printf "*** omake: file %s changed@." (Omake_node.Node.fullname node)
    else
      fun _ -> ()
  in
  let rec loop changed =
    let event = Omake_exec.Exec.next_event exec in
    let changed = changed || process_changes (Omake_node.NodeSet.mem files) print_msg venv cwd cache event in
    if (not changed || Omake_exec.Exec.pending exec) then
      loop changed
  in
  loop false

let print_restart options reason =
  if Omake_options.opt_print_status options then
    let reason =
      match reason with
      | None -> "a configuration file changed"
      | Some reason -> reason
    in
    Lm_printf.printf "*** omake: %s, restarting@." reason

(*
 * Create and parse, given a cache.
 *)
let create_env exec options cache targets =
  let venv = Omake_env.create options "." exec cache in
  let venv = Omake_builtin.venv_add_command_defs venv in
  let targets_value : Omake_value_type.t =
    ValArray (List.map (fun v -> Omake_value_type.ValData v) targets) in
  let venv = Omake_env.venv_add_var venv Omake_var.targets_var targets_value in
  let venv = Omake_builtin.venv_add_builtins venv in

  (* Summary file *)
  let summary =
    let summary, outx = Filename.open_temp_file ~mode:[Open_binary] "omake" ".error" in
    Pervasives.close_out outx;
    summary
  in
  let summary_value : Omake_value_type.t = ValNode (Omake_env.venv_intern venv PhonyProhibited summary) in
  let venv = Omake_env.venv_add_var venv Omake_var.build_summary_var summary_value in

  (* Ignore match errors *)
  let venv = Omake_env.venv_add_var venv Omake_var.glob_options_var (ValString "n") in

  (* Start reading files *)
  let now = Unix.gettimeofday () in
  let cwd = Omake_env.venv_dir venv in
  let () =
    if Omake_options.opt_print_dir options then
      Lm_printf.printf "make[0]: Entering directory `%s'@." (Omake_node.Dir.absname cwd);
    if Omake_options.opt_print_status options then begin
      Lm_printf.printf "*** omake: reading %ss@." Omake_state.makefile_name
    end
  in
  let venv =
    try
      let venv = Omake_builtin.venv_include_rc_file venv Omake_state.omakeinit_file in
      let venv = Omake_builtin.venv_add_pervasives venv in
      let venv = Omake_builtin.venv_include_rc_file venv Omake_state.omakerc_file in
      Omake_eval.compile venv;
      venv
    with exn ->
      Omake_env.venv_save_static_values venv;
      if Omake_options.opt_poll options && restartable_exn exn && not (Omake_node.NodeSet.is_empty (Omake_env.venv_files venv)) then begin
        if Omake_options.opt_print_status options then begin
          let now' = Unix.gettimeofday () in
          Lm_printf.printf "*** omake: reading %ss failed (%a)@." Omake_state.makefile_name Lm_unix_util.pp_time (now' -. now);
        end;
        Format.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
        notify_wait_simple venv cwd exec cache;
        raise (Restart None)
      end else begin
        Lm_unix_util.try_unlink_file summary;
        raise exn
      end
  in
  let () =
    if Omake_options.opt_print_status options then
      let now' = Unix.gettimeofday () in
      Lm_printf.printf "*** omake: finished reading %ss (%a)@." Omake_state.makefile_name 
        Lm_unix_util.pp_time (now' -. now)
  in
  let env = create exec venv cache summary in
  Omake_build_util.set_env env;
  env

let rec create_env_loop exec options cache targets =
  try create_env exec options cache targets with
    Restart reason ->
    print_restart options reason;
    create_env_loop exec options cache targets

let probe_create_env_loop = I.create "create_env_loop"

let create_env_loop = I.instrument probe_create_env_loop create_env_loop

(*
 * Load the environment if possible.
 * If not, create a new one.
 *)
let load_omake options targets =
  let cwd  = Omake_node.Dir.cwd () in
  let exec = Omake_exec.Exec.create cwd options in
  let cache =
    match
      (* Load cache from the db file *)
      try
        let inx = open_in_bin Omake_state.db_name in
        let cache =
          try Omake_cache.from_channel options inx with
            exn ->
            close_in inx;
            raise exn
        in
        close_in inx;
        Some cache
      with
        Unix.Unix_error _
      | End_of_file
      | Sys_error _
      | Failure _ ->
        None
    with
      None -> Omake_cache.create ()
    | Some cache -> cache
  in
  create_env_loop exec options cache targets

(*
 * Special version for use by osh.
 * We assume the starting directory for osh is the project root.
 *)
let load_osh venv options targets =
  (* Replace the cache *)
  let cache =
      try
        let inx = open_in_bin Omake_state.db_name in
        Lm_unix_util.finally inx (Omake_cache.from_channel options ) close_in 
      with
      |  Unix.Unix_error _
      |  End_of_file 
      |  Sys_error _ 
      |Failure _ -> Omake_cache.create ()
  in
  let venv = Omake_env.venv_add_cache venv cache in

  (* Add the targets *)
  let targets_value : Omake_value_type.t =
    ValArray 
      (List.map (fun v -> (ValData v : Omake_value_type.t )) targets) in
  let venv = Omake_env.venv_add_var venv Omake_var.targets_var targets_value in

  (* Add the summary file *)
  let summary =
    let summary, outx = Filename.open_temp_file ~mode:[Open_binary] "omake" ".error" in
    Pervasives.close_out outx;
    summary
  in
  let summary_value : Omake_value_type.t = 
    ValNode (Omake_env.venv_intern venv PhonyProhibited summary) in
  let venv = Omake_env.venv_add_var venv Omake_var.build_summary_var summary_value in

  (* Create the environment *)
  let exec = Omake_env.venv_exec venv in
  let env = create exec venv cache summary in
  Omake_build_util.set_env env;
  env

let load venv_opt options targets =
  match venv_opt with
  | Some venv ->
    load_osh venv options targets
  | None ->
    load_omake options targets

let rec main_loop env (progress : prompt_state) =
  if Lm_debug.debug debug_build then 
    Omake_build_util.eprint_env env ;
  let progress : prompt_state =
    let flushed = Omake_exec_print.progress_flushed () in
    if flushed || progress.count <> env.env_succeeded_count then
      let progress = { progress with count = env.env_succeeded_count } in
      let options = Omake_env.venv_options env.env_venv in
      let now = Unix.gettimeofday () in
      let will_save = ! save_interval > 0.0 && now > progress.save in
      let progress =
        if will_save then begin
          save env;
          Omake_exec_print.print_saving options;
          { progress with save = now +. ! save_interval }
        end else
          progress
      in
      if flushed || will_save || now > progress.progress then 
        begin
        let total = Omake_node.NodeTable.cardinal env.env_commands - env.env_optional_count in
        Omake_exec_print.print_progress options env.env_succeeded_count total;
        { progress with progress = now +. prompt_interval }
        end
      else
        progress
    else
      progress in

  if not (Omake_build_util.command_list_is_empty env CommandInitialTag) then begin
    process_initial env;
    main_loop env progress
  end
  else if not (Omake_build_util.command_list_is_empty env CommandScannedTag) then 
    begin
      process_scanned env;
      main_loop env progress
    end
  else if (env.env_idle_count > 0)
       && (env.env_error_code = 0)
       && not (Omake_build_util.command_list_is_empty env CommandReadyTag)
  then 
    begin
      process_ready env;
      main_loop env progress
    end
  else if
    env.env_idle_count == 0 || not (Omake_build_util.command_list_is_empty env CommandRunningTag) 
  then 
    begin
    process_running env true;
    main_loop env progress
    end
  else
    begin
      assert (env.env_idle_count >= 0);
      Omake_exec_print.progress_flush ()
    end

(**  Make the targets. *)
let make (env : Omake_build_type.t) =
  let now = Unix.gettimeofday () in
  main_loop env {
    count = env.env_succeeded_count;
    progress =  now +. prompt_interval;
    save = now +. !save_interval;
  }

(*
 * Wait for notifications.
 *)
let notify_wait (env : Omake_build_type.t) =
  match env with 
    { env_exec = exec;
      env_venv = venv;
      _
    } -> 
    let db_node = 
      Omake_env.venv_intern_cd venv PhonyProhibited 
        (Omake_node.Dir.cwd ()) Omake_state.db_name in
    let rec loop found =
      if not found || Omake_exec.Exec.pending exec then
        let event = Omake_exec.Exec.next_event exec in
        let changed = invalidate_event env event in
        loop (changed || found)
    in
    Format.eprintf "*** omake: polling for filesystem changes@.";
    save env;
    ignore (Omake_cache.stat_changed env.env_cache db_node);
    Omake_build_util.unlock_db ();
    loop false;
    Omake_build_util.wait_for_lock ();
    if Omake_cache.stat_changed env.env_cache db_node then
      raise (Restart (Some "another OMake process have modified the build DB"));
    if Omake_options.opt_print_status (env_options env) then
      Format.eprintf "*** omake: rebuilding@."

let notify_wait_omakefile env =
  Format.eprintf "*** omake: polling for filesystem changes (OMakefiles only)@.";
  let rec loop () =
    ignore (invalidate_event env (Omake_exec.Exec.next_event env.env_exec));
    loop ()
  in
  try
    loop ()
  with
    Restart reason -> reason


let print_summary ?(unlink = true) (env : Omake_build_type.t) =
  let inx = open_in_bin env.env_summary in
  let buffer = Bytes.create 256 in
  let rec copy () =
    let amount = input inx buffer 0 (String.length buffer) in
    if amount > 0 then begin
      Pervasives.output Pervasives.stderr buffer 0 amount;
      copy ()
    end
  in
  copy ();
  Pervasives.flush Pervasives.stderr;
  close_in inx;
  if unlink then
    Lm_unix_util.try_unlink_file env.env_summary

(**  Create or find a command to build it. *)
let build_target env (print : bool) (target : Omake_node.NodeTable.key ) : unit =
  (try
     let command = find_command env target in
     start_command env command
   with
     Not_found ->
     let name = Omake_node.Node.fullname target in
     let loc = Lm_location.bogus_loc name in
     let pos = Pos.string_pos "build_target" (Pos.loc_exp_pos loc) in
     build_command env pos loc target);
  if print then print_node_dependencies env target


(*
 * Worklist switching.
 *
 * Build a pseudo-phased target .BUILD_* with a fresh worklist.
 * The reason for switching worklists is so we don't damage the
 * main build, and also so that we ignore the main build
 * when executing phases.
 *)
let build_phase (env : Omake_build_type.t) target : bool =
  let code = env.env_error_code in
  let restore_wl () =
    env.env_current_wl <- env.env_main_wl;
    if env.env_error_code = 0 then
      env.env_error_code <- code;
    Queue.iter (fun event -> ignore (do_invalidate_event env event)) env.env_pending_events;
    Queue.clear env.env_pending_events
  in
  Lm_unix_util.finally () (function () -> 
      env.env_current_wl <- Omake_build_util.create_wl ();
      env.env_error_code <- 0;
      build_target env false target;
      invalidate_children env (Omake_node.NodeSet.singleton target);
      make env;
      Omake_build_util.command_list_is_empty env CommandFailedTag
    ) restore_wl

(*
 * Build command line targets.
 *)
let rec build_targets (env : Omake_build_type.t) save_flag start_time parallel print ?(summary = true) targets =
  let options : Omake_options.t = env_options env in
  (*
 * Summary management.
 *)
  let create_tmpfile (env : Omake_build_type.t) =
    close_out @@ Pervasives.open_out_gen 
      [Open_wronly; Open_binary; Open_creat; Open_trunc] 0o600 env.env_summary  in
  let () =
    try
      let begin_success =
        (* Build the initial summary *)
        not summary || (create_tmpfile env; build_phase env build_begin_target)
      in
      let process_summary () =
        (* Print out the final summary *)
        Lm_unix_util.with_file_fmt env.env_summary (fun buf -> 
          if env.env_error_code <> 0 then begin
            Omake_build_util.print_stats env "failed" start_time;
            Omake_build_util.print_failed_targets env buf;
            false
          end else if not (Omake_build_util.command_list_is_empty env CommandBlockedTag) then begin
            Omake_build_util.print_stats env "blocked" start_time;
            Omake_build_util.print_failed env buf CommandBlockedTag;
            false
          end else if not (Omake_build_util.command_list_is_empty env CommandScanBlockedTag) then begin
            Omake_build_util.print_stats env "scanner is blocked" start_time;
            Omake_build_util.print_failed env buf CommandScanBlockedTag;
            false
          end else if not (Omake_build_util.command_list_is_empty env CommandFailedTag) then begin
            Omake_build_util.print_stats env "failed" start_time;
            Omake_build_util.print_failed_targets env buf;
            false
          end else
            true) in
      let () =
        if begin_success then
          (* Build the core *)
          if parallel || Omake_options.opt_parallel options then begin
            (* Add commands to build the targets *)
            List.iter (build_target env print) targets;
            (* Build *)
            make env
          end
          else begin
            (* Make them in order *)
            List.iter (fun target -> build_target env print target; make env) targets
          end;
      in
      if summary then begin
        if not (process_summary () && begin_success && build_phase env build_success_target && process_summary ()) then
          ignore (build_phase env build_failure_target);
        print_summary env
      end
    with
    | Sys_error _
    | Omake_value_type.ExitException _
    | Omake_value_type.ExitParentException _
    | Omake_value_type.OmakeException _
    | Omake_value_type.UncaughtException _
    | Omake_value_type.RaiseException _
    | Unix.Unix_error _
    | Omake_value_type.OmakeFatalErr _
    | Omake_value_type.OmakeFatal _
    | Sys.Break
    | Failure _
    | Omake_value_type.Return _ as exn ->
      Lm_unix_util.with_file_fmt env.env_summary (fun buf -> 
      Format.fprintf buf "%a@." Omake_exn_print.pp_print_exn exn);
      Omake_build_util.print_stats env (match exn with Sys.Break -> "stopped" | _ -> "failed") start_time;
      print_summary env ~unlink:false;
      if Omake_options.opt_poll options && restartable_exn exn then begin
        Lm_unix_util.try_unlink_file env.env_summary;
        let reason = notify_wait_omakefile env in
        raise (Restart reason)
      end
      else if  options.osh then
        env.env_error_code <- Omake_state.exn_error_code
      else begin
        Omake_build_util.close env;
        save env;
        raise (BuildExit Omake_state.exn_error_code)
      end
  in
  (* Save database before exiting *)
  if save_flag then
    save env;

  (* Return error if that happened *)
  if env.env_error_code <> 0 then
    build_on_error env save_flag start_time parallel print targets options env.env_error_code
  else if not (Omake_build_util.command_list_is_empty env CommandBlockedTag) then
    build_on_error env save_flag start_time parallel print targets options Omake_state.deadlock_error_code
  else if not (Omake_build_util.command_list_is_empty env CommandScanBlockedTag) then
    build_on_error env save_flag start_time parallel print targets options Omake_state.deadlock_error_code
  else if not (Omake_build_util.command_list_is_empty env CommandFailedTag) then
    build_on_error env save_flag start_time parallel print targets options Omake_state.deadlock_error_code

and build_on_error env save_flag _ parallel print targets options error_code =
  if not (Omake_options.opt_poll options) then
    raise (BuildExit error_code)
  else begin
    notify_wait env;
    build_targets env save_flag (Unix.gettimeofday ()) parallel print targets
  end

let memstat (env : Omake_build_type.t) =
  let open Printf  in
  let size obj = Objsize.size_with_headers (Objsize.objsize obj) in
  printf "*** memory statistics:\n";
  printf "env (total):          %9d\n" (size env);
  printf "venv:                 %9d\n" (size env.env_venv);
  printf "cache:                %9d\n" (size env.env_cache);
  printf "exec:                 %9d\n" (size env.env_exec);
  printf "explicit_deps:        %9d\n" (size env.env_explicit_deps);
  printf "explicit_targets:     %9d\n" (size env.env_explicit_targets);
  printf "explicit_directories: %9d\n" (size env.env_explicit_directories);
  printf "includes:             %9d\n" (size env.env_includes);
  printf "commands:             %9d\n" (size env.env_commands);
  printf "inverse:              %9d\n" (size env.env_inverse);
  printf "print_dependencies:   %9d\n" (size env.env_print_dependencies);
  printf "current_wl:           %9d\n" (size env.env_current_wl);
  printf "main_wl:              %9d\n" (size env.env_main_wl)

(*
 * Notification loop.
 *)
let rec notify_loop env (options : Omake_options.t) targets =
  begin try
      notify_wait env
    with Sys.Break ->
      Format.eprintf "*** omake: Received Break signal, exiting@.";
      raise (BuildExit 0)
  end;

  (* Build the targets again *)
  let start_time = Unix.gettimeofday () in
  build_targets env true start_time false options.print_dependencies targets;
  Omake_build_util.print_stats env "done" start_time;
  notify_loop env options targets

(**  Start the core build. *)
let build_core (env : Omake_build_type.t) dir start_time (options : Omake_options.t) targets =
  (* First, build all the included files *)
  let changed =
    if  options.dry_run then false
    else
      let includes = Omake_node.NodeTable.fold (fun includes node _ -> node :: includes) [] env.env_includes in
      let _ = build_targets env false start_time true false ~summary:false includes in
      Omake_node.NodeTable.exists (fun node digest ->
        let digest' = Omake_cache.force_stat env.env_cache node in
        digest' <> digest) env.env_includes
  in
  let () =
    if changed then begin
      env.env_includes <- Omake_cache.stat_table env.env_cache env.env_includes;
      raise (Restart None)
    end
  in

  let venv : Omake_env.t = env.env_venv in
  let venv = Omake_env.venv_chdir_tmp venv dir in
  let targets = List.map (Omake_env.venv_intern venv PhonyOK) targets in
  let () = 
    List.iter (fun s -> print_node_dependencies env (Omake_env.venv_intern venv PhonyOK s))
      options.show_dependencies in
  let options = env_options env in
  build_targets env true start_time false options.print_dependencies  targets;
  Omake_build_util.print_stats env "done" start_time;

  (* Polling loop *)
  if Omake_options.opt_poll_on_done options then
    if not Lm_notify.enabled then
      Format.eprintf "*** omake: Polling is not enabled@."
    else
      notify_loop env options targets;
  Omake_build_util.close env;

  memstat env

(**  Main builder. *)
let rec build_time start_time venv_opt (options : Omake_options.t) dir_name targets =
  let env : Omake_build_type.t = load venv_opt options targets in
  let dir_name =
    if options.project  then "."
    else dir_name  in
  let dir = Omake_node.Dir.chdir env.env_cwd dir_name in

  (* Monitor the full tree if polling *)
  let () =
    if Omake_options.opt_poll options then
      try Omake_exec.Exec.monitor_tree env.env_exec env.env_cwd with
        Failure _ -> (* This is just an optimization anyway *)
        () in

  (*
   * Check that this directory is actually a .SUBDIR.
   * Don't do the check in osh mode; we assume the script knows
   * what it is doing.
   *)
  let () =
    if venv_opt = None && not ( options.project || Omake_node.DirTable.mem env.env_explicit_directories dir) then begin
      Format.eprintf "*** omake: the current directory %s@." (Omake_node.Dir.absname dir);
      Format.eprintf "*** omake: is not part of the root project in %s@." (Omake_node.Dir.absname env.env_cwd);
      raise (BuildExit 1)
    end
  in
  let restart reason =
    print_restart options reason;
    Omake_build_util.close env;
    save env;
    build_time start_time venv_opt options dir_name targets
  in
  try build_core env  dir start_time options targets with
  | Restart reason -> restart reason
  | Sys.Break as exn -> Omake_build_util.close env; save env;
    Format.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
    raise (BuildExit Omake_state.exn_error_code)
  | exn when Omake_options.opt_poll options && restartable_exn exn ->
    Format.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
    let reason = notify_wait_omakefile env in
    restart reason

let build options dir_name targets =
  try
    Omake_shell_sys.set_interactive false;
    Omake_build_util.wait_for_lock ();
    build_time (Unix.gettimeofday ()) None options dir_name targets
  with
    BuildExit code ->
    Pervasives.exit code


let build_fun venv targets =
  let options = Omake_env.venv_options venv in
  let dir = Omake_node.Dir.absname (Omake_env.venv_dir venv) in
  Unix.chdir dir;
  Omake_node.Dir.reset_cwd ();
  try
    Omake_build_util.wait_for_lock ();
    build_time (Unix.gettimeofday ()) (Some venv) options "." targets;
    true
  with
    BuildExit _ -> false

