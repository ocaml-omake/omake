(*
 * Do the actual build given a set of commands to run.
 * The basic algorithm is to use a depth-first search on the
 * command DAG, and build nodes that are out-of-date.
 *
 * In addition, we want to perform multi-tasking.  This build
 * process is not multi-threaded.  Instead, we fork jobs and
 * asynchronously continue building the other parts of
 * the project.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech and HRL Laboratories, LLC
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
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf

open Lm_heap
open Lm_debug
open Lm_symbol
open Lm_notify
open Lm_location
open Lm_string_util
open Lm_string_set

open Omake_util
open Omake_ir
open Omake_env
open Omake_pos
open Omake_eval
open Omake_node
open Omake_exec
open Omake_rule
open Omake_eval
open Omake_state
open Omake_symbol
open Omake_command
open Omake_node_sig
open Omake_exec_type
open Omake_exec_util
open Omake_exec_id
open Omake_exec_print
open Omake_exec_remote
open Omake_cache_type
open Omake_build_tee
open Omake_build_type
open Omake_value_type
open Omake_builtin_util
open Omake_command_digest
open Omake_options
open Omake_var

module Pos = MakePos (struct let name = "Omake_build" end);;
open Pos;;

exception BuildExit of int

type prompt_state = {
   ps_count : int; (* success count *)
   ps_save : float; (* next .omakedb save time *)
   ps_progress : float; (* next progress bar update time *)
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
   create_debug (**)
      { debug_name = "rule";
        debug_description = "Display debugging information for rule execution";
        debug_value = false
      }

let debug_build =
   create_debug (**)
      { debug_name = "build";
        debug_description = "Display debugging information during the build process";
        debug_value = false
      }

let debug_deps =
   create_debug (**)
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
exception UnknownTarget of Node.t


type default_scanner_mode =
   DefaultScannerIsEnabled
 | DefaultScannerIsDisabled
 | DefaultScannerIsWarning
 | DefaultScannerIsError

(************************************************************************
 * Utilities.
 *)

(*
 * Special nodes.
 *)
let build_begin   = ".BUILD_BEGIN"
let build_success = ".BUILD_SUCCESS"
let build_failure = ".BUILD_FAILURE"
let build_begin_target   = Node.create_phony_global build_begin
let build_success_target = Node.create_phony_global build_success
let build_failure_target = Node.create_phony_global build_failure

(*
 * Directory listing.
 *)
let rec list_directory dir =
   let dirx =
      try Some (Unix.opendir dir) with
         Unix.Unix_error _ ->
            None
   in
      match dirx with
         None ->
            []
       | Some dirx ->
            let rec list entries =
               let name =
                  try Some (Unix.readdir dirx) with
                     Unix.Unix_error _
                   | End_of_file ->
                        None
               in
                  match name with
                     Some "."
                   | Some ".." ->
                        list entries
                   | Some name ->
                        list (Filename.concat dir name :: entries)
                   | None ->
                        entries
            in
            let entries = list [] in
               Unix.closedir dirx;
               entries

(*
 * Check if a command list contains value dependencies.
 *)
let commands_have_value_dependencies commands =
   List.exists (fun command ->
         command.command_values <> []) commands

(*
 * Flatten the list of *all* dependencies.
 *)
let flatten_deps table =
   NodeTable.fold (fun deps1 _ deps2 ->
         NodeSet.union deps1 deps2) NodeSet.empty table

(*
 * Get the scanner mode.
 *)
let venv_find_scanner_mode venv pos =
   try
      let v = venv_find_var_exn venv scanner_mode_var in
         match string_of_value venv pos v with
            "enabled" ->
               DefaultScannerIsEnabled
          | "disabled" ->
               DefaultScannerIsDisabled
          | "warning" ->
               DefaultScannerIsWarning
          | "error" ->
               DefaultScannerIsError
          | s ->
               raise (OmakeException (pos, StringStringError ("bad scanner mode (should be enabled, disabled, error, or warning)", s)))
   with
      Not_found ->
         DefaultScannerIsError

let restartable_exn = function
 | OmakeException _
 | UncaughtException _
 | RaiseException _ ->
      true
 | _ ->
      false

(*
 * JYH: the overhead of scanning directories every time
 * it changes is pretty high.  We may want to think of
 * other ways of doing this.
 *
 * Intercept directory change events and pretend that every file
 * in the directory has changed.
 *)
let process_changes is_node_relevant process_node venv cwd cache event =
   let process_node name =
      let node = venv_intern_cd venv PhonyProhibited cwd name in
      let changed = is_node_relevant node && Omake_cache.stat_changed cache node in
         if !debug_notify then
            eprintf "Omake_build.process_changes: received %s event for node: %a@."
               (if changed then "relevant" else "ignored") pp_print_node node;
         if changed then
            process_node node;
         changed
   in
      match event with
         { notify_code = DirectoryChanged; notify_name = name } ->
            List.fold_left (fun changed name -> process_node name || changed) false (list_directory name)
       | { notify_code = (Changed | Created); notify_name = name } ->
            process_node name
       | _ ->
            false

(*
 * Find a command from a target.
 * May raise Not_found.
 *)
let find_command env target =
   NodeTable.find env.env_commands target

(*
 * Find the immediate parents of a node in the dependency DAG
 *)
let find_parents env node =
   try
      let inverse = NodeTable.find env.env_inverse node in
         NodeTable.fold (fun nodes node _ -> NodeSet.add nodes node) NodeSet.empty inverse
   with
      Not_found ->
         NodeSet.empty

(************************************************************************
 * Printing.
 *)
let pp_print_deps buf deps =
   NodeTable.iter (fun target deps ->
         fprintf buf "@ @[<b 3>%a:%a@]" (**)
            pp_print_node target
            pp_print_node_set deps) deps

let pp_print_command_state buf state =
   match state with
      CommandIdle                      -> pp_print_string buf "idle"
    | CommandInitial                   -> pp_print_string buf "initial"
    | CommandScanBlocked               -> pp_print_string buf "scan-blocked"
    | CommandScannedPending            -> pp_print_string buf "scanned-pending"
    | CommandScanned                   -> pp_print_string buf "scanned"
    | CommandBlocked                   -> pp_print_string buf "blocked"
    | CommandPending                   -> pp_print_string buf "pending"
    | CommandReady                     -> pp_print_string buf "ready"
    | CommandRunning (pid, None)       -> fprintf buf "running(%a)" pp_print_pid pid
    | CommandRunning (pid, Some name)  -> fprintf buf "scanning(%a, %s)" pp_print_pid pid name
    | CommandSucceeded _               -> pp_print_string buf "succeeded"
    | CommandFailed code               -> fprintf buf "failed(%d)" code

let pp_print_command_opt buf command_opt =
   match command_opt with
      Some command ->
         let { command_target = target;
               command_state = state
             } = command
         in
            fprintf buf "%a[%a]" pp_print_node target pp_print_command_state state
    | None ->
         pp_print_string buf "<none>"

let pp_print_command buf command =
   let { command_target       = target;
         command_effects      = effects;
         command_locks        = locks;
         command_state        = state;
         command_scanner_deps = scanner_deps;
         command_build_deps   = build_deps;
         command_blocked      = blocked
       } = command
   in
      fprintf buf "@[<v 3>%a[%a],@ @[<b 3>effects =%a@]@ @[<b 3>locks =%a@]@ @[<b 3>scanner deps =%a@]@ @[<b 3>build deps =%a@]@ @[<b 3>blocked =%a@]@]" (**)
         pp_print_node target
         pp_print_command_state state
         pp_print_node_set effects
         pp_print_node_set locks
         pp_print_node_set scanner_deps
         pp_print_node_set build_deps
         pp_print_node_list blocked

let pp_print_node_states env buf nodes =
   NodeSet.iter (fun target ->
         try
            let command = find_command env target in
               fprintf buf "@ %a(%a)" (**)
                  pp_print_node target
                  pp_print_command_state command.command_state
         with
            Not_found ->
               pp_print_node buf target) nodes

(*
 * Compute all of the dependencies.
 *)
let all_dependencies dependencies_of env nodes =
   let commands = env.env_commands in
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
                     let deps = dependencies_of command in
                     let found = NodeSet.add found node in
                        found, deps
                  with
                     Not_found ->
                        found, NodeSet.empty
               in
               let unexamined = NodeSet.union unexamined deps in
                  find_deps found examined unexamined
   in
      find_deps NodeSet.empty NodeSet.empty nodes

let all_build_dependencies =
   all_dependencies (fun command -> command.command_build_deps)

let all_scanner_dependencies =
   all_dependencies (fun command -> command.command_scanner_deps)

(*
 * Print the dependency information.
 *)
let rec pp_print_dependencies_aux show_all env buf command =
   let { command_target       = target;
         command_effects      = effects;
         command_scanner_deps = scanner_deps;
         command_static_deps  = static_deps;
         command_build_deps   = build_deps
       } = command
   in
   let inverse = find_parents env target in
   let options = venv_options env.env_venv in
   let total, build_deps, scanner_deps =
      if show_all && opt_all_dependencies options then
         "all transitive ", all_build_dependencies env build_deps, all_scanner_dependencies env scanner_deps
      else
         "", build_deps, scanner_deps
   in
      fprintf buf "@[<v 3>target: %a@ @[<b 3>%sscanner dependencies:%a@]@ @[<b 3>static dependencies:%a@]@ @[<b 3>%sbuild dependencies:%a@]@ @[<b 3>dependencies are merged from:%a@]@ @[<b 3>targets that depend on this node at this point:%a@]@]" (**)
         pp_print_node target
         total
         pp_print_node_set scanner_deps
         pp_print_node_set static_deps
         total
         pp_print_node_set build_deps
         pp_print_node_set effects
         pp_print_node_set inverse;

      if show_all && opt_verbose_dependencies options then
         let nodes = NodeSet.union scanner_deps build_deps in
            fprintf buf "@ @ --- Complete dependency listing ---@ ";
            NodeSet.iter (fun node ->
                  let command = find_command env node in
                     fprintf buf "@ %a" (pp_print_dependencies_aux false env) command) nodes

let pp_print_dependencies =
   pp_print_dependencies_aux true

(************************************************************************
 * Command queues.
 *)

(*
 * Worklist creation.
 *)
let create_wl () =
   { env_idle_wl            = ref None;
     env_initial_wl         = ref None;
     env_scan_blocked_wl    = ref None;
     env_scanned_pending_wl = ref None;
     env_scanned_wl         = ref None;
     env_blocked_wl         = ref None;
     env_ready_wl           = ref None;
     env_pending_wl         = ref None;
     env_running_wl         = ref None;
     env_succeeded_wl       = ref None;
     env_failed_wl          = ref None
   }

(*
 * Get the list pointer for a node class.
 *)
let command_tag = function
    CommandIdle            -> CommandIdleTag
  | CommandInitial         -> CommandInitialTag
  | CommandScanBlocked     -> CommandScanBlockedTag
  | CommandScannedPending  -> CommandScannedPendingTag
  | CommandScanned         -> CommandScannedTag
  | CommandBlocked         -> CommandBlockedTag
  | CommandReady           -> CommandReadyTag
  | CommandPending         -> CommandPendingTag
  | CommandRunning _       -> CommandRunningTag
  | CommandSucceeded _     -> CommandSucceededTag
  | CommandFailed _        -> CommandFailedTag

let get_worklist_command wl = function
   CommandIdleTag           -> wl.env_idle_wl
 | CommandInitialTag        -> wl.env_initial_wl
 | CommandScanBlockedTag    -> wl.env_scan_blocked_wl
 | CommandScannedPendingTag -> wl.env_scanned_pending_wl
 | CommandScannedTag        -> wl.env_scanned_wl
 | CommandBlockedTag        -> wl.env_blocked_wl
 | CommandReadyTag          -> wl.env_ready_wl
 | CommandPendingTag        -> wl.env_pending_wl
 | CommandRunningTag        -> wl.env_running_wl
 | CommandSucceededTag      -> wl.env_succeeded_wl
 | CommandFailedTag         -> wl.env_failed_wl

let command_worklist env state =
   get_worklist_command env.env_current_wl state

(*
 * Reclassify the commands.
 *)
let reclassify_command env command state =
   (* Unlink the node from its current list *)
   let pred = command.command_pred in
   let succ = !(command.command_succ) in
   let _ =
      pred := succ;
      match succ with
         Some next -> next.command_pred <- pred
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
   let l = command_worklist env (command_tag state) in
   let next = !l in
      l := Some command;
      command.command_state <- state;
      command.command_pred <- l;
      command.command_succ := next;
      match next with
         Some next -> next.command_pred <- command.command_succ
       | None -> ()

(*
 * Iterate through the node list.
 *)
let command_iter env state f =
   let rec iter command_opt =
      match command_opt with
         Some command ->
            let next = !(command.command_succ) in
               f command;
               iter next
       | None ->
            ()
   in
      iter (!(command_worklist env state))

(*
 * Fold through the command list.
 *)
let command_fold env state f x =
   let rec fold x command =
      match command with
         Some command ->
            let next = !(command.command_succ) in
            let x = f x command in
               fold x next
       | None ->
            x
   in
   let x = fold x (!(command_worklist env state)) in
      if env.env_main_wl == env.env_current_wl then
         x
      else
         fold x (!(get_worklist_command env.env_main_wl state))

(*
 * Existential test.
 *)
let command_exists env state f x =
   let rec exists command =
      match command with
         Some command ->
            let next = !(command.command_succ) in
               f command || exists next
       | None ->
            x
   in
      exists (!(command_worklist env state))

(*
 * Find a particular command.
 *)
let command_find env state f =
   let rec find command =
      match command with
         Some command ->
               if f command then
                  command
               else
                  find (!(command.command_succ))
       | None ->
            raise Not_found
   in
      find (!(command_worklist env state))

(*
 * Test for empty.
 *)
let command_list_is_empty env state =
   let l = command_worklist env state in
      match !l with
         Some _ -> false
       | None -> true

(*
 * Get the head of a list.
 *)
let command_list_head env state =
   let l = command_worklist env state in
      match !l with
         Some command -> command
       | None -> raise (Invalid_argument "command_list_head")

(************************************************************************
 * Other target utilities.
 *)

let target_is_phony = Node.is_phony

let target_exists env node =
   Omake_cache.exists env.env_cache node

let target_is_buildable env venv node =
   Omake_target.target_is_buildable env.env_cache venv node

(************************************************************************
 * Environment.
 *)

(*
 * Get options.
 *)
let env_options env =
   venv_options env.env_venv

(*
 * Add a target to the print-dependency list.
 * The target dependencies will be printed just before the
 * build rule is executed.
 *)
let print_node_dependencies env target =
   env.env_print_dependencies <- NodeSet.add env.env_print_dependencies target

(*
 * Start command if it is idle.
 *)
let start_command env command =
   if command.command_state = CommandIdle then
      reclassify_command env command CommandInitial

(*
 * Find a process by pid.
 *)
let find_pid env pid =
   command_find env CommandRunningTag (fun command ->
         match command.command_state with
            CommandRunning (pid', _) ->
               pid' = pid
          | _ ->
               false)

(*
 * Get the command lines.
 *)
let command_lines command =
   match command.command_lines with
      CommandNone ->
         [], None
    | CommandScanner (_, _, lines, digest)
    | CommandLines (_, lines, digest) ->
         lines, digest
    | CommandInfo _ ->
         raise (Invalid_argument "build_lines")

(*
 * See if this is a scanner command.
 *)
let command_is_scanner command =
   Node.kind command.command_target = NodeScanner

let set_tee env command tee =
   NodeSet.iter (fun target -> unlink_tee (find_command env target)) command.command_effects;
   unlink_tee command;
   command.command_tee <- tee

(************************************************************************
 * Command creation.
 *)

(*
 * Create a command for a target that always exists.
 *)
let create_exists_command env pos loc target =
   (* Create the command, and link it to the worklist *)
   let l = command_worklist env CommandSucceededTag in
   let next = !l in
   let succ = ref next in
   let effects = NodeSet.singleton target in
   let command =
      { command_venv               = env.env_venv;
        command_state              = CommandSucceeded NodeTable.empty;
        command_target             = target;
        command_locks              = effects;
        command_effects            = effects;
        command_scanner_deps       = NodeSet.empty;
        command_static_deps        = NodeSet.empty;
        command_build_deps         = NodeSet.empty;
        command_blocked            = [];
        command_loc                = loc;
        command_lines              = CommandNone;
        command_tee                = tee_none;
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
      env.env_commands <- NodeTable.add env.env_commands target command

(*
 * Create a command for a squashed target,
 * where the digest value is ignored, but the
 * target should be built.
 *)
let create_squashed_command env pos loc target =
   (* Create the command, and link it to the worklist *)
   let l = command_worklist env CommandInitialTag in
   let next = !l in
   let succ = ref next in
   let effects = NodeSet.singleton target in
   let static_deps = NodeSet.singleton (Node.core target) in
   let command =
      { command_venv               = env.env_venv;
        command_state              = CommandInitial;
        command_target             = target;
        command_effects            = effects;
        command_locks              = static_deps;
        command_static_deps        = static_deps;
        command_scanner_deps       = NodeSet.empty;
        command_build_deps         = NodeSet.empty;
        command_blocked            = [];
        command_loc                = loc;
        command_lines              = CommandNone;
        command_tee                = tee_none;
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
      env.env_commands <- NodeTable.add env.env_commands target command

(*
 * Create a command in a state.
 *)
let create_command env venv target effects lock_deps static_deps scanner_deps loc dir commands =
   (* Create the command, and link it to the worklist *)
   let l = command_worklist env CommandInitialTag in
   let next = !l in
   let succ = ref next in
   let () =
      if not (NodeSet.for_all (fun node -> Node.kind node = NodeScanner) scanner_deps) then
         let print_error buf =
            fprintf buf "@[<v 3>Malformed scanner dependencies:";
            fprintf buf "@ target: %a" pp_print_node target;
            fprintf buf "@ @[<b 3>lock_deps:%a@]" pp_print_node_set lock_deps;
            fprintf buf "@ @[<b 3>static_deps:%a@]" pp_print_node_set static_deps;
            fprintf buf "@ @[<b 3>scanner_deps:%a@]" pp_print_node_set scanner_deps;
            fprintf buf "@]"
         in
            raise (OmakeException (loc_exp_pos loc, LazyError print_error))
   in
   let effects = NodeSet.add effects target in
   let locks = NodeSet.union lock_deps effects in
   let command =
      { command_venv               = venv;
        command_state              = CommandInitial;
        command_target             = target;
        command_effects            = effects;
        command_locks              = locks;
        command_static_deps        = static_deps;
        command_scanner_deps       = scanner_deps;
        command_build_deps         = NodeSet.empty;
        command_blocked            = [];
        command_loc                = loc;
        command_lines              = commands;
        command_tee                = tee_none;
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
      env.env_commands <- NodeTable.add env.env_commands target command

(*
 * Build a command given a directory and a command list.
 *)
let build_any_command env pos loc venv target effects locks sources scanners commands =
   let pos = string_pos "build_any_command" (loc_pos loc pos) in

   (* Directory for this target *)
   let dir = venv_dir venv in

   (* Get all the extra dependencies that are statically defined *)
   let lock_deps, static_deps, scanner_deps =
      try NodeTable.find env.env_explicit_deps target with
         Not_found ->
            NodeSet.empty, NodeSet.empty, NodeSet.empty
   in
   let lock_deps = NodeSet.union lock_deps locks in
   let static_deps = NodeSet.union static_deps sources in
   let scanner_deps = NodeSet.union scanner_deps scanners in
   let implicit_lock_deps, implicit_static_deps, implicit_scanner_deps, implicit_values = venv_find_implicit_deps venv target in
   let lock_deps = NodeSet.union lock_deps implicit_lock_deps in
   let static_deps = NodeSet.union static_deps implicit_static_deps in
   let scanner_deps = NodeSet.union scanner_deps implicit_scanner_deps in
   let scanner_deps =
      if Node.kind target = NodeScanner || not (NodeSet.is_empty scanner_deps) then
         scanner_deps
      else
         let scanner_mode = venv_find_scanner_mode venv pos in
            if scanner_mode = DefaultScannerIsDisabled then
               scanner_deps
            else
               let scanner_target = Node.create_escape NodeScanner target in
                  if target_is_buildable env venv pos scanner_target then
                     match scanner_mode with
                        DefaultScannerIsWarning ->
                           eprintf "*** omake: warning: using default scanner %a@." pp_print_node scanner_target;
                           NodeSet.add scanner_deps scanner_target
                      | DefaultScannerIsError ->
                           raise (OmakeException (loc_pos loc pos, StringNodeError ("default scanners are not allowed", scanner_target)))
                      | DefaultScannerIsEnabled ->
                           NodeSet.add scanner_deps scanner_target
                      | DefaultScannerIsDisabled ->
                           scanner_deps
                  else
                     scanner_deps
   in
   let () =
      if debug debug_build then
         eprintf "@[<hv 3>Building new rule: %s@ @[<b 3>lock deps:%a@]@ @[<b 3>static deps:%a@]@ @[<b 3>scanner deps:%a@]@]@." (**)
            (Node.fullname target)
            pp_print_node_set lock_deps
            pp_print_node_set static_deps
            pp_print_node_set scanner_deps
   in
   let commands =
      match implicit_values, commands with
         [], [] ->
            CommandNone
       | [], _ :: _ ->
            CommandInfo commands
       | _ :: _, _ ->
            let command =
               { command_env = venv;
                 command_sources = NodeSet.to_list sources;
                 command_values = implicit_values;
                 command_body = []
               }
            in
               CommandInfo (command :: commands)
   in
      create_command env venv target effects lock_deps static_deps scanner_deps loc dir commands

(*
 * Build a null command for a file that exists but has no
 * build rules.
 *)
let build_null_command env pos loc venv target =
   let pos = string_pos "build_null_command" pos in
      if debug debug_implicit then
         eprintf "build_null_command: %a@." pp_print_node target;
      if target_is_phony target || target_exists env target then begin
         build_any_command env pos loc venv target NodeSet.empty NodeSet.empty NodeSet.empty NodeSet.empty [];
         if opt_poll (env_options env) && not (target_is_phony target) then
            Exec.monitor env.env_exec target
      end
      else
         raise (UnknownTarget target)

(*
 * Build a command from an environment, a set of sources, and
 * a list of commands.
 *)
let build_explicit_command env pos loc target effects locks venv sources scanners commands =
   let pos = string_pos "build_explicit_command" pos in
   let () =
      if debug debug_implicit then
         eprintf "@[<hv 3>build_explicit_command: explicit rule %a:@ @[<b 3>effects =%a@]@ @[<b 3>sources =%a@]@ @[<b 3>scanners =%a@]@]@." (**)
            pp_print_node target
            pp_print_node_set effects
            pp_print_node_set sources
            pp_print_node_set scanners
   in

   (* Check that all the effects have the same environment *)
   let bogus =
      NodeSet.fold (fun bogus effect ->
            try
               let erule = NodeTable.find env.env_explicit_targets effect in
                  if erule.rule_env != venv then
                     NodeSet.add bogus effect
                  else
                     bogus
            with
               Not_found ->
                  bogus) NodeSet.empty effects
   in
   let _ =
      if not (NodeSet.is_empty bogus) then
         let pp_print_target_loc buf (target, loc) =
            fprintf buf "@ @[<hv3>%a@ (%a)@]" pp_print_node target pp_print_location loc
         in
         let rec pp_print_bogus_set buf bogus =
            if not (NodeSet.is_empty bogus) then begin
               let effect = NodeSet.choose bogus in
                  pp_print_target_loc buf (effect, (NodeTable.find env.env_explicit_targets effect).rule_loc);
                  pp_print_bogus_set buf (NodeSet.remove bogus effect)
            end
         in
         eprintf "@[<v 3>*** omake:@ These file are targeted separately, but appear as effects of a single rule.@ This is likely to lead to unpredictable behavior.@ @[<v 3>targets:%a%a@]@]@." (**)
            pp_print_target_loc (target, loc)
            pp_print_bogus_set bogus
   in
      build_any_command env pos loc venv target effects locks sources scanners commands

(*
 * Build a command from a set of implicit rules.
 * We choose the first rule where the dependencies can be satisfied.
 *)
let build_implicit_command env pos loc target venv =
   let pos = string_pos "build_implicit_command" pos in
      match Omake_target.venv_find_buildable_implicit_rule env.env_cache venv pos target with
         Some { rule_loc      = loc;
                rule_effects  = effects;
                rule_locks    = locks;
                rule_sources  = sources;
                rule_scanners = scanners;
                rule_commands = commands
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
let build_explicit_target env pos loc target erule =
   let pos = string_pos "build_explicit_target" pos in
   let { rule_loc         = loc;
         rule_env         = venv;
         rule_effects     = effects;
         rule_locks       = locks;
         rule_sources     = sources;
         rule_scanners    = scanners;
         rule_commands    = commands
       } = expand_rule erule
   in
      if commands = [] then
         build_implicit_command env pos loc target venv
      else
         build_explicit_command env pos loc target effects locks venv sources scanners commands

(*
 * Create a new command for the target.
 *)
let build_command_non_escaped env pos loc target =
   let pos = string_pos "build_command_non_escaped" pos in

   (*
    * If the target has an explicit rule, use it.
    * Otherwise, this is a leaf in the dependency tree.
    *)
   let erule =
      try ExplicitTarget (NodeTable.find env.env_explicit_targets target) with
         Not_found ->
            let target_dir = Node.dir target in
               try ExplicitDirectory (DirTable.find env.env_explicit_directories target_dir) with
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
let build_scanner_command env pos loc target venv =
   let pos = string_pos "build_scanner_command" pos in

   (*
    * If the target has an explicit rule, use it.
    * Otherwise, this is a leaf in the dependency tree.
    *)
   let erule =
      try Some (NodeTable.find env.env_explicit_targets target) with
         Not_found ->
            None
   in
      match erule with
         Some erule ->
            build_explicit_target env pos loc target erule
       | None ->
            build_implicit_command env pos loc target venv

(*
 * If the node is escaped, just create it as succeeded.
 * If it is squashed, create a fake node that depends
 * on the original file.
 *)
let build_command env pos loc target =
   let pos = string_pos "build_command" pos in
   let () =
      if debug debug_build then
         eprintf "@[<hv 3>Building command for: %s@]@." (Node.fullname target)
   in
      match Node.kind target with
         NodeOptional
       | NodeExists ->
            create_exists_command env pos loc target
       | NodeSquashed ->
            create_squashed_command env pos loc target
       | NodePhony
       | NodeNormal
       | NodeScanner ->
            build_command_non_escaped env pos loc target

(*
 * Start commands, or build them.
 *)
let start_or_build_commands env pos loc parent targets =
   let pos = string_pos "start_or_build_commands" pos in
      NodeSet.iter (fun target ->
            try start_command env (find_command env target) with
               Not_found ->
                  (try build_command env pos loc target with
                      UnknownTarget target ->
                         let print_error buf =
                            Lm_printf.fprintf buf "Do not know how to build \"%a\" required for \"%a\"" pp_print_node target pp_print_node parent
                         in
                            raise (OmakeException (pos, LazyError print_error)))) targets

(*
 * Start scanners.  The difference is that a scanner inherits
 * the environment of the parent, unless the scanner
 * target is explicit.
 *)
let start_or_build_scanners env pos loc parent targets venv =
   let pos = string_pos "start_or_build_scanners" pos in
      NodeSet.iter (fun target ->
            try
               let command = find_command env target in
(*
                  if command.command_venv != venv then
                     eprintf "@[<hv 3>*** omake warning:@ @[<hv3>scanner uses a different environment than the target@ %a:@]@ @[<hv3>scanner definition:@ %a;@]@ @[<hv 3>current location:@ %a@]@]@." (**)
                        pp_print_node target pp_print_location command.command_loc pp_print_location loc;
 *)
                  start_command env command
            with
               Not_found ->
                  (try build_scanner_command env pos loc target venv with
                      UnknownTarget target ->
                         let print_error buf =
                            Lm_printf.fprintf buf "Do not know how to build \"%a\" required for \"%a\"" pp_print_node target pp_print_node parent
                         in
                            raise (OmakeException (pos, LazyError print_error)))) targets

(*
 * Make sure the effect sets form equivalence classes.
 * Every command in the effect set should have the
 * same effects.
 *)
let start_or_build_effects env pos loc target effects =
   let pos = string_pos "start_or_build_effects" pos in
   let step effects =
      start_or_build_commands env pos loc target effects;
      NodeSet.fold (fun (changed, effects) effect ->
            let command = find_command env effect in
            let effects' = command.command_effects in
               if effects' == effects then
                  changed, effects
               else
                  let effects = NodeSet.union effects effects' in
                     command.command_effects <- effects;
                     true, effects) (false, effects) effects
   in
   let rec fixpoint effects =
      let changed, effects = step effects in
         if changed then
            fixpoint effects
   in
      fixpoint effects

(*
 * Catch errors.
 *)
let build_command env pos loc target =
   try build_command env pos loc target with
      UnknownTarget _ ->
         raise (OmakeException (pos, StringNodeError ("Do not know how to build", target)))

(************************************************************************
 * Dependency management
 *)

(*
 * Add inverse entries from the command,
 * and set the blocked queue.
 *)
let command_set_blocked env command deps =
   let { command_target = target } = command in
   let inverse =
      NodeSet.fold (fun inverse dep ->
            NodeTable.filter_add inverse dep (fun commands ->
                  let commands =
                     match commands with
                        Some commands ->
                           commands
                      | None ->
                           NodeTable.empty
                  in
                     NodeTable.add commands target command)) env.env_inverse deps
   in
      env.env_inverse <- inverse;
      command.command_blocked <- NodeSet.to_list deps

(*
 * Add the build deps.
 *)
let command_set_build_deps env command deps =
   command_set_blocked env command deps;
   command.command_build_deps <- deps

(*
 * Check if the command overlaps with a running process.
 *)
let command_conflicts_with_running env command =
   let locks = command.command_locks in
      command_exists env CommandRunningTag (fun command ->
            NodeSet.exists (NodeSet.mem locks) command.command_locks) false

(*
 * Check if a command succeeded.
 *)
let command_succeeded command =
   match command.command_state with
      CommandSucceeded _ ->
         true
    | _ ->
         false

(*
 * Command is blocked until all dependencies have been built.
 *)
let command_is_blocked env command =
   let { command_blocked = blocked } = command in
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
let command_effects_are_scanned env command =
   let { command_target = target;
         command_effects = effects
       } = command
   in
      NodeSet.for_all (fun effect ->
            if Node.equal effect target then
               true
            else
               match (find_command env effect).command_state with
                  CommandScannedPending
                | CommandSucceeded _ ->
                     true
                | _ ->
                     false) effects

(*
 * Reclassify dependent rules.
 *)
let enable_parents env command =
   let enable_parent _ command =
      if not (command_is_blocked env command) then
         let state =
            match command.command_state with
               CommandScanBlocked ->
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
      try NodeTable.find env.env_inverse command.command_target with
         Not_found ->
            NodeTable.empty
   in
      NodeTable.iter enable_parent parents

(************************************************************************
 * Generic execution.
 *)

(*
 * Parse the dependency list.
 *)
let parse_deps env venv target file =
   let deps = compile_deps venv target file in
      if !debug_deps then
         begin
            eprintf "@[<v 3>Scanner: %s" file;
            List.iter (fun (targets, sources) ->
                  eprintf "@ @[<hv 0>@[<b 3>targets =";
                  List.iter (fun target -> eprintf "@ %s" target) targets;
                  eprintf "@]@ @[<b 3>sources =";
                  List.iter (fun source -> eprintf "@ %s" source) sources;
                  eprintf "@]@]") deps;
            eprintf "@]@."
         end;
      List.fold_left (fun table (targets, sources) ->
            let sources =
               List.fold_left (fun set s ->
                     let node = venv_intern venv PhonyOK s in
                        NodeSet.add set node) NodeSet.empty sources
            in
               List.fold_left (fun table target ->
                     let target = venv_intern venv PhonyOK target in
                        NodeTable.filter_add table target (fun set ->
                              match set with
                                 Some set ->
                                    NodeSet.union set sources
                               | None ->
                                    sources)) table targets) NodeTable.empty deps

(*
 * Unlink a file, no errors.
 *)
let unlink_file filename =
   try Unix.unlink filename with
      Unix.Unix_error _ ->
         ()

(*
 * A command finished with an error.
 *)
let abort_command env command code =
   if opt_terminate_on_error (env_options env) then
      env.env_error_code <- code;
   env_close_failed_tee env command;
   reclassify_command env command (CommandFailed code)

let abort_commands env targets code =
   if opt_terminate_on_error (env_options env) then
      env.env_error_code <- code;
   NodeSet.iter (fun target ->
         reclassify_command env (find_command env target) (CommandFailed code)) targets

(************************************************************************
 * Scanner execution.
 *)

(*
 * All scanner subgoals have finished, and all effects
 * have been scanned too.  Take the union of all the dependencies.
 *)
let finish_scanned env command =
   let { command_loc          = loc;
         command_target       = target;
         command_effects      = effects
       } = command
   in
   let pos = loc_exp_pos loc in

   (* Get the command for each of the effects *)
   let effects_commands =
      NodeSet.fold (fun commands command ->
            find_command env command :: commands) [] effects
   in

   (* Find all the scanner results *)
   let scanner_deps =
      List.fold_left (fun scanner_deps command ->
            NodeSet.union scanner_deps command.command_scanner_deps) NodeSet.empty effects_commands
   in
   let dep_tables =
      NodeSet.fold (fun dep_tables scanner ->
            let scan_command = find_command env scanner in
               match scan_command.command_state with
                  CommandSucceeded table ->
                     table :: dep_tables
                | _ ->
                     let print_error buf =
                        fprintf buf "@[<hv 3>Internal error in Omake_build.finish_scanned:@ %a@ %a@ @[<v 3>Effects:%a@]@]" (**)
                           pp_print_command command
                           pp_print_command scan_command
                           (pp_print_node_states env) effects
                     in
                        raise (OmakeFatalErr (pos, LazyError print_error))) [] scanner_deps
   in

   (* Now collect all the deps *)
   let deps =
      List.fold_left (fun deps command ->
            let { command_target = target;
                  command_static_deps = static_deps;
                  command_scanner_deps = scanner_deps
                } = command
            in
            let deps = NodeSet.union deps static_deps in
            let deps = NodeSet.union deps scanner_deps in
               List.fold_left (fun deps table ->
                     try NodeSet.union deps (NodeTable.find table target) with
                        Not_found ->
                           deps) deps dep_tables) NodeSet.empty effects_commands
   in
      (* Make sure all the newly discovered dependencies have commands *)
      start_or_build_commands env pos loc target deps;

      (* Set the state of all the effects *)
      List.iter (fun command ->
            let target = command.command_target in
               (* Set the dependencies *)
               command_set_build_deps env command deps;

               (* Dependencies are final at this point *)
               if NodeSet.mem env.env_print_dependencies target then
                  eprintf "@[<v 3>dependencies:@ %a@]@." (pp_print_dependencies env) command;

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

(*
 * A scanner has finished successfully.
 * Notify the parents.
 *)
let finish_scanner env command scanned_deps =
   let { command_loc = loc;
         command_target = target;
         command_effects = effects
       } = command
   in
      if debug debug_scanner then
         eprintf "@[<hv 3>finish_scanner %a:%a@]@." (**)
            pp_print_node target
            pp_print_node_set_table scanned_deps;

      (* This command has been scanned *)
      reclassify_command env command (CommandSucceeded scanned_deps);

      (* Notify parents that something is done *)
      enable_parents env command

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
let save_and_finish_scanner_results env command scanned_deps =
   (* Add the run to the cache *)
   let { command_loc            = loc;
         command_venv           = venv;
         command_target         = target;
         command_lines          = scanner;
         command_locks          = locks;
         command_build_deps     = build_deps
       } = command
   in

   (* Save in cache *)
   let cache = env.env_cache in
   let targets = NodeSet.singleton target in

   (* Re-stat the locks *)
   let () = NodeSet.iter (fun lock -> ignore (Omake_cache.force_stat cache lock)) locks in

   (* Recompute the scanner digest *)
   let digest =
      match scanner with
         CommandNone ->
            None
       | CommandLines (_, _, digest) ->
            digest
       | CommandScanner (info, sloppy_deps, _, digest) ->
            let deps = flatten_deps scanned_deps in
               if NodeSet.equal deps sloppy_deps then
                  digest
               else
                  let pos = loc_exp_pos loc in
                  let scanner_commands = eval_commands venv loc target deps info in
                  let scanner_commands = List.map command_allow_output scanner_commands in
                     digest_of_commands pos scanner_commands
       | CommandInfo _ ->
            raise (Invalid_argument "scanner_lines")
   in
      Omake_cache.add cache scanner_fun target targets build_deps digest (MemoSuccess scanned_deps);
      finish_scanner env command scanned_deps

(*
 * Add the run to the cache.
 *)
let save_and_finish_scanner_success env command filename =
   let { command_loc = loc;
         command_venv = venv;
         command_target  = target
       } = command
   in

   (*
    * Get the result.
    * The parser may still fail.
    *)
   let result =
      try
         let result = parse_deps env venv target filename in
            (* Remove the file as early as possible *)
            unlink_file filename;
            Some result
      with
         OmakeException _
       | UncaughtException _
       | Failure _
       | Not_found
       | Parsing.Parse_error
       | Sys_error _ ->
            None
   in
      match result with
         Some result ->
            if debug debug_scanner then
               eprintf "@[<v 3>Saving dependencies: %a@ @[<b 3>scanned deps:%a@]@]@." (**)
                  pp_print_node target
                  pp_print_deps result;
            env_close_success_tee env command;
            save_and_finish_scanner_results env command result

       | None ->
            (* Don't remove the file, in case the user wants to look at it *)
            let pos = string_pos "save_and_finish_scanner" (loc_exp_pos loc) in
            let lines, _ = command_lines command in
            let shell = eval_shell venv pos in
            let options = env_options env in
            let divert_only = not (opt_output options OutputNormal) in
            let handle_err = tee_stderr command.command_tee divert_only null_id in
            let out = make_formatter handle_err (fun () -> handle_err "" 0 0) in
               fprintf out "@?*** omake: scanner produced ill-formed output@.";
               pp_status_lines out options shell "scan" lines;
               fprintf out "*** omake: @[<hv0>scanner output is saved in@ %s@]@." filename;
               abort_command env command scanner_error_code

(*
 * Failed run.
 *)
let save_and_finish_scanner_failed env command filename code =
   unlink_file filename;
   abort_command env command code

(*
 * Run the command.
 *)
let execute_scanner env command =
   let { command_target = target;
         command_loc = loc;
         command_venv = venv
       } = command
   in
   let pos = string_pos "execute_scanner" (loc_exp_pos loc) in
   let scanner, _ = command_lines command in

   (* Save errors to the tee *)
   let options = venv_options venv in
   let tee = tee_create (opt_divert options) in
   let divert_only = not (opt_output options OutputNormal) in
   let copy_stdout = tee_stdout tee divert_only in
   let copy_stderr = tee_stderr tee divert_only in

   (* Save output into a temporary file *)
   let tmpfile = Filename.temp_file "omake" ".deps" in
   let handle_out = copy_file tmpfile in

   (* Debugging *)
   let () =
      if debug debug_scanner then
         eprintf "@[<v 3>run_scanner %a@ to tmp file %s:%a@]@." (**)
            pp_print_node target
            tmpfile
            pp_print_arg_command_lines scanner

   in
   let shell = eval_shell venv pos in
      set_tee env command tee;
      env.env_scan_exec_count <- succ env.env_scan_exec_count;
      match Exec.spawn env.env_exec shell (venv_options venv) copy_stdout handle_out copy_stderr "scan" target scanner with
         ProcessFailed ->
            (* The fork failed *)
            abort_command env command fork_error_code
       | ProcessStarted pid ->
            (* Process was started *)
            env.env_idle_count <- pred env.env_idle_count;
            reclassify_command env command (CommandRunning (pid, Some tmpfile))

(*
 * Execute a command.
 * Check with the cache to see if this command is already
 * up-to-date.
 *)
let start_scanner env command =
   let { command_venv           = venv;
         command_loc            = loc;
         command_target         = target;
         command_lines          = scanner;
         command_build_deps     = build_deps
       } = command
   in
   let pos = string_pos "start_scanner" (loc_exp_pos loc) in
   let sloppy_deps =
      try flatten_deps (Omake_cache.find_result_sloppy env.env_cache scanner_fun target) with
         Not_found ->
            NodeSet.empty
   in
   let scanner, scanner_digest =
      match scanner with
         CommandNone ->
            [], None
       | CommandInfo info ->
            assert (info <> []);
            let scanner_commands = eval_commands venv loc target sloppy_deps info in
            let scanner_commands = List.map command_allow_output scanner_commands in
            let scanner_digest = digest_of_commands pos scanner_commands in
            let info =
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
         if debug debug_scanner then
            eprintf "@[<hv 3>start_scanner: target has no scanner: %a@]@." pp_print_node target;
         save_and_finish_scanner_results env command NodeTable.empty
      end
      else begin
         (* Look up previous results from the cache *)
         env.env_scan_count <- succ env.env_scan_count;
         try
            let scanned_deps = Omake_cache.find_result env.env_cache scanner_fun build_deps scanner_digest in
               if debug debug_scanner then
                  eprintf "@[<hv 3>start_scanner: target dependencies are accurate %a:@ @[<b 3>scanner's build deps:%a@]@ @[<b 3>scanned deps:%a@]@]@." (**)
                     pp_print_node target
                     pp_print_node_set build_deps
                     pp_print_node_set_table scanned_deps;
               finish_scanner env command scanned_deps
         with
            Not_found ->
               execute_scanner env command
      end

(************************************************************************
 * Rule execution.
 *)

(*
 * A command finished successfuly.
 *)
let finish_rule_success env command =
   (* Get a list of all commands that might be updated *)
   reclassify_command env command (CommandSucceeded NodeTable.empty);
   enable_parents env command

(*
 * A command remains failed.
 *)
let finish_rule_failed env command code =
   let { command_effects = effects } = command in
      abort_commands env effects code

(*
 * A command finished successfuly.
 *)
let hexify_digest = function
   Some digest ->
      Lm_string_util.hexify digest
 | None ->
      "none"

let save_and_finish_rule_success env command =
   (* Add the run to the cache *)
   let { command_loc        = loc;
         command_target     = target;
         command_effects    = effects;
         command_locks      = locks;
         command_build_deps = build_deps
       } = command
   in
   let cache = env.env_cache in
   let commands, commands_digest = command_lines command in

   (* Collect the effects that are not phony, check that they were created *)
   let effects =
      NodeSet.fold (fun effects effect ->
            let digest = Omake_cache.force_stat cache effect in
               if Node.is_phony effect then
                  effects
               else if digest = None then begin
                  abort_command env command exn_error_code;
                  raise (OmakeException (loc_exp_pos loc, StringNodeError ("rule failed to build its target", effect)))
               end else
                  NodeSet.add effects effect) NodeSet.empty effects
   in

      (* Re-stat the locks *)
      NodeSet.iter (fun lock ->
            ignore (Omake_cache.force_stat cache lock)) locks;

      (* Add a memo for a specific target *)
      if debug debug_rule then
         eprintf "@[<v 3>saving %a:@ @[<hv 3>build-deps:%a@]@ @[<hv 3>effects:%a@]@ digest: %s@]@." (**)
            pp_print_node target
            pp_print_node_set build_deps
            pp_print_node_set effects
            (hexify_digest commands_digest);

      (* Add the memo only if the target is not phony *)
      if not (NodeSet.is_empty effects) then
         Omake_cache.add cache rule_fun target effects build_deps commands_digest (MemoSuccess NodeTable.empty);

      (* Remove the tees *)
      env_close_success_tee env command;

      (* Now tell parents that this job succeeded *)
      finish_rule_success env command

(*
 * A command failed.
 *)
let save_and_finish_rule_failed env command code =
   (* Add the run to the cache *)
   let { command_target     = target;
         command_effects    = effects;
         command_build_deps = build_deps
       } = command
   in
   let cache = env.env_cache in
   let commands, commands_digest = command_lines command in
      env_close_failed_tee env command;
      Omake_cache.add cache rule_fun target effects build_deps commands_digest (MemoFailure code);
      abort_commands env effects code

(*
 * Run the command.
 *)
let run_rule env command =
   let { command_loc     = loc;
         command_target  = target;
         command_effects = effects;
         command_venv    = venv;
       } = command
   in
   let pos = string_pos "run_rule" (loc_exp_pos loc) in
   let commands, _ = command_lines command in
   let shell = eval_shell venv pos in

   (* Set up the tee *)
   let options = venv_options venv in
   let tee = tee_create (opt_divert options) in
   let divert_only = not (opt_output options OutputNormal) in
   let copy_stdout = tee_stdout tee divert_only in
   let copy_stderr = tee_stderr tee divert_only in
      set_tee env command tee;
      env.env_rule_exec_count <- succ env.env_rule_exec_count;
      match Exec.spawn env.env_exec shell (venv_options venv) copy_stdout copy_stdout copy_stderr "build" target commands with
         ProcessFailed ->
            (* The fork failed *)
            abort_command env command fork_error_code
       | ProcessStarted pid ->
            (* The process was started *)
            env.env_idle_count <- pred env.env_idle_count;
            reclassify_command env command (CommandRunning (pid, None))

(*
 * Execute a command.
 * Check with the cache to see if this command is already
 * up-to-date.
 *)
let execute_rule env command =
   let { command_loc          = loc;
         command_state        = state;
         command_target       = target;
         command_effects      = effects;
         command_lines        = commands;
         command_build_deps   = build_deps;
         command_venv         = venv
       } = command
   in
   let pos = string_pos "execute_rule" (loc_exp_pos loc) in
   let options = venv_options venv in
   let commands, commands_digest =
      match commands with
         CommandNone ->
            [], None
       | CommandScanner (_, _, lines, digest)
       | CommandLines (_, lines, digest) ->
            lines, digest
       | CommandInfo info ->
            assert (info <> []);
            let commands = eval_commands venv loc target build_deps info in
            let digest = digest_of_commands pos commands in
               command.command_lines <- CommandLines (info, commands, digest);
               commands, digest
   in
      if debug debug_rule then
         eprintf "@[<v 3>building %a:@ @[<hv 3>build-deps:%a@]@ @[<hv 3>effects:%a@]@ digest: %s@]@." (**)
            pp_print_node target
            pp_print_node_set build_deps
            pp_print_node_set effects
            (hexify_digest commands_digest);
      if commands = [] then
         save_and_finish_rule_success env command
      else begin
         env.env_rule_count <- succ env.env_rule_count;
         match Omake_cache.up_to_date_status env.env_cache rule_fun build_deps commands_digest with
            StatusSuccess ->
               if debug debug_rule then
                  eprintf "@[<hv 3>target %a is up to date:%a@]@." (**)
                     pp_print_node target
                     pp_print_node_set build_deps;
               finish_rule_success env command
          | StatusFailure code ->
               if debug debug_rule then
                  eprintf "@[<hv 3>target %a failure:%a@]@." (**)
                     pp_print_node target
                     pp_print_node_set build_deps;
               finish_rule_failed env command code
          | StatusUnknown ->
               if opt_touch_only options then begin
                  if opt_print_file options then
                     printf "updating %a@." pp_print_node target;
                  save_and_finish_rule_success env command
               end
               else if opt_dry_run options then begin
                  if opt_print_command options <> EvalNever then
                     List.iter (fun command ->
                           if not (List.mem Omake_command_type.QuietFlag command.Omake_command_type.command_flags) then
                              printf "+ %a@." pp_print_arg_command_inst command.Omake_command_type.command_inst)
                     commands;
                  save_and_finish_rule_success env command
               end
               else begin
                  if debug debug_rule then
                     eprintf "@[<v 3>running %a:%a@]@." (**)
                        pp_print_node target
                        pp_print_node_set build_deps;
                  run_rule env command
               end
      end

(************************************************************************
 * Saved versions.
 *)

(*
 * Create a new, empty environment.
 *)
let empty_env venv cache exec ~summary deps targets dirs includes =
   let cwd = Dir.cwd () in
   let options = venv_options venv in
   let wl = create_wl () in
      { env_venv                 = venv;
        env_cwd                  = cwd;
        env_cache                = cache;
        env_exec                 = exec;
        env_explicit_deps        = deps;
        env_explicit_targets     = targets;
        env_explicit_directories = dirs;
        env_includes             = includes;

        env_commands           = NodeTable.empty;
        env_inverse            = NodeTable.empty;
        env_error_code         = 0;
        env_idle_count         = opt_job_count options;
        env_print_dependencies = NodeSet.empty;

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
   let erule_info = venv_explicit_rules venv in
   let { explicit_targets     = target_table;
         explicit_directories = dir_table;
         explicit_deps        = dep_table
       } = erule_info
   in
   let includes = venv_files venv in
   let includes = Omake_cache.stat_set cache includes in
      empty_env venv cache exec ~summary dep_table target_table dir_table includes

(************************************************************************
 * Saving state to .omakedb
 *)

let pid = Unix.getpid () (* this is the PID of the main thread *)

(*
 * Save the cache and environment to a file.
 *)
let save_aux env =
   (* Only the "master" thread should be saving the DB *)
   if (pid <> Unix.getpid ()) then begin
      eprintf "@[<hv3>*** OMake Internal ERROR:@ Slave thread %i trying to save db opened by the master thread %i@]@." (Unix.getpid ()) pid;
      raise (Invalid_argument "Internal error: Slave thread trying to save the OMake DB")
   end;

   (* Save the static values *)
   let () = venv_save_static_values env.env_venv in

   (* Save the .omakedb *)
   let cache = env.env_cache in

   (* We want the name to be fairly unique in case locking had failed us. *)
   let db_tmp = sprintf ".#%s.%s.%i" db_name (Unix.gethostname ()) pid in

   (* Marshal the state to the output file *)
   let outx = Pervasives.open_out_bin db_tmp in
   let includes =
      NodeTable.fold (fun includes node _ ->
            NodeSet.add includes node) NodeSet.empty env.env_includes
   in
   let targets = NodeSet.singleton env_target in
      try
         Omake_cache.add cache env_fun env_target targets includes None (MemoSuccess NodeTable.empty);
         Omake_cache.to_channel outx cache;
         close_out outx;
         Unix.rename db_tmp db_name
      with
         Unix.Unix_error (errno, name, arg) ->
            eprintf "*** omake: failure during saving: %s: %s(%s)@." (Unix.error_message errno) name arg;
            close_out outx;
            unlink_file db_tmp
       | Sys_error _
       | Failure _ as exn ->
            eprintf "*** omake: failure during saving: %s@." (Printexc.to_string exn);
            close_out outx;
            unlink_file db_tmp

(*
 * Save to the .omakedb.
 *)
let save env =
   if not (opt_dry_run (env_options env)) then
      try save_aux env with
         Sys_error _ as exn ->
            eprintf "*** omake: failure during saving: %s@." (Printexc.to_string exn)

(*
 * Close the environment.
 *)
let unlink_file name =
   try Unix.unlink name with
      Unix.Unix_error _ ->
         ()

let close env =
   NodeTable.iter (fun _ command -> unlink_tee command) env.env_commands;
   Exec.close env.env_exec;
   unlink_file env.env_summary

(************************************************************************
 * Invalidation.
 *)

(*
 * Forms for walking up and down the tree.
 *)
let invalidate_parents env command =
   find_parents env command.command_target

let invalidate_children env command =
   command.command_build_deps

(*
 * General invalidation function.
 *
 * The invalidate_next function determines how to walk the tree.
 *)
let rec invalidate_aux invalidate_next env nodes =
   if not (NodeSet.is_empty nodes) then
      let node = NodeSet.choose nodes in
      let command = find_command env node in
      let nodes =
         if command.command_state <> CommandInitial then
            let nodes = NodeSet.union nodes command.command_effects in
            let nodes = NodeSet.union nodes (invalidate_next env command) in

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
         invalidate_aux invalidate_next env (NodeSet.remove nodes node)

let invalidate_ancestors = invalidate_aux invalidate_parents
let invalidate_children  = invalidate_aux invalidate_children

(************************************************************************
 * Command management.
 *)

(*
 * Process a command in the Initial state.
 * Check the dependencies.  For each dependency,
 * make sure there is a command to build it.
 * If the dependencies are all finished, then
 * schedule this command for scanning.
 *)
let process_initial env =
   let command = command_list_head env CommandInitialTag in
   let { command_loc = loc;
         command_venv = venv;
         command_target = target;
         command_effects = effects;
         command_scanner_deps = scanner_deps;
         command_static_deps = static_deps
       } = command
   in
   let pos = string_pos "process_initial" (loc_exp_pos loc) in
   let _ =
      if debug debug_build then
         eprintf "@[<hv 3>Process initial: %a@ @[<b 3>scanner deps:%a@]@ @[<b 3>static deps:%a@]@]@." (**)
            pp_print_node target
            pp_print_node_set scanner_deps
            pp_print_node_set static_deps
   in
      (* Add commands for all the dependencies *)
      start_or_build_commands env pos loc target static_deps;
      start_or_build_scanners env pos loc target scanner_deps venv;

      (* Take the union of all the effects *)
      if NodeSet.cardinal effects > 1 then
         start_or_build_effects env pos loc target effects;

      (* Initially, we enter scanning mode *)
      command_set_blocked env command scanner_deps;
      let state =
         if command_is_blocked env command then
            CommandScanBlocked
         else if command_effects_are_scanned env command then
            CommandScanned
         else
            CommandScannedPending
      in
         reclassify_command env command state

(*
 * A command has been scanned successfully.
 *)
let process_scanned env =
   let command = command_list_head env CommandScannedTag in
      finish_scanned env command

(*
 * Process a command in the Ready state.
 * Start it and place it on the run queue.
 *)
let process_ready env =
   let command = command_list_head env CommandReadyTag in
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
   command_iter env CommandPendingTag (fun command ->
         if not (command_conflicts_with_running env command) then
            reclassify_command env command CommandReady)

(*
 * Leaf dependency - a leaf node, or a node that appears as optional/exists node
 *)
let is_leaf_file env node =
   if NodeTable.mem env.env_commands node then
      is_leaf_node env node
   else
      (NodeTable.mem env.env_commands (Node.create_escape NodeOptional node) ||
       NodeTable.mem env.env_commands (Node.create_escape NodeExists node))

(*
 * Process the running queue.
 * Wait until a process exits.
 *)
let rec process_running env notify =
   match Exec.wait env.env_exec (env_options env) with
      WaitExited (pid, code, _) ->
         begin
            env.env_idle_count <- succ env.env_idle_count;
            try
               let command = find_pid env pid in
               let () =
                  match code, command with
                     0, { command_state = CommandRunning (_, None) } ->
                        save_and_finish_rule_success env command
                   | _, { command_state = CommandRunning (_, None) } ->
                        save_and_finish_rule_failed env command code
                   | 0, { command_state = CommandRunning (_, Some filename) } ->
                        save_and_finish_scanner_success env command filename
                   | _, { command_state = CommandRunning (_, Some filename) } ->
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
         if !debug_remote then
            eprintf "# new idle count: %d + %d@." env.env_idle_count additional_jobs;
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
   if not (command_list_is_empty env CommandRunningTag) then begin
      if verbose then
         eprintf "*** omake: waiting for all jobs to finish@.";
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
   let verbose = opt_print_status (env_options env) in
      if verbose then begin
         progress_flush ();
         eprintf "*** omake: file %s changed@." (Node.fullname node)
      end;

      (* If this is an OMakefile, abort and restart *)
      if NodeTable.mem env.env_includes node then begin
         wait_all env verbose;
         raise (Restart None)
      end else
         let nodes = if is_leaf_node env node then NodeSet.singleton node else NodeSet.empty in
         let nodes = NodeSet.union nodes (find_parents env (Node.create_escape NodeOptional node)) in
         let nodes = NodeSet.union nodes (find_parents env (Node.create_escape NodeExists node)) in
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
 * Main processing loop.
 *)
let rec main_loop env progress =
   if debug debug_build then begin
      eprintf "@[<hv 3>Initial:";
      command_iter env CommandInitialTag (fun command ->
            eprintf "@ %a" pp_print_command command);
      eprintf "@]@.";
      eprintf "@[<hv 3>ScanBlocked:";
      command_iter env CommandScanBlockedTag (fun command ->
            eprintf "@ %a" pp_print_command command);
      eprintf "@]@.";
      eprintf "@[<hv 3>Blocked:";
      command_iter env CommandBlockedTag (fun command ->
            eprintf "@ %a" pp_print_command command);
      eprintf "@]@.";
      eprintf "@[<hv 3>Ready:";
      command_iter env CommandReadyTag (fun command ->
            eprintf "@ %a" pp_print_command command);
      eprintf "@]@.";
      eprintf "@[<hv 3>Running:";
      command_iter env CommandRunningTag (fun command ->
            eprintf "@ %a" pp_print_command command);
      eprintf "@]@.";
      eprintf "@[<hv 3>Succeeded:";
      command_iter env CommandSucceededTag (fun command ->
            eprintf "@ %a" pp_print_command command);
      eprintf "@]@.";
      eprintf "@[<hv 3>Failed:";
      command_iter env CommandFailedTag (fun command ->
            eprintf "@ %a" pp_print_command command);
      eprintf "@]@.";
   end;

   let progress =
      let flushed = progress_flushed () in
      if flushed || progress.ps_count <> env.env_succeeded_count then
         let progress = { progress with ps_count = env.env_succeeded_count } in
         let options = venv_options env.env_venv in
         let now = Unix.gettimeofday () in
         let will_save = ! save_interval > 0.0 && now > progress.ps_save in
         let progress =
            if will_save then begin
               save env;
               print_saving options;
               { progress with ps_save = now +. ! save_interval }
            end else
               progress
         in
            if flushed || will_save || now > progress.ps_progress then begin
               let total = NodeTable.cardinal env.env_commands - env.env_optional_count in
                  print_progress options env.env_succeeded_count total;
                  { progress with ps_progress = now +. prompt_interval }
            end else
               progress
      else
         progress
   in

   if not (command_list_is_empty env CommandInitialTag) then begin
      process_initial env;
      main_loop env progress
   end
   else if not (command_list_is_empty env CommandScannedTag) then begin
      process_scanned env;
      main_loop env progress
   end
   else if (env.env_idle_count > 0)
           && (env.env_error_code = 0)
           && not (command_list_is_empty env CommandReadyTag)
   then begin
      process_ready env;
      main_loop env progress
   end
   else if env.env_idle_count == 0 || not (command_list_is_empty env CommandRunningTag) then begin
      process_running env true;
      main_loop env progress
   end else begin
      assert (env.env_idle_count >= 0);
      progress_flush ()
   end

(************************************************************************
 * Printing.
 *)

(*
 * Print statistics.
 *)
let print_stats env message start_time =
   let { env_cwd = root;
         env_venv = venv;
         env_cache = cache;
         env_scan_count = scan_count;
         env_scan_exec_count = scan_exec_count;
         env_rule_count = rule_count;
         env_rule_exec_count = rule_exec_count
       } = env
   in
   let stat_count, digest_count = Omake_cache.stats cache in
   let total_time = Unix.gettimeofday () -. start_time in
   let options = venv_options venv in
      print_leaving_current_directory options;
      if opt_print_status options then begin
         if message <> "done" then begin
            let total = NodeTable.cardinal env.env_commands - env.env_optional_count in
               printf "*** omake: %i/%i targets are up to date@." env.env_succeeded_count total
         end;
         printf "*** omake: %s (%a, %d/%d scans, %d/%d rules, %d/%d digests)@." (**)
            message pp_time total_time
            scan_exec_count scan_count
            rule_exec_count rule_count
            digest_count stat_count
      end

(*
 * All of the commands in the Blocked queue are deadlocked.
 *)
let print_deadlock_exn env buf state =
   (* Inconsistency *)
   let failwith_inconsistency command =
      let { command_target       = target;
            command_state        = state;
            command_effects      = effects;
            command_scanner_deps = scanner_deps;
            command_static_deps  = static_deps;
            command_build_deps   = build_deps;
            command_loc          = loc
          } = command
      in
         fprintf buf "@[<v 3>*** omake: inconsistent state %a@ state = %a@ @[<b 3>effects =%a@]@ @[<b 3>build deps =%a@]@ @[<b 3>scanner deps =%a@]@ @[<b 3>static deps = %a@]@." (**)
            pp_print_node target
            pp_print_command_state state
            (pp_print_node_states env) effects
            (pp_print_node_states env) build_deps
            (pp_print_node_states env) scanner_deps
            (pp_print_node_states env) static_deps;
         raise (OmakeException (loc_exp_pos loc, StringNodeError ("failed on target", target)))
   in

   (* Deadlock *)
   let failwith_deadlock loc target marked =
      let rec print_marked marked =
         match marked with
            mark :: marked ->
               fprintf buf "*** omake: is a dependency of %a@." pp_print_node mark;
               if not (Node.equal mark target) then
                  print_marked marked
          | [] ->
               fprintf buf "*** omake: not deadlocked!@."
      in
         fprintf buf "*** omake: deadlock on %a@." pp_print_node target;
         print_marked marked;
         raise (OmakeException (loc_exp_pos loc, StringNodeError ("failed on target", target)))
   in

   (*
    * Find the deadlock.
    *)
   let rec print marked command =
      let { command_target = target;
            command_loc = loc;
          } = command
      in

      (*
       * Find the first dependency that has not been built.
       *)
      let rec search deps' =
         match deps' with
            dep :: deps ->
               let command =
                  try
                     let command = find_command env dep in
                        if command_succeeded command then
                           None
                        else
                           Some command
                  with
                     Not_found ->
                        fprintf buf "*** omake: Do not know how to build \"%a\" required for \"%a\"@." pp_print_node dep pp_print_node target;
                        raise (Failure "blocked")
               in
                  (match command with
                      Some dep ->
                         dep
                    | None ->
                         search deps)
          | [] ->
               (* All deps have succeeded; this is an inconsistent state *)
               failwith_inconsistency command
      in
         (* Detect deadlock *)
         if List.exists (fun node -> Node.equal node target) marked then
            failwith_deadlock loc target marked;

         (* Otherwise, search for first unsatisfied dependency *)
         let deps =
            NodeSet.union (**)
               (NodeSet.union command.command_build_deps command.command_scanner_deps)
               command.command_static_deps
         in
            print (target :: marked) (search (NodeSet.to_list deps))
   in
      print [] (command_list_head env state)

let print_deadlock env buf state =
   try print_deadlock_exn env buf state with
      OmakeException _
    | Failure _ as exn ->
         fprintf buf "%a@." Omake_exn_print.pp_print_exn exn

(*
 * Print the failed commands.
 *)
let print_failed_targets env buf =
   if opt_print_status (venv_options env.env_venv) then begin
      fprintf buf "*** omake: targets were not rebuilt because of errors:";
      (* We use table to get an alphabetical order here - see http://bugzilla.metaprl.org/show_bug.cgi?id=621 *)
      let table = ref LexStringMTable.empty in
      let add_command command =
         table := LexStringMTable.add !table (Node.absname command.command_target) command
      in
      let () = command_iter env CommandFailedTag add_command in
         LexStringMTable.iter (fun _ command ->
            fprintf buf "@\n   @[<v 3>@[<v 3>%a" pp_print_node command.command_target;
            NodeSet.iter (fun dep ->
                  if Node.is_real dep && is_leaf_node env dep then
                     fprintf buf "@ depends on: %a" pp_print_node dep) command.command_static_deps;
            fprintf buf "@]";
            format_tee_with_nl buf command;
            fprintf buf "@]") !table;
         fprintf buf "@."
   end

let print_failed env buf state =
   if not (command_list_is_empty env CommandFailedTag) then
      print_failed_targets env buf
   else
      print_deadlock env buf state

(************************************************************************
 * Loading state from .omakedb
 *)
let notify_wait_simple venv cwd exec cache =
   eprintf "*** omake: polling for filesystem changes (OMakefiles only)@.";
   let files = venv_files venv in
   let () =
      NodeSet.iter (fun node ->
         ignore (Omake_cache.stat cache node);
         Exec.monitor exec node) files
   in
   let print_msg =
      if opt_print_status (venv_options venv) then
         fun node -> printf "*** omake: file %s changed@." (Node.fullname node)
      else
         fun node -> ()
   in
   let rec loop changed =
      let event = Exec.next_event exec in
      let changed = changed || process_changes (NodeSet.mem files) print_msg venv cwd cache event in
         if (not changed || Exec.pending exec) then
            loop changed
   in
      loop false

let print_restart options reason =
   if opt_print_status options then
      let reason =
         match reason with
            None -> "a configuration file changed"
          | Some reason -> reason
      in
         printf "*** omake: %s, restarting@." reason

(*
 * Create and parse, given a cache.
 *)
let create_env exec options cache targets =
   let venv = Omake_env.create options "." exec cache in
   let venv = Omake_builtin.venv_add_command_defs venv in
   let targets_value = ValArray (List.map (fun v -> ValData v) targets) in
   let venv = Omake_env.venv_add_var venv targets_var targets_value in
   let venv = Omake_builtin.venv_add_builtins venv in

   (* Summary file *)
   let summary =
      let summary, outx = Filename.open_temp_file ~mode:[Open_binary] "omake" ".error" in
         Pervasives.close_out outx;
         summary
   in
   let summary_value = ValNode (venv_intern venv PhonyProhibited summary) in
   let venv = venv_add_var venv build_summary_var summary_value in

   (* Ignore match errors *)
   let venv = venv_add_var venv glob_options_var (ValString "n") in

   (* Start reading files *)
   let now = Unix.gettimeofday () in
   let cwd = venv_dir venv in
   let () =
      if opt_print_dir options then
         printf "make[0]: Entering directory `%s'@." (Dir.absname cwd);
      if opt_print_status options then begin
         printf "*** omake: THIS VERSION OF OMAKE IS UNDERGOING CHANGES!
*** omake: It may act differently from what you expect.
*** omake: If you encounter problems,
*** omake: consider using a released version,
*** omake: ---@.";
         printf "*** omake: reading %ss@." makefile_name
      end
   in
   let venv =
      try
         let venv = Omake_builtin.venv_include_rc_file venv omakeinit_file in
         let venv = Omake_builtin.venv_add_pervasives venv in
         let venv = Omake_builtin.venv_include_rc_file venv omakerc_file in
            Omake_eval.compile venv;
            venv
      with exn ->
         venv_save_static_values venv;
         if opt_poll options && restartable_exn exn && not (NodeSet.is_empty (venv_files venv)) then begin
            if opt_print_status options then begin
               let now' = Unix.gettimeofday () in
                  printf "*** omake: reading %ss failed (%a)@." makefile_name pp_time (now' -. now);
            end;
            eprintf "%a@." Omake_exn_print.pp_print_exn exn;
            notify_wait_simple venv cwd exec cache;
            raise (Restart None)
         end else begin
            unlink_file summary;
            raise exn
         end
   in
   let () =
      if opt_print_status options then
         let now' = Unix.gettimeofday () in
            printf "*** omake: finished reading %ss (%a)@." makefile_name pp_time (now' -. now)
   in
   let env = create exec venv cache summary in
      Omake_builtin_util.set_env env;
      env

let rec create_env_loop exec options cache targets =
   try create_env exec options cache targets with
      Restart reason ->
         print_restart options reason;
         create_env_loop exec options cache targets

(*
 * Load the environment if possible.
 * If not, create a new one.
 *)
let load_omake options targets =
   let cwd  = Dir.cwd () in
   let exec = Exec.create cwd options in
   let cache =
      match
         (* Load cache from the db file *)
         try
            let inx = open_in_bin db_name in
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
      match
         (* Load cache from the db file *)
         try
            let inx = open_in_bin db_name in
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
   let venv = venv_add_cache venv cache in

   (* Add the targets *)
   let targets_value = ValArray (List.map (fun v -> ValData v) targets) in
   let venv = Omake_env.venv_add_var venv targets_var targets_value in

   (* Add the summary file *)
   let summary =
      let summary, outx = Filename.open_temp_file ~mode:[Open_binary] "omake" ".error" in
         Pervasives.close_out outx;
         summary
   in
   let summary_value = ValNode (venv_intern venv PhonyProhibited summary) in
   let venv = venv_add_var venv build_summary_var summary_value in

   (* Create the environment *)
   let exec = venv_exec venv in
   let env = create exec venv cache summary in
      Omake_builtin_util.set_env env;
      env

let load venv_opt options targets =
   match venv_opt with
      Some venv ->
         load_osh venv options targets
    | None ->
         load_omake options targets

(************************************************************************
 * Main build command.
 *)

(*
 * Take a lock to prevent multiple builds from competing.
 *)
let copy_to_stderr fd =
   let inx = Unix.in_channel_of_descr fd in
   let rec loop () =
      let line = input_line inx in
         eprintf "%s@." line;
         loop ()
   in
      try loop () with
         End_of_file ->
            ()

let wait_for_lock, unlock_db =
   let name = db_name ^ ".lock" in
   let save_fd = ref None in
   let unlock_db () =
      match !save_fd with
         None ->
            ()
       | Some fd ->
            let () =
               (* XXX: JYH: this is bad style.
                * Under what circumstances will this fail?
                * BTW, don't use wildcard exception patterns please:/ *)
               try Omake_shell_sys.close_fd fd with
                  Unix.Unix_error _ ->
                     ()
            in
               save_fd := None
   in
   let wait_for_lock () =
      unlock_db ();
      let fd =
         try Lm_unix_util.openfile name [Unix.O_RDWR; Unix.O_CREAT] 0o666 with
            Unix.Unix_error _ ->
               raise (Failure ("project lock file is not writable: " ^ name))
      in
      let () =
         (*
          * XXX: TODO: We use lockf, but it is not NFS-safe if filesystem is mounted w/o locking.
          * .omakedb locking is only convenience, not safety, so it's not a huge problem.
          * But may be we should implement a "sloppy" locking as well - see
          * also the mailing list discussions:
          *    - http://lists.metaprl.org/pipermail/omake/2005-November/thread.html#744
          *    - http://lists.metaprl.org/pipermail/omake-devel/2005-November/thread.html#122
          *)
         try
            (* Try for a lock first, and report it if the file is locked *)
            try Lm_unix_util.lockf fd Unix.F_TLOCK 0 with
               Unix.Unix_error (Unix.EAGAIN, _, _) ->
                  eprintf "*** omake: the project is currently locked.@.";
                  (try copy_to_stderr fd with _ -> ());

                  (* Unfortunately, we have to poll, since OCaml doesn't allow ^C during the lock request *)
                  let rec poll col =
                     let col =
                        if col >= 40 then begin
                           if col = 40 then eprintf "@.";
                           eprintf "*** omake: waiting for project lock: .@?";
                           0
                        end
                        else begin
                           eprintf ".@?";
                           succ col
                        end
                     in
                        Unix.sleep 1;
                        try Lm_unix_util.lockf fd Unix.F_TLOCK 0 with
                           Unix.Unix_error (Unix.EAGAIN, _, _) ->
                              poll col
                  in
                     poll 1000
         with
            (*
             * XXX: When lockf is not supported, we just print a warning and keep going.
             *      .omakedb locking is only convenience, not safety, so it's not a huge problem.
             *)
            Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOLCK) as err, _, _) ->
               eprintf "*** omake WARNING: Can not lock the project database file .omakedb:
\t%s. Will proceed anyway.
\tWARNING: Be aware that simultaneously running more than one instance
\t\tof OMake on the same project is not recommended.  It may
\t\tresult in some OMake instances failing to record their
\t\tprogress in the database@."
                  (Unix.error_message err)
          | Unix.Unix_error (err, _, _) ->
               raise (Failure ("Failed to lock the file " ^ name ^ ": " ^ (Unix.error_message err)))
          | Failure err ->
               raise (Failure ("Failed to lock the file " ^ name ^ ": " ^ err))
      in
         Omake_shell_sys.set_close_on_exec fd;
         save_fd := Some fd;
         (* Print the message to the lock file  *)
         try
            ignore (Unix.lseek fd 0 Unix.SEEK_SET);
            Lm_unix_util.ftruncate fd;
            let outx = Unix.out_channel_of_descr fd in
               Printf.fprintf outx "*** omake: the project was last locked by %s:%d.\n" (Unix.gethostname ()) (Unix.getpid ());
               Pervasives.flush outx
         with
            Unix.Unix_error _
          | Sys_error _
          | Failure _ ->
               ()
   in
      wait_for_lock, unlock_db

(*
 * We want to build the target string.
 * Create or find a command to build it.
 *)
let build_target_command env target =
   try
      let command = find_command env target in
         start_command env command
   with
      Not_found ->
         let name = Node.fullname target in
         let loc = bogus_loc name in
         let pos = string_pos "build_target" (loc_exp_pos loc) in
            build_command env pos loc target

(*
 * Catch the dependency printer.
 *)
let build_target env print target =
   (* Generate the command *)
   build_target_command env target;

   (* Add the print flag *)
   if print then
      print_node_dependencies env target

(*
 * Make the targets.
 *)
let make env =
   let now = Unix.gettimeofday () in
   let progress = {
      ps_count = env.env_succeeded_count;
      ps_progress =  now +. prompt_interval;
      ps_save = now +. !save_interval;
   } in
      main_loop env progress

(*
 * Wait for notifications.
 *)
let notify_wait env =
   let { env_exec = exec;
         env_venv = venv
       } = env
   in
   let db_node = venv_intern_cd venv PhonyProhibited (Dir.cwd ()) db_name in
   let rec loop found =
      if not found || Exec.pending exec then
         let event = Exec.next_event exec in
         let changed = invalidate_event env event in
            loop (changed || found)
   in
      eprintf "*** omake: polling for filesystem changes@.";
      save env;
      ignore (Omake_cache.stat_changed env.env_cache db_node);
      unlock_db ();
      loop false;
      wait_for_lock ();
      if Omake_cache.stat_changed env.env_cache db_node then
         raise (Restart (Some "another OMake process have modified the build DB"));
      if opt_print_status (env_options env) then
         eprintf "*** omake: rebuilding@."

let notify_wait_omakefile env =
   eprintf "*** omake: polling for filesystem changes (OMakefiles only)@.";
   let rec loop () =
      ignore (invalidate_event env (Exec.next_event env.env_exec));
      loop ()
   in
      try
         loop ()
      with
         Restart reason -> reason

(*
 * Summary management.
 *)
let create_tmpfile env =
   let outx = Pervasives.open_out_gen [Open_wronly; Open_binary; Open_creat; Open_trunc] 0o600 env.env_summary in
      close_out outx

let open_tmpfile env =
   let outx = Pervasives.open_out_gen [Open_wronly; Open_binary; Open_creat; Open_append] 0o600 env.env_summary in
   let buf = formatter_of_out_channel outx in
      buf, outx

let print_summary ?(unlink = true) env =
   let inx = open_in_bin env.env_summary in
   let buffer = String.create 256 in
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
         unlink_file env.env_summary

(*
 * Worklist switching.
 *
 * Build a pseudo-phased target .BUILD_* with a fresh worklist.
 * The reason for switching worklists is so we don't damage the
 * main build, and also so that we ignore the main build
 * when executing phases.
 *)
let build_phase env target =
   let code = env.env_error_code in
   let restore_wl () =
      env.env_current_wl <- env.env_main_wl;
      if env.env_error_code = 0 then
         env.env_error_code <- code;
      Queue.iter (fun event -> ignore (do_invalidate_event env event)) env.env_pending_events;
      Queue.clear env.env_pending_events
   in
      try
         env.env_current_wl <- create_wl ();
         env.env_error_code <- 0;
         build_target env false target;
         invalidate_children env (NodeSet.singleton target);
         make env;
         let success = command_list_is_empty env CommandFailedTag in
            restore_wl();
            success
      with
         exn ->
            restore_wl();
            raise exn

(*
 * Build command line targets.
 *)
let rec build_targets env save_flag start_time parallel print ?(summary = true) targets =
   let options = env_options env in
   let () =
      try
         let begin_success =
            (* Build the initial summary *)
            not summary || (create_tmpfile env; build_phase env build_begin_target)
         in
         let process_summary () =
            (* Print out the final summary *)
            let buf, outx = open_tmpfile env in
            let core_success =
               if env.env_error_code <> 0 then begin
                  print_stats env "failed" start_time;
                  print_failed_targets env buf;
                  false
               end else if not (command_list_is_empty env CommandBlockedTag) then begin
                  print_stats env "blocked" start_time;
                  print_failed env buf CommandBlockedTag;
                  false
               end else if not (command_list_is_empty env CommandScanBlockedTag) then begin
                  print_stats env "scanner is blocked" start_time;
                  print_failed env buf CommandScanBlockedTag;
                  false
               end else if not (command_list_is_empty env CommandFailedTag) then begin
                  print_stats env "failed" start_time;
                  print_failed_targets env buf;
                  false
               end else
                  true
            in
               close_out outx;
               core_success
         in
         let () =
            if begin_success then
               (* Build the core *)
               if parallel || opt_parallel options then begin
                  (* Add commands to build the targets *)
                  List.iter (build_target env print) targets;

                  (* Build *)
                  make env
               end
               else begin
                  (* Make them in order *)
                  List.iter (fun target ->
                        build_target env print target;
                        make env) targets
               end;
         in
            if summary then begin
               if not (process_summary () && begin_success && build_phase env build_success_target && process_summary ()) then
                  ignore (build_phase env build_failure_target);
               print_summary env
            end
      with
         Sys_error _
       | ExitException _
       | ExitParentException _
       | OmakeException _
       | UncaughtException _
       | RaiseException _
       | Unix.Unix_error _
       | OmakeFatalErr _
       | OmakeFatal _
       | Sys.Break
       | Failure _
       | Return _ as exn ->
            let buf, outx = open_tmpfile env in
               fprintf buf "%a@." Omake_exn_print.pp_print_exn exn;
               close_out outx;
               print_stats env (match exn with Sys.Break -> "stopped" | _ -> "failed") start_time;
               print_summary env ~unlink:false;
               if opt_poll options && restartable_exn exn then begin
                  unlink_file env.env_summary;
                  let reason = notify_wait_omakefile env in
                     raise (Restart reason)
               end
               else if opt_osh options then
                  env.env_error_code <- exn_error_code
               else begin
                  close env;
                  save env;
                  raise (BuildExit exn_error_code)
               end
   in
      (* Save database before exiting *)
      if save_flag then
         save env;

      (* Return error if that happened *)
      if env.env_error_code <> 0 then
         build_on_error env save_flag start_time parallel print targets options env.env_error_code
      else if not (command_list_is_empty env CommandBlockedTag) then
         build_on_error env save_flag start_time parallel print targets options deadlock_error_code
      else if not (command_list_is_empty env CommandScanBlockedTag) then
         build_on_error env save_flag start_time parallel print targets options deadlock_error_code
      else if not (command_list_is_empty env CommandFailedTag) then
         build_on_error env save_flag start_time parallel print targets options deadlock_error_code

and build_on_error env save_flag start_time parallel print targets options error_code =
   if not (opt_poll options) then
      raise (BuildExit error_code)
   else begin
      notify_wait env;
      build_targets env save_flag (Unix.gettimeofday ()) parallel print targets
   end

(*
 * Notification loop.
 *)
let rec notify_loop env options targets =
   begin try
      notify_wait env
   with Sys.Break ->
      eprintf "*** omake: Received Break signal, exiting@.";
      raise (BuildExit 0)
   end;

   (* Build the targets again *)
   let start_time = Unix.gettimeofday () in
      build_targets env true start_time false (opt_print_dependencies options) targets;
      print_stats env "done" start_time;
      notify_loop env options targets

(*
 * Start the core build.
 *)
let build_core env dir_name dir start_time options targets =
   (* First, build all the included files *)
   let changed =
      if opt_dry_run options then
         false
      else
         let includes = NodeTable.fold (fun includes node _ -> node :: includes) [] env.env_includes in
         let _ = build_targets env false start_time true false ~summary:false includes in
            NodeTable.exists (fun node digest ->
                  let digest' = Omake_cache.force_stat env.env_cache node in
                     digest' <> digest) env.env_includes
   in
   let () =
      if changed then begin
         env.env_includes <- Omake_cache.stat_table env.env_cache env.env_includes;
         raise (Restart None)
      end
   in

   let venv = env.env_venv in
   let venv = venv_chdir_tmp venv dir in
   let targets = List.map (venv_intern venv PhonyOK) targets in
   let () = List.iter (fun s -> print_node_dependencies env (venv_intern venv PhonyOK s)) (opt_show_dependencies options) in
   let options = env_options env in
      build_targets env true start_time false (opt_print_dependencies options) targets;
      print_stats env "done" start_time;

      (* Polling loop *)
      if opt_poll_on_done options then
         if not Lm_notify.enabled then
            eprintf "*** omake: Polling is not enabled@."
         else
            notify_loop env options targets;
      close env

(*
 * Main builder.
 *)
let rec build_time start_time venv_opt options dir_name targets =
   let env = load venv_opt options targets in
   let dir_name =
      if opt_project options then
         "."
      else
         dir_name
   in
   let dir = Dir.chdir env.env_cwd dir_name in

   (* Monitor the full tree if polling *)
   let () =
      if opt_poll options then
         let root = env.env_cwd in
            try Exec.monitor_tree env.env_exec root with
               Failure _ ->
                  (* This is just an optimization anyway *)
                  ()
   in

   (*
    * Check that this directory is actually a .SUBDIR.
    * Don't do the check in osh mode; we assume the script knows
    * what it is doing.
    *)
   let () =
      if venv_opt = None && not (opt_project options || DirTable.mem env.env_explicit_directories dir) then begin
         eprintf "*** omake: the current directory %s@." (Dir.absname dir);
         eprintf "*** omake: is not part of the root project in %s@." (Dir.absname env.env_cwd);
         raise (BuildExit 1)
      end
   in
   let restart reason =
      print_restart options reason;
      close env;
      save env;
      build_time start_time venv_opt options dir_name targets
   in
      try build_core env dir_name dir start_time options targets with
         Restart reason ->
            restart reason
       | Sys.Break as exn ->
            close env;
            save env;
            eprintf "%a@." Omake_exn_print.pp_print_exn exn;
            raise (BuildExit exn_error_code)
       | exn when opt_poll options && restartable_exn exn ->
            eprintf "%a@." Omake_exn_print.pp_print_exn exn;
            let reason = notify_wait_omakefile env in
               restart reason

let build options dir_name targets =
   try
      Omake_shell_sys.set_interactive false;
      wait_for_lock ();
      build_time (Unix.gettimeofday ()) None options dir_name targets
   with
      BuildExit code ->
         Pervasives.exit code

let build_fun venv targets =
   let options = venv_options venv in
   let dir = Dir.absname (venv_dir venv) in
      Unix.chdir dir;
      Omake_node.Dir.reset_cwd ();
      try
         wait_for_lock ();
         build_time (Unix.gettimeofday ()) (Some venv) options "." targets;
         true
      with
         BuildExit _ ->
            false

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
