(*
 * Constants and types used by the build module Omake_build.
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
open Lm_symbol
open Lm_location

open Omake_ir
open Omake_env
open Omake_node
open Omake_exec
open Omake_exec_util
open Omake_cache_type
open Omake_command_type

(*
 * Magic number for marshaling.
 *)
let magic_number = ""

(*
 * A command corresponds to a rule,
 * where all variables have been resolved.
 *
 * A command can be in several states:
 *    Idle: the command is not currently used
 *    Initial: the command needs to be processed
 *    ScanBlocked: the command can't be scanned until
 *       the scanner dependencies have been built
 *    ScannedPending: the scanner dependencies have been
 *       satisfied, but the scan is blocked because the
 *       effects of this command have not been scanned yet.
 *    Scanned: the command has been scanned, and all
 *       effects have been scanned too
 *    Blocked: the command can't be run until all of its
 *       dependencies are built
 *    Ready: all dependencies have been satisfied, and the
 *       command is ready to run
 *    Pending: command is ready to run, but it conflicts
 *       with another job that is Running, so it has
 *       to wait
 *    Running (pid, file): the command is running, with the given pid.
 *       If this is a scanner, the output is being saved in the file
 *    Succeeded deps: the command succeeded.  If this is a scanner
 *       the deps are a table of the dependencies.
 *    Failed code: the command failed with the given non-zero exit code.
 *
 * As a command is processed, it passes through each of these states
 * in the given order, with the following exceptions:
 *    - The CommandScannedPending may be skipped
 *    - Only one of the Succeeded or Failed states is assigned
 *)
type command_state =
   CommandIdle
 | CommandInitial
 | CommandScanBlocked
 | CommandScannedPending
 | CommandScanned
 | CommandBlocked
 | CommandReady
 | CommandPending
 | CommandRunning of Omake_exec_id.id * string option
 | CommandSucceeded of NodeSet.t NodeTable.t
 | CommandFailed of int

type command_tag =
   CommandIdleTag
 | CommandInitialTag
 | CommandScanBlockedTag
 | CommandScannedPendingTag
 | CommandScannedTag
 | CommandBlockedTag
 | CommandReadyTag
 | CommandPendingTag
 | CommandRunningTag
 | CommandSucceededTag
 | CommandFailedTag

(*
 * Commands for a rule.
 *
 * Invariant: CommandInfo and CommandLines must have
 * at least one command.
 *
 * CommandLines is CommandInfo after evaluation.
 * CommandScanner saves the commands so they may be re-evaluated after the scan.
 *)
type command_body =
   CommandNone
 | CommandInfo of command_info list
 | CommandLines of command_info list * arg_command_line list * command_digest
 | CommandScanner of command_info list * NodeSet.t * arg_command_line list * command_digest

(*
 * The command.
 * There is a 1-to-1 correspondence between commands and targets.
 *
 *     command_state: the current command state
 *
 *     command_target: the target node to be built
 *     command_effects: the nodes that may be modified by this command
 *     command_loc: the location of the rule being used to build
 *        the target.
 *     command_lines: the commands that should be run sequentially
 *        to build the target.
 *
 *     command_static_deps: the dependencies statically defined
 *        in the OMakefiles.
 *     command_scanner_deps: the :scanner: dependencies for a command
 *
 *     command_build_deps: the dependencies, including implicit
 *        dependencies produced by the scanner
 *     command_blocked: at least one of the deps in the set
 *        is blocked
 *)
type command =
   { command_venv                       : venv;
     mutable command_state              : command_state;
     command_target                     : Node.t;
     mutable command_effects            : NodeSet.t;
     command_locks                      : NodeSet.t;
     command_loc                        : loc;

     (* Scanners for this command *)
     command_scanner_deps               : NodeSet.t;

     (* Static deps from the OMakefiles *)
     command_static_deps                : NodeSet.t;

     (* Actual dynamic dependencies *)
     mutable command_build_deps         : NodeSet.t;
     mutable command_blocked            : Node.t list;
     mutable command_lines              : command_body;

     (* Output tees *)
     mutable command_tee                : tee;

     (* Linked list *)
     mutable command_pred               : command option ref;
     command_succ                       : command option ref
   }

(*
 * The environment remembers all the commands.  In addition
 * we compute an inverted command graph , so that we know what to do
 * when a command finishes.
 *
 *    env_venv: the default environment
 *    env_cache: the build cache, so we don't run commands
 *       if the target is already up-to-date.
 *    env_explicit_deps: gives explicitly-defined dependencies of nodes
 *    env_explicit_targets: maps nodes to rules where the node is a target
 *    env_explicit_directories: maps directories to environments
 *
 *    env_commands: all the commands
 *    env_inverse: the inverted dependency graph
 *    env_error_code: the exit code when an error occurs
 *    env_idle_count: number of idle processors
 *)
type env_wl =
   { env_idle_wl                    : command option ref;
     env_initial_wl                 : command option ref;
     env_scan_blocked_wl            : command option ref;
     env_scanned_pending_wl         : command option ref;
     env_scanned_wl                 : command option ref;
     env_blocked_wl                 : command option ref;
     env_ready_wl                   : command option ref;
     env_pending_wl                 : command option ref;
     env_running_wl                 : command option ref;
     env_succeeded_wl               : command option ref;
     env_failed_wl                  : command option ref
   }

type env =
   { env_venv                       : venv;
     env_cwd                        : Dir.t;
     env_cache                      : Omake_cache.t;
     env_exec                       : exec;
     mutable env_explicit_deps      : (NodeSet.t * NodeSet.t * NodeSet.t) NodeTable.t;
     env_explicit_targets           : erule NodeTable.t;
     env_explicit_directories       : venv DirTable.t;
     mutable env_includes           : digest NodeTable.t;

     (* Build state *)
     mutable env_commands           : command NodeTable.t;
     mutable env_inverse            : command NodeTable.t NodeTable.t;
     mutable env_error_code         : int;
     mutable env_idle_count         : int;
     mutable env_print_dependencies : NodeSet.t;

     (* Worklists *)
     mutable env_current_wl         : env_wl;
     env_main_wl                    : env_wl;

     (* Pending events *)
     mutable env_pending_events     : Lm_notify.event Queue.t;

     (* Output files *)
     env_summary                    : string;

     (* Statistics *)
     mutable env_succeeded_count    : int;
     mutable env_optional_count     : int;
     mutable env_scan_count         : int;
     mutable env_scan_exec_count    : int;
     mutable env_rule_count         : int;
     mutable env_rule_exec_count    : int
   }

(*
 * Helper type for determining how to build a command
 * from a rule.
 *)
type explicit_rule =
   ExplicitTarget of erule
 | ExplicitDirectory of venv
 | ExplicitNone

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
