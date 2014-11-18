
(*
 * Magic number for marshaling.
 *)
let magic_number = ""

(**
  A command corresponds to a rule,
  where all variables have been resolved.
 
  A command can be in several states:
     Idle: the command is not currently used
     Initial: the command needs to be processed
     ScanBlocked: the command can't be scanned until
        the scanner dependencies have been built
     ScannedPending: the scanner dependencies have been
        satisfied, but the scan is blocked because the
        effects of this command have not been scanned yet.
     Scanned: the command has been scanned, and all
        effects have been scanned too
     Blocked: the command can't be run until all of its
        dependencies are built
     Ready: all dependencies have been satisfied, and the
        command is ready to run
     Pending: command is ready to run, but it conflicts
        with another job that is Running, so it has
        to wait
     Running (pid, file): the command is running, with the given pid.
        If this is a scanner, the output is being saved in the file
     Succeeded deps: the command succeeded.  If this is a scanner
        the deps are a table of the dependencies.
     Failed code: the command failed with the given non-zero exit code.
 
  As a command is processed, it passes through each of these states
  in the given order, with the following exceptions:
     - The CommandScannedPending may be skipped
     - Only one of the Succeeded or Failed states is assigned
 *)
type command_state =
  | CommandIdle
  | CommandInitial
  | CommandScanBlocked
  | CommandScannedPending
  | CommandScanned
  | CommandBlocked
  | CommandReady
  | CommandPending
  | CommandRunning of Omake_exec_id.t * scanner_detail option
  | CommandSucceeded of Omake_node.NodeSet.t Omake_node.NodeTable.t
  | CommandFailed of int

and scanner_detail =
  { scanner_out_file : string;
    scanner_post_action : (Lm_location.t * scanner_post_action) option;
  }

and scanner_post_action =
  (Omake_value_type.t, Omake_command_type.arg, Omake_env.apply) 
    Omake_shell_type.poly_apply

type command_tag =
  | CommandIdleTag
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
  | CommandInfo of Omake_env.command_info list
  | CommandLines of Omake_env.command_info list * Omake_env.arg_command_line list * Omake_command_type.command_digest
  | CommandScanner of Omake_env.command_info list * Omake_node.NodeSet.t * 
    Omake_env.arg_command_line list * Omake_command_type.command_digest

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
  { command_venv                       : Omake_env.t;
    mutable command_state              : command_state;
    command_target                     : Omake_node.Node.t;
    mutable command_effects            : Omake_node.NodeSet.t;
    command_locks                      : Omake_node.NodeSet.t;
    command_loc                        : Lm_location.t;

    (* Scanners for this command *)
    command_scanner_deps               : Omake_node.NodeSet.t;

    (* Static deps from the OMakefiles *)
    command_static_deps                : Omake_node.NodeSet.t;

    (* Actual dynamic dependencies *)
    mutable command_build_deps         : Omake_node.NodeSet.t;
    mutable command_blocked            : Omake_node.Node.t list;
    mutable command_lines              : command_body;

    (* Output tees *)
    mutable command_tee                : Omake_exec_util.tee;

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

type t =
  { env_venv                       : Omake_env.t;
    env_cwd                        : Omake_node.Dir.t;
    env_cache                      : Omake_cache.t;
    env_exec                       : Omake_env.exec;
    mutable env_explicit_deps      : 
      (Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_node.NodeSet.t) Omake_node.NodeTable.t;
    env_explicit_targets           : 
      Omake_env.erule Omake_node.NodeTable.t;
    env_explicit_directories       : 
      Omake_env.t Omake_node.DirTable.t;
    mutable env_includes           : 
      Omake_cache_type.digest Omake_node.NodeTable.t;

    (* Build state *)
    mutable env_commands           : command Omake_node.NodeTable.t;
    mutable env_inverse            : command Omake_node.NodeTable.t Omake_node.NodeTable.t;
    mutable env_error_code         : int;
    mutable env_idle_count         : int;
    mutable env_print_dependencies : Omake_node.NodeSet.t;

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
  | ExplicitTarget of Omake_env.erule
  | ExplicitDirectory of Omake_env.t
  | ExplicitNone

