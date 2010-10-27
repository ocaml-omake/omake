(*
 * Options for the omake program.
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
 * Authors:
 *    Jason Hickey @email{jyh@cs.caltech.edu}
 *    Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 *)

open Lm_printf

(*
 * When to print output.
 *)
type eval_flag =
   EvalNever
 | EvalLazy
 | EvalEager

(*
 * Diversion control.
 *)
type output_flag =
   OutputNormal
 | OutputPostponeSuccess
 | OutputPostponeError
 | OutputRepeatErrors

(*
 * Make the default state explicit (the actual value may depend on the value of other settings).
 *)
type 'a setting =
   Default
 | Set of 'a

(*
 * The basic make flags.
 *)
type omake_options =
   { opt_job_count            : int;
     opt_remote_servers       : (string * int) list;
     opt_terminate_on_error   : bool setting;
     opt_dry_run              : bool;
     opt_print_command        : eval_flag;
     opt_print_dir            : bool;
     opt_print_file           : bool;
     opt_print_status         : bool;
     opt_print_exit           : bool;
     mutable opt_print_progress : bool setting;
     opt_verbose              : bool;
     opt_touch_only           : bool;
     opt_flush_cache          : bool;
     opt_flush_dependencies   : bool;
     opt_print_dependencies   : bool;
     opt_show_dependencies    : string list;
     opt_all_dependencies     : bool;
     opt_verbose_dependencies : bool;
     opt_cd_root              : bool;
     opt_project              : bool;
     opt_poll                 : bool setting;
     opt_osh                  : bool;
     opt_poll_on_done         : bool;
     opt_flush_include        : bool;
     opt_flush_static         : bool;
     opt_allow_exceptions     : bool;
     opt_absname              : bool;
     opt_output               : (output_flag * bool) list;

     (* Warnings *)
     opt_warn_declare        : bool;
     opt_warn_error          : bool
   }

let opt_job_count opts =
   opts.opt_job_count

let opt_remote_servers opts =
   opts.opt_remote_servers

(*
 * Predicate returns true iff there are parallel jobs.
 *)
let opt_parallel options =
   (opt_job_count options) > 1 || (opt_remote_servers options) <> []

let set_job_count_and_servers_opt opts cnt srvs =
   { opts with opt_job_count = cnt; opt_remote_servers = srvs }

(*
 * The argument string is a colon-separated list of server specification.
 * A server spec can be:
 *    1. a number: this specifies the job_count
 *    2. a machine: this specified a remote server that will handle 1 job
 *    3. a machine=count: a remote server that will handle <count> jobs
 *)
let set_job_count options s =
   let set_job (job_count, remote_servers) job =
      try
         let index = String.index job '=' in
         let len = String.length job in
         let machine = String.sub job 0 index in
         let count = String.sub job (succ index) (len - index - 1) in
         let count =
            try int_of_string count with
               Failure _ ->
                  1
         in
            job_count, (machine, count) :: remote_servers
      with
         Not_found ->
            try int_of_string job, remote_servers with
               Failure _ ->
                  job_count, (job, 1) :: remote_servers
   in
   let job_count, remote_servers = List.fold_left set_job (1, []) (Lm_string_util.split ":" s) in
      set_job_count_and_servers_opt options (max 1 job_count) (List.rev remote_servers)

let opt_dry_run opts =
   opts.opt_dry_run

let set_dry_run_opt opts flag =
   { opts with opt_dry_run = flag }

let opt_print_command opts =
   opts.opt_print_command

let set_print_command_opt opts flag =
   { opts with opt_print_command = flag }

let opt_print_dir opts =
   opts.opt_print_dir

let set_print_dir_opt opts flag =
   { opts with opt_print_dir = flag }

let opt_print_file opts =
   opts.opt_print_file

let set_print_file_opt opts flag =
   { opts with opt_print_file = flag }

let opt_print_status opts =
   opts.opt_print_status

let set_print_status_opt opts flag =
   { opts with opt_print_status = flag }

let opt_print_exit opts =
   opts.opt_print_exit

let set_print_exit_opt opts flag =
   { opts with opt_print_exit = flag }

let opt_print_progress opts =
   match opts.opt_print_progress with
      Set b ->
         b
    | Default ->
         let ok_to_print =
            try
               (* XXX: TODO: in OCaml 3.10, use Unix.isatty *)
               ignore (Unix.tcgetattr Unix.stdout); true
            with
               Unix.Unix_error _ ->
                  eprintf "@[<hov3>*** omake: warning:@ stdout is not a tty,@ disabling the progress bar@ (use --progress to override).@]@.";
                  false
             | Invalid_argument "Unix.tcgetattr not implemented" ->
                  (* We are on Windows :-( *)
                  true
             | exn ->
                  eprintf "@[<hov3>*** omake: warning:@ tcgetattr failed for unknown reason:@ %s@]@." (**)
                  (Printexc.to_string exn);
                  true
         in
            opts.opt_print_progress <- Set ok_to_print;
            ok_to_print

let set_print_progress_opt opts flag =
   { opts with opt_print_progress = Set flag }

let opt_touch_only opts =
   opts.opt_touch_only

let set_touch_only_opt opts flag =
   { opts with opt_touch_only = flag }

let opt_flush_cache opts =
   opts.opt_flush_cache

let set_flush_cache_opt opts flag =
   { opts with opt_flush_cache = flag }

let opt_flush_dependencies opts =
   opts.opt_flush_dependencies

let set_flush_dependencies_opt opts flag =
   { opts with opt_flush_dependencies = flag }

let opt_print_dependencies opts =
   opts.opt_print_dependencies

let set_print_dependencies_opt opts flag =
   { opts with opt_print_dependencies = flag }

let opt_show_dependencies opts =
   opts.opt_show_dependencies

let add_show_dependency_opt opts dep =
   { opts with opt_show_dependencies = dep :: opts.opt_show_dependencies }

let opt_all_dependencies opts =
   opts.opt_all_dependencies

let set_all_dependencies_opt opts flag =
   { opts with opt_all_dependencies = flag }

let opt_verbose opts =
   opts.opt_verbose

let opt_verbose_dependencies opts =
   opts.opt_verbose_dependencies

let set_verbose_dependencies_opt opts flag =
   { opts with opt_verbose_dependencies = flag }

let opt_cd_root opts =
   opts.opt_cd_root

let set_cd_root_opt opts flag =
   { opts with opt_cd_root = flag }

let opt_project opts =
   opts.opt_project

let set_project_opt opts flag =
   { opts with opt_project = flag }

let opt_poll_on_done opts =
   opts.opt_poll_on_done

let set_poll_on_done_opt opts b =
   { opts with opt_poll_on_done = b }

let opt_poll opts =
   match opts.opt_poll with
      Set v -> v
    | Default -> (opt_poll_on_done opts)

let set_poll_opt opts b =
   { opts with opt_poll = Set b }

let opt_osh opts =
   opts.opt_osh

let set_osh_opt opts =
   { opts with opt_osh = true }

let opt_terminate_on_error opts =
   match opts.opt_terminate_on_error with
      Set v  -> v
    | Default -> not (opt_poll opts)

let set_terminate_on_error_opt opts flag =
   { opts with opt_terminate_on_error = Set flag }

let opt_flush_include opts =
   opts.opt_flush_include

let set_flush_include_opt opts flag =
   { opts with opt_flush_include = flag }

let opt_flush_static opts =
   opts.opt_flush_static

let set_flush_static_opt opts flag =
   { opts with opt_flush_static = flag }

let opt_allow_exceptions opts =
   opts.opt_allow_exceptions

let set_allow_exceptions_opt opts flag =
   { opts with opt_allow_exceptions = flag }

let opt_absname opts =
   opts.opt_absname

let set_absname_opt opts flag =
   { opts with opt_absname = flag }

let opt_warn_declare opts =
   opts.opt_warn_declare

let set_warn_declare_opt opts flag =
   { opts with opt_warn_declare = flag }

let opt_warn_error opts =
   opts.opt_warn_error

let set_warn_error_opt opts flag =
   { opts with opt_warn_error = flag }

(*
 * Output control.
 *)
let output_opt_char options c =
   match c with
      '0' ->
         (* -s --output-errors-only --no--progress *)
         { options with opt_print_status   = false;
                        opt_print_dir      = false;
                        opt_print_file     = false;
                        opt_print_exit     = false;
                        opt_print_command  = EvalNever;
                        opt_print_progress = Set false;
                        opt_output         = [(OutputPostponeError, true)]
         }
    | '1' ->
         (* -S --progress --output-errors-only *)
         { options with opt_print_command = EvalLazy;
                        opt_print_progress = Set true;
                        opt_output = [(OutputPostponeError, true)]
         }
    | '2' ->
         (* --progress --output-postpone *)
         { options with opt_print_progress = Set true;
                        opt_output = [(OutputPostponeSuccess, true); (OutputPostponeError, true)]
         }
    | 'W' ->
         set_print_dir_opt options true
    | 'w' ->
         set_print_dir_opt options false
    | 'P' ->
         set_print_progress_opt options true
    | 'p' ->
         set_print_progress_opt options false
    | 'X' ->
         set_print_exit_opt options true
    | 'x' ->
         set_print_exit_opt options false
    | 'S' ->
         set_print_status_opt options true
    | 's' ->
         set_print_status_opt options false
    | _ ->
         (* Ignore, for forward compatibility *)
         options

let set_output_opts options s =
   let len = String.length s in
   let rec loop options i =
      if i = len then
         options
      else
         loop (output_opt_char options s.[i]) (succ i)
   in
      loop options 0

let rec opt_output opts flag =
   let answer = try Some(List.assoc flag opts.opt_output) with Not_found -> None in
   (* A few extra wrinkles *)
   match answer, flag with
      Some true, _ ->
         (* Everything should be on when explicitly enabled *)
         true
    | (Some false | None), OutputPostponeError ->
         (* If successes are printed, errors should be too, no matter what *)
         opt_output opts OutputPostponeSuccess
    | Some false, _ ->
         (* Everything else should be off when explicitly disabled *)
         false
    | None, OutputNormal ->
         not (opt_output opts OutputPostponeSuccess || opt_output opts OutputPostponeError)
    | None, OutputRepeatErrors ->
         (* default is "on iff -k/-p/-P" *)
         not (opt_terminate_on_error opts)
    | None, OutputPostponeSuccess ->
         (* off by default *)
         false

let set_output_opt flag opts on =
   let flags = (flag, on) :: (List.remove_assoc flag opts.opt_output) in
      { opts with opt_output = flags }

let opt_divert opts =
   List.exists (opt_output opts) [OutputPostponeSuccess; OutputPostponeError; OutputRepeatErrors]

(*
 * Default options.
 *)
let default_options =
   { opt_job_count            = 1;
     opt_remote_servers       = [];
     opt_terminate_on_error   = Default;
     opt_dry_run              = false;
     opt_print_command        = EvalLazy;
     opt_print_dir            = false;
     opt_print_file           = true;
     opt_print_status         = true;
     opt_print_exit           = false;
     opt_print_progress       = Default;
     opt_verbose              = false;
     opt_touch_only           = false;
     opt_flush_cache          = false;
     opt_flush_dependencies   = false;
     opt_print_dependencies   = false;
     opt_show_dependencies    = [];
     opt_all_dependencies     = false;
     opt_verbose_dependencies = false;
     opt_cd_root              = false;
     opt_project              = false;
     opt_poll                 = Default;
     opt_poll_on_done         = false;
     opt_osh                  = false;
     opt_flush_include        = false;
     opt_flush_static         = false;
     opt_allow_exceptions     = false;
     opt_absname              = false;
     opt_output               = [];
     opt_warn_declare         = false;
     opt_warn_error           = false;
   }

(*
 * Argument specifier.
 *
 * NOTE!  This set of options is functional and scoped in OMakefiles.
 * Global, non-scoped options that assign to reference cells should be
 * put in the option list in Omake_main, not here.
 *)
let options_spec =
   ["-j", Lm_arg.StringFold set_job_count, (**)
       "Specify parallel jobs and remote servers";
    "-k", Lm_arg.ClearFold set_terminate_on_error_opt, (**)
       "Do not stop when an error occurs; implied by -p and -P";
    "-p", Lm_arg.SetFold set_poll_opt, (**)
       "Poll filesystem for changes (until build succeeds); implies -k";
    "-P", Lm_arg.SetFold set_poll_on_done_opt, (**)
       "Poll filesystem for changes (keep polling \"forever\"); implies -k and -p";
    "-n", Lm_arg.SetFold set_dry_run_opt, (**)
       "Print commands, but do not execute them";
    "--project", Lm_arg.SetFold set_project_opt, (**)
       "Ignore the current directory and build the project";
    "-t", Lm_arg.SetFold set_touch_only_opt, (**)
       "Update database to force files to be up-to-date";
    "--depend", Lm_arg.SetFold set_flush_dependencies_opt, (**)
       "Do not trust cached dependecy information";
    "-U", Lm_arg.SetFold set_flush_cache_opt, (**)
       "Do not trust the dependency cache or cached OMakefiles";
    "--flush-includes", Lm_arg.SetFold set_flush_include_opt, (**)
       "Do not trust cached .omc files";
    "--configure", Lm_arg.SetFold set_flush_static_opt, (**)
       "Recompute static. sections";
    "-R", Lm_arg.SetFold set_cd_root_opt, (**)
       "Command-line targets are relative to the project root; builds all .DEFAULT targets if no targets given";
    "--print-dependencies", Lm_arg.SetFold set_print_dependencies_opt, (**)
       "Build and print dependencies";
    "--show-dependencies", Lm_arg.StringFold add_show_dependency_opt, (**)
       "Show dependencies if the file is built";
    "--all-dependencies", Lm_arg.SetFold set_all_dependencies_opt, (**)
       "For --print-dependencies and --show-dependencies, print dependencies recursively";
    "--verbose-dependencies", Lm_arg.SetFold set_verbose_dependencies_opt, (**)
       "For --print-dependencies and --show-dependencies, print all dependencies too";
    "--absname", Lm_arg.SetFold set_absname_opt, (**)
       "Filenames are always displayed as absolute paths";
    "-Wdeclare", Lm_arg.SetFold set_warn_declare_opt, (**)
       "Warn about undeclared variables";
    "-warn-error", Lm_arg.SetFold set_warn_error_opt, (**)
       "Treat warnings as errors"
   ]

let progress_usage =
   match Sys.os_type with
      "Unix" | "Cygwin" ->
         "(enabled by default when the stdout is a terminal)"
    | "Windows" ->
         "(default)"
    | _ -> (* Should not happen *)
         "(may be enabled by default)"

(*
 * Output control.
 *)
let output_spec =
   [
    "--verbose", Lm_arg.UnitFold (fun options ->
            { options with
              opt_print_command = EvalEager;
              opt_verbose = true;
              opt_print_status = true;
              opt_print_exit = true;
              opt_print_file = true
            }),
       "Verbose output (equivalent to \"--no-S --print-status --print-exit VERBOSE=true\")";
    "--print-exit", Lm_arg.SetFold set_print_exit_opt, (**)
       "Print the exit codes of commands";
    "-S", Lm_arg.SetFold (fun options b -> { options with opt_print_command = if b then EvalLazy else EvalEager }), (**)
       "Print command only if the command prints output (default)";
    "-s", Lm_arg.ClearFold (fun options b ->
          { options with opt_print_status  = b;
                         opt_print_dir     = b;
                         opt_print_file    = b;
                         opt_print_exit    = b;
                         opt_print_command = if b then EvalEager else EvalNever }), (**)
       "Never print commands before they are executed";
    "--progress", Lm_arg.SetFold set_print_progress_opt, (**)
       ("Print a progress indicator " ^ progress_usage);
    "--print-status", Lm_arg.SetFold set_print_status_opt, (**)
       "Print status lines (default)";
    "-w", Lm_arg.SetFold set_print_dir_opt, (**)
       "Print the directory in \"make format\" as commands are executed";
    "--output-normal", Lm_arg.SetFold (set_output_opt OutputNormal), (**)
       "Relay the output of the rule commands to the OMake output right away. This is the default when no --output-postpone and no --output-only-errors flags are given.";
    "--output-postpone", Lm_arg.SetFold (fun opt flag ->
            set_output_opt OutputPostponeSuccess (set_output_opt OutputPostponeError opt flag) flag), (**)
       "Postpone printing command output until a rule terminates.";
    "--output-only-errors", Lm_arg.SetFold (set_output_opt OutputPostponeError), (**)
       "Same as --output-postpone, but postponed output will only be printed for commands that fail.";
    "--output-at-end", Lm_arg.SetFold (set_output_opt OutputRepeatErrors), (**)
       "The output of the failed commands will be printed after OMake have stopped. Off by default, unless -k is enabled (directly or via -p/-P).";
    "-o", Lm_arg.StringFold set_output_opts, (**)
       "Short output options [01jwWpPxXsS] (see the manual)";
    ]

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
