(*
 * When to print output.
 *)
type eval_flag =
  |  EvalNever
  | EvalLazy
  | EvalEager

(*
 * Diversion control.
 *)
type output_flag =
  | OutputNormal
  | OutputPostponeSuccess
  | OutputPostponeError
  | OutputRepeatErrors

(*
 * Make the default state explicit (the actual value may depend on the value of other settings).
 *)
type  setting =
  | Default
  | Set of bool

(*
 * The basic make flags.
 *)
type t =
   { job_count            : int;
     remote_servers       : (string * int) list;
     terminate_on_error   :  setting;
     dry_run              : bool;
     print_command        : eval_flag;
     print_dir            : bool;
     print_file           : bool;
     print_status         : bool;
     print_exit           : bool;
     mutable print_progress :  setting;
     verbose              : bool;
     touch_only           : bool;
     flush_cache          : bool;
     flush_dependencies   : bool;
     print_dependencies   : bool;
     show_dependencies    : string list;
     all_dependencies     : bool;
     verbose_dependencies : bool;
     cd_root              : bool;
     project              : bool;
     poll                 :  setting;
     osh                  : bool;
     poll_on_done         : bool;
     flush_include        : bool;
     flush_static         : bool;
     allow_exceptions     : bool;
     absname              : bool;
     output               : (output_flag * bool) list;

     (* Warnings *)
     warn_declare        : bool;
     warn_error          : bool
   }

let opt_job_count opts =
   opts.job_count

let opt_remote_servers opts =
   opts.remote_servers

(*
 * Predicate returns true iff there are parallel jobs.
 *)
let opt_parallel options =
   (opt_job_count options) > 1 || (opt_remote_servers options) <> []

let set_job_count_and_servers_opt opts cnt srvs =
   { opts with job_count = cnt; remote_servers = srvs }

(*
 * The argument string is a colon-separated list of server specification.
 * A server spec can be:
 *    1. a number: this specifies the job_count
 *    2. a machine: this specified a remote server that will handle 1 job
 *    3. a machine=count: a remote server that will handle <count> jobs
 *)

let get_job_count (s : string) : int * (string * int) list = 
  let set_job (job_count, remote_servers) job =
    try
      let index = String.index job '=' in
      let len = String.length job in
      let machine = String.sub job 0 index in
      let count = String.sub job (index + 1) (len - index - 1) in
      let count =
        try int_of_string count with
          Failure _ -> 1
      in
      job_count, (machine, count) :: remote_servers
    with
      Not_found ->
      try int_of_string job, remote_servers with
        Failure _ ->
        job_count, (job, 1) :: remote_servers
  in
  let job_count, remote_servers = 
    List.fold_left (fun acc x -> set_job acc x)  (1, []) (Lm_string_util.split ":" s) in
  job_count , List.rev remote_servers 

let set_job_count (options : t) (s: string) : t   =
  let job_count, remote_servers = get_job_count s in
  set_job_count_and_servers_opt options (max  1 job_count) remote_servers 


let opt_dry_run opts =
   opts.dry_run

let set_dry_run_opt opts flag =
   { opts with dry_run = flag }

let opt_print_command opts =
   opts.print_command

let set_print_command_opt opts flag =
   { opts with print_command = flag }

let opt_print_dir opts =
   opts.print_dir

let set_print_dir_opt opts flag =
   { opts with print_dir = flag }

let opt_print_file opts =
   opts.print_file

let set_print_file_opt opts flag =
   { opts with print_file = flag }

let opt_print_status opts =
   opts.print_status

let set_print_status_opt opts flag =
   { opts with print_status = flag }

let opt_print_exit opts =
   opts.print_exit

let set_print_exit_opt opts flag =
   { opts with print_exit = flag }

let opt_print_progress opts =
  match opts.print_progress with
  | Set b -> b
  | Default ->
    let ok_to_print =
        Unix.isatty Unix.stdout in
    opts.print_progress <- Set ok_to_print;
    ok_to_print

let set_print_progress_opt opts flag =
   { opts with print_progress = Set flag }

let opt_touch_only opts =
   opts.touch_only

let set_touch_only_opt opts flag =
   { opts with touch_only = flag }

let opt_flush_cache opts =
   opts.flush_cache

let set_flush_cache_opt opts flag =
   { opts with flush_cache = flag }

let opt_flush_dependencies opts =
   opts.flush_dependencies

let set_flush_dependencies_opt opts flag =
   { opts with flush_dependencies = flag }

let opt_print_dependencies opts =
   opts.print_dependencies

let set_print_dependencies_opt opts flag =
   { opts with print_dependencies = flag }

let opt_show_dependencies opts =
   opts.show_dependencies

let add_show_dependency_opt opts dep =
   { opts with show_dependencies = dep :: opts.show_dependencies }

let opt_all_dependencies opts =
   opts.all_dependencies

let set_all_dependencies_opt opts flag =
   { opts with all_dependencies = flag }

let opt_verbose opts =
   opts.verbose

let opt_verbose_dependencies opts =
   opts.verbose_dependencies

let set_verbose_dependencies_opt opts flag =
   { opts with verbose_dependencies = flag }

let opt_cd_root opts =
   opts.cd_root

let set_cd_root_opt opts flag =
   { opts with cd_root = flag }

let opt_project opts =
   opts.project

let set_project_opt opts flag =
   { opts with project = flag }

let opt_poll_on_done opts =
   opts.poll_on_done

let set_poll_on_done_opt opts b =
   { opts with poll_on_done = b }

let opt_poll opts =
  match opts.poll with
  | Set v -> v
  | Default -> (opt_poll_on_done opts)

let set_poll_opt opts b =
   { opts with poll = Set b }

let opt_osh opts = opts.osh

let set_osh_opt opts =
   { opts with osh = true }

let opt_terminate_on_error opts =
   match opts.terminate_on_error with
   | Set v  -> v
   | Default -> not (opt_poll opts)

let set_terminate_on_error_opt opts flag =
   { opts with terminate_on_error = Set flag }

let opt_flush_include opts =
   opts.flush_include

let set_flush_include_opt opts flag =
   { opts with flush_include = flag }

let opt_flush_static opts =
   opts.flush_static

let set_flush_static_opt opts flag =
   { opts with flush_static = flag }

let opt_allow_exceptions opts =
   opts.allow_exceptions

let set_allow_exceptions_opt opts flag =
   { opts with allow_exceptions = flag }

let opt_absname opts =
   opts.absname

let set_absname_opt opts flag =
   { opts with absname = flag }

let opt_warn_declare opts =
   opts.warn_declare

let set_warn_declare_opt opts flag =
   { opts with warn_declare = flag }

let opt_warn_error opts =
   opts.warn_error

let set_warn_error_opt opts flag =
   { opts with warn_error = flag }

(*
 * Output control.
 *)
let output_opt_char (options : t)  (c : char) : t  =
  match c with
  | '0' ->
    (* -s --output-errors-only --no--progress *)
    { options with print_status   = false;
                   print_dir      = false;
                   print_file     = false;
                   print_exit     = false;
                   print_command  = EvalNever;
                   print_progress = Set false;
                   output         = [(OutputPostponeError, true)]
    }
  | '1' ->
    (* -S --progress --output-errors-only *)
    { options with print_command = EvalLazy;
                   print_progress = Set true;
                   output = [(OutputPostponeError, true)]
    }
  | '2' ->
    (* --progress --output-postpone *)
    { options with print_progress = Set true;
                   output = [(OutputPostponeSuccess, true); (OutputPostponeError, true)]
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
;;
let set_output_opts (options : t) (s : string) : t =
  Lm_string_util.fold_left (fun opt char -> output_opt_char  opt char)    options s

let rec opt_output (opts : t) (flag : output_flag) : bool =
  let answer = try Some(List.assoc flag opts.output) with Not_found -> None in
  (* A few extra wrinkles *)
  match answer, flag with
  | Some true, _ ->
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
   let flags = (flag, on) :: (List.remove_assoc flag opts.output) in
   { opts with output = flags }

let opt_divert opts =
   List.exists (opt_output opts) [OutputPostponeSuccess; OutputPostponeError; OutputRepeatErrors]

(*
 * Default options.
 *)
let default_options =
   { job_count            =  max (Lm_terminfo.get_number_of_cores ()) 1;
     remote_servers       = [];
     terminate_on_error   = Default;
     dry_run              = false;
     print_command        = EvalLazy;
     print_dir            = false;
     print_file           = true;
     print_status         = true;
     print_exit           = false;
     print_progress       = Default;
     verbose              = false;
     touch_only           = false;
     flush_cache          = false;
     flush_dependencies   = false;
     print_dependencies   = false;
     show_dependencies    = [];
     all_dependencies     = false;
     verbose_dependencies = false;
     cd_root              = false;
     project              = false;
     poll                 = Default;
     poll_on_done         = false;
     osh                  = false;
     flush_include        = false;
     flush_static         = false;
     allow_exceptions     = false;
     absname              = false;
     output               = [];
     warn_declare         = false;
     warn_error           = false;
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
   | "Unix" | "Cygwin" -> "(enabled by default when the stdout is a terminal)"
   | "Windows" -> "(default)"
   | _ -> (* Should not happen *)
         "(may be enabled by default)"

(*
 * Output control.
 *)
let output_spec =
  [
    "--verbose", 
    Lm_arg.UnitFold
      (fun options ->
         { options with
           print_command = EvalEager;
           verbose = true;
           print_status = true;
           print_exit = true;
           print_file = true
         }),
    "Verbose output (equivalent to \"--no-S --print-status --print-exit VERBOSE=true\")";
   
    "--print-exit", Lm_arg.SetFold set_print_exit_opt, "Print the exit codes of commands";

    "-S", Lm_arg.SetFold
      (fun options b -> { options with print_command = if b then EvalLazy else EvalEager }), 
    "Print command only if the command prints output (default)";

    "-s", Lm_arg.ClearFold (fun options b ->
        { options with print_status  = b;
                       print_dir     = b;
                       print_file    = b;
                       print_exit    = b;
                       print_command = if b then EvalEager else EvalNever }),
    "Never print commands before they are executed";

    "--progress", Lm_arg.SetFold set_print_progress_opt,
    ("Print a progress indicator " ^ progress_usage);

    "--print-status", Lm_arg.SetFold set_print_status_opt,
    "Print status lines (default)";

    "-w", Lm_arg.SetFold set_print_dir_opt,
    "Print the directory in \"make format\" as commands are executed";

    "--output-normal", Lm_arg.SetFold (set_output_opt OutputNormal), 
    "Relay the output of the rule commands to the OMake output right away. This is the default when no --output-postpone and no --output-only-errors flags are given.";

    "--output-postpone", Lm_arg.SetFold (fun opt flag ->
        set_output_opt OutputPostponeSuccess (set_output_opt OutputPostponeError opt flag) flag), 
    "Postpone printing command output until a rule terminates.";

    "--output-only-errors", Lm_arg.SetFold (set_output_opt OutputPostponeError), 
    "Same as --output-postpone, but postponed output will only be printed for commands that fail.";

    "--output-at-end", Lm_arg.SetFold (set_output_opt OutputRepeatErrors), 
    "The output of the failed commands will be printed after OMake have stopped. Off by default, unless -k is enabled (directly or via -p/-P).";
    "-o", Lm_arg.StringFold set_output_opts, (**)
    "Short output options [01jwWpPxXsS] (see the manual)";
  ]

