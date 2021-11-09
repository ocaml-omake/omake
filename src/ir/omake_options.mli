(*
 * Options for the omake program.
 *
 *)

(*
 * When to print output.
 *)
type eval_flag =
  | EvalNever
  | EvalLazy
  | EvalEager

(*
 * Diversion control.
 *)
type output_flag =
  | OutputDirect
  | OutputNormal
  | OutputPostponeSuccess
  | OutputPostponeError
  | OutputRepeatErrors

(*
 * Make the default state explicit (the actual value may depend on the value of other settings).
 *)
type setting 


type t = 
  { job_count            : int;
    remote_servers       : (string * int) list;
    terminate_on_error   : setting;
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
 

(*
 * Initial options.
 *)
val default_options : t

(*
 * Argument specifier.
 *)
val options_spec : (string * t Lm_arg.poly_spec * string) list
val output_spec  : (string * t Lm_arg.poly_spec * string) list

(*
 * Parallel build options
 *)
val opt_parallel : t -> bool
val opt_job_count : t -> int
val opt_remote_servers : t -> (string * int) list

val opt_terminate_on_error : t -> bool
val opt_poll : t -> bool
val opt_poll_on_done : t -> bool

val set_osh_opt : t -> t


val opt_print_dir : t -> bool
val opt_print_status : t -> bool
val opt_print_exit : t -> bool
val opt_print_progress : t -> bool
val opt_touch_only : t -> bool
val opt_flush_cache : t -> bool
val opt_flush_dependencies : t -> bool


val opt_all_dependencies : t -> bool
val opt_verbose_dependencies : t -> bool
val opt_cd_root : t -> bool
val opt_project : t -> bool
val opt_flush_include : t -> bool
val opt_flush_static : t -> bool
val opt_verbose : t -> bool

val opt_print_command : t -> eval_flag
val set_print_command_opt : t -> eval_flag -> t
val opt_print_file : t -> bool
val set_print_file_opt : t -> bool -> t

val opt_absname : t -> bool
val set_absname_opt : t -> bool -> t

val opt_divert : t -> bool
    (* true when some --output-* diversions other than --output-normal are enabled *)

val opt_output : t -> output_flag -> bool

val opt_allow_exceptions : t -> bool
val set_allow_exceptions_opt : t -> bool -> t

val opt_warn_declare : t -> bool
val opt_warn_error : t -> bool

