(*
 * Options for the omake program.
 *
 *)

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
 * The basic make flags.
 *)
type t

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
val opt_osh : t -> bool
val set_osh_opt : t -> t

val opt_dry_run : t -> bool
val opt_print_dir : t -> bool
val opt_print_status : t -> bool
val opt_print_exit : t -> bool
val opt_print_progress : t -> bool
val opt_touch_only : t -> bool
val opt_flush_cache : t -> bool
val opt_flush_dependencies : t -> bool
val opt_print_dependencies : t -> bool
val opt_show_dependencies : t -> string list
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

