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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
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
type omake_options

(*
 * Initial options.
 *)
val default_options : omake_options

(*
 * Argument specifier.
 *)
val options_spec : (string * omake_options Lm_arg.poly_spec * string) list
val output_spec  : (string * omake_options Lm_arg.poly_spec * string) list

(*
 * Parallel build options
 *)
val opt_parallel : omake_options -> bool
val opt_job_count : omake_options -> int
val opt_remote_servers : omake_options -> (string * int) list

val opt_terminate_on_error : omake_options -> bool
val opt_poll : omake_options -> bool
val opt_poll_on_done : omake_options -> bool
val opt_osh : omake_options -> bool
val set_osh_opt : omake_options -> omake_options

val opt_dry_run : omake_options -> bool
val opt_print_command : omake_options -> eval_flag
val opt_print_dir : omake_options -> bool
val opt_print_file : omake_options -> bool
val opt_print_status : omake_options -> bool
val opt_print_exit : omake_options -> bool
val opt_print_progress : omake_options -> bool
val opt_touch_only : omake_options -> bool
val opt_flush_cache : omake_options -> bool
val opt_flush_dependencies : omake_options -> bool
val opt_print_dependencies : omake_options -> bool
val opt_show_dependencies : omake_options -> string list
val opt_all_dependencies : omake_options -> bool
val opt_verbose_dependencies : omake_options -> bool
val opt_cd_root : omake_options -> bool
val opt_project : omake_options -> bool
val opt_flush_include : omake_options -> bool
val opt_flush_static : omake_options -> bool
val opt_verbose : omake_options -> bool

val opt_absname : omake_options -> bool
val set_absname_opt : omake_options -> bool -> omake_options

val opt_divert : omake_options -> bool (* true when some --output-* diversions other than --output-normal are enabled *)
val opt_output : omake_options -> output_flag -> bool

val opt_allow_exceptions : omake_options -> bool
val set_allow_exceptions_opt : omake_options -> bool -> omake_options

val opt_warn_declare : omake_options -> bool
val opt_warn_error : omake_options -> bool

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
