(*
 * Variables.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2007 Mojave Group, California Institute of Technology, and
 * HRL Laboratories, LLC
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
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol

open Omake_ir

(*
 * Generally useful variables in pervasives.
 *)
val explicit_target_var        : var_info
val wild_var                   : var_info
val cwd_var                    : var_info
val stdlib_var                 : var_info
val stdroot_var                : var_info
val ostype_var                 : var_info
val omakepath_var              : var_info
val path_var                   : var_info
val auto_rehash_var            : var_info
val printexitvalue_var         : var_info
val system_var                 : var_info
val oshell_var                 : var_info
val cdpath_var                 : var_info
val history_file_var           : var_info
val history_length_var         : var_info
val targets_var                : var_info
val build_summary_var          : var_info

val prompt_var                 : var_info
val ignoreeof_var              : var_info
val exit_on_uncaught_exception_var : var_info
val abort_on_command_error_var : var_info
val create_subdirs_var         : var_info
val allow_empty_subdirs_var    : var_info
val glob_options_var           : var_info
val glob_ignore_var            : var_info
val glob_allow_var             : var_info
val scanner_mode_var           : var_info

val stdin_var                  : var_info
val stdout_var                 : var_info
val stderr_var                 : var_info

val argv_var                   : var_info
val options_var                : var_info

val star_var                   : var_info
val at_var                     : var_info
val gt_var                     : var_info
val plus_var                   : var_info
val hat_var                    : var_info
val lt_var                     : var_info
val amp_var                    : var_info
val braces_var                 : var_info

val parse_loc_var              : var_info
val zero_var                   : var_info
val nf_var                     : var_info
val fs_var                     : var_info
val rs_var                     : var_info
val filename_var               : var_info
val fnr_var                    : var_info

val object_var                 : var_info
val int_object_var             : var_info
val float_object_var           : var_info
val string_object_var          : var_info
val sequence_object_var        : var_info
val array_object_var           : var_info
val fun_object_var             : var_info
val rule_object_var            : var_info
val file_object_var            : var_info
val dir_object_var             : var_info
val body_object_var            : var_info
val in_channel_object_var      : var_info
val out_channel_object_var     : var_info
val in_out_channel_object_var  : var_info
val lexer_object_var           : var_info
val parser_object_var          : var_info
val location_object_var        : var_info
val map_object_var             : var_info
val shell_object_var           : var_info
val target_object_var          : var_info
val stat_object_var            : var_info
val passwd_object_var          : var_info
val group_object_var           : var_info
val pipe_object_var            : var_info
val select_object_var          : var_info
val runtime_exception_var      : var_info
val var_object_var             : var_info
val tm_object_var              : var_info

val printexitvalue_var         : var_info

val extends_var                : var_info
val omakeflags_var             : var_info
val omakeargv_var              : var_info

(*
 * Internal fields.
 *)
val loc_field_var              : var_info
val builtin_field_var          : var_info
val map_field_var              : var_info
val current_prec_field_var     : var_info
val lexer_field_var            : var_info
val file_var                   : var_info
val file_id_var                : var_info

(*
 * $0, $1, $2...
 *)
val create_numeric_var         : int -> var_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
