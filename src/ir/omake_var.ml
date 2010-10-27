(*
 * Virtual identifiers for various things in files.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2007 Mojave Group, California Institute of Technology, and
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
open Lm_hash
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_ir
open Omake_node
open Omake_util
open Omake_state
open Omake_symbol
open Omake_print_util

(*
 * These are all builtin variables.
 *
 * ZZZ: in 0.9.8 they are VarGlobal.
 *)
let loc = bogus_loc "Builtin"

let create_pervasives_var v =
   VarGlobal (loc, v)

(*
 * Generally useful virtual variables.
 *)
let argv_var                   = create_pervasives_var argv_sym
let options_var                = create_pervasives_var options_object_sym

let explicit_target_var        = create_pervasives_var explicit_target_sym
let cwd_var                    = create_pervasives_var cwd_sym
let stdlib_var                 = create_pervasives_var stdlib_sym
let stdroot_var                = create_pervasives_var stdroot_sym
let ostype_var                 = create_pervasives_var ostype_sym
let omakepath_var              = create_pervasives_var omakepath_sym
let path_var                   = create_pervasives_var path_sym
let auto_rehash_var            = create_pervasives_var auto_rehash_sym
let printexitvalue_var         = create_pervasives_var printexitvalue_sym
let system_var                 = create_pervasives_var system_sym
let oshell_var                 = create_pervasives_var oshell_sym
let cdpath_var                 = create_pervasives_var cdpath_sym
let history_file_var           = create_pervasives_var history_file_sym
let history_length_var         = create_pervasives_var history_length_sym
let targets_var                = create_pervasives_var targets_sym
let build_summary_var          = create_pervasives_var build_summary_sym

let prompt_var                 = create_pervasives_var prompt_sym
let ignoreeof_var              = create_pervasives_var ignoreeof_sym
let exit_on_uncaught_exception_var = create_pervasives_var exit_on_uncaught_exception_sym
let abort_on_command_error_var = create_pervasives_var abort_on_command_error_sym
let create_subdirs_var         = create_pervasives_var create_subdirs_sym
let allow_empty_subdirs_var    = create_pervasives_var allow_empty_subdirs_sym
let glob_options_var           = create_pervasives_var glob_options_sym
let glob_ignore_var            = create_pervasives_var glob_ignore_sym
let glob_allow_var             = create_pervasives_var glob_allow_sym
let scanner_mode_var           = create_pervasives_var scanner_mode_sym

let stdin_var                  = create_pervasives_var stdin_sym
let stdout_var                 = create_pervasives_var stdout_sym
let stderr_var                 = create_pervasives_var stderr_sym

let star_var                   = create_pervasives_var star_sym
let at_var                     = create_pervasives_var at_sym
let gt_var                     = create_pervasives_var gt_sym
let plus_var                   = create_pervasives_var plus_sym
let hat_var                    = create_pervasives_var hat_sym
let lt_var                     = create_pervasives_var lt_sym
let amp_var                    = create_pervasives_var amp_sym
let braces_var                 = create_pervasives_var braces_sym
let fs_var                     = create_pervasives_var fs_sym
let rs_var                     = create_pervasives_var rs_sym
let filename_var               = create_pervasives_var filename_sym
let fnr_var                    = create_pervasives_var fnr_sym

let parse_loc_var              = create_pervasives_var parse_loc_sym
let zero_var                   = create_pervasives_var zero_sym
let nf_var                     = create_pervasives_var nf_sym

let object_var                 = create_pervasives_var object_sym
let int_object_var             = create_pervasives_var int_object_sym
let float_object_var           = create_pervasives_var float_object_sym
let string_object_var          = create_pervasives_var string_object_sym
let sequence_object_var        = create_pervasives_var sequence_object_sym
let array_object_var           = create_pervasives_var array_object_sym
let fun_object_var             = create_pervasives_var fun_object_sym
let rule_object_var            = create_pervasives_var rule_object_sym
let file_object_var            = create_pervasives_var file_object_sym
let dir_object_var             = create_pervasives_var dir_object_sym
let body_object_var            = create_pervasives_var body_object_sym
let in_channel_object_var      = create_pervasives_var in_channel_object_sym
let out_channel_object_var     = create_pervasives_var out_channel_object_sym
let in_out_channel_object_var  = create_pervasives_var in_out_channel_object_sym
let lexer_object_var           = create_pervasives_var lexer_object_sym
let parser_object_var          = create_pervasives_var parser_object_sym
let location_object_var        = create_pervasives_var location_object_sym
let map_object_var             = create_pervasives_var map_object_sym
let shell_object_var           = create_pervasives_var shell_object_sym
let target_object_var          = create_pervasives_var target_object_sym
let stat_object_var            = create_pervasives_var stat_object_sym
let passwd_object_var          = create_pervasives_var passwd_object_sym
let group_object_var           = create_pervasives_var group_object_sym
let pipe_object_var            = create_pervasives_var pipe_object_sym
let select_object_var          = create_pervasives_var pipe_object_sym
let runtime_exception_var      = create_pervasives_var runtime_exception_sym
let var_object_var             = create_pervasives_var var_object_sym
let tm_object_var              = create_pervasives_var tm_object_sym

let extends_var                = create_pervasives_var extends_sym
let omakeflags_var             = create_pervasives_var omakeflags_sym
let omakeargv_var              = create_pervasives_var omakeargv_sym

let printexitvalue_var         = create_pervasives_var printexitvalue_sym

let loc_field_var              = VarThis (loc, loc_sym)
let builtin_field_var          = VarThis (loc, builtin_sym)
let map_field_var              = VarThis (loc, map_sym)
let current_prec_field_var     = VarThis (loc, current_prec_sym)
let lexer_field_var            = VarThis (loc, lexer_sym)

let file_var                   = VarPrivate (loc, file_sym)
let file_id_var                = VarPrivate (loc, file_id_sym)
let wild_var                   = VarPrivate (loc, wild_sym)

(*
 * Special handling for small numeric vars.
 *)
let create_numeric_var =
   let numeric_vars = ref [||] in
   let resize i =
      let size = (i + 1) * 2 in
      let table = Array.create size wild_var in
         for j = 0 to pred size do
            table.(j) <- create_pervasives_var (Lm_symbol.add (string_of_int j))
         done;
         numeric_vars := table;
         table
   in
   let get i =
      let table = !numeric_vars in
         if i < Array.length table then
            table.(i)
         else
            (resize i).(i)
   in
      get

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
