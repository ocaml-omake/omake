(*
 * Add builtin functions to environment.
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

open Omake_env
open Omake_exec
open Omake_build_type
open Omake_builtin_type

(*
 * Register some builtin info.
 *)
val register_builtin : builtin_info -> unit

(*
 * Add a command line variable definition.
 *)
val add_command_def : string -> string -> unit

(*
 * Check if there are command defs.
 *)
val command_defs_are_nonempty : unit -> bool

(*
 * Add all the command-line defs to the encironment.
 *)
val venv_add_command_defs : venv -> venv

(*
 * Builtin functions.
 *)
val venv_add_builtins : venv -> venv
val venv_add_pervasives : venv -> venv
val venv_include_rc_file : venv -> string -> venv

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
