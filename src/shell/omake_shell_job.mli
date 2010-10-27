(*
 * Shell execution.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Lm_glob
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_env
open Omake_pos
open Omake_shell_type
open Omake_value_type

(*
 * Create a thread or process running the function.
 *)
val create_thread : venv -> (Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int) ->
   Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> pid

(*
 * Start a job given a pipe specification.
 *)
val create_job : venv -> string_pipe -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> venv * value

(*
 * Create a process in the background.
 *)
val create_process : venv -> string_pipe -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> pid
val waitpid : venv -> pos -> pid -> int * Unix.process_status * value

(*
 * Shell operations.
 *)
val jobs    : venv -> unit
val bg      : venv -> pos -> int -> unit
val fg      : venv -> pos -> int -> unit
val stop    : venv -> pos -> int -> unit
val kill    : venv -> pos -> int -> signal -> unit
val wait    : venv -> pos -> int -> unit
val cleanup : venv -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
