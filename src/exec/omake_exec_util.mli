(*
 * Utilities for process execution on any platform.
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
open Lm_printf
open Lm_map_sig

open Omake_node
open Omake_exec_id
open Omake_cache_type

(*
 * Debugging.
 *)
val debug_exec : bool ref

(*
 * File descriptor table.
 *)
module IntTable : LmMap with type key = int;;
module FdTable : LmMap with type key = Unix.file_descr;;

(*
 * Open a pipe.  Close automatically on exceptions.
 *)
val with_pipe : (Unix.file_descr -> Unix.file_descr -> 'a) -> 'a

(*
 * Copy data to standard channels.
 *)
val copy_stdout : id -> string -> int -> int -> unit
val copy_stderr : id -> string -> int -> int -> unit

(*
 * Copy data to a file.
 *)
val copy_file : string -> (id -> string -> int -> int -> unit)

(*
 * Tee to a file.
 *)
type tee

val tee_none        : tee
val tee_create      : bool -> tee
val tee_close       : tee -> unit
val tee_file        : tee -> string option
val tee_stdout      : tee -> bool -> id -> string -> int -> int -> unit
val tee_stderr      : tee -> bool -> id -> string -> int -> int -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
