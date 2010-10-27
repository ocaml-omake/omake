(*
 * Status printing.
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf

open Omake_node
open Omake_exec_id
open Omake_exec_util
open Omake_exec_type
open Omake_cache_type
open Omake_options

(*
 * Flush the progress meter.
 *)
val progress_flush : unit -> unit
val progress_flushed : unit -> bool

(*
 * Print a progress indicator.
 *)
val print_progress : omake_options -> int -> int -> unit

(*
 * Saving the cache messages.
 *)
val print_saving   : omake_options -> unit

(*
 * Directory changes.
 *)
val print_entering_current_directory : omake_options -> Dir.t -> unit
val print_leaving_current_directory  : omake_options -> unit

(*
 * Print a status line.
 *)
val print_status :
   (string -> int -> int -> unit) ->    (* Diversion *)
   omake_options ->                     (* Options currently in effect *)
   ('exp, 'pid, 'value) shell ->        (* The context *)
   string option ->                     (* Remote host name *)
   string ->                            (* Name of operation being performed *)
   ('exp, 'pid, 'value) print_flag ->   (* What to print *)
   unit

(*
 * Print a status lines.
 *)
val pp_status_lines :
   formatter ->                         (* Output channel *)
   omake_options ->                     (* Options currently in effect *)
   ('exp, 'pid, 'value) shell ->        (* The current shell *)
   string ->                            (* Name of operation being performed *)
   'exp list ->                         (* What to print *)
   unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
