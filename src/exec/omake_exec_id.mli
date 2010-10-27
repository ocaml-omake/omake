(*
 * Job identifiers.
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

open Omake_marshal

(*
 * Type of job identifiers.
 *)
type id

(*
 * Tables by id.
 *)
module IdTable : Lm_map_sig.LmMap with type key = id

(*
 * "Null" id for the "master" process.
 *)
val null_id : id

(*
 * Get a new id.
 *)
val create : unit -> id

(*
 * Print it.
 *)
val pp_print_pid : formatter -> id -> unit

(*
 * Marshaling.
 *)
val marshal_id : id -> msg
val unmarshal_id : msg -> id

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
