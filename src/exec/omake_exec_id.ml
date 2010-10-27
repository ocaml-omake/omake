(*
 * An identifier server.
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

open Lm_map

open Fmarshal

open Omake_marshal

(*
 * A job identifier is just an integer.
 *)
type id = int

(*
 * Table based on file descriptor.
 *)
module IdCompare =
struct
   type t = id

   let compare = (-)
end

module IdTable = Lm_map.LmMake (IdCompare)

(*
 * Print an id.
 *)
let pp_print_pid = pp_print_int

(*
 * Id allocation.
 *)
let null_id = 0
 
let index = ref 1

let create () =
   let id = !index in
      index := succ id;
      id

(*
 * Marshaling.
 *)
let marshal_id id =
   List [Magic IdMagic; Int id]

let unmarshal_id l =
   match l with
      List [Magic IdMagic; Int id] ->
         id
    | _ ->
         raise MarshalError

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
