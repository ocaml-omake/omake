(*
 * Lm_position informat for debugging.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002-2005 Mojave Group, Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol
open Lm_location
open Lm_printf

(*
 * Lm_debug flags.
 *)
val debug_pos : bool ref
val trace_pos : bool ref

(*
 * Lm_position information.
 *)
type 'a pos

(*
 * Module for creating positions.
 * You have to specify the name of the module
 * where the exception are being created: use
 * MakePos in each file where Name.name is set
 * to the name of the module.
 *)
module type PosSig =
sig
   type t

   (* Creating positions *)
   val loc_exp_pos : loc -> t pos
   val loc_pos     : loc -> t pos -> t pos
   val base_pos    : t -> t pos
   val cons_pos    : t -> t pos -> t pos
   val pos_pos     : t pos -> t pos -> t pos
   val int_pos     : int -> t pos -> t pos
   val string_pos  : string -> t pos -> t pos
   val symbol_pos  : symbol -> t pos -> t pos
   val del_pos     : (out_channel -> unit) -> loc -> t pos
   val del_exp_pos : (out_channel -> unit) -> t pos -> t pos

   (* Utilities *)
   val loc_of_pos  : t pos -> loc
   val pp_print_pos  : formatter -> t pos -> unit
end

module type NameSig =
sig
   type t

   (* This is the name of the module where the position info is created *)
   val name : string

   (* Utilities for managing values *)
   val loc_of_value : t -> loc
   val pp_print_value : formatter -> t -> unit
end

module MakePos (Name : NameSig) : PosSig with type t = Name.t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
