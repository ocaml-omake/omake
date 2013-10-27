(*
 * Source file locations.
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
open Lm_printf
open Lm_symbol

type loc

(*
 * Comparison.
 *)
val compare : loc -> loc -> int

(*
 * Don't use this if you can avoid it.
 *)
val bogus_loc : string -> loc

(*
 * This is the normal way to make a location.
 *    filename, start_line, start_char, end_line, end_char
 *)
val create_loc : symbol -> int -> int -> int -> int -> loc

(*
 * For marshaling.
 *)
val dest_loc : loc -> symbol * int * int * int * int

(*
 * Combine two locations.
 * The resulting span covers both.
 *)
val union_loc : loc -> loc -> loc

(*
 * Print a file location.
 *)
val pp_print_location : out_channel -> loc -> unit
val string_of_location : loc -> string

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
