(*
 * Locale functions.  Like in <ctype.h>
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2005 Mojave Group, Caltech
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

(************************************************************************
 * Locale functions
 *)
external set_locale: unit -> unit = "set_locale"
external is_print: char -> bool = "is_print"
external is_digit: char -> bool = "is_digit"
external is_alnum: char -> bool = "is_alnum"
external is_upper: char -> bool = "is_upper"
external is_graph: char -> bool = "is_graph"

let _ = set_locale ()

let is_capitalized s = is_upper s.[0]

(*
 * Functions to quote and unquote strings.
 *)
let rec is_simple l i s =
   if i = l then
      true
   else
      match String.unsafe_get s i with
         '"' | '\\' | '\r' | '\n' | '\t' | ' ' ->
           false
       | c ->
           is_print c && is_simple l (succ i) s

let quote s =
   if s <> "" && is_simple (String.length s) 0 s then
      s
   else
      "\"" ^ String.escaped s ^ "\""

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
