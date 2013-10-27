(*
 * A trace is like a nested list.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001-2005 Mojave Group, Caltech
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

type 'a trace = 'a trace_node list

and 'a trace_node =
   Elem of 'a
 | Lm_trace of 'a * 'a trace

(*
 * Convert to a list.
 *)
val to_list : 'a trace -> 'a list
val of_list : 'a list -> 'a trace

(*
 * Usual map, fold functions.
 *)
val map : ('a -> 'b) -> 'a trace -> 'b trace
val map_depth : (int -> 'a -> 'b) -> 'a trace -> 'b trace
val fold : ('a -> 'b -> 'a) -> 'a -> 'b trace -> 'a
val iter : ('a -> unit) -> 'a trace -> unit
val iter_depth : (int -> 'a -> unit) -> 'a trace -> unit
val fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b trace -> 'a * 'c trace
val header_nodes : 'a trace -> 'a list
val special_nodes : 'a trace -> 'a list

(*
 * Printing.
 *)
val pp_print : formatter -> (formatter -> 'a -> unit) -> 'a trace -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
