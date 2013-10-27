(*
 * This module implements dominator calculations and
 * loop-nest trees.
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
open Lm_symbol
open Lm_trace

(*
 * State type.
 *)
type 'a t

(*
 * Lm_debug flag.
 *)
val debug_loop : bool ref

(*
 * Printing.
 *)
val pp_print_trace : formatter -> ('a -> symbol) -> 'a trace -> unit

(*
 * Calculate the loop nest tree.
 * The arguments are:
 *    node_name : 'a -> symbol
 *    node_succ : 'a -> symbol list
 *    root : 'a
 *    nodes : 'a list
 *
 * You have to provide the root node for the graph.
 * Typically, this means you should create a trivial
 * node that has edges to all loop header nodes.
 *)
val create : string -> ('a -> symbol) -> ('a -> symbol list) -> 'a -> 'a list -> 'a t
val loop_nest : 'a t -> ('a -> symbol) -> 'a trace
val dominators : 'a t -> ('a -> symbol) -> symbol SymbolTable.t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
