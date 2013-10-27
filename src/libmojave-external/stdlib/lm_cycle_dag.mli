(*
 * This is a DAG used for cycle detection.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
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
 * jyh@cs.cornell.edu
 *)

(*
 * Exception for reporting cycles.
 *)
exception Cycle

(*
 * Nodes are labelled with 'node,
 * and edges have label 'edge.
 *
 * 'node should be an equality type.
 *)
type ('node, 'edge) t

(*
 * Create an empty DAG.
 *)
val create : unit -> ('node, 'edge) t

(*
 * Make it from a previous subst.
 *)
val make : ('node list * ('edge * 'node list) option) list -> ('node, 'edge) t

(*
 * Find an edge.
 *)
val find : ('node, 'edge) t -> 'node -> 'edge

(*
 * Equate two nodes.
 * This raises Cycle if the two nodes are already
 * related by an edge.
 *)
val equate : ('node, 'edge) t -> 'node -> 'node -> unit

(*
 * Insert an edge-list from the source
 * to a list of sinks, with the given label.
 * This may raise the Cycle exception.
 *)
val insert : ('node, 'edge) t -> 'node -> 'edge -> 'node list -> unit

(*
 * Sort the edges, and return them in a list.
 *)
val sort : ('node, 'edge) t -> ('node list * 'edge option) list

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
