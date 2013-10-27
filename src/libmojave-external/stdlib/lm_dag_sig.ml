(*
 * Imperative DAGs.
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
 * This describes a relation between two nodes in a DAG.
 *)
type relation =
   NoRelation
 | LessThan
 | Equal
 | GreaterThan

(*
 * This signature is an imperative DAG.
 *)
module type ImpDagSig =
sig
   (* Abstract types *)
   type 'a t
   type 'a node

   (* Constructors *)
   val create : unit -> 'a t
   val insert : 'a t -> 'a -> 'a node
   val delete : 'a t -> 'a node -> unit
   val add_edge : 'a t -> 'a node -> 'a node -> unit
   val equate : 'a t -> 'a node -> 'a node -> unit

   (* Raw equality *)
   val eq : 'a node -> 'a node -> bool

   (* Graph operations *)
   val roots : 'a t -> 'a node list
   val node_value : 'a t -> 'a node -> 'a
   val node_out_edges : 'a t -> 'a node -> 'a node list
   val node_in_edges : 'a t -> 'a node -> 'a node list

   (*
    * Topological sort, from the leaves to the root.
    *)
   val sort : 'a t -> 'a node list

   (*
    * Compute relation between two nodes.
    * Roots are smaller.
    *)
   val node_rel : 'a t -> 'a node -> 'a node -> relation

   (*
    * Mappers.
    * Sweep_up maps a function up the DAG, calling on the leaves first;
    *    it returns a list of the roots in the DAG
    * Sweep_down maps a function down the DAG, calling on the roots first;
    *    it returns a list of the leaves in the DAG.
    * The _all versions are the same, except they return a list of all the nodes
    *    that were swept.
    *)
   val sweep_up : 'a t -> ('a -> 'b list -> 'b) -> 'b list
   val sweep_down : 'a t -> ('a -> 'b list -> 'b) -> 'b list
   val sweep_up_all : 'a t -> ('a -> 'b list -> 'b) -> 'b list
   val sweep_down_all : 'a t -> ('a -> 'b list -> 'b) -> 'b list
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
