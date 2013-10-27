(*
 * DAG used for cycle detection.
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

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The vertices na the DAG represent the DAG.
 * Nodes can be added in an equaivalence class.
 * The out-edges are mutable, so that they can be cached.
 *)
type ('node, 'edge) data =
   Leaf
 | Node of 'edge * ('node, 'edge) vertex list

and ('node, 'edge) vertex =
   { mutable vert_mark : int;
     mutable vert_nodes : 'node list;
     mutable vert_data : ('node, 'edge) data
   }

(*
 * A DAG is a hashtablw to look up the nodes,
 * and a mark count.
 *)
type ('node, 'edge) t =
   { mutable dag_mark : int;
     dag_table : ('node, ('node, 'edge) vertex) Hashtbl.t
   }

exception Cycle

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty DAG.
 *)
let create () =
   { dag_mark = 0;
     dag_table = Hashtbl.create 19
   }

(*
 * Add leaves to the table.
 *)
let insert_leaf table node =
   let vert =
      { vert_mark = 0;
        vert_nodes = [node];
        vert_data = Leaf
      }
   in
      Hashtbl.add table node vert;
      vert

let find_or_insert table node =
   try Hashtbl.find table node with
      Not_found ->
         insert_leaf table node

let find { dag_table = table } node =
   match Hashtbl.find table node with
      { vert_data = Node (edge, _) } ->
         edge
    | { vert_data = Leaf } ->
         raise Not_found

(*
 * Make a dag from previous info.
 * We assume the info has no cycles.
 *)
let make items =
   let table = Hashtbl.create 19 in
   let insert (nodes, edge) =
      let vert =
         match edge with
            Some (edge, children) ->
               { vert_mark = 0;
                 vert_nodes = nodes;
                 vert_data = Node (edge, List.map (find_or_insert table) children)
               }
          | None ->
               { vert_mark = 0;
                 vert_nodes = nodes;
                 vert_data = Leaf
               }
      in
         List.iter (fun node -> Hashtbl.add table node vert) nodes
   in
      List.iter insert items;
      { dag_mark = 0; dag_table = table }

(*
 * Check for path from the first node to the second.
 * Depth-first-search.
 *)
let rec check_cycle_vert mark node1 vert =
   if vert.vert_mark <> mark then
      begin
         vert.vert_mark <- mark;
         if List.mem node1 vert.vert_nodes then
            raise Cycle;
         match vert.vert_data with
            Node (_, children) ->
               List.iter (check_cycle_vert mark node1) children
          | Leaf ->
               ()
      end

let check_cycle_node mark table node1 node2 =
   try
      let vert2 = Hashtbl.find table node2 in
         check_cycle_vert mark node1 vert2;
         vert2
   with
      Not_found ->
         insert_leaf table node2

let check_cycle dag node1 nodes2 =
   let { dag_mark = mark; dag_table = table } = dag in
   let mark = succ mark in
      dag.dag_mark <- mark;
      List.map (check_cycle_node mark table node1) nodes2

(*
 * Leaf two nodes.  One of the two nodes should not
 * already be in the DAG.
 *)
let equate dag node1 node2 =
   let dag = dag.dag_table in
      try
         let vert1 = Hashtbl.find dag node1 in
            if Hashtbl.mem dag node2 then
               raise Cycle
            else
               vert1.vert_nodes <- node2 :: vert1.vert_nodes
      with
         Not_found ->
            try
               let vert2 = Hashtbl.find dag node2 in
                  vert2.vert_nodes <- node1 :: vert2.vert_nodes
            with
               Not_found ->
                  let vert =
                     { vert_mark = 0;
                       vert_nodes = [node1; node2];
                       vert_data = Leaf
                     }
                  in
                     Hashtbl.add dag node1 vert;
                     Hashtbl.add dag node2 vert

(*
 * Insert an edge between node1 and node2.
 *)
let insert dag node1 edge nodes2 =
   let table = dag.dag_table in
      if List.mem node1 nodes2 then
         raise Cycle;
      try
         let vert1 = Hashtbl.find table node1 in
         let verts2 =
            match vert1.vert_data with
               Leaf ->
                  check_cycle dag node1 nodes2
             | Node _ ->
                  raise Cycle
         in
            vert1.vert_data <- Node (edge, verts2)
      with
         Not_found ->
            let verts2 = List.map (find_or_insert table) nodes2 in
            let vert =
               { vert_mark = 0;
                 vert_nodes = [node1];
                 vert_data = Node (edge, verts2)
               }
            in
               Hashtbl.add dag.dag_table node1 vert

(*
 * Sort the nodes in the DAG.
 * Mark node in a depth-first-search.
 *)
let sort dag =
   let { dag_mark = mark; dag_table = table } = dag in
   let mark = succ mark in
   let root = ref [] in
   let rec explore vert =
      if vert.vert_mark <> mark then
         begin
            vert.vert_mark <- mark;
            begin
               match vert.vert_data with
                  Node (_, children) ->
                     List.iter explore children
                | Leaf ->
                     ()
            end;
            root := vert :: !root;
         end
   in
   let rec convert data = function
      vert :: verts ->
         let v =
            match vert.vert_data with
               Node (edge, _) ->
                  Some edge
             | Leaf ->
                  None
         in
            convert ((vert.vert_nodes, v) :: data) verts
    | [] ->
         List.rev data
   in
      dag.dag_mark <- mark;
      Hashtbl.iter (fun _ vert -> explore vert) table;
      convert [] !root

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
