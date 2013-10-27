(*
 * Imperative DAGS.  We implement the DAG as a vector.
 * Each entry in the vector contains the value for a node,
 * and all the outgoing edges, represented by their destination
 * index.
 *
 * This is a space-saving dag.  Only the real edges
 * and the quiried edges are actually saved.
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

open Lm_debug
open Lm_dag_sig

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Imp_dag%t"

module ImpDag =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * An entry in the vector contains some value, and the edges
    * in the graph.  When a query is made.
    *    1. if the version number
    *       is up-to-date, and the edge is in unrelated, then
    *       NoRelation is returned.  If the edge is in entry_out_edges,
    *       the LessThan is returned.  Else, a search is made, and a new edge
    *       either added to out_edges or unrelated.
    *    2. If the version number is out of date, then the
    *       unrelated edges are cleared.
    *
    * Invariant: edge lists are sorted
    *
    *)
   type 'a entry =
      { mutable entry_value : 'a;
        mutable entry_in_edges : int list;
        mutable entry_out_edges : int list;
        mutable entry_no_relation : int list;
        mutable entry_version : int
      }

   (*
    * A DAG is a vector of entries.
    * We add a level of indirection to allow for equating nodes.
    *)
   type 'a t =
      { mutable entries : ('a entry) array;
        mutable version : int
      }

   type 'a node = int

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Raw equality.
    *)
   let eq (node1 : 'a node) (node2 : 'a node) =
      node1 == node2

   (*
    * Insert a value into a sorted list.
    *)
   let rec list_insert i l =
      match l with
         h :: t ->
            if h < i then
               h :: (list_insert i t)
            else if h > i then
               i :: l
            else
               l
       | [] ->
            [i]

   (*
    * Remove a particular entry from the list, and
    * decrement all larger indices.
    *)
   let rec list_delete i = function
      h :: t ->
         if h < i then
            h :: list_delete i t
         else if h = i then
            list_delete i t
         else
            h - 1 :: list_delete i t
    | [] ->
         []

   (*
    * Construct a new DAG.
    *)
   let create () =
      { entries = [||]; version = 0 }

   (*
    * Add a new node unrelated to the rest of the DAG.
    *)
   let insert dag v =
      let { entries = entries; version = version } = dag in
      let length = Array.length entries in
      let entry =
         { entry_value = v;
           entry_in_edges = [];
           entry_out_edges = [];
           entry_no_relation = [];
           entry_version = version;
         }
      in
      let newentries = Array.create (length + 1) entry in
         Array.blit entries 0 newentries 0 length;
         dag.entries <- newentries;
         length

   (*
    * Delete a node by updating all indices.
    *)
   let delete dag node =
      let { entries = entries; version = version } = dag in
      let length = Array.length entries in
      let newentries = Array.create (length - 1) entries.(0) in
      let delete entry =
         entry.entry_in_edges <- list_delete node entry.entry_in_edges;
         entry.entry_out_edges <- list_delete node entry.entry_out_edges
      in
         Array.blit entries 0 newentries 0 node;
         Array.blit entries (node + 1) newentries node (length - node - 1);
         Array.iter delete entries;
         dag.version <- version + 1

   (*
    * Add an edge from node1 to node2.
    *)
   let add_edge dag node1 node2 =
      let { entries = entries; version = version } = dag in
      let entry1 = entries.(node1) in
      let entry2 = entries.(node2) in
         entry1.entry_out_edges <- list_insert node2 entry1.entry_out_edges;
         entry2.entry_in_edges <- list_insert node1 entry2.entry_in_edges;
         dag.version <- version + 1

   (*
    * Two nodes are made equal by creating a cycle.
    *)
   let equate dag node1 node2 =
      add_edge dag node1 node2;
      add_edge dag node2 node1

   (*
    * Projections.
    *)
   let node_value { entries = entries } i =
      entries.(i).entry_value

   let node_out_edges { entries = entries } i =
      entries.(i).entry_out_edges

   let node_in_edges { entries = entries } i =
      entries.(i).entry_in_edges

   (*
    * Sweep a function up the DAG, calling on the children first.
    * The function is called only once on each node, so we keep a vector
    * of cached values.  The oproj and iproj are the projection
    * functions for entry.entry_out_edges and entry.entry_in_edges, or
    * vice versa.
    *)
   let sweep_aux entries f oproj iproj =
      let len = Array.length entries in
      let values = Array.create len None in
      let rec expand i =
         match values.(i) with
            None ->
               let entry = entries.(i) in
               let children = List.map expand (oproj entry) in
               let result = f entry.entry_value children in
                  values.(i) <- Some result;
                  result

          | Some result ->
               result
      in
      let rec aux i =
         if i < len then
            let entry = entries.(i) in
            let result = expand i in
               if (iproj entry) = [] then
                  result::(aux (i + 1))
               else
                  aux (i + 1)
         else
            []
      in
         aux 0

   let out_edges_f { entry_out_edges = edges } = edges

   let in_edges_f { entry_in_edges = edges } = edges

   let some_edges_f { entry_in_edges = _ } = []

   let sweep_up { entries = entries } f =
      sweep_aux entries f out_edges_f in_edges_f

   let sweep_down { entries = entries } f =
      sweep_aux entries f in_edges_f out_edges_f

   let sweep_up_all { entries = entries } f =
      sweep_aux entries f out_edges_f some_edges_f

   let sweep_down_all { entries = entries } f =
      sweep_aux entries f in_edges_f some_edges_f

   (*
    * Get the roots of the DAG.
    * These nodes have no in_edges.
    *)
   let roots { entries = entries } =
      let length = Array.length entries in
      let rec collect i =
         if i = length then
            []
         else
            let entry = entries.(i) in
               if entry.entry_in_edges = [] then
                  i :: collect (i + 1)
               else
                  collect (i + 1)
      in
         collect 0

   (*
    * Sort the list from the roots.
    *)
   let sort info =
      let entries = info.entries in
      let length = Array.length entries in
      let found = Array.create length false in
      let roots = roots info in
      let rec collect l i =
         if found.(i) then
            l
         else
            let entry = entries.(i) in
            let next = entry.entry_out_edges in
               found.(i) <- true;
               List.fold_left collect (i :: l) next
      in
         List.fold_left collect [] roots

   (*
    * Expand to all the reachable nodes.
    *)
   let search select entries node1 node2 =
      let rec loop searched front =
         match front with
            node :: t ->
               if node = node2 then
                  true
               else if List.mem node searched then
                  loop searched t
               else
                  let edges = select entries.(node) in
                     loop (node :: searched) (Sort.merge (<) edges t)
          | [] ->
               false
      in
         loop [] [node1]

   (*
    * Find the relation and cache it.
    * If version is out-of-date, delete the no_relation.
    * If edge is found, then return it, else search.
    *)
   let node_rel dag node1 node2 =
      let { entries = entries; version = version } = dag in
      let entry1 = entries.(node1) in
      let { entry_in_edges = in_edges;
            entry_out_edges = out_edges;
            entry_no_relation = no_relation;
            entry_version = version'
          } = entry1
      in
      let no_relation =
         if version' = version then
            no_relation
         else
            begin
               entry1.entry_no_relation <- [];
               entry1.entry_version <- version;
               []
            end
      in
         (* First check for cached edges *)
         if List.mem node2 no_relation then
            NoRelation
         else if List.mem node2 out_edges then
            if List.mem node2 in_edges then
               Equal
            else
               LessThan
         else if List.mem node2 in_edges then
            GreaterThan

         (* If not in cache, then do a search *)
         else if search out_edges_f entries node1 node2 then
            if search in_edges_f entries node1 node2 then
               begin
                  entry1.entry_out_edges <- list_insert node2 out_edges;
                  entry1.entry_in_edges <- list_insert node2 in_edges;
                  Equal
               end
            else
               begin
                  entry1.entry_out_edges <- list_insert node2 out_edges;
                  LessThan
               end
         else if search in_edges_f entries node1 node2 then
            begin
               entry1.entry_in_edges <- list_insert node2 in_edges;
               GreaterThan
            end
         else
            begin
               entry1.entry_no_relation <- list_insert node2 no_relation;
               NoRelation
            end
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
