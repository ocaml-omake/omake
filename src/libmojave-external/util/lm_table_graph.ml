(*
 * This implementation of a graph is based on functional
 * tables.  The graph is a table of nodes, where each node
 * has a list of its immediate neighbors.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999-2005 Mojave Group, Caltech
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
 * jyh@cs.caltech.edu
 *)

open Lm_debug
open Set
open Graph_sig

(*
 * Array functor.
 *)
module type NodeArrayArgSig =
sig
   type graph
   type dummy

   module Table : Lm_map.LmMap with type key = int

   val dest_graph : graph -> int * dummy Table.t
end

module MakeNodeArray (Arg : NodeArrayArgSig) =
struct
   open Arg

   type elt = int
   type graph = Arg.graph
   type 'a t = 'a option array

   (* Create the array *)
   let create graph x =
      let next, table = Arg.dest_graph graph in
      let a = Array.create next None in
      let _ =
         Table.iter (fun i _ ->
               a.(i) <- Some x) table
      in
         a

   (* Create with initializer *)
   let init graph f =
      let next, table = Arg.dest_graph graph in
      let a = Array.create next None in
      let _ =
         Table.iter (fun i _ ->
               a.(i) <- Some (f i)) table
      in
         a

   (* Get an element *)
   let get a x =
      match a.(x) with
         Some v -> v
       | None -> raise Not_found

   (* Set an element *)
   let set a x y = a.(x) <- Some y

   (* Iterate *)
   let iter f a =
      Array.iteri (fun node x ->
            match x with
               Some x -> f node x
             | None -> ()) a

   (* Map *)
   let map f a =
      Array.mapi (fun node x ->
            match x with
               Some x -> Some (f node x)
             | None -> None) a

   (* Fold *)
   let fold f b a =
      let i = Array.length a in
      let rec collect b j =
         if j = i then
            b
         else
            match a.(j) with
               Some x -> collect (f b j x) (succ j)
             | None -> collect b (succ j)
      in
         collect b 0
end

(*
 * BUG: in these implementations: the nodes are represented by
 * numbers, and node allocation always increments the index.  That means
 * that for graphs with a lot of activity, the node index may overflow.
 *
 * A smarter way would be to use a real free list to manage
 * allocation of nodes.  Even better, the nodes should use weak
 * pointers, so we can reclaim parts of the graph after garbage
 * collection.
 *
 * For the compiler, these implementations should be adequate because
 * the graphs are short-lived.
 *)
module UndirectedGraph =
struct
   (*
    * A node is just a number.
    *)
   type node = int

   module NodeCompare =
   struct
      type t = node
      let compare = Pervasives.compare
   end

   module Set = Lm_set.LmMake (NodeCompare)
   module Table = Lm_map.LmMakeList (NodeCompare)

   (*
    * The graph contains:
    *    1. a Table that maps nodes to lists of neighbors,
    *    2. a list of nodes that are hidden
    *    3. a current allocation index for the next node.
    *)
   type t =
      { table : (node option) Table.t;
        hidden : Set.t;
        next : node
      }

   module ArrayArg =
   struct
      type node = int
      type graph = t
      type dummy = node option
      module Table = Table
      let dest_graph { next = next; table = table } = next, table
   end

   module NodeArray = MakeNodeArray (ArrayArg)

   (*
    * List operations as sets.
    *)
   let list_add x l =
      if List.mem x l then
         l
      else
         x :: l

   let list_union l1 l2 =
      List.fold_left (fun l x -> list_add (Some x) l) l1 l2

   let table_add table node1 node2 =
      Table.filter table node1 (list_add (Some node2))

   (*
    * Create a new, empty graph.
    *)
   let create () =
      { table = Table.empty;
        hidden = Set.empty;
        next = 1
      }

   (*
    * Check that the table is closed.
    * All nodes must exist.
    *)
   let check_table { table = table } =
      Table.iter (fun i j ->
         match j with
            Some(j) ->
               if i = j || not (Table.mem table j) then
                  raise (Invalid_argument (Lm_printf.sprintf "check_closed: %d->%d" i j))
          | None -> ()) table


   (*
    * Nodes are compared as integers.
    *)
   let compare_node n1 n2 =
      n1 - n2

   let eq_node n1 n2 =
      n1 = n2

   (*
    * Add a node.
    *)
   let new_node { table = table; hidden = hidden; next = next } =
      { table = Table.add table next None;
        hidden = hidden;
        next = succ next
      }, next

   (*
    * Find all relevant destinations from a node
    *)
   let find_all table node =
      List.fold_left (fun all node ->
         match node with
            Some(node) -> node :: all
          | None -> all) [] (Table.find_all table node)

   (*
    * Add an edge to the table
    *)
   let add table node1 node2 = Table.add table node1 (Some node2)

   (*
    * When an edge is added, it should be added to the
    * adjacency lists of both nodes.
    *)
   let new_edge ({ table = table } as t) node1 node2 =
      assert(node1 <> node2);
      if List.mem node2 (find_all table node1) then
         t
      else
         let table = add table node1 node2 in
         let table = add table node2 node1 in
            { t with table = table }

   (*
    * Return the list of neighbors of the node.
    *)
   let neighbors { table = table; hidden = hidden } node =
      List.filter (fun node -> not (Set.mem hidden node)) (find_all table node)

   let degree { table = table; hidden = hidden } node =
      let rec count_visible i = function
         node :: nodes ->
            let i = if Set.mem hidden node then i else succ i in
               count_visible i nodes
       | [] ->
            i
      in
         count_visible 0 (find_all table node)

   (*
    * Check if an edge exists.
    *)
   let query { table = table } node1 node2 =
      List.mem node2 (find_all table node1)

   (*
    * When a node is deleted, all the edges to the node
    * must also be deleted.
    *)
   let delete_node { table = table; hidden = hidden; next = next } node =
      let delete_node table key =
         Table.filter table key (Lm_list_util.remove (Some node))
      in
      let neighbors = find_all table node in
      let table = List.fold_left delete_node table neighbors in
      let table = Table.remove table node in
      let table =
         { table = table;
           hidden = Set.remove hidden node;
           next = next
         }
      in
         (* check_table table; *)
         table

   (*
    * Delete a single edge.
    *)
   let delete_edge ({ table = table } as t) node1 node2 =
      let table = Table.filter table node1 (Lm_list_util.remove (Some node2)) in
      let table = Table.filter table node2 (Lm_list_util.remove (Some node1)) in
      let table = { t with table = table } in
         (* check_table table; *)
         table

   (*
    * Map a function over the nodes.
    *)
   let fold f arg { table = table; hidden = hidden } =
      let fold_fun arg node _ =
         if Set.mem hidden node then
            arg
         else
            f arg node
      in
         Table.fold fold_fun arg table

   (*
    * Temporary hiding.
    *)
   let hide_node ({ hidden = hidden } as t) node =
      { t with hidden = Set.add hidden node }

   let hide_nodes ({ hidden = hidden } as t) nodes =
      { t with hidden = Set.subtract_list hidden nodes }

   let restore_node ({ hidden = hidden } as t) node =
      { t with hidden = Set.remove hidden node }

   (*
    * When nodes are coalesced, only the first node remains
    * in the graph, with edges for the union of the edges of
    * both nodes.
    *)
   let coalesce ({ table = table; hidden = hidden } as t) node1 node2 =
      (* Neither node should be hidden *)
      assert(node1 <> node2);
      if Set.mem hidden node1 || Set.mem hidden node2 then
         raise (Invalid_argument "coalesce");

      (* Combine the edge lists *)
      let nodes2 = Lm_list_util.remove node1 (find_all table node2) in
      let table = Table.filter table node1 (fun nodes -> list_union nodes nodes2) in
      let table = List.fold_left (fun table node2 -> table_add table node2 node1) table nodes2 in
      let table = { t with table = table } in
         (* check_table table; *)

         (* Delete the second node *)
         delete_node table node2
end

(*
 * The directed graph is similar to the undirected graph,
 * but we keep two values for each node: the list of predecessors
 * and the list of successors.
 *)
module DirectedGraph =
struct
   (*
    * A node is just a number.
    *)
   type node = int

   module NodeCompare =
   struct
      type t = node
      let compare = Pervasives.compare
   end

   module Set = Lm_set.LmMake (NodeCompare)
   module Table = Lm_map.LmMake (NodeCompare)

   (*
    * The graph is a Table that maps nodes to lists of neighbors,
    * and a current allocation index for the next node.
    *)
   type edges =
      { pred : Set.t;
        succ : Set.t
      }

   type t =
      { table : edges Table.t;
        next : node
      }

   module ArrayArg =
   struct
      type node = int
      type graph = t
      type dummy = edges
      module Table = Table
      let dest_graph { next = next; table = table } = next, table
   end

   (*
    * Array.
    *)
   module NodeArray = MakeNodeArray (ArrayArg)

   (*
    * Create a new, empty graph.
    *)
   let create () =
      { table = Table.empty;
        next = 0
      }

   (*
    * Nodes are compared as integers.
    *)
   let eq_node n1 n2 =
      n1 = n2

   (*
    * Add a node.
    *)
   let new_node { table = table; next = next } =
      let node = next in
      let table = Table.add table node { pred = Set.empty; succ = Set.empty } in
         if not (Table.mem table node)
            then  Lm_printf.eprintf "tried to add node %d, but it's gone missing already!\n" node;
         { table = table; next = succ next }, node

   (*
    * Update the value in the table.
    *)
   let update table node f =
      let x = f (Table.find table node) in
      let table = Table.remove table node in
      let table = Table.add table node x in
         table

   (*
    * When an edge is added, it should be added to the
    * adjacency lists of both nodes.
    *)
   let new_edge { table = table; next = next } node1 node2 =
      let add_succ { pred = pred; succ = succ } =
         { pred = pred; succ = Set.add succ node2 }
      in
      let add_pred { pred = pred; succ = succ } =
         { pred = Set.add pred node1; succ = succ }
      in
      let table = update table node1 add_succ in
      let table = update table node2 add_pred in
         if not (Table.mem table node1)
            then  Lm_printf.eprintf "tried to add %d->%d, but %d node is missing!\n" node1 node2 node1
         else if not (Set.mem (Table.find table node1).succ node2)
            then  Lm_printf.eprintf "tried to add %d->%d, but successor element is missing!\n" node1 node2;
         if not (Table.mem table node2)
            then  Lm_printf.eprintf "tried to add %d->%d, but %d node is missing!\n" node1 node2 node2
         else if not (Set.mem (Table.find table node2).pred node1)
            then  Lm_printf.eprintf "tried to add %d->%d, but predecessor element is missing!\n" node1 node2;
         { table = table; next = next }

   (*
    * Return the list of neighbors of the node.
    *)
   let pred { table = table } node =
      if Table.mem table node
         then Set.elements (Table.find table node).pred
         else []

   let succ { table = table } node =
      if Table.mem table node
         then Set.elements (Table.find table node).succ
         else []

   (*
    * Check if a node exists.
    *)
   let query_node { table = table } node =
      Table.mem table node

   (*
    * Check if an edge exists.
    *)
   let query { table = table } node1 node2 =
      if Table.mem table node1
         then Set.mem (Table.find table node1).succ node2
         else false

   (*
    * When a node is deleted, all the edges to the node
    * must also be deleted.
    *)
   let delete_node { table = table; next = next } node =
      let remove_pred { pred = pred; succ = succ } =
         { pred = Set.remove pred node; succ = succ }
      in
      let remove_pred' table node =
         update table node remove_pred
      in
      let remove_succ { pred = pred; succ = succ } =
         { pred = pred; succ = Set.remove succ node }
      in
      let remove_succ' table node =
         update table node remove_succ
      in
      let table =
         if Table.mem table node
            then  let { pred = pred; succ = succ } = Table.find table node in
                  let table = List.fold_left remove_succ' table (Set.elements pred) in
                  let table = List.fold_left remove_pred' table (Set.elements succ) in
                  let table = Table.remove table node in
                     table
            else  table
      in
         { table = table; next = next }

   (*
    * Delete a single edge.
    *)
   let delete_edge { table = table; next = next } node1 node2 =
      let remove_succ { pred = pred; succ = succ } =
         { pred = pred; succ = Set.remove succ node2 }
      in
      let remove_pred { pred = pred; succ = succ } =
         { pred = Set.remove pred node1 ; succ = succ }
      in
      let table = update table node1 remove_succ in
      let table = update table node2 remove_pred in
         { table = table; next = next }

   (*
    * In the depth-first-search, the graph is traversed
    * from the originating node outward.  We keep a bit-vector
    * for marking nodes as we traverse them.
    *)
   let depth_first_search graph node f arg1 arg2 =
      let { table = table; next = next } = graph in
      let marked = Array.create next false in
      let rec search arg1 arg2 = function
         node :: nodes ->
            let arg1 =
               if marked.(node) then
                  arg1
               else
                  let arg1, arg2 = f arg1 arg2 node in
                     marked.(node) <- true;
                     search arg1 arg2 (succ graph node)
            in
               search arg1 arg2 nodes
       | [] ->
            arg1
      in
         search arg1 arg2 [node]

   (*
    * Get the roots (nodes that have no in-edges).
    * There may be no roots in a cyclic graph.
    *)
   let roots { table = table } =
      Table.fold (fun roots i edges ->
            let { pred = pred } = edges in
               if Set.equal pred Set.empty then
                  i :: roots
               else
                  roots) [] table
   (*
    * Sort the nodes.
    * If the graph is acyclic, this will give a topological sort.
    *)
   let sort graph =
      let { table = table; next = next } = graph in
      let marked = Array.create next false in

      (* Invariant: all nodes' are larger than
       * any non-searched child of node :: nodes
       *)
      let rec search nodes' = function
         node :: nodes ->
            if marked.(node) then
               search nodes' nodes
            else
               begin
                  marked.(node) <- true;
                  let nodes' = search nodes' (succ graph node) in
                     search (node :: nodes') nodes
               end
       | [] ->
            nodes'
      in

      let roots = roots graph in
      let nodes = Table.fold (fun nodes i _ -> i :: nodes) [] table in
         search (search [] roots) nodes
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
