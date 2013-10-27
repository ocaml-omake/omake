(*
 * This module implements dominator calculations and
 * loop-nest trees.  We use the Lengauer-Tarjen method, dreived
 * from Appel, "Modern Compiler Implementation in ML", 1998,
 * Cambridge University Press.
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
open Lm_debug
open Lm_trace
open Lm_symbol
open Lm_printf

(*
 * Lm_debug flag.
 *)
let debug_loop = ref false

(*
 * Node identifiers are just numbers.
 *)
type node_id = int
type node_dfnum = int

module IntCompare =
struct
   type t = int
   let compare = Pervasives.compare
end

module NodeIdSet = Lm_set.LmMake (IntCompare)
module NodeIdTable = Lm_map.LmMake (IntCompare)

(*
 * A node has:
 *    node_data     : the data
 *    node_id       : the index
 *    node_name     : the name of the data
 *    node_parent   : the depth-first parent
 *
 *    node_best     : best ancestor choice
 *    node_sdno     : the dfs number of the semi-dominator
 *    node_size     : the size of the data struct (for balancing)
 *    node_ancestor : ancestor with lowest semi-dominator
 *    node_idom     : immediate dominator
 *    node_bucket   :
 *)
type 'a node =
   { node_data             : 'a;
     node_id               : node_id;
     node_dfnum            : node_dfnum;
     node_name             : symbol;

     (* The graph *)
     node_parent           : node_id;
     node_pred             : node_id list;
     node_succ             : node_id list;
     mutable node_bucket   : NodeIdSet.t;

     (* Semi-dominators *)
     mutable node_idom     : node_id;
     mutable node_ancestor : node_id;
     mutable node_child    : node_id;
     mutable node_label    : node_id;
     mutable node_sdno     : node_dfnum;
     mutable node_size     : int
   }

(*
 * This is the state record for the computation.
 *   state_nodes  : an array node_id->node
 *   state_vertex : an array indexed by dfnum
 *   state_roots  : a list of all the root node ids
 *)
type 'a t =
   { state_nodes  : 'a node array;
     state_vertex : 'a node array
   }

(************************************************************************
 * UTILITIES
 ************************************************************************)

(*
 * Subtract two sorted lists.
 *)
let rec subtract_sorted_list l1 l2 =
   match l1, l2 with
      [], _ ->
         [], l2
    | _, [] ->
         l1, []
    | x1 :: l1', x2 :: l2' ->
         if x1.node_dfnum = x2.node_dfnum then
            let l1, l2 = subtract_sorted_list l1' l2' in
               x1 :: l1, l2
         else if x1.node_dfnum < x2.node_dfnum then
            let l1, l2 = subtract_sorted_list l1' l2 in
               x1 :: l1, l2
         else
            let l1, l2 = subtract_sorted_list l1 l2' in
               l1, x2 :: l2

let rec merge_sorted_list l1 l2 =
   match l1, l2 with
      [], _ ->
         l2
    | _, [] ->
         l1
    | x1 :: l1', x2 :: l2' ->
         if x1.node_dfnum = x2.node_dfnum then
            x1 :: merge_sorted_list l1' l2'
         else if x1.node_dfnum < x2.node_dfnum then
            x1 :: merge_sorted_list l1' l2
         else
            x2 :: merge_sorted_list l1 l2'

(*
 * Print the tree.
 *)
let tabstop = 3

(*
 * BUG: inherit until we fix this file.
 *)
let print_symbol = pp_print_symbol std_formatter

let print_node_int s v =
   print_string s;
   print_string ": ";
   print_int v

let print_node_id_list s l =
   print_string s;
   print_string " = [";
   ignore (List.fold_left (fun flag v ->
                 if flag then
                    print_string "; ";
                 print_int v;
                 true) false l);
   print_string "]"

let print_node node =
   let { node_id = id;
         node_name = name;

         node_parent = parent;
         node_pred = pred;
         node_succ = succ;
         node_bucket = bucket;

         node_idom = idom;
         node_ancestor = ancestor;
         node_child = child;
         node_label = label;
         node_sdno = sdno;
         node_size = size
       } = node
   in
      open_hvbox tabstop;
      print_string "Node [name=";
      print_string (string_of_symbol name);
      print_string "] [id=";
      print_int id;
      print_string "]";
      print_space ();

      print_node_int "Parent" parent;
      print_space ();
      print_node_id_list "Pred" pred;
      print_space ();
      print_node_id_list "Succ" succ;
      print_space ();
      print_node_id_list "Bucket" (NodeIdSet.to_list bucket);
      print_space ();

      print_node_int "Idom" idom;
      print_space ();
      print_node_int "Ancestor" ancestor;
      print_space ();
      print_node_int "Child" child;
      print_space ();
      print_node_int "Label" label;
      print_space ();
      print_node_int "Sdno" sdno;
      print_space ();
      print_node_int "Size" size;
      print_space ();
      close_box ()

let print_state state =
   open_vbox 0;
   ignore (Array.fold_left (fun flag node ->
                 if flag then
                    print_space ();
                 print_node node;
                 true) false state.state_vertex);
   close_box ()

(*
 * Print the trace.
 *)
let pp_print_trace buf node_name trace =
   fprintf buf "@[<v 3>*** Lm_loop trace:";
   Lm_trace.pp_print buf (fun buf node -> pp_print_symbol buf (node_name node)) trace;
   fprintf buf "@]@."

(************************************************************************
 * CREATION
 ************************************************************************)

(*
 * Catch Not_found and print the name.
 *)
let symbol_find table v =
   try SymbolTable.find table v with
      Not_found ->
         raise (Invalid_argument ("Lm_loop.symbol_find: unbound var: " ^ Lm_symbol.string_of_symbol v))

(*
 * Create the node array.
 *)
let create_nodes node_name node_succ nodes =
   (* Construct the var->node table, and the list of nodes *)
   let ntable, _ =
      List.fold_left (fun (ntable, i) node ->
            let ntable = SymbolTable.add ntable (node_name node) i in
               ntable, succ i) (SymbolTable.empty, 0) nodes
   in

   (* Create an array of nodes *)
   let nodes = Array.of_list nodes in

   (* Create the edges *)
   let succ =
      Array.map (fun node ->
            List.map (symbol_find ntable) (node_succ node)) nodes
   in
   let pred = Array.create (Array.length nodes) [] in
   let _ =
      Array.iteri (fun i succ ->
            List.iter (fun j ->
                  pred.(j) <- i :: pred.(j)) succ) succ
   in

   (* Create nodes  *)
   let nodes =
      Array.mapi (fun i node ->
            { node_data = node;
              node_id = i;
              node_dfnum = 0;
              node_name = node_name node;

              node_parent = 0;
              node_pred = pred.(i);
              node_succ = succ.(i);
              node_bucket = NodeIdSet.empty;

              node_idom = 0;
              node_ancestor = 0;
              node_child = 0;
              node_label = i;
              node_sdno = 0;
              node_size = 1
            }) nodes
   in
      nodes

(*
 * Order the nodes in depth-first order.
 * Start from the roots.
 *)
let dfs nodes =
   (* Walk from a root in DFS order *)
   let rec search id1 i id2 =
      let node2 = nodes.(id2) in
         if node2.node_sdno = 0 then
            let node2 =
               { node2 with node_parent = id1;
                            node_dfnum = i;
                            node_sdno = i
               }
            in
               nodes.(id2) <- node2;
               search_list (succ i) id2 node2.node_succ
         else
            i

   and search_list i id idl =
      List.fold_left (search id) i idl
   in

   (* Walk each of the roots *)
   let _ = search_list 1 0 nodes.(0).node_succ in

   (* Now construct the vertex array *)
   let vertex = Array.copy nodes in
      for i = 0 to pred (Array.length nodes) do
         let node = nodes.(i) in
            vertex.(node.node_sdno) <- node
      done;
      vertex

(*
 * Create the state.
 * Assign numbers to all the nodes.
 *)
let create_state node_name node_succ root nodes =
   let nodes = create_nodes node_name node_succ (root :: nodes) in
   let vertex = dfs nodes in
      { state_nodes = nodes;
        state_vertex = vertex
      }

(************************************************************************
 * OPERATIONS ON THE STATE
 ************************************************************************)

(*
 * Compare two nodes.
 *)
let node_eq node1 node2 =
   node1.node_id = node2.node_id

(*
 * Get the node corresponding to an id.
 *)
let node_of_id state id =
   state.state_nodes.(id)

(*
 * Accessors.
 *)
let is_root_node node =
   node.node_id = 0

let node_succ state node =
   List.map (node_of_id state) node.node_succ

let node_pred state node =
   List.map (node_of_id state) node.node_pred

let node_parent state node =
   node_of_id state node.node_parent

let node_ancestor state node =
   node_of_id state node.node_ancestor

let node_child state node =
   node_of_id state node.node_child

let node_sdno node =
   node.node_sdno

let node_size node =
   node.node_size

let node_label state node =
   node_of_id state node.node_label

let node_idom state node =
   node_of_id state node.node_idom

let node_bucket node =
   node.node_bucket

(*
 * Modify the node.
 *)
let set_ancestor node a =
   node.node_ancestor <- a.node_id

let set_child node c =
   node.node_child <- c.node_id

let set_size node s =
   node.node_size <- s

let set_label node l =
   node.node_label <- l.node_id

let set_sdno node i =
   node.node_sdno <- i

let set_idom _ _ node idom =
   node.node_idom <- idom.node_id

let add_to_bucket node w =
   let bucket = NodeIdSet.add node.node_bucket w.node_id in
      node.node_bucket <- bucket

let clear_bucket node =
   node.node_bucket <- NodeIdSet.empty

(************************************************************************
 * DOMINATORS
 ************************************************************************)

(*
 * Link two nodes.
 *)
let link state v w =
   assert (not (node_eq v w));
   if debug debug_loop then
      eprintf "Link@.";

   (*
    * Rebalance the forest maintained by the
    * child and ancestor data structures.
    *)
   let label_w = node_label state w in
   let sdno_label_w = node_sdno label_w in
   let rec balance s =
      if debug debug_loop then
         begin
            print_string "Balance:";
            print_space ();
            print_node s;
            print_newline ()
         end;
      let child_s = node_child state s in
         if sdno_label_w < node_sdno (node_label state child_s) then
            let child_child_s = node_child state child_s in
            let size_s = node_size s in
            let s =
               if size_s + node_size child_child_s >= node_size child_s * 2 then
                  begin
                     set_ancestor child_s s;
                     set_child s child_child_s;
                     s
                  end
               else
                  begin
                     set_size child_s size_s;
                     set_ancestor s child_s;
                     child_s
                  end
            in
               balance s
         else
            s
   in
   let s = balance w in
   let _ = set_label s label_w in
   let size_v = node_size v in
   let size_w = node_size w in
   let size_t = size_v + size_w in
   let s =
      set_size v size_t;
      if size_t < 2 * size_w then
         let s' = node_child state v in
            set_child v s;
            s'
      else
         s
   in

   (* Set all the ancestors *)
   let rec set_ancestors s =
      if not (is_root_node s) then
         begin
            set_ancestor s v;
            set_ancestors (node_child state s)
         end
   in
      set_ancestors s

(*
 * Compress the ancestor path to node v to the node
 * whose label has the maximal semidominator number.
 *)
let rec compress state v =
   if debug debug_loop then
      eprintf "Compress@.";
   let ancestor_v = node_ancestor state v in
      if not (is_root_node (node_ancestor state ancestor_v)) then
         begin
            compress state ancestor_v;
            let ancestor_v = node_ancestor state v in
            let ancestor_label_v = node_label state ancestor_v in
               if node_sdno ancestor_label_v < node_sdno (node_label state v) then
                  set_label v ancestor_label_v;
               set_ancestor v (node_ancestor state ancestor_v)
         end

(*
 * Find ancestor.
 *)
let eval state v =
   if debug debug_loop then
      eprintf "Eval@.";
   if is_root_node (node_ancestor state v) then
      node_label state v
   else
      let _ = compress state v in
      let label_v = node_label state v in
      let ancestor_v = node_ancestor state v in
      let ancestor_label_v = node_label state ancestor_v in
         if node_sdno ancestor_label_v >= node_sdno label_v then
            label_v
         else
            ancestor_label_v

(*
 * Dominator calculation.
 *)
let dominators state =
   let { state_vertex = vertex } = state in
   let n = Array.length state.state_vertex in
      for i = pred n downto 1 do
         let w = vertex.(i) in
         let parent_w = node_parent state w in

            (*
             * Compute the initial values for semindominators and store
             * nodes with the same semidominator in the same bucket.
             *)
            List.iter (fun v ->
                  let sdno_v = node_sdno v in
                  let u =
                     if sdno_v <= node_sdno w then
                        v
                     else
                        eval state v
                  in
                  let sdno_u = node_sdno u in
                     if sdno_u < node_sdno w then
                        set_sdno w sdno_u) (node_pred state w);

            add_to_bucket vertex.(node_sdno w) w;
            link state parent_w w;

            (*
             * Compute immediate dominators for nodes in the bucket
             * of w's parent.
             *)
            NodeIdSet.iter (fun v ->
                  let v = node_of_id state v in
                  let u = eval state v in
                  let idom =
                     if node_sdno u < node_sdno v then
                        u
                     else
                        parent_w
                  in
                     set_idom state "Hello" v idom) (node_bucket parent_w);
            clear_bucket parent_w
      done;

      (*
       * Now perform all the deferred dominator calculations.
       *)
      for i = 1 to pred n do
         let w = vertex.(i) in
         let idom_w = node_idom state w in
            if not (node_eq idom_w vertex.(node_sdno w)) then
               set_idom state "World" w (node_idom state idom_w)
      done

(************************************************************************
 * LOOPS
 ************************************************************************)

(*
 * Build the dominator tree from the immediate dominators.
 *)
let build_dtree state =
   Array.fold_left (fun dtree node ->
         let { node_id = id } = node in
            if id = 0 then
               dtree
            else
               let { node_id = parent} = node_idom state node in
                  NodeIdTable.filter_add dtree parent (fun children ->
                        let children =
                           match children with
                              Some children ->
                                 children
                            | None ->
                                 NodeIdSet.empty
                        in
                           NodeIdSet.add children id)) NodeIdTable.empty state.state_vertex

let dtree_children dtree id =
   try NodeIdTable.find dtree id with
      Not_found ->
         NodeIdSet.empty

(*
 * Perform a DFS on the dominator tree,
 * looking for backedges.  When we find one, classify all
 * the nodes along the path.
 *)
let classify_nodes state dtree =
   (*
    * Find the best back-edge, given a table that gives the
    * depth-first depth of the parents.
    *)
   let rec find_best_backedge parents best succ =
      match succ with
         id1 :: succ ->
            let best =
               try
                  let depth1 = NodeIdTable.find parents id1 in
                     match best with
                        Some (depth2, _)
                        when depth2 > depth1 ->
                           best
                      | _ ->
                           Some (depth1, id1)
               with
                  Not_found ->
                     best
            in
               find_best_backedge parents best succ
       | [] ->
            best
   in

   (*
    * Reclassify all the nodes along the path up to the
    * parent.
    *)
   let rec reclassify classes parent_id depth1 id1 =
      if id1 = parent_id then
         classes
      else
         let parent2, depth2 =
            try NodeIdTable.find classes id1 with
               Not_found ->
                  0, 0
         in
            if depth2 > depth1 then
               (* We found a nested loop, so skip through it *)
               reclassify classes parent_id depth1 parent2
            else
               (* This node belongs to the current loop *)
               let classes = NodeIdTable.add classes id1 (parent_id, depth1) in
               let { node_idom = idom } = node_of_id state id1 in
                  reclassify classes parent_id depth1 idom
   in

   (*
    * Perform a DFS, looking for backedges.
    *)
   let rec search loops classes parents depth id =
      (* Catch self-edges *)
      let { node_succ = succ_edges } = node_of_id state id in
      let loops =
         if List.mem id succ_edges then
            NodeIdTable.add loops id []
         else
            loops
      in

      (* Reclassify nodes with backedges *)
      let classes =
         match find_best_backedge parents None succ_edges with
            Some (depth, parent) ->
               reclassify classes parent depth id
          | None ->
               classes
      in

      (* Search the rest *)
      let depth = succ depth in
      let parents = NodeIdTable.add parents id depth in
      let children = dtree_children dtree id in
         NodeIdSet.fold (fun (loops, classes) id ->
               search loops classes parents depth id) (loops, classes) children
   in
      search NodeIdTable.empty NodeIdTable.empty NodeIdTable.empty 0 0

(*
 * Perform a second DFS, building the loop contents.
 *)
let collect_natural_loops _ loops classes dtree =
   let rec collect loops id =
      let parent =
         try fst (NodeIdTable.find classes id) with
            Not_found ->
               0
      in
      let loops =
         NodeIdTable.filter_add loops parent (fun l ->
               let l =
                  match l with
                     Some l ->
                        l
                   | None ->
                        []
               in
                  id :: l)
      in
      let children = dtree_children dtree id in
         NodeIdSet.fold collect loops children
   in
      NodeIdSet.fold collect loops (dtree_children dtree 0)

(*
 * Now build the trace.
 *)
let build_trace state loops =
   let rec collect id =
      try
         let nodes = NodeIdTable.find loops id in
         let trace = collect_list nodes in
            Lm_trace (id, trace)
      with
         Not_found ->
            Elem id
   and collect_list nodes =
      List.fold_left (fun nodes id ->
            collect id :: nodes) [] nodes
   in
   let trace =
      match collect 0 with
         Lm_trace (root, l) ->
            Elem root :: l
       | Elem _ ->
            raise (Invalid_argument "build_trace")
   in
      Lm_trace.map (fun id -> (node_of_id state id).node_data) trace

(*
 * Put them all together.
 *)
let build_nest state =
   let dtree = build_dtree state in
   let loops, classes = classify_nodes state dtree in
   let loops = collect_natural_loops state loops classes dtree in
      build_trace state loops

(************************************************************************
 * GLOBAL FUNCTION
 ************************************************************************)

(*
 * Build the loop nest from the graph.
 *)
let print_state state debug =
      if Lm_debug.debug debug_loop then
         begin
            open_vbox 3;
            print_string "*** Lm_loop: ";
            print_string debug;
            print_space ();
            print_state state;
            print_space ();
            print_string "*** Done";
            close_box ();
            print_newline()
         end

let create debug node_name node_succ root nodes =
   let state = create_state node_name node_succ root nodes in
   let _ = print_state state debug in
   let _ = dominators state in
   let _ = print_state state "XDominators" in
      state

let loop_nest state node_name =
   let trace = build_nest state in
      if !debug_loop then
         pp_print_trace err_formatter node_name trace;
      trace

let dominators state _ =
   Array.fold_left (**)
      (fun dom { node_name = name; node_idom = id } ->
         let idom = state.state_nodes.(id) in
            SymbolTable.add dom name idom.node_name)
      SymbolTable.empty state.state_nodes

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
