(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
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
open Lm_map_sig
open Lm_printf

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Table is a binary tree.
 * Each node has five fields:
 *    1. a key
 *    2. a list of values associated with the key
 *    3. a left child
 *    4. a right child
 *    5. the total number of keys in the tree
 *)
type ('elt, 'data) tree =
   Leaf
 | Node of 'elt * 'data list * ('elt, 'data) tree * ('elt, 'data) tree * int

(*
 * The tree is mutable
 * so that we can rearrange the tree in place.
 * However, we all splay operations are functional,
 * and we assume that the rearranged tree can be
 * assigned atomically to this field.
 *)
type ('elt, 'data) table =
   ('elt, 'data) tree ref

type ('elt, 'data) t = ('elt, 'data) table

(*
 * Directions are used to define
 * paths in the tree.
 *)
type ('elt, 'data) direction =
   Left of ('elt, 'data) tree
 | Right of ('elt, 'data) tree

(*
 * Result of a splay operation.
 *)
type ('elt, 'data) splay_result =
   SplayFound of ('elt, 'data) tree
 | SplayNotFound of ('elt, 'data) tree

(*
 * Build the set from an ordered type.
 *)
let create
    (ord_print : out_channel -> 'elt -> 'data list -> unit)
    (ord_compare : 'elt -> 'elt -> int)
    (ord_append : 'data list -> 'data list -> 'data list) =

   (*
    * Size of a table.
    *)
   let cardinality = function
      Node (_, _, _, _, size) ->
         size
    | Leaf ->
         0
   in

   let cardinal tree =
      cardinality !tree
   in

   (*
    * Add two nodes.
    *)
   let new_node key data left right =
      if data == [] then raise (Invalid_argument "Splay_table.new_node : empty data") else
      Node (key, data, left, right, cardinality left + cardinality right + 1)
   in

   (*
    * This function performs the action of moving an entry
    * to the root.  The argument is the path to the entry.
    *)
   let rec lift key data left right = function
      [] ->
         new_node key data left right
    | [Left (Node (key', data', _, right', _))] ->
         new_node key data left (new_node key' data' right right')
    | [Right (Node (key', data', left', _, _))] ->
         new_node key data (new_node key' data' left' left) right
    | Left (Node (key_left, data_left, _, left_right, _)) :: Left (Node (key', data', _, right', _)) :: ancestors ->
         lift key data left (new_node key_left data_left right (new_node key' data' left_right right')) ancestors
    | Right (Node (key_right, data_right, right_left, _, _)) :: Right (Node (key', data', left', _, _)) :: ancestors ->
         lift key data (new_node key_right data_right (new_node key' data' left' right_left) left) right ancestors
    | Left (Node (key_right, data_right, _, right_right, _)) :: Right (Node (key', data', left', _, _)) :: ancestors ->
         lift key data (new_node key' data' left' left) (new_node key_right data_right right right_right) ancestors
    | Right (Node (key_left, data_left, left_left, _, _)) :: Left (Node (key', data', _, right', _)) :: ancestors ->
         lift key data (new_node key_left data_left left_left left) (new_node key' data' right right') ancestors
    | _ ->
         raise (Invalid_argument "lift")
   in

   (*
    * Find an entry in the tree.
    * Returns true iff the entry is found.
    * Transforms the tree so that either the
    * entry becomes the root, or an adjacent entry
    * becomes the root if the entry is not found.
    *)
   let rec splay key0 path = function
      Node (key, data, left, right, _) as node ->
         let comp = ord_compare key0 key in
            if comp = 0 then
               SplayFound (lift key data left right path)
            else if comp < 0 then
               (* node is down the left branch *)
               if left = Leaf then
                  SplayNotFound (lift key data left right path)
               else
                  splay key0 (Left node :: path) left
            else if right = Leaf then
               SplayNotFound (lift key data left right path)
            else
               splay key0 (Right node :: path) right

    | Leaf ->
         SplayNotFound Leaf
   in

   (*
    * Move the rioghtmost node to the root.
    *)
   let rec lift_right = function
      Node (key, data, left, Leaf, _) ->
         key, data, left
    | Node (key', data', left', Node (key, data, left, Leaf, _), _) ->
         key, data, new_node key' data' left' left
    | Node (key', data', left', Node (key_right, data_right, right_left, right, _), _) ->
         let key, data, left = lift_right right in
            key, data, new_node key_right data_right (new_node key' data' left' right_left) left
    | Leaf ->
         raise (Invalid_argument "lift_right")
   in

   (*
    * An empty tree is just a leaf.
    *)
   let empty = Leaf
   in

   let is_empty = function
      { contents = Leaf } ->
         true
    | _ ->
         false
   in

   let make key data =
      ref (Node (key, data, Leaf, Leaf, 1))
   in

   (*
    * Get the elements of the list.
    *)
   let rec to_list_aux elements = function
      Node (key, data, left, right, _) ->
         to_list_aux ((key, data) :: to_list_aux elements right) left
    | Leaf ->
         elements
   in

   let to_list tree =
      to_list_aux [] !tree
   in

   let elements = to_list
   in

   (*
    * Build a table from a list.
    *)
   let rec of_array elements off len =
      if len = 0 then
         Leaf
      else if len = 1 then
         let key, data = elements.(off) in
            Node (key, data, Leaf, Leaf, 1)
      else if len = 2 then
         let key1, data1 = elements.(off) in
         let key0, data0 = elements.(succ off) in
            Node (key0, data0, Node (key1, data1, Leaf, Leaf, 1), Leaf, 2)
      else
         let len2 = len lsr 1 in
         let key0, data0 = elements.(off + len2) in
            Node (key0, data0,
                  of_array elements off len2,
                  of_array elements (off + len2 + 1) (len - len2 - 1),
                  len)
   in

   let of_list elements =
      let tree =
         match elements with
            [] ->
               Leaf
          | [key, data] ->
               Node (key, data, Leaf, Leaf, 1)
          | elements ->
               let elements = Array.of_list elements in
               let length = Array.length elements in
                  of_array elements 0 length
      in
         ref tree
   in

   (*
    * Check if a key is listed in the table.
    *)
   let mem t key =
      match splay key [] !t with
         SplayFound tree ->
            t := tree;
            true
       | SplayNotFound tree ->
            t := tree;
            false
   in

   let find t key =
      match splay key [] !t with
         SplayFound tree ->
            begin
               t := tree;
               match tree with
                  Node (_, data :: _, _, _, _) ->
                     data
                | _ ->
                     raise (Invalid_argument "Splay_table.find")
            end
       | SplayNotFound tree ->
            t := tree;
            raise Not_found
   in

   let find_all t key =
      match splay key [] !t with
         SplayFound tree ->
            begin
               t := tree;
               match tree with
                  Node (_, data, _, _, _) ->
                     data
                | _ ->
                     raise (Invalid_argument "Splay_table.find_all")
            end
       | SplayNotFound tree ->
            t := tree;
            []
   in

	let add_list_aux key data = function
      Node (key', data', left, right, _) ->
         if ord_compare key key' < 0 then
            (* Root should become right child *)
            new_node key data left (new_node key' data' empty right)
         else
            (* Root should become left child *)
            new_node key data (new_node key' data' left empty) right
    | Leaf ->
         (* Tree is empty, so make a new root *)
          new_node key data empty empty
	in

   (*
    * Add an entry to the table.
    * If the entry already exists,
    * the new value is added to the data.
    *)
   let add_list tree key data =
      match splay key [] tree with
         SplayFound tree ->
            begin
               match tree with
                  Node (key, data', left, right, size) ->
                     Node (key, ord_append data data', left, right, size)
                | Leaf ->
                     raise (Invalid_argument "Splay_table.add_list")
            end
       | SplayNotFound tree ->
				add_list_aux key data tree
   in

   let add t key data =
      ref (add_list !t key [data])
   in

   (*
    * Remove the first entry from the hashtable.
    * If the value list becomes empty, remove the
    * entire entry from the tree.
    *)
   let remove t key =
      match splay key [] !t with
         SplayFound tree ->
            begin
               match tree with
                  Node (_, [_], Leaf, right, _) ->
                     ref right
                | Node (_, [_], left, Leaf, _) ->
                     ref left
                | Node (_, [_], left, right, _) ->
                     let key, data, left_left = lift_right left in
                     ref (new_node key data left_left right)
                | Node (key, _ :: data, left, right, size) ->
                     ref (Node (key, data, left, right, size))
                | _ ->
                     raise (Invalid_argument "Splay_table.remove")
            end
       | SplayNotFound tree ->
            t := tree;
            t
   in

	let replace t key data =
      match splay key [] !t with
         SplayFound tree ->
            begin
               match tree with
                  Node (key, _, left, right, size) ->
                     ref (Node (key, data, left, right, size))
                | Leaf ->
                     raise (Invalid_argument "Splay_table.replace: Leaf reached")
            end
       | SplayNotFound tree ->
				ref (add_list_aux key data tree)
	in

   (*
    * Merge two hashtables.
    * Data fields get concatenated.
    *)
   let rec union_aux s1 s2 =
      match s1, s2 with
         Leaf, _ ->
            s2
       | _, Leaf ->
            s1
       | Node (key1, data1, left1, right1, size1),
         Node (key2, data2, left2, right2, size2) ->
            if size1 >= size2 then
               if size2 = 1 then
                  add_list s1 key2 data2
               else
                  match splay key1 [] s2 with
                     SplayFound (Node (_, data2, left2, right2, _)) ->
                        let left3 = union_aux left1 left2 in
                        let right3 = union_aux right1 right2 in
                           new_node key1 (ord_append data1 data2) left3 right3
                   | SplayNotFound (Node (key2, data2, left2, right2, _)) ->
                        if compare key1 key2 < 0 then
                           let left3 = union_aux left1 left2 in
                           let right3 = union_aux right1 (new_node key2 data2 empty right2) in
                              new_node key1 data1 left3 right3
                        else
                           let left3 = union_aux left1 (new_node key2 data2 left2 empty) in
                           let right3 = union_aux right1 right2 in
                              new_node key1 data1 left3 right3
                   | _ ->
                        raise (Invalid_argument "Splay_table.union")
            else if size1 = 1 then
               add_list s2 key1 data1
            else
               match splay key2 [] s1 with
                  SplayFound (Node (_, data1, left1, right1, _)) ->
                     let left3 = union_aux left1 left2 in
                     let right3 = union_aux right1 right2 in
                        new_node key2 (ord_append data1 data2) left3 right3
                | SplayNotFound (Node (key1, data1, left1, right1, _)) ->
                     if compare key2 key1 < 0 then
                        let left3 = union_aux left1 left2 in
                        let right3 = union_aux (new_node key1 data1 empty right1) right2 in
                           new_node key2 data2 left3 right3
                     else
                        let left3 = union_aux (new_node key1 data1 left1 empty) left2 in
                        let right3 = union_aux right1 right2 in
                           new_node key2 data2 left3 right3
                | _ ->
                     raise (Invalid_argument "Splay_table.union")
   in

   let union s1 s2 =
      ref (union_aux !s1 !s2)
   in

   (*
    * Build a path into a tree.
    *)
   let rec initial_path path node =
      match node with
         Node (_, _, Leaf, _, _) ->
            Left node :: path
       | Node (_, _, left, _, _) ->
            initial_path (Left node :: path) left
       | Leaf ->
            raise (Invalid_argument "initial_path")
   in

   let key_of_path = function
      Left (Node (key, _, _, _, _)) :: _
    | Right (Node (key, _, _, _, _)) :: _ ->
         key
    | _ ->
         raise (Invalid_argument "key_of_path")
   in

   let rec next_path = function
      Left (Node (_, _, _, Leaf, _)) :: path
    | Right  _ :: path ->
         next_path path
    | Left (Node (_, _, _, right, _)) :: path ->
         initial_path path right
    | [] ->
         raise Not_found
    | _ ->
         raise (Invalid_argument "next_path")
   in

   (*
    * See if two sets intersect.
    *)
   let rec intersect_aux path1 path2 =
      let key1 = key_of_path path1 in
      let key2 = key_of_path path2 in
      let comp = ord_compare key1 key2 in
         if comp = 0 then
            true
         else if comp < 0 then
            intersect_aux (next_path path1) path2
         else
            intersect_aux path1 (next_path path2)
   in

   let intersectp s1 s2 =
      match !s1, !s2 with
         Leaf, _
       | _, Leaf ->
            false
       | s1, s2 ->
            let path1 = initial_path [] s1 in
            let path2 = initial_path [] s2 in
               try intersect_aux path1 path2 with
                  Not_found ->
                     false
   in

   (*
    * Iterate a function over the hashtable.
    *)
   let rec iter_aux f = function
      Node (key, data, left, right, _) ->
         List.iter (f key) data;
         iter_aux f left;
         iter_aux f right
    | Leaf ->
         ()
   in

   let iter f t =
      iter_aux f !t
   in

   let rec fold_aux f acc = function
      Node (key, data, left, right, _) ->
         let acc' = List.fold_left (fun acc item -> f key item acc) acc data in
         let acc'' = fold_aux f acc' left in
         fold_aux f acc'' right
    | Leaf ->
         acc
   in

	let fold_map f acc t =
		fold_aux f acc !t
	in

	let rec list_of_aux table l =
		match table with
			Node (key, data, left, right, _) ->
				list_of_aux left ((key,data)::(list_of_aux right l))
		 | Leaf ->
				l
   in

   let list_of t =
      list_of_aux !t []
   in

(*
    * Map a function over the table.
    *)
   let rec map_aux f = function
      Node (key, data, left, right, size) ->
         Node (key, List.map (f key) data, map_aux f left, map_aux f right, size)
    | Leaf ->
         Leaf
   in

   let map f tree =
      ref (map_aux f !tree)
   in


   (*
    * Intersection.
    *)
   let rec mem_filt s = function
      [] ->
         []
    | (h :: t) as l ->
         if mem s h then
            let rem = mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem
         else
            mem_filt s t
   in

   let rec not_mem_filt s = function
      [] ->
         []
    | (h :: t) as l ->
         if mem s h then
            not_mem_filt s t
         else
            let rem = not_mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem
   in

   let rec fst_mem_filt s = function
      [] ->
         []
    | (((v, _) as h) :: t) as l ->
         if mem s v then
            let rem = fst_mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem
         else
            fst_mem_filt s t
   in

   let deletemax t =
      let k,d,t'=lift_right !t in
         k,d,ref t'
   in

   (*
    * Debugging.
    *)
   let rec print_aux out = function
      Leaf ->
         fprintf out "@ Leaf"
    | Node (key, data, left, right, size) ->
         fprintf out "@ (@[<hv 0>%t:%d%a%a)@]" (**)
            (fun out -> ord_print out key data)
            size
            print_aux left
            print_aux right
   in

   let print out table =
      print_aux out !table

   in
      { empty = ref empty;
        is_empty = is_empty;
        mem = mem;
        add = add;
        replace = replace;
        find = find;
        find_all = find_all;
        make = make;
        remove = remove;
        union = union;
        elements = elements;
        iter = iter;
        fold_map = fold_map;
        map = map;
        cardinal = cardinal;
        mem_filt = mem_filt;
        not_mem_filt = not_mem_filt;
        intersectp = intersectp;
        of_list = of_list;
        list_of = list_of;
        deletemax = deletemax;
        print = print
      }

module Create =
struct
   type ('elt, 'data) t = ('elt, 'data) table

   let create = create
end

(*
 * Module version.
 *)
module MakeTable (Base : TableBaseSig) =
   Lm_table_util.MakeTable (Create) (Base)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
