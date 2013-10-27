(*
 * Build a table using a red-black tree.
 * Every node in the tree is colored either black or red.
 * A red-black tree has the following invariants:
 *    1. Every leaf is colored black
 *    2. All children of every red node are black.
 *    3. Every path from the root to a leaf has the
 *       same number of black nodes as every other path.
 *    4. The root is always black.
 *
 * We get some corollaries:
 *    1. The longest path from the root to a leaf is
 *       at most twice as long as the shortest path.
 *    2. Both children of a red node are either leaves,
 *       or they are both not.
 *
 * This code is meant to be fast, so all the cases have
 * been expanded, and the insert and delete functions are
 * long (12 cases for insert, 18 for delete in lift_black).
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
open Lm_map_sig

(*
 * Table is a binary tree.
 * Color is kept in the label to save space.
 *)
(* %%MAGICBEGIN%% *)
type ('elt, 'data) tree =
   Leaf
 | Red of 'elt * 'data * ('elt, 'data) tree * ('elt, 'data) tree * int
 | Black of 'elt * 'data * ('elt, 'data) tree * ('elt, 'data) tree * int
(* %%MAGICEND%% *)

(*
 * Make the set.
 *)
module LmMake (Base : OrderedType) =
struct
   (*
    * Path into the tree.
    *)
   type ('elt, 'data) path =
      Left of ('elt, 'data) tree
    | Right of ('elt, 'data) tree
    | Delete of ('elt, 'data) tree

   type key = Base.t
   type 'a t = (key, 'a) tree

   exception Unchanged

   (*
    * Size of a table.
    *)
   let cardinality = function
      Red (_, _, _, _, size)
    | Black (_, _, _, _, size) ->
         size
    | Leaf ->
         0

   let cardinal = cardinality

   (*
    * Add two nodes.
    *)
   let new_black key data left right =
      Black (key, data, left, right, cardinality left + cardinality right + 1)

   let new_red key data left right =
      Red (key, data, left, right, cardinality left + cardinality right + 1)

   (************************************************************************
    * DEBUGGING                                                            *
    ************************************************************************)

   (*
    * Check the size of the set.
    *)
   let rec check_size = function
      Black (_, _, left, right, size)
    | Red (_, _, left, right, size) ->
         let lsize = check_size left in
         let rsize = check_size right in
         if size <> lsize + rsize + 1 then
            let msg = "Lm_map.check_size: " ^ (string_of_int size) ^ " <> " ^
                     (string_of_int lsize) ^ "+" ^ (string_of_int rsize) in
               raise (Failure msg)
         else
            size

    | Leaf ->
         0

   (*
    * Check the red-invariant.
    *)
   let rec check_red = function
      Red (_, _, left, right, _) ->
         begin
            match left, right with
               Red _, _
             | _, Red _ ->
                  raise (Failure "Red_black_table.red_black_set.check_red")

             | _ ->
                  check_red left;
                  check_red right
         end
    | Black (_, _, left, right, _) ->
         check_red left;
         check_red right

    | Leaf ->
         ()

   (*
    * Check the black invariant.
    *)
   let rec black_depth i = function
      Black (_, _, left, _, _) ->
         black_depth (succ i) left
    | Red (_, _, left, _, _) ->
         black_depth i left
    | Leaf ->
         i

   let rec check_black_aux i j = function
      Black (_, _, left, right, _) ->
         check_black_aux i (succ j) left;
         check_black_aux i (succ j) right
    | Red (_, _, left, right, _) ->
         check_black_aux i j left;
         check_black_aux i j right
    | Leaf ->
         if j <> i then
            raise (Failure "Red_black_table.check_black")

   let check_black tree =
      check_black_aux (black_depth 0 tree) 0 tree

   (*
    * Check that all the nodes are sorted.
    *)
   let rec check_sort_lt key = function
      Black (key', _, left, right, _) ->
         if Base.compare key' key >= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_lt key' left;
         check_sort_gt_lt key' key right

    | Red (key', _, left, right, _) ->
         if Base.compare key' key >= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_lt key' left;
         check_sort_gt_lt key' key right

    | Leaf ->
         ()

   and check_sort_gt key = function
      Black (key', _, left, right, _)
    | Red (key', _, left, right, _) ->
         if Base.compare key' key <= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_gt_lt key key' left;
         check_sort_gt key right

    | Leaf ->
         ()

   and check_sort_gt_lt key key' = function
      Black (key'', _, left, right, _)
    | Red (key'', _, left, right, _) ->
         if Base.compare key'' key <= 0 || Base.compare key'' key' >= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_gt_lt key key'' left;
         check_sort_gt_lt key'' key' right

    | Leaf ->
         ()

   let check_sort = function
      Black (key, _, left, right, _) ->
         check_sort_lt key left;
         check_sort_gt key right
    | Red _ ->
         raise (Failure "Red_black_table.check_sort: root is red")
    | Leaf ->
         ()

   (*
    * Perform all the checks.
    *)
   let check tree =
      check_red tree;
      check_black tree;
      check_sort tree;
      ignore (check_size tree);
      tree

   (************************************************************************
    * INSERTION                                                            *
    ************************************************************************)

   (*
    * Insert an entry into the tree.
    *)
   let rec insert (key : 'elt) (dataf : 'data option -> 'data) = function
      Black (key0, data0, left0, right0, size0) ->
         begin
            let comp = Base.compare key key0 in
               if comp = 0 then
                  let data = dataf (Some data0) in
                     if data == data0 then
                        raise Unchanged;
                     Black (key0, data, left0, right0, size0)

               else if comp < 0 then
                  match left0 with
                     Black _
                   | Leaf ->
                        (*
                         * Ok even if child becomes red.
                         *)
                        new_black key0 data0 (insert key dataf left0) right0

                   | Red (key1, data1, left1, right1, size1) ->
                        let comp = Base.compare key key1 in
                           if comp = 0 then
                              Black (key0, data0,
                                     Red (key1, dataf (Some data1), left1, right1, size1),
                                     right0,
                                     size0)
                           else if comp < 0 then
                              match insert key dataf left1, right0 with
                                 Red _ as node, Red (key2, data2, left2, right2, size2) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *     key0:b             key0:r
                                     *    /     \             /    \
                                     *  key1:r key2:r      key1:b  key2:b
                                     *   /    \             /   \
                                     * key2:r right1     key2:r right1
                                     *)
                                    new_red key0 data0 (**)
                                       (new_black key1 data1 node right1)
                                       (Black (key2, data2, left2, right2, size2))

                               | Red _ as node, _ ->
                                    (*
                                     * Rotation:
                                     *
                                     *      key0:b             key1:b
                                     *     /     \             /    \
                                     *   key1:r key2:b      key3:r  key0:b
                                     *   /    \                    /    \
                                     * key3:r right1             right1 key2:r
                                     *)
                                    new_black key1 data1 node (new_red key0 data0 right1 right0)

                               | node, _ ->
                                    (*
                                     * Inline:
                                     *
                                     *        key0:b         key0:b
                                     *        /    \         /    \
                                     *     key1:r key2    key1:r  key2
                                     *     /   \          /    \
                                     *  key3:b right1  key3:b right1
                                     *)
                                    new_black key0 data0 (**)
                                       (new_red key1 data1 node right1)
                                       right0

                           else
                              match insert key dataf right1, right0 with
                                 Red _ as node, Red (key2, data2, left2, right2, size2) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *       key0:b              key0:r
                                     *       /    \              /   \
                                     *    key1:r key2:r       key1:b key2:b
                                     *    /   \               /   \
                                     *  left1 node:r        left1 node:r
                                     *)
                                    new_red key0 data0 (**)
                                       (new_black key1 data1 left1 node)
                                       (Black (key2, data2, left2, right2, size2))

                               | Red (key3, data3, left3, right3, _), _ ->
                                    (*
                                     * Rotation:
                                     *
                                     *       key0:b              key3:b
                                     *       /    \             /      \
                                     *    key1:r  right0     key1:r    key0:r
                                     *    /   \              /    \    /    \
                                     *  left1 key3:r     left1 left3 right3 right0
                                     *        /   \
                                     *      left3 right3
                                     *)
                                    new_black key3 data3 (**)
                                       (new_red key1 data1 left1 left3)
                                       (new_red key0 data0 right3 right0)

                               | node3, _ ->
                                    (*
                                     * Inline:
                                     *
                                     *      key0:b
                                     *      /    \
                                     *   key1:r  right0
                                     *   /     \
                                     * left1  node3:b
                                     *)
                                    new_black key0 data0 (**)
                                       (new_red key1 data1 left1 node3)
                                       right0

               else
                  (* comp > 0 *)
                  match right0 with
                     Black _
                   | Leaf ->
                        (*
                         * Node can be replaced even if it becomes red.
                         *)
                        new_black key0 data0 left0 (insert key dataf right0)

                   | Red (key2, data2, left2, right2, size2) ->
                        let comp = Base.compare key key2 in
                           if comp = 0 then
                              Black (key0, data0, left0,
                                     Red (key2, dataf (Some data2), left2, right2, size2),
                                     size0)

                           else if comp < 0 then
                              match left0, insert key dataf left2 with
                                 Red (key1, data1, left1, right1, size1), (Red _ as node) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *       key0:b              key0:r
                                     *       /    \              /   \
                                     *    key1:r key2:r       key1:b key2:b
                                     *           /   \               /   \
                                     *        node:r right2       node:r right2
                                     *)
                                    new_red key0 data0 (**)
                                       (Black (key1, data1, left1, right1, size1))
                                       (new_black key2 data2 node right2)

                               | _, Red (key3, data3, left3, right3, _) ->
                                    (*
                                     * Rotate:
                                     *
                                     *       key0:b                  key3:b
                                     *       /    \                  /     \
                                     *    key1:b  key2:r          key0:r   key2:r
                                     *            /    \          /   \    /     \
                                     *         key3:r  right2 left0 left3 right3 right2
                                     *         /   \
                                     *      left3 right3
                                     *)
                                    new_black key3 data3 (**)
                                       (new_red key0 data0 left0 left3)
                                       (new_red key2 data2 right3 right2)

                               | _, node3 ->
                                    (*
                                     * Inline:
                                     *
                                     *      key0:b
                                     *      /    \
                                     *   left0  key2:r
                                     *          /   \
                                     *      key3:b right2
                                     *)
                                    new_black key0 data0 (**)
                                       left0
                                       (new_red key2 data2 node3 right2)

                           else
                              match left0, insert key dataf right2 with
                                 Red (key1, data1, left1, right1, size1), (Red _ as node) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *     key0:b                  key0:r
                                     *     /    \                  /   \
                                     *   key1:r key2:r          key1:b key2:b
                                     *          /    \                 /    \
                                     *        left2  node:r          left2 node:r
                                     *)
                                    new_red key0 data0 (**)
                                       (Black (key1, data1, left1, right1, size1))
                                       (new_black key2 data2 left2 node)

                               | _, (Red _ as node) ->
                                    (*
                                     * Rotation:
                                     *
                                     *      key0:b                 key2:b
                                     *      /    \                 /    \
                                     *   left0:b key2:r         key0:r node:r
                                     *           /   \          /   \
                                     *         left2 node:r left0:b left2
                                     *)
                                    new_black key2 data2 (**)
                                       (new_red key0 data0 left0 left2)
                                       node

                               | _, node3 ->
                                    (*
                                     * Inline:
                                     *
                                     *     key0:b
                                     *     /   \
                                     * left0:b key2:r
                                     *         /    \
                                     *       left2 node3:b
                                     *)
                                    new_black key0 data0 (**)
                                       left0
                                       (new_red key2 data2 left2 node3)
         end
    | Leaf ->
         (* Leaf is colored red *)
         Red (key, dataf None, Leaf, Leaf, 1)

    | Red _ ->
         (* Red nodes will not come up *)
         raise (Invalid_argument "Red_black_table.insert")

   (*
    * Append an element to the list.
    *)
   let filter_add (tree : ('elt, 'data) tree) (key : 'elt)
                  (dataf : 'data option -> 'data) =
      let tree =
         match tree with
            Leaf ->
               Black (key, dataf None, Leaf, Leaf, 1)
          | node ->
               try
                  match insert key dataf node with
                     Red (key, data, left, right, size) ->
                        Black (key, data, left, right, size)
                   | tree ->
                        tree
               with
                  Unchanged ->
                     tree
      in
         (tree : ('elt, 'data) tree)

   (*
    * Like filter-add, but the value must already exist.
    *)
   let replace tree key dataf =
      filter_add tree key (fun x ->
            match x with
               Some x ->
                  dataf x
             | None ->
                  raise Not_found)

   (*
    * Add an element to the set.
    *)
   let add (tree : ('elt, 'data) tree) (key : 'elt) (data : 'data) =
      filter_add tree key (fun _ -> data)

   (************************************************************************
    * FIND ENTRIES                                                         *
    ************************************************************************)

   (*
    * Return the data for the entry.
    *)
   let rec find_aux key = function
      Black (key0, data0, left0, right0, _)
    | Red (key0, data0, left0, right0, _) ->
         let comp = Base.compare key key0 in
            if comp = 0 then
               data0
            else if comp < 0 then
               find_aux key left0
            else
               find_aux key right0
    | Leaf ->
         raise Not_found

   let find tree key =
      find_aux key tree

   (*
    * Return the data for the entry.
    *)
   let rec find_aux key = function
      Black (key0, data0, left0, right0, _)
    | Red (key0, data0, left0, right0, _) ->
         let comp = Base.compare key key0 in
            if comp = 0 then
               data0
            else if comp < 0 then
               find_aux key left0
            else
               find_aux key right0
    | Leaf ->
         []

   let find_all tree key =
      find_aux key tree

   (************************************************************************
    * REMOVAL                                                              *
    ************************************************************************)

   (*
    * Construct a path during the removal.
    *)
   let rec delete key filter path node =
      match node with
         Black (key0, data0, left0, right0, size0) ->
            let comp = Base.compare key key0 in
               if comp = 0 then
                  match filter data0 with
                     None ->
                        begin
                           match left0, right0 with
                              Leaf, Leaf ->
                                 lift_black (key0, data0) path Leaf
                            | Red (key1, data1, left1, right1, size1), Leaf ->
                                 lift (key0, data0) path (Black (key1, data1, left1, right1, size1))
                            | _ ->
                                 delete_min (Delete node :: path) right0
                        end
                   | Some data0 ->
                        restore path (Black (key0, data0, left0, right0, size0))
               else if comp < 0 then
                  delete key filter (Left node :: path) left0
               else
                  delete key filter (Right node :: path) right0
       | Red (key0, data0, left0, right0, size0) ->
            let comp = Base.compare key key0 in
               if comp = 0 then
                  (* Dammit!  The filter needs to be applied here too!  -n8 *)
                  match filter data0 with
                     None ->
                        begin
                           (* This is all that was here before *)
                           match right0 with
                              Leaf ->
                                 lift (key0, data0) path Leaf
                            | _ ->
                                 delete_min (Delete node :: path) right0
                        end
                   | Some data0 ->
                        restore path (Red (key0, data0, left0, right0, size0))
               else if comp < 0 then
                  delete key filter (Left node :: path) left0
               else
                  delete key filter (Right node :: path) right0
       | Leaf ->
            raise Not_found

   and restore path node =
      match path with
         Left (Black (key0, data0, _, right0, size0)) :: path ->
            restore path (Black (key0, data0, node, right0, size0))
       | Left (Red (key0, data0, _, right0, size0)) :: path ->
            restore path (Red (key0, data0, node, right0, size0))
       | Right (Black (key0, data0, left0, _, size0)) :: path ->
            restore path (Black (key0, data0, left0, node, size0))
       | Right (Red (key0, data0, left0, _, size0)) :: path ->
            restore path (Red (key0, data0, left0, node, size0))
       | [] ->
            (* JYH: seems like we spend all our time checking
            check node     (* n8 debugging *)
             *)
            node
       | Left Leaf :: _
       | Right Leaf :: _
       | Delete _ :: _ ->
            raise (Invalid_argument "restore")

   and delete_min path node =
      match node with
         Black (key0, data0, Leaf, Leaf, _) ->
            lift_black (key0, data0) path Leaf
       | Black (key0, data0, Leaf, Red (key2, data2, left2, right2, size2), _) ->
            lift (key0, data0) path (Black (key2, data2, left2, right2, size2))
       | Red (key0, data0, Leaf, Leaf, _) ->
            lift (key0, data0) path Leaf
       | Black (_, _, left0, _, _) ->
            delete_min (Left node :: path) left0
       | Red (_, _, left0, _, _) ->
            delete_min (Left node :: path) left0
       | Leaf ->
            raise Not_found

   (*
    * Copy the tree with no need to propagate black.
    *)
   and lift key path node =
      match path, node with
         Left (Black (key0, data0, _, right0, size0)) :: path, left ->
            lift key path (Black (key0, data0, left, right0, pred size0))
       | Left (Red (key0, data0, _, right0, size0)) :: path, left ->
            lift key path (Red (key0, data0, left, right0, pred size0))
       | Right (Black (key0, data0, left0, _, size0)) :: path, right ->
            lift key path (Black (key0, data0, left0, right, pred size0))
       | Right (Red (key0, data0, left0, _, size0)) :: path, right ->
            lift key path (Red (key0, data0, left0, right, pred size0))
       | Delete (Black (_, _, left0, _, size0)) :: path, right ->
            let key0, data0 = key in
               lift key path (Black (key0, data0, left0, right, pred size0))
       | Delete (Red (_, _, left0, _, size0)) :: path, right ->
            let key0, data0 = key in
               lift key path (Red (key0, data0, left0, right, pred size0))
       | [], node ->
            node
       | Left Leaf :: _, _
       | Right Leaf :: _, _
       | Delete Leaf :: _, _ ->
            raise (Invalid_argument "lift")

   (*
    * Propagate the extra black up the tree.
    *)
   and lift_black key path node =
      match path, node with
         Left (Black (key0, data0, _, right0, size0)) :: path, left ->
            begin
               match right0 with
                  Black (key2, data2, left2, right2, size2) ->
                     begin
                        match left2, right2 with
                           _, Red (key3, data3, left3, right3, size3) ->
                              (*
                               *    key0:b                 key2:b
                               *   /     \                 /    \
                               * left:bb key2:b          key0:b right2:b
                               *         /   \           /    \
                               *      left2  right2:r  left:b left2
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_black key0 data0 left left2,
                                         Black (key3, data3, left3, right3, size3),
                                         pred size0))

                         | Red (key3, data3, left3, right3, _), _ ->
                              (*
                               *      key0:b                    key3:b
                               *      /    \                  /       \
                               *   left:bb key2:b          key0:b     key2:b
                               *           /    \          /    \     /     \
                               *        key3:r right2:b left:b left3 right3 right2:b
                               *        /   \
                               *     left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_black key0 data0 left left3,
                                         new_black key2 data2 right3 right2,
                                         pred size0))

                         | _ ->
                              (*
                               *     key0:b                 key0:bb
                               *     /    \                 /    \
                               * left:bb  key2:b       left:b    key2:r
                               *          /    \                 /    \
                               *       left2:b right2:b       left2:b right2:b
                               *)
                              lift_black key path (**)
                                 (Black (key0,
                                         data0,
                                         left,
                                         Red (key2, data2, left2, right2, size2),
                                         pred size0))
                     end

                | Red (key2, data2, left2, right2, _) ->
                     begin
                        match left2 with
                           Black (key3, data3, Red (key4, data4, left4, right4, _), d, _) ->
                              (*
                               *     key0:b                   key2:b
                               *     /    \                   /    \
                               *  left:bb key2:r           key4:r right2:b
                               *          /    \           /     \
                               *        key3:b right2:b  key0:b   key3:b
                               *        /   \           /   \    /     \
                               *     key4:r  d     left:b left4 right4 d
                               *     /   \
                               *   left4 right4
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_red key4 data4 (**)
                                            (new_black key0 data0 left left4)
                                            (new_black key3 data3 right4 d),
                                         right2,
                                         pred size0))

                         | Black (key3, data3, c, Red (key4, data4, left4, right4, size4), _) ->
                              (*
                               *     key0:b                   key2:b
                               *     /    \                   /    \
                               * left:bb  key2:r            key3:r right2
                               *          /    \            /    \
                               *       key3:b  right2     key0:b key4:b
                               *       /    \             /    \
                               *      c    key4:r       left:b  c
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_red key3 data3 (**)
                                            (new_black key0 data0 left c)
                                            (Black (key4, data4, left4, right4, size4)),
                                            right2,
                                            pred size0))

                         | Black (key3, data3, c, d, _) ->
                              (*
                               *     key0:b               key2:b
                               *     /    \               /     \
                               * left:bb key2:r         key0:b  right2:b
                               *         /   \          /     \
                               *      key3:b right2:b left:b  key3:r
                               *      /   \                   /   \
                               *     c:b  d:b                c:b   d:b
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_black key0 data0 left (new_red key3 data3 c d),
                                         right2,
                                         pred size0))

                         | Red _
                         | Leaf ->
                              raise (Invalid_argument "lift_black1")
                     end

                | Leaf ->
                     raise (Invalid_argument "lift_black2")
            end

       | Right (Black (key0, data0, left0, _, size0)) :: path, right ->
            begin
               match left0 with
                  Black (key1, data1, left1, right1, size1) ->
                     begin
                        match left1, right1 with
                           Red (key3, data3, left3, right3, size3), _ ->
                              (*
                               *        key0:b              key1:b
                               *        /    \              /    \
                               *      key1:b right:bb   left1:b key0:b
                               *      /    \                    /    \
                               *  left1:r right1            right1   right:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         Black (key3, data3, left3, right3, size3),
                                         new_black key0 data0 right1 right,
                                         pred size0))

                         | _, Red (key3, data3, left3, right3, _) ->
                              (*
                               *      key0:b                    key3:b
                               *      /     \                 /        \
                               *    key1:b  right:bb        key1:b     key0:b
                               *    /    \                 /    \      /    \
                               * left1:b key3:r         left1:b left3 right3 right
                               *         /    \
                               *       left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_black key1 data1 left1 left3,
                                         new_black key0 data0 right3 right,
                                         pred size0))

                         | _ ->
                              (*
                               *        key0:b                 key0:bb
                               *        /    \                 /    \
                               *    key1:b  right:bb      key1:r    right:bb
                               *    /    \                /    \
                               * left1:b right1:b     left1:b right1:b
                               *)
                              lift_black key path (**)
                                 (Black (key0,
                                         data0,
                                         Red (key1, data1, left1, right1, size1),
                                         right,
                                         pred size0))

                     end

                | Red (key1, data1, left1, right1, _) ->
                     begin
                        match right1 with
                           Black (key3, data3, d, Red (key4, data4, left4, right4, _), _) ->
                              (*
                               *        key0:b                key1:b
                               *        /     \               /    \
                               *    key1:r   right:bb    left1:b  key4:r
                               *    /    \                        /    \
                               * left1:b key3:b              key3:b    key0:b
                               *         /   \               /   \     /    \
                               *        d    key4:r         d  left4 right4 right:b
                               *             /   \
                               *          left4 right4
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         left1,
                                         new_red key4 data4 (**)
                                            (new_black key3 data3 d left4)
                                            (new_black key0 data0 right4 right),
                                            pred size0))

                         | Black (key3, data3, Red (key4, data4, left4, right4, size4), c, _) ->
                              (*
                               *     key0:b                 key1:b
                               *     /    \                 /    \
                               *  key1:r  right:bb       left1  key3:r
                               *  /    \                        /    \
                               * left1 key3:b                 key4:b key0:b
                               *       /   \                         /   \
                               *    key4:r c                        c   right:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         left1,
                                         new_red key3 data3 (**)
                                            (Black (key4, data4, left4, right4, size4))
                                            (new_black key0 data0 c right),
                                            pred size0))

                         | Black (key3, data3, c, d, size3) ->
                              (*
                               *      key0:b               key1:b
                               *      /    \               /    \
                               *   key1:r  right:bb     left1  key0:b
                               *   /   \                       /    \
                               * left1 key3:b               key3:r right:b
                               *       /   \                /    \
                               *      c:b  d:b            c:b    d:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         left1,
                                         new_black key0 data0 (Red (key3, data3, c, d, size3)) right,
                                         pred size0))


                         | Red _
                         | Leaf ->
                              raise (Invalid_argument "lift_black3")
                     end

                | Leaf ->
                     raise (Invalid_argument "lift_black4")
            end

       | Left (Red (key0, data0, _, right0, size0)) :: path, left ->
            begin
               match right0 with
                  Black (key2, data2, left2, right2, size2) ->
                     begin
                        match left2, right2 with
                           _, Red (key3, data3, left3, right3, size3) ->
                              (*
                               *     key0:r                   key2:r
                               *     /    \                   /    \
                               *  left:bb key2:b           key0:b right2:b
                               *          /    \           /    \
                               *       left2:b right2:r left:b left2:b
                               *)
                              lift key path (**)
                                 (Red (key2,
                                       data2,
                                       new_black key0 data0 left left2,
                                       Black (key3, data3, left3, right3, size3),
                                       pred size0))

                         | Red (key3, data3, left3, right3, _), _ ->
                              (*
                               *     key0:r                   key3:b
                               *     /    \                  /       \
                               * left:bb  key2:b          key0:r     key2:r
                               *          /    \         /   \       /    \
                               *        key3:r right2 left:b left3 right3 right2
                               *        /   \
                               *     left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_red key0 data0 left left3,
                                         new_red key2 data2 right3 right2,
                                         pred size0))

                         | _ ->
                              (*
                               *     key0:r                  key0:b
                               *    /     \                  /    \
                               * left:bb  key2:b          left:b key2:r
                               *          /    \                 /   \
                               *     left2:b  right2:b      left2:b right2:b
                               *)
                              lift key path (**)
                                 (Black (key0,
                                         data0,
                                         left,
                                         Red (key2, data2, left2, right2, size2),
                                         pred size0))
                     end
                | Red _
                | Leaf ->
                     raise (Invalid_argument "lift_black5")
            end

       | Right (Red (key0, data0, left0, _, size0)) :: path, right ->
            begin
               match left0 with
                  Black (key1, data1, left1, right1, size1) ->
                     begin
                        match left1, right1 with
                           Red (key3, data3, left3, right3, size3), _ ->
                              (*
                               *       key0:r                key1:r
                               *       /    \                /    \
                               *    key1:b  right:bb      left1:b key0:b
                               *   /     \                        /    \
                               * left1:r right1                right1 right:b
                               *)
                              lift key path (**)
                                 (Red (key1,
                                       data1,
                                       Black (key3, data3, left3, right3, size3),
                                       new_black key0 data0 right1 right,
                                       pred size0))

                         | _, Red (key3, data3, left3, right3, _) ->
                              (*
                               *       key0:r                 key3:b
                               *       /    \                /       \
                               *     key1:b right:bb      key1:r    key0:r
                               *     /    \               /    \    /    \
                               *  left1  key3:r        left1 left3 right3 right:b
                               *         /    \
                               *       left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_red key1 data1 left1 left3,
                                         new_red key0 data0 right3 right,
                                         pred size0))

                         | _ ->
                              (*
                               *        key0:r              key0:b
                               *        /    \              /    \
                               *     key1:b right:bb     key1:r right:b
                               *     /   \               /    \
                               * left1:b right1:b     left1:b right1:b
                               *)
                              lift key path (**)
                                 (Black (key0,
                                         data0,
                                         Red (key1, data1, left1, right1, size1),
                                         right,
                                         pred size0))

                     end
                | Red _
                | Leaf ->
                     raise (Invalid_argument "lift_black6")
            end

       | Delete (Black (_, _, left0, right0, size0)) :: path, node ->
            let key0, data0 = key in
               lift_black key (Right (Black (key0, data0, left0, right0, size0)) :: path) node

       | Delete (Red (_, _, left0, right0, size0)) :: path, node ->
            let key0, data0 = key in
               lift_black key (Right (Red (key0, data0, left0, right0, size0)) :: path) node

       | [], node ->
            node

       | Left Leaf :: _, _
       | Right Leaf :: _, _
       | Delete Leaf :: _, _ ->
            raise (Invalid_argument "lift_black7")

   (*
    * Remove the item.
    *)
   let remove tree key =
      try delete key (fun _ -> None) [] tree with
         Not_found ->
            tree

   let filter_remove tree key filter =
      delete key filter [] tree

   (************************************************************************
    * UNION & INTERSECTION                                                 *
    ************************************************************************)

   (*
    * Get the smallest element.
    *)
   let rec choose = function
      Black (key, data, Leaf, _, _)
    | Red (key, data, Leaf, _, _) ->
         key, data
    | Black (_, _, left, _, _)
    | Red (_, _, left, _, _) ->
         choose left
    | Leaf ->
         raise Not_found

   (*
    * Get the elements of the list.
    *)
   let rec to_list_aux elements = function
      Black (key, data, left, right, _)
    | Red (key, data, left, right, _)  ->
         to_list_aux ((key, data) :: to_list_aux elements right) left
    | Leaf ->
         elements

   let to_list tree =
      to_list_aux [] tree

   let elements = to_list

   let rec keys_aux elements = function
      Black (key, _, left, right, _)
    | Red (key, _, left, right, _)  ->
         keys_aux (key :: keys_aux elements right) left
    | Leaf ->
         elements

   let keys = keys_aux []

   let rec reverse elements = function
      h :: t ->
         reverse (h :: elements) t
    | [] ->
         elements

   let rec merge elements elements1 elements2 =
      match elements1, elements2 with
         ((key1, data1) as hd1) :: tl1, ((key2, data2) as hd2) :: tl2 ->
            let comp = Base.compare key1 key2 in
               if comp = 0 then
                  merge ((key1, data1 @ data2) :: elements) tl1 tl2
               else if comp < 0 then
                  merge (hd1 :: elements) tl1 elements2
               else
                  merge (hd2 :: elements) elements1 tl2
       | _, [] ->
            reverse elements1 elements
       | [], _ ->
            reverse elements2 elements

   (*
    * Log of a number.
    *)
   let rec log2 i x =
      if 1 lsl i >= x then
         i
      else
         log2 (succ i) x

   (*
    * Build a set from a list.
    *)
   let rec log2 i j =
      if 1 lsl i >= j then
         i
      else
         log2 (succ i) j

   let rec of_array depth max_depth elements off len =
      if len = 1 then
         let key, data = elements.(off) in
            if depth = max_depth then
               Red (key, data, Leaf, Leaf, 1)
            else
               Black (key, data, Leaf, Leaf, 1)
      else if len = 2 then
         let key1, data1 = elements.(off) in
         let key0, data0 = elements.(succ off) in
            Black (key0, data0, Red (key1, data1, Leaf, Leaf, 1), Leaf, 2)
      else
         let len2 = len lsr 1 in
         let key0, data0 = elements.(off + len2) in
            Black (key0, data0,
                   of_array (succ depth) max_depth elements off len2,
                   of_array (succ depth) max_depth elements (off + len2 + 1) (len - len2 - 1),
                   len)

   let of_list elements =
      match elements with
         [] ->
            Leaf
       | [key, data] ->
            Black (key, data, Leaf, Leaf, 1)
       | elements ->
            let elements = Array.of_list elements in
            let length = Array.length elements in
            let max_depth = pred (log2 1 (succ length)) in
               of_array 0 max_depth elements 0 length

   (*
    * Union flattens the two trees,
    * merges them, then creates a new tree.
    *)
   let union_append (append : 'elt -> 'data -> 'data -> 'data)
                    (s : ('elt, 'data) tree)
                    (key : 'elt)
                    (data : 'data) =
      filter_add s key (function
         None ->
            data
       | Some data' ->
            append key data' data)

   let rec union_aux (append : 'elt -> 'data -> 'data -> 'data)
                     (s1 : ('elt, 'data) tree)
                     (s2 : ('elt, 'data) tree) =
      match s2 with
         Black (key, data, left, right, _)
       | Red (key, data, left, right, _) ->
            union_aux append (union_append append (union_aux append s1 left) key data) right
       | Leaf ->
            s1

   let union append s1 s2 =
      let size1 = cardinality s1 in
      let size2 = cardinality s2 in
         if size1 < size2 then
            union_aux append s2 s1
         else
            union_aux append s1 s2

   (*
    * See if two sets intersect.
    *)
   let rec intersect_aux elems1 elems2 =
      match elems1, elems2 with
         elem1 :: elems1', elem2 :: elems2' ->
            let comp = Base.compare elem1 elem2 in
               if comp = 0 then
                  true
               else if comp < 0 then
                  intersect_aux elems1' elems2
               else
                  intersect_aux elems1 elems2'
       | [], _
       | _, [] ->
            false

   let intersectp s1 s2 =
      intersect_aux (keys s1) (keys s2)

   (*
    * Equality of sets.
    *)
   let equal eq set1 set2 =
      if cardinality set1 = cardinality set2 then
         let list1 = to_list set1 in
         let list2 = to_list set2 in
            List.for_all2 (fun (x1, x2) (y1, y2) -> Base.compare x1 y1 = 0 && eq x2 y2) list1 list2
      else
         false

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Search without reorganizing the tree.
    *)
   let rec mem tree key =
      match tree with
         Black (key', _, left, right, _)
       | Red (key', _, left, right, _) ->
            let comp = Base.compare key key' in
               if comp = 0 then
                  true
               else if comp < 0 then
                  mem left key
               else
                  mem right key

       | Leaf ->
            false

   let rec find_key tree key =
      match tree with
         Black (key', _, left, right, _)
       | Red (key', _, left, right, _) ->
            let comp = Base.compare key key' in
               if comp = 0 then
                  Some key'
               else if comp < 0 then
                  find_key left key
               else
                  find_key right key

       | Leaf ->
            None

   (*
    * An empty tree is just a leaf.
    *)
   let empty = Leaf

   let is_empty = function
      Leaf ->
         true
    | Red _
    | Black _ ->
         false

   let make key data =
      Black (key, data, Leaf, Leaf, 1)

   (*
    * Iterate a function over the hashtable.
    *)
   let rec iter f = function
      Black (key, data, left, right, _)
    | Red (key, data, left, right, _) ->
         iter f left;
         f key data;
         iter f right
    | Leaf ->
         ()

   let iter_all = iter

   let rec map f = function
      Black (key, data, left, right, size) ->
         let left = map f left in
         let data = f data in
         let right = map f right in
            Black (key, data, left, right, size)
      | Red (key, data, left, right, size) ->
         let left = map f left in
         let data = f data in
         let right = map f right in
            Red (key, data, left, right, size)
      | Leaf ->
           Leaf

   let rec mapi f = function
      Black (key, data, left, right, size) ->
         let left = mapi f left in
         let data = f key data in
         let right = mapi f right in
            Black (key, data, left, right, size)
      | Red (key, data, left, right, size) ->
         let left = mapi f left in
         let data = f key data in
         let right = mapi f right in
            Red (key, data, left, right, size)
      | Leaf ->
           Leaf

   let rec mapi_all f = function
      Black (key, data, left, right, size) ->
         let left = mapi_all f left in
         let data = f key data in
         let right = mapi_all f right in
            Black (key, data, left, right, size)
      | Red (key, data, left, right, size) ->
         let left = mapi_all f left in
         let data = f key data in
         let right = mapi_all f right in
            Red (key, data, left, right, size)
      | Leaf ->
           Leaf

   let rec fold f arg = function
       Black (key, data, left, right, _)
     | Red (key, data, left, right, _) ->
          let arg = fold f arg left in
          let arg = f arg key data in
             fold f arg right
     | Leaf ->
          arg

   let fold_all = fold

   let rec fold_map f arg = function
      Black (key, data, left, right, size) ->
         let arg, left = fold_map f arg left in
         let arg, data = f arg key data in
         let arg, right = fold_map f arg right in
            arg, Black (key, data, left, right, size)
    | Red (key, data, left, right, size) ->
         let arg, left = fold_map f arg left in
         let arg, data = f arg key data in
         let arg, right = fold_map f arg right in
            arg, Red (key, data, left, right, size)
    | Leaf ->
         arg, Leaf

   let forall2 cmp t1 t2 =
      (cardinal t1 = cardinal t2)
      && List.for_all2 (fun (x1, e1) (x2, e2) ->
            Base.compare x1 x2 = 0 && cmp e1 e2) (to_list t1) (to_list t2)

   let rec forall cmp t =
      match t with
         Black (key, data, left, right, _)
       | Red (key, data, left, right, _) ->
            cmp key data && forall cmp left && forall cmp right
       | Leaf ->
            true

   let rec exists cmp t =
      match t with
         Black (key, data, left, right, _)
       | Red (key, data, left, right, _) ->
            cmp key data || exists cmp left || exists cmp right
       | Leaf ->
            false

   let rec find_iter cmp t =
      match t with
         Black (key, data, left, right, _)
       | Red (key, data, left, right, _) ->
            let x = cmp key data in
               (match x with
                   Some _ ->
                      x
                 | None ->
                      let x = find_iter cmp left in
                         match x with
                            Some _ ->
                               x
                          | None ->
                               find_iter cmp right)
       | Leaf ->
            None

   let isect_mem t test =
      fold (fun t' v x ->
            if test v then
               add t' v x
            else
               t') empty t

   let rec keys_acc t acc =
      match t with
         Black (key,_,left,right,_)
       | Red (key,_,left,right,_) ->
         keys_acc left (key::keys_acc right acc)
       | Leaf -> acc

   let keys t = keys_acc t []

   let rec data_acc t acc =
      match t with
         Black (_,data,left,right,_)
       | Red (_,data,left,right,_) ->
         data_acc left (data::data_acc right acc)
       | Leaf -> acc

   let data t = data_acc t []

   let rec add_list map = function
      [] -> map
    | (key,data)::tl -> add_list (add map key data) tl
end

(*
 * Recursive version.
 *)
module LmMakeRec = LmMake

(*
 * List version.
 *)
module LmMakeList (Ord : OrderedType) =
struct
   module MMap = LmMake (Ord)

   type key = Ord.t
   type 'a t = 'a list MMap.t

   let empty = MMap.empty

   let is_empty = MMap.is_empty

   let cardinal = MMap.cardinal

   let filter_add t key f =
      MMap.filter_add t key (function
         Some (h :: t) -> f (Some h) :: t
       | Some [] | None -> [f None])

   let filter_remove t key f =
      MMap.filter_remove t key (function
         h :: t ->
           (match f h with
               None -> None
             | Some h -> Some (h :: t))
       | [] -> None)

   let replace t key f =
      MMap.filter_add t key (function
         Some (h :: t) -> f h :: t
       | Some [] | None -> raise Not_found)

   let add t key x =
      MMap.filter_add t key (function
         Some l -> x :: l
       | None -> [x])

   let find t key =
      match MMap.find t key with
         [] -> raise Not_found
       | h :: _ -> h

   let remove = MMap.remove

   let mem = MMap.mem

   let find_key = MMap.find_key

   let iter f t =
      MMap.iter (fun i l ->
            match l with
               [] -> ()
             | h :: _ -> f i h) t

   let map f t =
      MMap.map (function
         [] -> []
       | h :: _ -> [f h]) t

   let mapi f t =
      MMap.mapi (fun i l ->
            match l with
               [] -> []
             | h :: _ -> [f i h]) t

   let fold f =
      MMap.fold (fun x key -> function
          [] -> x
       | h :: _ -> f x key h)

   let fold_map f x t =
      MMap.fold_map (fun x key data ->
            List.fold_left (fun (x, data) d ->
                  let x, d = f x key d in
                  let data = d :: data in
                     x, data) (x, []) data) x t

   let iter_all = MMap.iter
   let mapi_all = MMap.mapi
   let fold_all = MMap.fold
   let data_all = MMap.data

   let rec add_list map = function
      [] -> map
    | (key,data)::tl -> add_list (add map key data) tl

   let choose t =
      let key, data = MMap.choose t in
         match data with
            [] ->
               raise Not_found
          | h :: _ ->
               key, h

   let choose_all = MMap.choose

   (*
    * find_all is a total function.
    * find_all_partial fails if the element is not in the table.
    *)
   let find_all_partial = MMap.find
   let find_all = MMap.find_all

   let filter t key f = (* table_ext v_ext search *)
      try
         MMap.filter_remove t key (fun l ->
               match f l with
                  [] ->
                     None
                | l ->
                     Some l)
      with
         Not_found ->
            match f [] with
               [] ->
                  t
             | l ->
                  MMap.add t key l

   let forall2 cmp t1 t2 =
      let cmp l1 l2 =
         let len1 = List.length l1 in
         let len2 = List.length l2 in
            len1 = len2 && List.for_all2 cmp l1 l2
      in
         MMap.forall2 cmp t1 t2

   let forall cmp t =
      let cmp_list key l =
         List.for_all (fun x -> cmp key x) l
      in
         MMap.forall cmp_list t

   let exists cmp t =
      let cmp_list key l =
         List.exists (fun x -> cmp key x) l
      in
         MMap.exists cmp_list t

   let rec find_iter cmp t =
      let rec cmp_list key l =
         match l with
            h :: t ->
               let x = cmp key h in
                  (match x with
                      Some _ ->
                         x
                    | None ->
                         cmp_list key t)
          | [] ->
               None
      in
         MMap.find_iter cmp_list t

   let isect_mem t test =
      MMap.isect_mem t test

   let keys t = MMap.keys t

   let data t = List.concat (MMap.data t)

   let union = MMap.union

   let equal eq set1 set2 =
      if MMap.cardinality set1 = MMap.cardinality set2 then
         let list1 = MMap.to_list set1 in
         let list2 = MMap.to_list set2 in
            List.for_all2 (fun (_x1, x2) (_y1, y2) ->
                  List.length x2 = List.length y2 && List.for_all2 eq x2 y2) list1 list2
      else
         false
end

(*
 * Backwards-compatible version.
 *)
module Make (Ord : OrderedType) =
struct
   module XMap = LmMake (Ord)

   type 'a t = 'a XMap.t
   type key = XMap.key

   let empty = XMap.empty

   let add key data tbl =
      XMap.add tbl key data

   let find key tbl =
      XMap.find tbl key

   let remove key tbl =
      XMap.remove tbl key

   let mem key tbl =
      XMap.mem tbl key

   let iter = XMap.iter

   let map = XMap.map

   let mapi = XMap.mapi

   let fold f tbl x =
      XMap.fold (fun x key data -> f key data x) x tbl
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
