(*
 * Extra utilities for lists.
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

(*
 * Exception for operations that have no effect.
 *)
exception Unchanged

(*
 * Array-style operations.
 *)
let sub =
   let rec skip l off len =
      if off = 0 then
         collect [] l len
      else
         match l with
            _ :: l ->
               skip l (off - 1) len
          | [] ->
               raise (Invalid_argument "Lm_list_util.sub")
   and collect l1 l2 len =
      if len = 0 then
         List.rev l1
      else
         match l2 with
            x :: l2 ->
               collect (x :: l1) l2 (len - 1)
          | [] ->
               raise (Invalid_argument "Lm_list_util.sub")
   in
      skip

(*
 * Iterate over two lists, but stop at the end of the
 * shorter one.
 *)
let rec short_iter2 f l1 l2 =
   match l1, l2 with
      h1 :: l1, h2 :: l2 ->
         f h1 h2;
         short_iter2 f l1 l2
    | _ ->
         ()

let rec iter3 f l1 l2 l3 =
   match l1, l2, l3 with
      h1 :: l1, h2 :: l2, h3 :: l3 ->
         f h1 h2 h3;
         iter3 f l1 l2 l3
    | [], [], [] ->
         ()
    | _ ->
         raise (Invalid_argument "iter3")

(*
 * Filter items out of a list.
 *)
let rec filter f = function
   [] -> []
 | (h::t) as l ->
      if f h then
         let rem = filter f t in
            if rem == t then
               l
            else
               h::rem
      else
         filter f t

(*
 * Insert an element into a position.
 *)
let rec insert_nth i x l =
   if i = 0 then
      x :: l
   else
      match l with
         h::t ->
            h :: insert_nth (i - 1) x t
       | [] ->
            raise (Failure "Lm_list_util.insert_nth")

(*
 * Remove an element from a position.
 *)
let rec remove_nth i l =
   match l with
      x :: l ->
         if i = 0 then
            l
         else
            x :: remove_nth (pred i) l
    | [] ->
         raise (Failure "Lm_list_util.remove_nth")

(*
 * Work left-to-right, but reverse the result.
 *)
let rec rev_map' f l = function
   h :: t ->
      rev_map' f (f h :: l) t
 | [] ->
      l

let rev_map f l =
   rev_map' f [] l

(*
 * Reverse do_list.
 *)
let rec rev_iter f = function
   h::t ->
      rev_iter f t;
      ignore (f h)
 | [] ->
      ()

(*
 * Flat map.
 *)
let rec flat_map_aux f accum l = function
   h::t ->
      flat_map_aux f (h::accum) l t
 | [] ->
      begin match l with
         [] ->
            accum
       | h :: t ->
            flat_map_aux f accum t (f h)
      end

let flat_map f l =
   List.rev (flat_map_aux f [] l [])

(*
 * Map, and discard errors.
 *)
let rec fail_map f = function
   h::t ->
      begin
         try
            let h = f h in
               h :: fail_map f t
         with
            Failure _ ->
               fail_map f t
      end
 | [] ->
      []

(*
 * Map, and discard None.
 *)
let rec some_map_aux unchanged f = function
   h :: t ->
      begin
         match f h with
            Some h' ->
               h' :: some_map_aux (unchanged && h' == h) f t
          | None ->
               some_map_aux false f t
      end
 | [] ->
      if unchanged then
         raise Unchanged;
      []

let some_map_safe f l =
   try some_map_aux true f l with
      Unchanged ->
         l

let rec some_map f = function
   h :: t ->
      begin
         match f h with
            Some h' ->
               h' :: some_map f t
          | None ->
               some_map f t
      end
 | [] ->
      []

(*
 * Cross between map and fold_left.
 *)
let rec fold_left_aux f x l = function
   h :: t ->
      let x', h' = f x h in
         fold_left_aux f x' (h' :: l) t
 | [] ->
      x, List.rev l

let fold_left f x l = fold_left_aux f x [] l

(*
 * Generalize fold_left over three lists.
 *)
let rec fold_left3 f arg l1 l2 l3 =
   match l1, l2, l3 with
      h1 :: t1, h2 :: t2, h3 :: t3 ->
         fold_left3 f (f arg h1 h2 h3) t1 t2 t3
    | [], [], [] ->
         arg
    | _ ->
         raise (Invalid_argument "fold_left3")

(*
 * Find the elemnt.
 *)
let rec find f = function
   h::t ->
      if f h then
         h
      else
         find f t
 | [] ->
      raise Not_found

let rec find_item_aux f i = function
   h::t ->
      if f h then
         i
      else
         find_item_aux f (i + 1) t
 | [] ->
      raise Not_found

let find_item f l = find_item_aux f 0 l

let rec find_index_aux v i = function
   h::t ->
      if h = v then
         i
      else
         find_index_aux v (i + 1) t
 | [] ->
      raise Not_found

let find_index v l = find_index_aux v 0 l

let rec fild_rindex_aux v i curr = function
   [] -> i
 | h :: t ->
      fild_rindex_aux v (if h=v then curr else i) (curr + 1) t

let find_rindex v l =
   let i = fild_rindex_aux v (-1) 0 l in
      if i>=0 then i else raise Not_found

let rec find_indexq_aux v i = function
   h::t ->
      if h == v then
         i
      else
         find_indexq_aux v (i + 1) t
 | [] ->
      raise Not_found

let find_indexq v l = find_indexq_aux v 0 l

(*
 * Find the index of an element in a list.
 *)
let find_index_eq eq x l =
   let rec search i = function
      h :: t ->
         if eq x h then
            i
         else
            search (succ i) t
    | [] ->
         raise Not_found
   in
      search 0 l

(*
 * Split a list.
 *)
let rec split_list i l = match (i,l) with
   0, _ ->
      [], l
 | _, h::t ->
      let l, l' = split_list (i - 1) t in
         h::l, l'
 | _, [] ->
      raise (Failure "Lm_list_util.split_list")

(*
 * Split off the last item.
 *)
let rec split_last = function
   [h] ->
      [], h
 | h::t ->
      let l, x = split_last t in
         h::l, x
 | [] ->
      raise (Failure "Lm_list_util.split_last")

(*
 * Split based on an index.
 *)
let rec split i l =
   if i = 0 then
      [], l
   else
      match l with
         h :: t ->
            let l1, l2 = split (pred i) t in
               h :: l1, l2
       | [] ->
            raise (Invalid_argument "split")

(*
 * Split into fragments.
 *)
let splitup n l =
   let rec split i vll vl1 vl2 =
      match vl2 with
         v :: vl2 ->
            if i = 0 then
               split n (List.rev vl1 :: vll) [v] vl2
            else
               split (pred i) vll (v :: vl1) vl2
       | [] ->
            List.rev vl1 :: vll
   in
      match l with
         [] ->
            []
       | h :: t ->
            split n [] [h] t

(*
 * Get the last element.
 *)
let rec last = function
   [x] -> x
 | _ :: t -> last t
 | [] -> raise (Invalid_argument "last")

(*
 * Return the first n elements of the list.
 *)
let rec firstn i l =
   if i = 0 then
      []
   else
      match l with
         h :: t ->
            h :: firstn (pred i) t
       | [] ->
            []

(*
 * Nth tail.
 *)
let rec nth_tl i l =
   if i = 0 then
      l
   else
      match l with
         _ :: l ->
            nth_tl (pred i) l
       | [] ->
            raise (Invalid_argument "nth_tl")

(*
 * Functional replacement.
 *)
let rec replacef_nth i f = function
   h::t ->
      if i = 0 then
         f h :: t
      else
         h :: replacef_nth (i - 1) f t
 | [] ->
      raise (Failure "Lm_list_util.replacef_nth")

let rec replacef_arg_nth i f = function
   h::t ->
      if i = 0 then
         let h, arg = f h in
            h :: t, arg
      else
         let t, arg = replacef_arg_nth (i - 1) f t in
            h :: t, arg
 | [] ->
      raise (Failure "Lm_list_util.replacef_arg_nth")

(*
 * Replace the nth element of the list.
 *)
let rec replace_nth i x l =
   match l with
      [] ->
         raise (Invalid_argument "replace_nth")
    | h :: l ->
         if i = 0 then
            x :: l
         else
            h :: replace_nth (pred i) x l

let rec replace_first f x = function
   h::t ->
      if f h then
         x :: t
      else
         h :: replace_first f x t
 | [] ->
      raise Not_found

let rec replace_all f x = function
   h::t ->
      (if f h then x else h) :: (replace_all f x t)
 | [] ->
      []

(*
 * Functional replacement.
 *)
let rec replaceq x1 x2 = function
   h::t ->
      if h == x1 then
         x2 :: replaceq x1 x2 t
      else
         h :: replaceq x1 x2 t
 | [] ->
      []

(*
 * Subtraction.
 *)
let rec subtract_list l1 l2 =
   match l1 with
      h :: l1 ->
         if List.mem h l2 then
            subtract_list l1 l2
         else
            h :: subtract_list l1 l2
    | [] ->
         []

(*
 * map2to1
 *)
let rec map2to1 f l1 l2 =
  match l1, l2 with
      [], [] -> []
    | h1::l1, h2::l2 ->
	f h1 h2 :: map2to1 f l1 l2
    | _ -> raise (Invalid_argument "map2to1")

(*
 * fold_left + map = fold_map
 *)
let rec fold_map f i l =
  match l with
      [] -> i, []
    | hd :: tl ->
	let i, hd = f i hd in
	let i, l = fold_map f i tl in
	  i, hd :: l

let rec fold_map2to1 f i l1 l2 =
  match l1, l2 with
      [], [] -> i, []
    | h1 :: l1, h2 :: l2 ->
	let i, h = f i h1 h2 in
	let i, l = fold_map2to1 f i l1 l2 in
	  i, h :: l
    | _ -> raise (Invalid_argument "fold_map2to1")

let rec fold_map1to2 f i l =
  match l with
      [] -> i, [], []
    | hd :: tl ->
	let i, hd1, hd2 = f i hd in
	let i, l1, l2 = fold_map1to2 f i tl in
	  i, hd1 :: l1, hd2 :: l2

(*
 * fold + filter = fold_filter
 *)
let rec fold_filter f x l =
  match l with
      [] -> x, []
    | h :: t ->
        let x, b = f x h in
          if b then
            let x, t = fold_filter f x t in
              x, h :: t
          else
            fold_filter f x t

(*
 * check list lengths equal
 *)
let rec length_eq l1 l2 =
  match l1, l2 with
      [], [] -> true
    | _ :: l1, _ :: l2 -> length_eq l1 l2
    | _ -> false

(*
 * Add an element based on physical equality.
 *)
let addq x l =
   if List.memq x l then
      l
   else
      x :: l

(*
 * Union of two lists using equality.
 *)
let rec union l1 l2 =
   match l1 with
      x :: l1 ->
         if List.mem x l2 then
            union l1 l2
         else
            x :: union l1 l2
    | [] ->
         l2

(*
 * Intersect two lists.
 * Quadratic algorithm.
 *)
let rec intersect l = function
   h::t ->
      if List.mem h l then
         h :: intersect l t
      else
         intersect l t
 | [] -> []

let rec intersectq l = function
   h::t ->
      if List.memq h l then
         h :: intersectq l t
      else
         intersectq l t
 | [] -> []

let rec intersects l = function
   h :: t ->
      List.mem h l or intersects l t
 | [] ->
      false

(*
 * Subtract an element from a list.
 * Quadratic algorithm.
 *)
let rec subtract l1 l2 =
   match l1 with
      h::t ->
         if List.mem h l2 then
            subtract t l2
         else
            h :: subtract t l2
    | [] ->
         []

(*
 * Subtract only the first occurrence.
 *)
let rec mem_once v head = function
   h :: t ->
      if v = h then
         Some (head @ t)
      else
         mem_once v (h :: head) t
 | [] ->
      None

let rec subtract_multiset l1 l2 =
   match l1 with
      h :: t ->
         begin
            match mem_once h [] l2 with
               Some l2 ->
                  subtract_multiset t l2
             | None ->
                  h :: subtract_multiset t l2
         end
    | [] ->
         []

(*
 * Subtract an element from a list.
 * Quadratic algorithm.
 *)
let rec subtractq l1 l2 =
   match l1 with
      h::t ->
         if List.memq h l2 then
            subtractq t l2
         else
            h :: subtractq t l2
    | [] ->
         []

(*
 * Union of lists by structural equality.
 *)
let rec union l = function
   h::t ->
      if List.mem h l then
         union l t
      else
         h::(union l t)
 | [] ->
      l

(*
 * Union of lists by physical equality.
 * The semantics are important:
 *    all the elements in the first argument that do not
 *    exist in the second argument are consed _in order_ onto
 *    the first argument.
 *)
let rec unionq h l =
   match h with
      h :: t ->
         if List.memq h l then
            unionq t l
         else
            h :: unionq t l
    | [] ->
         l

(*
 * The first list is a subset of the second one (based on structural equality)
 *)
let rec subset l1 l2 =
   match l1 with
      x :: l1 ->
         List.mem x l2 && subset l1 l2
    | [] ->
         true

(*
 * Remove marked elements.
 *)
let rec remove_elements l1 l2 =
   match l1, l2 with
      flag::ft, h::t ->
         if flag then
            remove_elements ft t
         else
            h :: remove_elements ft t
    | _, l ->
         l

let rec removeq x = function
   h::t ->
      if h == x then
         t
      else
         h :: removeq x t
 | [] ->
      raise (Failure "Lm_list_util.removeq")

let rec remove x = function
   h::t ->
      if h = x then
         t
      else
         h :: remove x t
 | [] ->
      raise (Failure "Lm_list_util.remove")

let rec tryremove x = function
   (h :: t) as l ->
      if h = x then
         t
      else
         let res = tryremove x t in
            if res == t then
               l
            else
               h :: res
 | [] ->
      []

(*
 * Remove the specified suffix from the list.
 *)
let rec remove_suffix_aux suffix = function
   (0, l') ->
      if l' = suffix then
         []
      else
         raise (Failure "Lm_list_util.remove_suffix")
 | (i, _::t) ->
      remove_suffix_aux suffix (i - 1, t)
 | _ ->
      (* This will never happen *)
      raise (Failure "Lm_list_util.remove_suffix")

let remove_suffix l suffix =
   let i = (List.length l) - (List.length suffix) in
      if i >= 0 then
         remove_suffix_aux suffix (i, l)
      else
         raise (Failure "Lm_list_util.remove_suffix")

(*
 * Compare two lists of things.
 *)
let rec compare_lists cmp l1 l2 =
   match (l1,l2) with
      h1::t1, h2::t2 ->
         let i = cmp h1 h2 in
            if i = 0 then
               compare_lists cmp t1 t2
            else
               i
    | [], [] -> 0
    | [], _ -> -1
    | _ -> 1

let rec compare_cmp cmp l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         cmp h1 h2 && compare_cmp cmp t1 t2
    | [], [] -> true
    | _ -> false

let rec compare_eq l1 l2 =
   match l1, l2 with
      h1::t1, h2::t2 ->
         h1 == h2 & compare_eq t1 t2
    | [], [] ->
         true
    | _ ->
         false

(*
 * Get the nth item.
 *)
let rec nth l i =
   if i <= 0 then
      raise (Failure "Lm_list_util.nth")
   else
      match l with
         _::t ->
            nth t (i - 1)
       | [] ->
            raise (Failure "Lm_list_util.nth")

(*
 * Map a function over two lists.
 *)
let rec map2 f l1 l2 = match (l1,l2) with
   h1::t1, h2::t2 ->
      let h = f h1 h2 in
         h :: map2 f t1 t2
 | [], [] -> []
 | _ -> raise (Failure "Lm_list_util.map2")

let rec iter2 f al bl =
   match (al, bl) with
      h1::t1, h2::t2 ->
         f h1 h2;
         iter2 f t1 t2
    | [], [] ->
         ()
    | _ ->
         raise (Failure "Lm_list_util.iter2")

let rec rev_iter2 f a b =
   match (a,b) with
      ([], []) -> ()
    | (ha::ta, hb::tb) -> rev_iter2 f ta tb; f ha hb
    | _ -> raise (Failure "Lm_list_util.rev_iter2")

(*
 * Test two lists.
 *)
let rec for_all2 f l1 l2 =
   match (l1,l2) with
      h1::t1, h2::t2 -> f h1 h2 & for_all2 f t1 t2
    | [], [] -> true
    | _ -> false

(*
 * Exists a pair in the two lists.
 *)
let rec exists2 f l1 l2 = match (l1,l2) with
   h1::t1, h2::t2 ->
      f h1 h2 or exists2 f t1 t2
 | _ -> false

(*
 * Fold left over two lists.
 *)
let rec fold_left2 f x al bl =
   match (al, bl) with
      (h1::t1, h2::t2) ->
         fold_left2 f (f x h1 h2) t1 t2
    | [], [] ->
         x
    | _ ->
         raise (Failure "Lm_list_util.fold_left2")

let rec smap f = function
   [] -> []
 | hd::tl as l ->
      let hd' = f hd in
      let tl' = smap f tl in
         if (hd == hd') && (tl == tl') then
            l
         else
            hd'::tl'

(************************************************************************
 * Association lists.
 *)

let rec zip_list l l1 l2 = match (l1,l2) with
   (h1::t1), (h2::t2) ->
      zip_list ((h1,h2)::l) t1 t2
 | [], [] ->
      l
 | _ -> raise (Failure "Lm_list_util.zip")

(*
 * Zip two lists. Same as List.combine, but raises Failure instead of Invalid_argument
 *)
let rec zip a b = match (a,b) with
   (h1::t1), (h2::t2) ->
      (h1, h2) :: zip t1 t2
 | [], [] ->
      []
 |
   _ -> raise (Failure "Lm_list_util.zip")

(*
 * Produce a list of first elements out of the list of pairs
 *)
let rec fst_split = function
   [] -> []
 | (a, _) :: tl -> a :: fst_split tl

(*
 * Apply a function to the key in an assoc list.
 *)
let rec apply_assoc v l f =
   match l with
      (v', k) as h :: t ->
         if v' = v then
            (v', f k) :: t
         else
            h :: apply_assoc v t f
    | [] ->
         raise Not_found

(*
 * Find index of association.
 *)
let rec assoc_index_aux a i = function
   (a', _)::t ->
      if a' = a then
         i
      else
         assoc_index_aux a (i + 1) t
 | [] -> raise Not_found

let assoc_index l a = assoc_index_aux a 0 l

(*
 * Replace an association, but preserve order.
 *)
let rec assoc_replace l a b = match l with
   (a', b')::t ->
      if a' = a then
         (a, b)::t
      else
         (a', b')::(assoc_replace t a b)
 | [] -> raise Not_found

(*
 * Add the association if it doesn't already exist.
 *)
let add_assoc (v1, v2) l =
   try
      let v2' = List.assoc v1 l in
         if v2 = v2' then
            l
         else
            raise (Failure "Lm_list_util.add_assoc")
   with
      Not_found -> (v1, v2)::l

(*
 * See if a value is in domain.
 *)
let rec assoc_in_dom eq y = function
   (y',_)::tl ->
      (eq y y') || (assoc_in_dom eq y tl)
 | [] ->
      false

(*
 * See if a value is in range.
 *)
let rec assoc_in_range eq y = function
   (_, y')::tl ->
      (eq y y') || (assoc_in_range eq y tl)
 | [] ->
      false

let rec assoc_append_replace_snd l v = function
   [] -> l
 | (v', _) :: tl -> (v', v) :: (assoc_append_replace_snd l v tl)

let rec check_assoc v v' = function
   [] -> v=v'
 | (v1,v2)::tl ->
      begin match v=v1, v'=v2 with
         true, true -> true
       | false, false -> check_assoc v v' tl
       | _ -> false
      end

let rec try_check_assoc v v' = function
   [] -> raise Not_found
 | (v1,v2)::tl ->
      begin match v=v1, v'=v2 with
         true, true -> true
       | false, false -> try_check_assoc v v' tl
       | _ -> false
      end

let rec try_assoc v = function
   [] -> v
 | (v1,v2)::tl ->
      if v1=v then v2 else try_assoc v tl

(*
 * Association list with an equality.
 *)
let rec assoc_eq eq x = function
   (x', y) :: t ->
      if eq x x' then
         y
      else
         assoc_eq eq x t
 | [] ->
      raise Not_found

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
