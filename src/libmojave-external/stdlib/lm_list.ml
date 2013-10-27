(*
 * List operations.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Mojave Group, Caltech
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

(************************************************************************
 * LIST OPERATIONS                                                      *
 ************************************************************************)

(*
 * Length of a list.
 *)
let rec length' i = function
   _ :: t ->
      length' (succ i) t
 | [] ->
      i

let length l = length' 0 l

(*
 * Destructors.
 *)
let hd = function
   h :: t ->
      h
 | [] ->
      raise (Failure "hd")

let tl = function
   h :: t ->
      t
 | [] ->
      raise (Failure "tl")

let rec nth l i =
   match l with
      h :: t ->
         if i = 0 then
            h
         else
            nth t (pred i)
    | [] ->
         raise (Failure "nth")

let rec rev' l = function
   h :: t ->
      rev' (h :: l) t
 | [] ->
      l

let rev l = rev' [] l

let append = (@)

let rec rev_append l l' =
   match l with
      h :: t ->
         rev_append t (h :: l')
    | [] ->
         l'

let rec concat = function
   h :: t ->
      h @ concat t
 | [] ->
      []

let flatten = concat

(************************************************************************
 * ITERATORS                                                            *
 ************************************************************************)

let rec iter f = function
   h :: t ->
      f h;
      iter f t
 | [] ->
      ()

let rec map f = function
   h :: t ->
      f h :: map f t
 | [] ->
      []

let rec rev_map' f l = function
   h :: t ->
      rev_map' f (f h :: l) t
 | [] ->
      l

let rev_map f l =
   rev_map' f [] l

let rec fold_left f x = function
   h :: t ->
      fold_left f (f x h) t
 | [] ->
      x

let rec fold_right f l x =
   match l with
      h :: t ->
         f h (fold_right f t x)
    | [] ->
         x

(************************************************************************
 * ITERATORS ON TWO LISTS                                               *
 ************************************************************************)

let rec iter2 f l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         f h1 h2;
         iter2 f t1 t2
    | [], [] ->
         ()
    | _ ->
         raise (Invalid_argument "iter2")

let rec map2 f l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         f h1 h2 :: map2 f t1 t2
    | [], [] ->
         []
    | _ ->
         raise (Invalid_argument "map2")

let rec rev_map2 f l l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         rev_map2 f (f h1 h2 :: l) t1 t2
    | [], [] ->
         l
    | _ ->
         raise (Invalid_argument "rev_map2")

let rev_map2 f l1 l2 =
   rev_map2 f [] l1 l2

let rec fold_left2 f x l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         fold_left2 f (f x h1 h2) t1 t2
    | [], [] ->
         x
    | _ ->
         raise (Invalid_argument "fold_left2")

let rec fold_right2 f l1 l2 x =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         f h1 h2 (fold_right2 f t1 t2 x)
    | [], [] ->
         x
    | _ ->
         raise (Invalid_argument "fold_right2")

(************************************************************************
 * LIST SCANNING                                                        *
 ************************************************************************)

let rec for_all f = function
   h :: t ->
      f h && for_all f t
 | [] ->
      true

let rec exists f = function
   h :: t ->
      f h || exists f t
 | [] ->
      false

let rec for_all2 f l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         f h1 h2 && for_all2 f t1 t2
    | [], [] ->
         true
    | _ ->
         raise (Invalid_argument "for_all2")

let rec exists2 f l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         f h1 h2 || exists2 f t1 t2
    | [], [] ->
         false
    | _ ->
         raise (Invalid_argument "exists2")

let rec mem x = function
   h :: t ->
      x = h || mem x t
 | [] ->
      false

let rec memq x = function
   h :: t ->
      x == h || memq x t
 | [] ->
      false

(************************************************************************
 * LIST SEARCHING                                                       *
 ************************************************************************)

let rec find f = function
   h :: t ->
      if f h then
         h
      else
         find f t
 | [] ->
      raise Not_found

let rec filter f = function
   h :: t ->
      if f h then
         h :: filter f t
      else
         filter f t
 | [] ->
      []

let find_all = filter

let rec partition f = function
   h :: t ->
      let l1, l2 = partition f t in
         if f h then
            h :: l1, l2
         else
            l1, h :: l2
 | [] ->
      [], []

(************************************************************************
 * ASSOCIATION LISTS                                                    *
 ************************************************************************)

let rec assoc x = function
   (x', y) :: t ->
      if x = x' then
         y
      else
         assoc x t
 | [] ->
      raise Not_found


let rec assq x = function
   (x', y) :: t ->
      if x == x' then
         y
      else
         assq x t
 | [] ->
      raise Not_found

let rec mem_assoc x = function
   (x', _) :: t ->
      x = x' || mem_assoc x t
 | [] ->
      false

let rec mem_assq x = function
   (x', _) :: t ->
      x == x' || mem_assq x t
 | [] ->
      false

let rec remove_assoc x = function
   (x', _) as h :: t ->
      if x = x' then
         t
      else
         h :: remove_assoc x t
 | [] ->
      []

let rec remove_assq x = function
   (x', _) as h :: t ->
      if x == x' then
         t
      else
         h :: remove_assq x t
 | [] ->
      []

(************************************************************************
 * LISTS OF PAIRS                                                       *
 ************************************************************************)

let rec split = function
   (x, y) :: t ->
      let l1, l2 = split t in
         x :: l1, y :: l2
 | [] ->
      [], []

let rec combine l1 l2 =
   match l1, l2 with
      h1 :: t1, h2 :: t2 ->
         (h1, h2) :: combine t1 t2
    | [], [] ->
         []
    | _ ->
         raise (Invalid_argument "combine")

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
