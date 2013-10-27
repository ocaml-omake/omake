(*
 * Overlay list of elements as small sets over another
 * set implementation.
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
open Lm_set_sig

module Make (BigSet : MinimalSetSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Max size of small sets.
    * This number is arbitrary.
    *)
   let max_size = 12

   (*
    * Type of elements in the set.
    *)
   type elt = BigSet.elt

   (*
    * Either a list or a set.
    *)
   type t =
      List of elt list
    | Set of BigSet.t

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Conversion.
    *)
   let rec set_of_list_aux set = function
      h :: t ->
         set_of_list_aux (BigSet.add set h) t
    | [] ->
         set

   let set_of_list l =
      Set (set_of_list_aux BigSet.empty l)

   (*
    * Set operations.
    *)
   let empty = List []

   let is_empty = function
         List [] ->
            true
       | List (_ :: _) ->
            false
       | Set s ->
            BigSet.is_empty s

   let mem set x =
      match set with
         List l ->
            List.mem x l
       | Set s ->
            BigSet.mem s x

   let add set x =
      match set with
         List l ->
            if List.length l = max_size then
               set_of_list (x :: l)
            else
               List (x :: l)
       | Set s ->
            Set (BigSet.add s x)

   let singleton x =
      List [x]

   let rec remove_list x = function
      h :: t ->
         if x = h then
            remove_list x t
         else
            h :: remove_list x t
    | [] ->
         []

   let remove s x = match s with
      List l ->
         List (remove_list x l)
    | Set s ->
         Set (BigSet.remove s x)

   let rec union_set_list set = function
      x :: t ->
         union_set_list (BigSet.add set x) t
    | [] ->
         Set set

   let union set1 set2 =
      match set1, set2 with
         List l1, List l2 ->
            let l = Lm_list_util.union l1 l2 in
               if List.length l <= max_size then
                  List l
               else
                  set_of_list l
       | Set s1, Set s2 ->
            Set (BigSet.union s1 s2)
       | List l1, Set s2 ->
            union_set_list s2 l1
       | Set s1, List l2 ->
            union_set_list s1 l2

   let elements = function
      List l ->
         l
    | Set s ->
         BigSet.elements s

   let to_list = elements

   let of_sorted_list l =
      if List.length l <= max_size then
         List l
      else
         Set (BigSet.of_sorted_list l)

   let of_list l =
      if List.length l <= max_size then
         List l
      else
         Set (List.fold_left (fun set item -> BigSet.add set item) BigSet.empty l)

   let iter f = function
      List l ->
         List.iter f l
    | Set s ->
         BigSet.iter f s

   let cardinal = function
      List l ->
         List.length l
    | Set s ->
         BigSet.cardinal s

   (*
    * Filter out the elements that are in the intersection.
    *)
   let mem_filt set elements =
      match set with
         List l ->
            Lm_list_util.intersect l elements
       | Set s ->
            BigSet.mem_filt s elements

   let not_mem_filt set elements =
      match set with
         List l ->
            Lm_list_util.subtract elements l
       | Set s ->
            BigSet.not_mem_filt s elements

   let rec collect l = function
      (x, v) :: tl ->
         if List.mem x l then
            (x, v) :: collect l tl
         else
            collect l tl
    | [] ->
         []

   let fst_mem_filt set elements =
      match set with
         List l ->
            collect l elements
       | Set s ->
            BigSet.fst_mem_filt s elements


   let rec set_list_intersectp set = function
      h :: t ->
         (BigSet.mem set h) || (set_list_intersectp set t)
    | [] ->
         false

   let intersectp set1 set2 =
      match set1, set2 with
         List l1, List l2 ->
            Lm_list_util.intersects l1 l2
       | Set s1, Set s2 ->
            BigSet.intersectp s1 s2
       | List l1, Set s2 ->
            set_list_intersectp s2 l1
       | Set s1, List l2 ->
            set_list_intersectp s1 l2
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
