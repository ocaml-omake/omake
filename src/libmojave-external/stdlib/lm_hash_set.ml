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

module Make (Ord : Set.OrderedType) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Type of elements in the set.
    *)
   type elt = Ord.t

   (*
    * The set is a hashtable,
    * but most operations are delayed.
    *)
   type table =
      Empty
    | Element of elt
    | Union of table * table
    | Add of table * elt
    | Remove of table * elt
    | Hash of (elt, elt) Hashtbl.t * int

   type t =
      { mutable table : table }

   (*
    * Exception to abort intersections early.
    *)
   exception Found

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Get the delayed hashtable.
    *)
   let rec compile table = function
      Empty ->
         0
    | Element x ->
         if Hashtbl.mem table x then 0
         else begin
            Hashtbl.add table x x;
            1
         end
    | Union (s1, s2) ->
         compile table s1 + compile table s2
    | Add (s1, x) ->
         let count = compile table s1 in
            if Hashtbl.mem table x then
               count
            else begin
               Hashtbl.add table x x;
               succ count
            end
    | Remove (s1, x) ->
         let was_there = Hashtbl.mem table x in
         let count = compile table s1 in
            if (not was_there) && Hashtbl.mem table x then begin
               Hashtbl.remove table x;
               pred count
            end else
               count
    | Hash (hash, _) ->
         let count = ref 0 in
         let add x y =
            if not (Hashtbl.mem table x) then begin
               Hashtbl.add table x y;
               incr count
            end
         in begin
            Hashtbl.iter add hash;
            !count
         end

   let flush s1 =
      match s1.table with
         Hash (table, count) ->
            table, count
       | prog ->
            let table = Hashtbl.create 19 in
            let count = compile table prog in
               s1.table <- Hash (table, count);
               table, count

   (*
    * Create the set from a list.
    *)
   let of_sorted_list elements =
      let table = Hashtbl.create 19 in
         List.iter (fun x -> Hashtbl.add table x x) elements;
         { table = Hash (table, List.length elements) }

   (*
    * Get the elements in the set.
    * They are not sorted.
    *)
   let elements s1 =
      let table, _ = flush s1 in
      let elements = ref [] in
         Hashtbl.iter (fun x _ -> elements := x :: !elements) table;
         !elements

   let to_list = elements

   (*
    * Add an element.
    *)
   let add s1 x =
      { table = Add (s1.table, x) }

   (*
    * Membership in the set.
    *)
   let mem s1 x =
      let table, _ = flush s1 in
         try let _ = Hashtbl.find table x in true with
            Not_found ->
               false

   (*
    * Remove an element.
    *)
   let remove { table = table } x =
      { table = Remove (table, x) }

   (*
    * Set operations.
    *)
   let empty =
      { table = Empty }

   let is_empty s1 =
      let _, count = flush s1 in
         count = 0

   let singleton x =
      { table = Element x }

   let union s1 s2 =
      { table = Union (s1.table, s2.table) }

   let iter f s1 =
      Hashtbl.iter (fun x _ -> f x) (fst (flush s1))

   let cardinal s1 =
      snd (flush s1)

   let of_list l =
      List.fold_left (fun set item -> add set item) empty l

   (*
    * Intersection.
    *)
   let intersect_aux table1 table2 =
      let check x _ =
         try
            Hashtbl.find table1 x;
            raise Found
         with
            Not_found ->
               ()
      in
         try Hashtbl.iter check table2; false with
            Found ->
               true

   let intersectp set1 set2 =
      let table1, count1 = flush set1 in
      let table2, count2 = flush set2 in
         if count1 < count2 then
            intersect_aux table2 table1
         else
            intersect_aux table1 table2

   (*
    * Filter out the elements that are in the intersection.
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

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
