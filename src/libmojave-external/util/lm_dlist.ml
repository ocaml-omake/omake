(*
 * Doubly-linked list.
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

(*
 * List type.
 *)
type 'a t =
   'a handle option ref

and 'a handle =
   { mutable pred : 'a handle option ref;
     succ : 'a handle option ref;
     data : 'a
   }

(*
 * Create an empty list.
 *)
let create () =
   ref None

(*
 * Create an unattached handle.
 *)
let create_handle x =
   let opt = ref None in
      { pred = opt;
        succ = opt;
        data = x
      }

let data { data = data } =
   data

(*
 * Check if the list is empty.
 *)
let is_empty l =
   !l = None

(*
 * Get the head element.
 *)
let hd l =
   match !l with
      Some hand -> hand
    | None -> raise (Invalid_argument "Dlist.hd")

let tl hand =
   match !(hand.succ) with
      Some hand -> hand
    | None -> raise (Invalid_argument "Dlist.tl")

let no_tl hand =
   !(hand.succ) = None

(*
 * Turn into a list.
 *)
let to_list l =
   let rec collect = function
      Some hand ->
         hand :: collect (!(hand.succ))
    | None ->
         []
   in
      collect (!l)

(*
 * Insert a node at the head of a list.
 *)
let insert hand l =
   assert (!(hand.pred) = None && !(hand.succ) = None);
   let next = !l in
      l := Some hand;
      hand.pred <- l;
      hand.succ := next;
      match next with
         Some next ->
            next.pred <- hand.succ
       | None ->
            ()

(*
 * Delete a node from the list.
 *)
let delete hand =
   let pred = hand.pred in
   let succ = hand.succ in
   let succ' = !succ in
   let _ =
      pred := succ';
      match succ' with
         Some next ->
            next.pred <- pred
       | None ->
            ()
   in
      hand.pred <- succ;
      succ := None

(*
 * Get the list length.
 *)
let length l =
   let rec length i = function
      Some hand ->
         let next = !(hand.succ) in
            length (succ i) next
    | None ->
         i
   in
      length 0 (!l)

(*
 * Iterate.
 * Note that this is tricky if the function
 * modifes the list during iteration (what should
 * be the intended semantics?)
 *)
let iter f l =
   let rec iter = function
      Some hand ->
         let next = !(hand.succ) in
            f hand;
            iter next
    | None ->
         ()
   in
      iter (!l)

let fold f x l =
   let rec fold x = function
      Some hand ->
         let next = !(hand.succ) in
         let x = f x hand in
            fold x next
    | None ->
         x
   in
      fold x (!l)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
