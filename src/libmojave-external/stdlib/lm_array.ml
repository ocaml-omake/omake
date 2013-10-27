(*
 * Array operations.
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

(*
 * Array operations.
 *)
external length : 'a array -> int = "%array_length"
external get : 'a array -> int -> 'a = "%array_get"
external set : 'a array -> int -> 'a -> unit = "%array_set"
external make : int -> 'a -> 'a array = "%array_create"
external create : int -> 'a -> 'a array = "%array_create"
external append : 'a array -> 'a array -> 'a array = "%array_append"
external sub : 'a array -> int -> int -> 'a array = "%array_sub"
external copy : 'a array -> 'a array = "%array_copy"
external fill : 'a array -> int -> int -> 'a -> unit = "%array_fill"
external blit : 'a array -> int -> 'a array -> int -> int -> unit = "%array_blit"

let init i f =
   if i = 0 then
      [||]
   else
      let x = f 0 in
      let a = create i x in
         for j = 1 to pred i do
            a.(j) <- f j
         done;
         a

let make_matrix i j x =
   init i (fun _ -> create j x)

let create_matrix = make_matrix

let rec concat = function
   a :: tl ->
      append a (concat tl)
 | [] ->
      [||]

let to_list a =
   let rec loop l i =
      if i = 0 then
         l
      else
         let i = pred i in
            loop (a.(i) :: l) i
   in
      loop [] (length a)

let of_list l =
   match l with
      h :: t ->
         let len = List.length l in
         let a = create len h in
         let rec loop i = function
            h :: t ->
               a.(i) <- h;
               loop (succ i) t
          | [] ->
               ()
         in
            loop 1 t;
            a

    | [] ->
         [||]

let iter f a =
   for i = 0 to pred (length a) do
      f a.(i)
   done

let map f a =
   let len = length a in
      if len = 0 then
         [||]
      else
         let a' = create len (f a.(0)) in
            for i = 1 to pred len do
               a'.(i) <- f a.(i)
            done;
            a'

let iteri f a =
   for i = 0 to pred (length a) do
      f i a.(i)
   done

let mapi f a =
   let len = length a in
      if len = 0 then
         [||]
      else
         let a' = create len (f 0 a.(0)) in
            for i = 1 to pred len do
               a'.(i) <- f i a.(i)
            done;
            a'

let fold_left f x a =
   let len = length a in
   let rec loop i x =
      if i = len then
         x
      else
         loop (succ i) (f x a.(i))
   in
      loop 0 x

let fold_right f a x =
   let rec loop i x =
      if i = 0 then
         x
      else
         let i = pred i in
            loop i (f a.(i) x)
   in
      loop (length a) x

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
