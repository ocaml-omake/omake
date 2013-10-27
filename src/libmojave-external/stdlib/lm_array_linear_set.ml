(*
 * This module provides a linearly ordered numbered set implementation
 * with lazy functions based on arrays
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
 * Author: Alexey Nogin
 *)
open Lm_linear_set_sig

type 'a linear_set = 'a array

module Make (Type : TypeSig) =
struct
   type elt = Type.t
   type t = Type.t linear_set
   type index = int

   let empty = [||]
   let singleton x = [|x|]
   let length = Array.length
   let create = Array.create
   let make = Array.create
   let iter = Array.iter
   let map = Array.map
   let of_list = Array.of_list
   let to_list = Array.to_list
   let lazy_apply = Array.map

   let concat = Array.append

   let append a1 e a2 =
      let l1 = length a1 and l2 = length a2 in
      if l1 = 0 && l2 = 0 then [|e|] else begin
         let r = create (succ l1 + l2) e in
         for i = 0 to l1 - 1 do Array.unsafe_set r i (Array.unsafe_get a1 i) done;
         let l1 = succ l1 in
         for i = 0 to l2 - 1 do Array.unsafe_set r (i + l1) (Array.unsafe_get a2 i) done;
         r
      end

   let fold f x a =
      let r = ref x in
         for i = 0 to Array.length a - 1 do
            r := f !r i (Array.unsafe_get a i)
         done;
         !r

   let get = Array.get

   let split t ind =
      Array.sub t 0 ind, t.(ind), Array.sub t (succ ind) (Array.length t - ind - 1)

   let lazy_sub_map = Lm_array_util.sub_map
   let append_list = Lm_array_util.append_list_array
   let mapi = Array.mapi
   let init = Array.init
   let collect = Lm_array_util.collect

   let for_all f a =
      let len = Array.length a in
      let rec search i =
         i = len || (f (Array.unsafe_get a i) && search (succ i))
      in
         search 0

   let exists f a =
      let len = Array.length a in
      let rec search i =
         i < len && (f (Array.unsafe_get a i) || search (succ i))
      in
         search 0
end
