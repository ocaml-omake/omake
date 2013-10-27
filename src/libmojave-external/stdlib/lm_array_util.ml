(*
 * Utilities on arrays.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Lm_array_util%t"

(*
 * Parts for collecting arrays.
 *)
type ('a, 'b) array_part =
   ArrayElement of 'a
 | ArrayArray of 'b * int * int

(*
 * Boolean values.
 *)
let all_true v =
   let rec search i len v =
      i = len || (v.(i) && search (succ i) len v)
   in
      search 0 (Array.length v) v

let exists_true v =
   let rec search i len v =
      i <> len && (v.(i) || search (succ i) len v)
   in
      search 0 (Array.length v) v

let for_all f v =
   let rec search f i len v =
      i = len || (f v.(i) && search f (succ i) len v)
   in
      search f 0 (Array.length v) v

let for_all2 f a1 a2 =
   let len = Array.length a1 in
      Array.length a1 = Array.length a2 &&
      (let rec apply i =
         (i = len) || (f a1.(i) a2.(i) && apply (i + 1))
      in
         apply 0)

let exists f v =
   let rec search f i len v =
      i <> len && (f v.(i) || search f (succ i) len v)
   in
      search f 0 (Array.length v) v

(*
 * Membership in an array.
 *)
let mem i v =
   let l = Array.length v in
   let rec aux j =
      j < l & ( i = v.(j) or
                aux (j + 1) )
   in
      aux 0

(*
 * Membership in an array.
 *)
let index i v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if i = v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0

(*
 * Membership in an array.
 *)
let exists f v =
   let l = Array.length v in
   let rec aux j =
      j < l & ( f v.(j) or
                aux (j + 1) )
   in
      aux 0

let find_index f v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if f v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0

(*
 * Iterate over two arrays.
 *)
let iter2 f a1 a2 =
   let len = Array.length a1 in
      if Array.length a2 <> len then
         raise (Failure "Array.iter2")
      else
         let rec apply f a1 a2 i len =
            if i < len then
               begin
                  f a1.(i) a2.(i);
                  apply f a1 a2 (i + 1) len
               end
         in
            apply f a1 a2 0 len

let append_list a = function
   [] -> a
 | hd :: tl ->
      let l = Array.length a in
      let res = Array.create (l + List.length tl + 1) hd in
      for i = 0 to pred l do Array.unsafe_set res i (Array.unsafe_get a i) done;
      let rec aux i = function
         [] -> res
       | hd :: tl ->
            Array.unsafe_set res i hd;
            aux (succ i) tl
      in aux (succ l) tl

let append_list_array a1 l a2 =
   match l with
      [] -> Array.append a1 a2
    | hd::tl ->
         let l1 = Array.length a1 and l2 = Array.length a2 in
         let offs = succ l1 + List.length tl in
         let res = Array.create (offs + l2) hd in
         for i = 0 to pred l1 do Array.unsafe_set res i (Array.unsafe_get a1 i) done;
         let rec aux i = function
            [] -> ()
          | hd :: tl ->
               Array.unsafe_set res i hd;
               aux (succ i) tl
         in aux (succ l1) tl;
         for i = 0 to pred l2 do Array.unsafe_set res (i+offs) (Array.unsafe_get a2 i) done;
         res

let replace a i j = function
   [] ->
      if j>0 then Array.append (Array.sub a 0 i) (Array.sub a (i+j) (Array.length a-i-j))
      else raise (Invalid_argument "Lm_array_util.replace")
 | hd :: tl ->
      let l = Array.length a in
      let ij = i + j in
      if i>=0 && j>0 && ij<=l then
         let dl = List.length tl - j +1 in
         let res = Array.create (l+dl) hd in
         for k=0 to (pred i) do
            Array.unsafe_set res k (Array.unsafe_get a k)
         done;
         for k=ij to (pred l) do
            Array.unsafe_set res (k+dl) (Array.unsafe_get a k)
         done;
         let rec aux k = function
            [] -> res
          | hd :: tl ->
               Array.unsafe_set res k hd;
               aux (succ k) tl
         in aux (succ i) tl
      else raise (Invalid_argument "Lm_array_util.replace")

(*
 * Map over a subarray.
 *)
let sub_map f a i len =
   if i < 0 || len < 0 || i + len > Array.length a then
      raise (Invalid_argument "sub_map")
   else
      match len with
         0 ->
            [||]
       | 1 ->
            [| f (Array.unsafe_get a i) |]
       | _ ->
            let a' = Array.create len (f (Array.unsafe_get a i)) in
               for j = 1 to len - 1 do
                  Array.unsafe_set a' j (f (Array.unsafe_get a (i + j)))
               done;
               a'

(*
 * Compute the total length of the parts.
 * As a side-effect, we raise an exception if
 * any of the subarrays are ill-defined.
 *)
let rec parts_length len = function
   [] ->
      len
 | ArrayElement _ :: parts ->
      parts_length (len + 1) parts
 | ArrayArray (a, i, len') :: parts ->
      if i < 0 || len' < 0 || i + len' > Array.length a then
         raise (Invalid_argument "Array.collect")
      else
         parts_length (len + len') parts

(*
 * Add the parts to the array.
 * We are guaranteed that the arrays will be in bounds.
 *)
let rec collect_append a off = function
   [] ->
      a
 | ArrayElement x :: parts ->
      Array.unsafe_set a off x;
      collect_append a (off + 1) parts
 | ArrayArray (_, _, 0) :: parts ->
      collect_append a off parts
 | ArrayArray (a', i, len) :: parts ->
      Array.blit a' i a off len;
      collect_append a (off + len) parts

(*
 * Collect function works in two parts.
 * The first part creates the initial array,
 * and the second part adds to it.
 *)
let rec collect = function
   [] ->
      [||]
 | ArrayElement x :: parts ->
      let len = parts_length 1 parts in
      let a' = Array.create len x in
         collect_append a' 1 parts
 | ArrayArray (_, _, 0) :: parts ->
      collect parts
 | [ArrayArray(a, 0, len)] when len = Array.length a ->
      a
 | ArrayArray (a, i, len) :: parts ->
      let len' = parts_length len parts in
      let a' = Array.create len' a.(i) in
         if len > 1 then
            Array.blit a (i + 1) a' 1 (len - 1);
         collect_append a' len parts

(*
 * Sorts an array, than eliminates the duplicate elements
 * and moves the remaining elements into an initial segment
 * of the input array. Returns the # of distinct elements.
 *)
let distinct cmp = function
   [||] -> 0
 | array ->
      let l = Array.length array in
      let rec d_find i =
         let j = (succ i) in
            if j = l then
               j
            else if (cmp array.(i) array.(j)) = 0 then
               d_copy i (succ j)
            else
               d_find j
      and d_copy i j =
         if j = l then
            succ i
         else if (cmp array.(i) array.(j)) = 0 then
            d_copy i (succ j)
         else
            let i = succ i in
               array.(i) <- array.(j);
               d_copy i (succ j)
      in
         Array.fast_sort cmp array;
         d_find 0

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
