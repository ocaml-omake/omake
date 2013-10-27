(*
 * This is an array that can be larger than
 * the default OCaml arrays.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999-2005 PRL Group, Cornell University and Caltech
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
module LargeArray =
struct
   (*
    * Two-level array.
    *)
   type 'a t =
      { array_default : 'a;
        mutable array_shift : int;
        mutable array_mask : int;
        mutable array_length : int;
        mutable array_buckets : 'a array array
      }

   (*
    * Maximum minor size.
    *)
   let max_shift = Lm_int_util.log2 Sys.max_array_length
   let max_length = 1 lsl max_shift

   (*
    * Create an empty array.
    *)
   let create x =
      { array_default = x;
        array_shift = 0;
        array_mask = 0;
        array_length = 0;
        array_buckets = Array.create 1 [||]
      }

   (*
    * Return the length.
    *)
   let length { array_length = length } =
      length

   (*
    * Expand the array.
    *)
   let expand info i =
      let buckets = info.array_buckets in
      let x = info.array_default in
      let log = succ (Lm_int_util.log2 i) in
      let length = 1 lsl log in
         if log <= max_shift then
            let new_array = Array.create length x in
            let old_array = buckets.(0) in
               Array.blit old_array 0 new_array 0 (Array.length old_array);
               info.array_shift <- log;
               info.array_mask <- (1 lsl log) - 1;
               info.array_length <- 1 lsl log;
               buckets.(0) <- new_array
         else
            begin
               (* Possibly expand the first array to max_length *)
               (if info.array_length < max_length then
                   let new_array = Array.create max_length x in
                   let old_array = buckets.(0) in
                      Array.blit old_array 0 new_array 0 (Array.length old_array);
                      buckets.(0) <- new_array);

               (* Now expand the major level *)
               let count = (i + max_length - 1) lsr max_shift in
               let new_buckets = Array.create count buckets.(0) in
               let old_length = Array.length buckets in
                  Array.blit buckets 0 new_buckets 0 old_length;
                  for i = old_length to pred count do
                     new_buckets.(i) <- Array.create max_length x
                  done;
                  info.array_shift <- max_shift;
                  info.array_mask <- pred max_length;
                  info.array_length <- count * max_length;
                  info.array_buckets <- new_buckets
            end

   (*
    * Store a value in the array.
    *)
   let set info i x =
      if i > info.array_length then
         expand info i
      else
         info.array_buckets.(i lsr info.array_shift).(i land info.array_mask) <- x

   (*
    * Get a value in the array.
    *)
   let get info i =
      if i > info.array_length then
         raise (Invalid_argument "LargeArray.get");
      info.array_buckets.(i lsr info.array_shift).(i land info.array_mask)
end



(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
