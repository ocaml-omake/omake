(*
 * Serialized map. Acts as a normal map, but the order in
 * which elements are inserted is retained, and all iterating
 * functions visit elements in that order.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Mojave Group, Caltech
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
 * Author: Adam Granicz
 * granicz@cs.caltech.edu
 *)
open Lm_map_sig
open Lm_map

(*
 * These are the functions provided by the table.
 *)
module type SerialMap =
sig
   type key
   type 'a tt

   val empty : 'a tt
   val is_empty : 'a tt -> bool
   val cardinal : 'a tt -> int
   val add : 'a tt -> key -> 'a -> 'a tt
   val find : 'a tt -> key -> 'a
   val remove : 'a tt -> key -> 'a tt
   val mem : 'a tt -> key -> bool

   val iter : (key -> 'a -> unit) -> 'a tt -> unit
   val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b tt -> 'a

   (* Both keys and data return a list ordered by insertion order *)
   val keys : 'a tt -> key list
   val data : 'a tt -> 'a list
end

(*
 * Make the map.
 *)
module SerialMapMake (Base : OrderedType) : SerialMap with type key = Base.t =
struct
   module IntModule = struct
      type t = int
      let compare = Pervasives.compare
   end

   module SMap = LmMake (IntModule)
   module SMapShadow = LmMake (Base)

   type key = Base.t
   type 'elt tt = (key SMap.t * ('elt * int) SMapShadow.t)

   let counter = ref 0

   let empty = (SMap.empty, SMapShadow.empty)

   let is_empty (table, _) = SMap.is_empty table

   let cardinal (table, _) = SMap.cardinal table

   let add (table, shadow) key data =
      let index =
         try
            (* Key is already in map *)
            snd (SMapShadow.find shadow key)
         with
            Not_found ->
               incr counter;
               !counter
      in
         SMap.add table index key, SMapShadow.add shadow key (data, index)

   let find (_, shadow) key =
      fst (SMapShadow.find shadow key)

   let remove (table, shadow) key =
      let index = snd (SMapShadow.find shadow key) in
         SMap.remove table index, SMapShadow.remove shadow key

   let mem (_, shadow) key =
      SMapShadow.mem shadow key

   let iter f (table, shadow) =
      SMap.iter (fun _ key ->
         let data = fst (SMapShadow.find shadow key) in
            f key data) table

   let fold f accum (table, shadow) =
      SMap.fold (fun accum _ key ->
         let data = fst (SMapShadow.find shadow key) in
            f accum key data) accum table

   let keys (table, _) =
      let keys_rev =
         SMap.fold (fun keys _ key ->
            key :: keys) [] table
      in
         List.rev keys_rev

   let data (table, shadow) =
      let data_list_rev =
         SMap.fold (fun data_list _ key ->
            let data, _ = SMapShadow.find shadow key in
               data :: data_list) [] table
      in
         List.rev data_list_rev

end

