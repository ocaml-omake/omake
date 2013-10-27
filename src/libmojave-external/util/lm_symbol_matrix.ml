(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002-2005 Mojave Group, Caltech
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
open Lm_symbol

(************************************************************************
 * Types
 *)

(*
 * An adjacency matrix.
 *)
module type SymbolMatrixSetSig =
sig
   type t

   (*
    * Create a new table.
    * The int is the initial size.
    *)
   val create : int -> t

   (*
    * Test for membership.
    *)
   val query : t -> symbol -> symbol -> bool

   (*
    * Add an edge.
    *)
   val add : t -> symbol -> symbol -> unit

   (*
    * Iterate over the table.
    *)
   val iter : (symbol -> symbol -> unit) -> t -> unit
end

(*
 * An adjacency matrix.
 *)
module type SymbolMatrixTableSig =
sig
   type 'a t

   (*
    * Create a new table.
    * The int is the initial size.
    *)
   val create : int -> 'a t

   (*
    * This is an imperative data structure.
    * The copy operation builds an identical copy.
    *)
   val copy : 'a t -> 'a t

   (*
    * Test for membership.
    *)
   val find : 'a t -> symbol -> symbol -> 'a
   val query : 'a t -> symbol -> symbol -> bool

   (*
    * Add an edge.
    *)
   val add : 'a t -> symbol -> symbol -> 'a -> unit
   val filter_add : 'a t -> symbol -> symbol -> ('a option -> 'a) -> unit

   (*
    * Remove an edge.
    *)
   val remove : 'a t -> symbol -> symbol -> unit
   val filter_remove : 'a t -> symbol -> symbol -> ('a -> 'a option) -> unit

   (*
    * Iterate over the table.
    *)
   val iter : (symbol -> symbol -> 'a -> unit) -> 'a t -> unit

   (*
    * Map over the table.
    *)
   val map : ('a -> 'a) -> 'a t -> 'a t

   (*
    * Fold over the table.
    *)
   val fold : ('a -> symbol -> symbol -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

(*
 * The order can be used to determine
 * define symmetric tables.
 *)
module type OrderSig =
sig
   val reorder : symbol -> symbol -> symbol * symbol
end

(*
 * This is another implementation of a sparse bit matrix.
 * This is really a hash table, but performance may be better
 * than the hashtable.
 *)
module SymbolMatrix : SymbolMatrixSetSig =
struct
   type t =
      { hash_length : int;
        hash_buckets : (symbol * symbol) list array
      }

   (*
    * Assume a rough initial size.
    *)
   let create size =
      let length = size * 11 in
         { hash_length = length;
           hash_buckets = Array.create length []
         }

   (*
    * Hash a pair.
    *)
   let hash v1 v2 =
      let i1 = Lm_symbol.to_int v1 in
      let i2 = Lm_symbol.to_int v2 in
         ((i1 * i2 * 3) lxor 0x17ab2537) land 0x3fffffff

   (*
    * Check if the pair is in the list.
    *)
   let rec mem v1 v2 l =
      match l with
         (v1', v2') :: l ->
            (Lm_symbol.eq v1' v1 && Lm_symbol.eq v2' v2) || mem v1 v2 l
       | [] ->
            false

   (*
    * Query the edge.
    *)
   let query { hash_length = length; hash_buckets = buckets } v1 v2 =
      let v1, v2 =
         if Lm_symbol.compare v1 v2 < 0 then
            v1, v2
         else
            v2, v1
      in
      let index = (hash v1 v2) mod length in
         mem v1 v2 buckets.(index)

   (*
    * Add an edge.
    *)
   let add tbl v1 v2 =
      let v1, v2 =
         if Lm_symbol.compare v1 v2 < 0 then
            v1, v2
         else
            v2, v1
      in
      let { hash_length = length; hash_buckets = buckets } = tbl in
      let index = (hash v1 v2) mod length in
      let bucket = buckets.(index) in
         if not (mem v1 v2 bucket) then
            buckets.(index) <- (v1, v2) :: bucket

   (*
    * Iteration.
    *)
   let iter f { hash_buckets = buckets } =
      Array.iter (fun entries ->
            List.iter (fun (v1, v2) -> f v1 v2) entries) buckets
end

(*
 * This is another implementation of a sparse bit matrix.
 * This is really a hash table, but performance may be better
 * than the hashtable.
 *)
module MakeSymbolMatrixTable (Order : OrderSig) : SymbolMatrixTableSig =
struct
   type 'a t =
      { hash_length : int;
        hash_buckets : (symbol * symbol * 'a) list array
      }

   (*
    * Assume a rough initial size.
    *)
   let create size =
      let length = size * 11 in
         { hash_length = length;
           hash_buckets = Array.create length []
         }

   let copy table =
      let { hash_length = length;
            hash_buckets = buckets
          } = table
      in
         { hash_length = length;
           hash_buckets = Array.copy buckets
         }

   (*
    * Hash a pair.
    *)
   let hash v1 v2 =
      let i1 = Lm_symbol.to_int v1 in
      let i2 = Lm_symbol.to_int v2 in
         ((i1 * i2 * 3) lxor 0x17ab2537) land 0x3fffffff

   (*
    * Check if the pair is in the list.
    *)
   let rec mem v1 v2 l =
      match l with
         (v1', v2', _) :: l ->
            (Lm_symbol.eq v1' v1 && Lm_symbol.eq v2' v2) || mem v1 v2 l
       | [] ->
            false

   (*
    * Query the edge.
    *)
   let query { hash_length = length; hash_buckets = buckets } v1 v2 =
      let v1, v2 = Order.reorder v1 v2 in
      let index = (hash v1 v2) mod length in
         mem v1 v2 buckets.(index)

   let find { hash_length = length; hash_buckets = buckets } v1 v2 =
      let v1, v2 = Order.reorder v1 v2 in
      let index = (hash v1 v2) mod length in
      let rec search l =
         match l with
            (v1', v2', depth') :: l ->
               if Lm_symbol.eq v1' v1 && Lm_symbol.eq v2' v2 then
                  depth'
               else
                  search l
          | [] ->
               raise Not_found
      in
         search buckets.(index)

   (*
    * Add an edge.
    *)
   let add tbl v1 v2 depth =
      let v1, v2 = Order.reorder v1 v2 in
      let { hash_length = length; hash_buckets = buckets } = tbl in
      let index = (hash v1 v2) mod length in
      let rec add l =
         match l with
            (v1', v2', _depth') as h :: l ->
               if Lm_symbol.eq v1' v1 && Lm_symbol.eq v2' v2 then
                  (v1, v2, depth) :: l
               else
                  h :: add l
          | [] ->
               [v1, v2, depth]
      in
         buckets.(index) <- add buckets.(index)

   (*
    * Combine with a previous edge.
    *)
   let filter_add tbl v1 v2 f =
      let v1, v2 = Order.reorder v1 v2 in
      let { hash_length = length; hash_buckets = buckets } = tbl in
      let index = (hash v1 v2) mod length in
      let rec add l =
         match l with
            (v1', v2', depth') as h :: l ->
               if Lm_symbol.eq v1' v1 && Lm_symbol.eq v2' v2 then
                  (v1, v2, f (Some depth')) :: l
               else
                  h :: add l
          | [] ->
               [v1, v2, f None]
      in
         buckets.(index) <- add buckets.(index)

   (*
    * Remove an edge.
    *)
   let remove tbl v1 v2 =
      let v1, v2 = Order.reorder v1 v2 in
      let { hash_length = length; hash_buckets = buckets } = tbl in
      let index = (hash v1 v2) mod length in
      let rec remove l =
         match l with
            (v1', v2', _) as h :: l ->
               if Lm_symbol.eq v1' v1 && Lm_symbol.eq v2' v2 then
                  l
               else
                  h :: remove l
          | [] ->
               []
      in
         buckets.(index) <- remove buckets.(index)

   let filter_remove tbl v1 v2 f =
      let v1, v2 = Order.reorder v1 v2 in
      let { hash_length = length; hash_buckets = buckets } = tbl in
      let index = (hash v1 v2) mod length in
      let rec remove l =
         match l with
            (v1', v2', x) as h :: l ->
               if Lm_symbol.eq v1' v1 && Lm_symbol.eq v2' v2 then
                  (match f x with
                      Some x ->
                         (v1', v2', x) :: l
                    | None ->
                         l)
               else
                  h :: remove l
          | [] ->
               []
      in
         buckets.(index) <- remove buckets.(index)

   (*
    * Iteration.
    *)
   let iter f { hash_buckets = buckets } =
      Array.iter (fun entries ->
            List.iter (fun (v1, v2, depth) -> f v1 v2 depth) entries) buckets

   (*
    * Map.
    *)
   let map f table =
      let { hash_buckets = buckets } = table in
      let buckets = Array.map (List.map (fun (v1, v2, depth) -> v1, v2, f depth)) buckets in
         { table with hash_buckets = buckets }

   (*
    * Folding.
    *)
   let fold f x { hash_buckets = buckets } =
      Array.fold_left (fun x entries ->
            List.fold_left (fun x (v1, v2, depth) ->
                  f x v1 v2 depth) x entries) x buckets
end

module NoOrder =
struct
   let reorder v1 v2 = v1, v2
end

module SymOrder =
struct
   let reorder v1 v2 =
      if Lm_symbol.compare v1 v2 < 0 then
         v1, v2
      else
         v2, v1
end

module AsymSymbolMatrix = MakeSymbolMatrixTable (NoOrder)
module SymSymbolMatrix = MakeSymbolMatrixTable (SymOrder)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
