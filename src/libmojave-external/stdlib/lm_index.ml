(*
 * Index module based on tables.
 * An index is essentially a multi-level table.
 * Each entry has an associated data item and subtable.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Michael Maire, Caltech
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
 * Author: Michael Maire
 *         mmaire@caltech.edu
 *
 * ----------------------------------------------------------------
 * Revision History
 *
 *  2002  Apr 20  Michael Maire  Initial Version
 *  2002  Apr 25  Michael Maire  Renamed iter, maps, folds to *_all
 *                               added single level iters, maps, folds
 *  2002  Apr 26  Michael Maire  Added functions for explicitly adding
 *                               subindices
 *  2002  May  1  Michael Maire  Made all adds nondestructive towards
 *                               subindices (if subindex is not explicitly
 *                               specified in the add)
 *                               Changed interface
 *)

(*
 * Elements.
 * This type specifies the type of the keys used in the index.
 *)
module type OrderedType =
sig
   type t
   val compare : t -> t -> int
end

(*
 * These are the functions provided by the index.
 *)
module type LmIndex =
sig
   (* index maps key lists to elements of type 'a *)
   type key
   type 'a t

   (* empty index and empty test *)
   val empty : 'a t
   val is_empty : 'a t -> bool

   (* tests/lookups - single level*)
   val mem : 'a t -> key -> bool
   val find : 'a t -> key -> 'a t * 'a
   val find_index : 'a t -> key -> 'a t
   val find_data : 'a t -> key -> 'a

   (* tests/lookups - multi level*)
   val mem_list : 'a t -> key list -> bool
   val find_list : 'a t -> key list -> 'a t * 'a
   val find_list_index : 'a t -> key list -> 'a t
   val find_list_data : 'a t -> key list -> 'a

   (* addition and removal - single level*)
   val add : 'a t -> key -> 'a -> 'a t
   val add_i : 'a t -> key -> 'a t * 'a -> 'a t
   val remove : 'a t -> key -> 'a t

   (* addition of a chain of nested entries *)
   val add_list : 'a t -> key list -> 'a list -> 'a t
   val add_list_i : 'a t -> key list -> ('a t * 'a) list -> 'a t

   (* addition/removal of single entries *)
   val add_entry : 'a t -> key list -> 'a -> 'a t
   val add_entry_i : 'a t -> key list -> 'a t * 'a -> 'a t
   val remove_entry : 'a t -> key list -> 'a t

   (* filter addition/removal - single level *)
   val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
   val filter_add_i : 'a t -> key -> (('a t * 'a) option -> ('a t * 'a)) -> 'a t
   val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
   val filter_remove_i : 'a t -> key -> (('a t * 'a) -> ('a t * 'a) option) -> 'a t

   (* filter addition of a chain of nested entries *)
   val filter_add_list : 'a t -> key list -> ('a option -> 'a) list -> 'a t
   val filter_add_list_i : 'a t -> key list -> (('a t * 'a) option -> ('a t * 'a)) list -> 'a t

   (* filter addition/removal of single entries *)
   val filter_add_entry : 'a t -> key list -> ('a option -> 'a) -> 'a t
   val filter_add_entry_i : 'a t -> key list -> (('a t * 'a) option -> ('a t * 'a)) -> 'a t
   val filter_remove_entry : 'a t -> key list -> ('a -> 'a option) -> 'a t
   val filter_remove_entry_i : 'a t -> key list -> (('a t * 'a) -> ('a t * 'a) option) -> 'a t

   (* iterators, maps, and folds - single level *)
   val iter : (key -> ('a t * 'a) -> unit) -> 'a t -> unit
   val map : (('a t * 'a) -> ('b t * 'b)) -> 'a t -> 'b t
   val mapi : (key -> ('a t * 'a) -> ('b t * 'b)) -> 'a t -> 'b t
   val fold : ('a -> key -> ('b t * 'b) -> 'a) -> 'a -> 'b t -> 'a
   val fold_map : ('a -> key -> ('b t * 'b) -> 'a * ('c t * 'c)) -> 'a -> 'b t -> 'a * 'c t

   (* iterators, maps, and folds - entire index *)
   val iter_all : (key list -> 'a -> unit) -> 'a t -> unit
   val map_all : ('a -> 'b) -> 'a t -> 'b t
   val mapi_all : (key list -> 'a -> 'b) -> 'a t -> 'b t
   val fold_all : ('a -> key list -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val fold_map_all : ('a -> key list -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
end

(*
 * Make the index.
 *)
module LmMake (Base : OrderedType) : LmIndex with type key = Base.t =
struct

   (*
    * Construct table type for elements.
    *)
   module EltTable = Lm_map.LmMake (Base)

   (*
    * An index is a tree of nested tables.
    *)
   type ('elt, 'data) tree =
      Leaf
    | Index of (('elt, 'data) tree * 'data) EltTable.t

   type key = Base.t
   type 'a t = (key, 'a) tree

   (*
    * An empty index is a leaf.
    *)
   let empty = Leaf

   (*
    * Test if index is empty.
    *)
   let is_empty = function
      Leaf ->
         true
    | Index _ ->
         false

   (*
    * Test membership of a key in an index.
    *)
   let mem index key =
   	match index with
   		Leaf ->
   			false
   	 | Index table ->
   	 		EltTable.mem table key

   (*
    * Lookup a key in an index.
    *)
   let find index key =
   	match index with
   		Leaf ->
   			raise Not_found
   	 | Index table ->
   			EltTable.find table key

   (*
    * Lookup the subindex for a key in an index.
    *)
   let find_index index key =
      let index, _ = find index key in
         index

   (*
    * Lookup the data for a key in an index.
    *)
   let find_data index key =
      let _, data = find index key in
         data

   (*
    * Test for membership of a key sequence in an index.
    *)
   let rec mem_list index keys =
      match keys with
         [] ->
            false
       | key :: [] ->
            mem index key
       | key :: keys ->
            let index = find_index index key in
               mem_list index keys

   (*
    * Lookup a key sequence in an index.
    *)
   let rec find_list index keys =
      match keys with
         [] ->
            raise Not_found
       | key :: [] ->
            find index key
       | key :: keys ->
            let index = find_index index key in
               find_list index keys

   (*
    * Lookup the subindex for a key sequence.
    *)
   let find_list_index index keys =
      let index, _ = find_list index keys in
         index

   (*
    * Lookup the data for a key sequence.
    *)
   let find_list_data index keys =
      let _, data = find_list index keys in
         data

   (*
    * Lookup subindex and data for a key, return an option.
    *)
   let find_opt index key =
      try
         Some (find index key)
      with
         Not_found ->
            None
   (*
    * Lookup subindex for a key, return an option.
    *)
   let find_index_opt index key =
      try
         Some (find_index index key)
      with
         Not_found ->
            None

   (*
    * Lookup data for key, return an option (Some data or None).
    *)
   let find_data_opt index key =
      try
         Some (find_data index key)
      with
         Not_found ->
            None

   (*
    * Filter add a (key, (index, data)) pair to an index.
    *)
   let filter_add_i index key f =
      match index with
         Leaf ->
            Index (EltTable.add (EltTable.empty) key (f None))
       | Index table ->
            let i_entry = find_opt index key in
               Index (EltTable.add table key (f i_entry))

   (*
    * Filter add a (key, data) pair to an index.
    *)
   let filter_add index key f =
      filter_add_i index key (
         fun i_entry ->
            match i_entry with
               None ->
                  Leaf, f None
             | Some (i, d) ->
                  i, f (Some d)
      )

   (*
    * Filter remove a key from an index using its (index, data) entry
    *)
   let filter_remove_i index key f =
      match index with
         Leaf ->
            Leaf
       | Index table ->
            let i_entry = f (find index key) in
               match i_entry with
                  Some _ ->
                     Index (EltTable.remove table key)
                | None ->
                     index

   (*
    * Filter remove a key from an index using its data.
    *)
   let filter_remove index key f =
      filter_remove_i index key (
         fun (i, d) ->
            match (f d) with
               Some d ->
                  Some (i, d)
             | None ->
                  None
      )

   (*
    * Filter add a chain of (key, (index, data)) pairs to an index.
    *)
   let rec filter_add_list_i index keys fs =
      match keys, fs with
         [], [] ->
            index
       | key :: [], f :: [] ->
            filter_add_i index key f
       | key :: keys, f :: fs ->
            let index = filter_add_i index key f in
            let subindex, data = find index key in
            let subindex = filter_add_list_i subindex keys fs in
               filter_add_i index key (fun _ -> subindex, data)
       | _ ->
            raise (Invalid_argument "filter_add_list_i")

   (*
    * Filter add a chain of (key, data) pairs to an index.
    *)
   let rec filter_add_list index keys fs =
      match keys, fs with
         [], [] ->
            index
       | key :: [], f :: [] ->
            filter_add index key f
       | key :: keys, f :: fs ->
            let index = filter_add index key f in
            let subindex, data = find index key in
            let subindex = filter_add_list subindex keys fs in
               filter_add_i index key (fun _ -> subindex, data)
       | _ ->
            raise (Invalid_argument "filter_add_list")

   (*
    * Filter add an (index, data) entry at the end of a chain in the index.
    *)
   let rec filter_add_entry_i index keys f =
      match keys with
         [] ->
            index
       | key :: [] ->
            filter_add_i index key f
       | key :: keys ->
            let subindex, data = find index key in
            let subindex = filter_add_entry_i subindex keys f in
               filter_add_i index key (fun _ -> subindex, data)


   (*
    * Filter add a data entry at the end of a chain in the index.
    *)
   let filter_add_entry index keys f =
      filter_add_entry_i index keys (
         fun i_entry ->
            match i_entry with
               None ->
                  Leaf, f None
             | Some (i, d) ->
                  i, f (Some d)
      )

   (*
    * Filter remove an (index, data) entry at the end of a chain in the index.
    *)
   let rec filter_remove_entry_i index keys f =
      match keys with
         [] ->
            index
       | key :: [] ->
            filter_remove_i index key f
       | key :: keys ->
            let subindex, data = find index key in
            let subindex = filter_remove_entry_i subindex keys f in
               filter_add_i index key (fun _ -> subindex, data)

   (*
    * Filter remove a data entry at the end of a chain in the index.
    *)
   let filter_remove_entry index keys f =
      filter_remove_entry_i index keys (
         fun (i, d) ->
            match (f d) with
               Some d ->
                  Some (i, d)
             | None ->
                  None
      )

   (*
    * Add a (key, data) pair to an index.
    *)
   let add index key data =
      filter_add index key (fun _ -> data)

   (*
    * Add a (key, (index, data)) pair to an index.
    *)
   let add_i index key i_entry =
      filter_add_i index key (fun _ -> i_entry)

   (*
    * Remove a key from an index.
    *)
   let remove index key =
      filter_remove index key (fun data -> Some data)

   (*
    * Add a chain of (key, data) pairs to an index.
    *)
   let add_list index keys datas =
      let fs = List.map (fun data -> (fun _ -> data)) datas in
         filter_add_list index keys fs

   (*
    * Add a chain of (key, (index, data)) pairs to an index.
    *)
   let add_list_i index keys i_entries =
      let fs = List.map (fun i_entry -> (fun _ -> i_entry)) i_entries in
         filter_add_list_i index keys fs

   (*
    * Add a data entry at the end of a chain in the index.
    *)
   let add_entry index keys data =
      filter_add_entry index keys (fun _ -> data)

   (*
    * Add an (index, data) entry at the end of a chain in the index.
    *)
   let add_entry_i index keys i_entry =
      filter_add_entry_i index keys (fun _ -> i_entry)

   (*
    * Remove an entry at the end of a chain in the index.
    *)
   let remove_entry index keys =
      filter_remove_entry index keys (fun data -> Some data)

   (*
    * Iterate over the top level entries.
    *)
   let iter f = function
      Leaf ->
         ()
    | Index table ->
         EltTable.iter f table

   (*
    * Apply map to the top level entries.
    *)
   let map f = function
      Leaf ->
         Leaf
    | Index table ->
         Index (EltTable.map f table)

   (*
    * Apply map to top level entries using keys.
    *)
   let mapi f = function
      Leaf ->
         Leaf
    | Index table ->
         Index (EltTable.mapi f table)

   (*
    * Fold over top level entries.
    *)
   let fold f data = function
      Leaf ->
         data
    | Index table ->
         EltTable.fold f data table

   (*
    * Fold map over top level entries.
    *)
   let fold_map f data = function
      Leaf ->
         data, Leaf
    | Index table ->
         let data', table' = EltTable.fold_map f data table in
            data', Index (table')

   (*
    * Iterate over every entry in the index.
    *)
   let rec iter_all_list f keys = function
      Leaf ->
         ()
    | Index table ->
         EltTable.iter (
            fun key (index, data) ->
               let keys = keys @ [key] in
                  f keys data;
                  iter_all_list f keys index
         ) table

   let iter_all f index =
      iter_all_list f [] index

   (*
    * Apply map to every element in the index.
    *)
   let rec map_all f = function
      Leaf ->
         Leaf
    | Index table ->
         Index (
            EltTable.map (
               fun (index, data) ->
                  (map_all f index, f data)
            ) table
         )

   (*
    * Apply map using keys.
    *)
   let rec mapi_all_list f keys = function
      Leaf ->
         Leaf
    | Index table ->
         Index (
            EltTable.mapi (
               fun key (index, data) ->
                  let keys = keys @ [key] in
                     (mapi_all_list f keys index, f keys data)
            ) table
         )

   let mapi_all f index =
      mapi_all_list f [] index

   (*
    * Fold over the index.
    *)
   let rec fold_all_list f keys data = function
      Leaf ->
         data
    | Index table ->
         EltTable.fold (
            fun a key (index, b) ->
               let keys = keys @ [key] in
                  (fold_all_list f keys (f a keys b) index)
         ) data table

   let fold_all f data index =
      fold_all_list f [] data index

   (*
    * Fold map over the index.
    *)
   let rec fold_map_all_list f keys data = function
      Leaf ->
         data, Leaf
    | Index table ->
         let data', table' =
            EltTable.fold_map (
               fun a key (index, b) ->
                  let keys = keys @ [key] in
                  let res_a, res_c = f a keys b in
                  let res_a', index' = fold_map_all_list f keys res_a index in
                     res_a', (index', res_c)
            ) data table
         in
            data', Index (table')

   let fold_map_all f data index =
      fold_map_all_list f [] data index
end
