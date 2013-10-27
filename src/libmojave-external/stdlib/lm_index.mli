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
 *  2002  May  1  Michael Maire  Changed interface for managing
 *                               subindices
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
module LmMake (Ord : OrderedType) : (LmIndex with type key = Ord.t)
