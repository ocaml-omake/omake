(*
 * Types of the Set and Table modules.
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
open Lm_printf

(************************************************************************
 * Maps.
 *)
module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type LmMapBase =
sig
   type key
   type 'a t

   val empty : 'a t
   val is_empty : 'a t -> bool
   val cardinal : 'a t -> int
   val add : 'a t -> key -> 'a -> 'a t
   val find : 'a t -> key -> 'a
   val remove : 'a t -> key -> 'a t
   val mem : 'a t -> key -> bool
   val find_key : 'a t -> key -> key option
   val iter : (key -> 'a -> unit) -> 'a t -> unit
   val map : ('a -> 'b) -> 'a t -> 'b t
   val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
   val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
   val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
   val forall : (key -> 'a -> bool) -> 'a t -> bool
   val exists : (key -> 'a -> bool) -> 'a t -> bool
   val find_iter : (key -> 'a -> 'b option) -> 'a t -> 'b option
   val isect_mem : 'a t -> (key -> bool) -> 'a t
   val choose : 'a t -> key * 'a

   val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
   val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
   val replace : 'a t -> key -> ('a -> 'a) -> 'a t
   val keys : 'a t -> key list
   val data : 'a t -> 'a list
   val add_list : 'a t -> (key * 'a) list -> 'a t

   val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module type LmMap =
sig
   include LmMapBase

   val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

(*
 * This is the backwards-compatible version.
 *)
module type S =
sig
   type key
   type 'a t

   val empty : 'a t
   val add : key -> 'a -> 'a t -> 'a t
   val find : key -> 'a t -> 'a
   val remove : key -> 'a t -> 'a t
   val mem : key -> 'a t -> bool
   val iter : (key -> 'a -> unit) -> 'a t -> unit
   val map : ('a -> 'b) -> 'a t -> 'b t
   val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
   val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type LmMapList =
sig
   include LmMapBase

   val filter     : 'a t -> key -> ('a list -> 'a list) -> 'a t
   val find_all   : 'a t -> key -> 'a list
   val find_all_partial : 'a t -> key -> 'a list
   val iter_all   : (key -> 'a list -> unit) -> 'a t -> unit
   val mapi_all   : (key -> 'a list -> 'b list) -> 'a t -> 'b t
   val fold_all   : ('a -> key -> 'b list -> 'a) -> 'a -> 'b t -> 'a
   val data_all   : 'a t -> 'a list list
   val union      : (key -> 'a list -> 'a list -> 'a list) -> 'a t -> 'a t -> 'a t
   val choose_all : 'a t -> key * 'a list
end

(************************************************************************
 * Tables
 *)

(*
 * The record of methods.
 *)
type ('elt, 'data, 'table) table_methods =
   { empty        : 'table;
     make         : 'elt -> 'data list -> 'table;
     is_empty     : 'table -> bool;
     mem          : 'table -> 'elt -> bool;
     add          : 'table -> 'elt -> 'data -> 'table;
     replace      : 'table -> 'elt -> 'data list -> 'table;
     find         : 'table -> 'elt -> 'data;
     find_all     : 'table -> 'elt -> 'data list;
     remove       : 'table -> 'elt -> 'table;
     union        : 'table -> 'table -> 'table;
     elements     : 'table -> ('elt * 'data list) list;
     iter         : ('elt -> 'data -> unit) -> 'table -> unit;
     fold_map     : ('elt -> 'data -> 'table -> 'table) -> 'table -> 'table -> 'table;
     map          : ('elt -> 'data -> 'data) -> 'table -> 'table;
     cardinal     : 'table -> int;
     mem_filt     : 'table -> 'elt list -> 'elt list;
     not_mem_filt : 'table -> 'elt list -> 'elt list;
     intersectp   : 'table -> 'table -> bool;
     of_list      : ('elt * 'data list) list -> 'table;
     list_of      : 'table -> ('elt * 'data list) list;
     deletemax    : 'table -> ('elt * 'data list * 'table);

     (* Debugging *)
     print        : out_channel -> 'table -> unit
   }

(*
 * Creation functions.
 *)
type ('elt, 'data, 'table) table_create_type =
   (out_channel -> 'elt -> 'data list -> unit) ->    (* printer *)
   ('elt -> 'elt -> int) ->                          (* comparison function *)
   ('data list -> 'data list -> 'data list) ->       (* append during union *)
   ('elt, 'data, 'table) table_methods

(*
 * Module containing a creation function.
 *)
module type TableCreateSig =
sig
   type ('elt, 'data) t

   val create : ('elt, 'data, ('elt, 'data) t) table_create_type
end

(*
 * Ordering module.
 *)
module type TableBaseSig =
sig
   type elt
   type data

   val print : out_channel -> elt -> data list -> unit
   val compare : elt -> elt -> int
   val append : data list -> data list -> data list
end

(*
 * These are the functions provided by the table.
 *)
module type TableSig =
sig
   type elt
   type data
   type t

   val empty :  t
   val is_empty : t -> bool
   val length : t -> int
   val add : t -> elt -> data -> t
   val replace : t -> elt -> data list -> t
   val union : t -> t -> t
   val mem : t -> elt -> bool
   val find : t -> elt -> data
   val find_all : t -> elt -> data list (* last added first *)
   val remove : t -> elt -> t
   val iter : (elt -> data -> unit) -> t -> unit
   val fold_map : (elt -> data -> t -> t) -> t -> t -> t
   val map : (elt -> data -> data) -> t -> t
   val list_of : t -> (elt * data list) list
   val deletemax : t -> (elt * data list * t)
   val print : out_channel -> t -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
