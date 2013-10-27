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

(*
 * Used for expressing sharing constraints.
 *)
module type TypeSig =
sig
   type t
end

(*
 * A generic type of totally ordered elements.
 *)
module type OrderedType =
sig
   type t
   val compare : t -> t -> int
end

(*
 * Ordered type need for debugging.
 *)
module type OrderedTypeDebug =
sig
   type t

   val print : out_channel -> t -> unit
   val compare : t -> t -> int
end

(*
 * Set signature.
 *)
module type MinimalSetSig =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : t -> elt -> bool
   val add : t -> elt -> t
   val singleton : elt -> t
   val remove : t -> elt -> t
   val cardinal : t -> int

   val union : t -> t -> t
   val intersectp : t -> t -> bool

   val iter : (elt -> unit) -> t -> unit

   val mem_filt : t -> elt list -> elt list
   val fst_mem_filt : t -> (elt * 'a) list -> (elt * 'a) list
   val not_mem_filt : t -> elt list -> elt list

   val of_sorted_list : elt list -> t
   val of_list : elt list -> t
   val to_list : t -> elt list
   val elements : t -> elt list
end

(*
 * Our version.
 *)
module type LmSet =
sig
   include MinimalSetSig

   val inter : t -> t -> t
   val diff : t -> t -> t
   val compare : t -> t -> int
   val equal : t -> t -> bool

   (*
    * These two functions are identical.
    * subset s1 s2 tests whether s1 is a subset of s2.
    *)
   val subset : t -> t -> bool
   val is_subset : t -> t -> bool

   val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
   val range_fold : (elt -> int) -> ('a -> elt -> 'a) -> 'a -> t -> 'a

   val for_all : (elt -> bool) -> t -> bool
   val exists : (elt -> bool) -> t -> bool

   val filter : (elt -> bool) -> t -> t
   val partition : (elt -> bool) -> t -> t * t

   val min_elt : t -> elt
   val max_elt : t -> elt
   val choose : t -> elt

   val add_list : t -> elt list -> t
   val subtract_list : t -> elt list -> t
end

module type LmSetDebug =
sig
   include LmSet

   val print : out_channel -> t -> unit
end

(*
 * Debugging version of a set.
 *)
module type DebugMinimalSetSig =
sig
   include MinimalSetSig

   (* Debugging function *)
   val print : out_channel -> t -> unit
end

(*
 * Backwards-compatible version.
 *)
module type S =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : elt -> t -> bool
   val add : elt -> t -> t
   val singleton : elt -> t
   val remove : elt -> t -> t
   val union : t -> t -> t
   val inter : t -> t -> t
   val diff : t -> t -> t
   val compare : t -> t -> int
   val equal : t -> t -> bool
   val subset : t -> t -> bool
   val iter : (elt -> unit) -> t -> unit
   val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
   val for_all : (elt -> bool) -> t -> bool
   val exists : (elt -> bool) -> t -> bool
   val filter : (elt -> bool) -> t -> t
   val partition : (elt -> bool) -> t -> t
   val cardinal : t -> int
   val elements : t -> elt list
   val min_elt : t -> elt
   val max_elt : t -> elt
   val choose : t -> elt
end

(*
 * Linearly ordered set.
 *)
module type LinearSetSig =
sig
   type elt
   type t
   type index = int

   val empty : t
   val singleton : elt -> t
   val length : t -> int
   val get : t -> index -> elt
   val make : int -> elt -> t
   val create : int -> elt -> t
   val to_list : t -> elt list
   val of_sorted_list : elt list -> t
   val of_list : elt list -> t
   val iter : (elt -> unit) -> t -> unit
   val split : t -> index -> t * elt * t
   val append : t -> elt -> t -> t
   val append_list : t -> elt list -> t -> t
   val lazy_apply : (elt -> elt) -> t -> t
   val lazy_sub_map : (elt -> elt) -> t -> index -> index -> t

   val mapi : (index -> elt -> elt) -> t -> t
   val init : int -> (index -> elt) -> t
   val collect : (elt, t) Lm_array_util.array_part list -> t
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
