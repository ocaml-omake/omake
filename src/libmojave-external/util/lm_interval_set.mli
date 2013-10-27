(*
 * Interval sets.
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
 * Elements.
 *)
module type OrderedType =
sig
   type t
   val compare : t -> t -> int
end

(*
 * Elements in countable sets.
 *)
module type CountableOrderedType =
sig
   type t

   val compare : t -> t -> int
   val succ : t -> t
   val pred : t -> t
   val min : t
   val max : t
end

(*
 * Sets specified as intervals.
 *)
type 'a bound =
   Infinity
 | Open of 'a
 | Closed of 'a

module type IntervalSetSig =
sig
   type elt
   type t

   (*
    * Set constructors.
    *)
   val empty : t
   val max_set : t
   val of_interval : elt bound -> elt bound -> t
   val is_empty : t -> bool
   val is_total : t -> bool
   val is_enum : t -> elt -> bool
   val to_enum : t -> elt

   (*
    * Set operations.
    *)
   val subset : t -> t -> bool
   val equal : t -> t -> bool
   val subtract : t -> t -> t
   val negate : t -> t
   val union : t -> t -> t
   val isect : t -> t -> t

   (*
    * Singletons.
    *)
   val of_point : elt -> t
   val mem_point : elt -> t -> bool
   val add_point : t -> elt -> t
   val is_singleton : t -> bool
   val dest_singleton : t -> elt
   val subtract_point : t -> elt -> t

   (*
    * Mapping.
    *)
   val iter : (elt bound -> elt bound -> unit) -> t -> unit
   val fold : ('a -> elt bound -> elt bound -> 'a) -> 'a -> t -> 'a
end

(*
 * Intervals over a dense set (like strings or rationals).
 * Need both open and closed intervals.
 *)
module DenseIntervalSet (Element : OrderedType)
: IntervalSetSig with type elt = Element.t

(*
 * Countable set has predecessor and successor functions.
 *)
module CountableIntervalSet (Element : CountableOrderedType)
: IntervalSetSig with type elt = Element.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
