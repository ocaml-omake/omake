(*
 * Signatures for the various hash functions and hash-cons tables.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2007 Mojave Group, California Institute of Technology
 * and HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)

(************************************************************************
 * A basic table for adding a hash code to every element.
 * Nothing else is done, so comparisons are still slow.
 * This table is safe to marshal.
 *)
module type HashArgSig =
sig
   type t

   (* For debugging *)
   val debug : string

   (* The client needs to provide hash and comparison functions *)
   val hash : t -> int
   val compare : t -> t -> int
end

(*
 * A basic hashtbale.
 *)
module type HashSig =
sig
   type elt
   type t

   (* Creation *)
   val create : elt -> t
   val get : t -> elt

   (* Hash code *)
   val hash : t -> int

   (* Comparison *)
   val compare : t -> t -> int
end

(************************************************************************
 * Table-based hash-consing.
 * Items are represented by their indexes into a table.
 *
 * This is the fastest implementation, but it is not safe to marshal
 * unless you also marshal the table.
 *
 * If you need a version that is safe to marshal, consider using the
 * HashMarshal below.  It is only slightly slower.
 *)
module type HashConsSig =
sig
   type hash
   type state
   type elt
   type t

   (* States *)
   val create_state : unit -> state
   val length : state -> int

   (* Normal creation *)
   val icreate : state -> hash -> t
   val create : state -> elt -> t
   val get : state -> t -> elt

   (* Hash code *)
   val hash : t -> int

   (* Comparison *)
   val compare : t -> t -> int

   (* Map over an array of hash codes *)
   val map_array : (t -> elt -> 'a) -> state -> 'a array

   (* Fold over all of the items *)
   val fold : ('a -> t -> 'a) -> 'a -> state -> 'a
end

(************************************************************************
 * Marshalable version.
 *
 * This takes a slightly different approach, wrapping the value in
 * a triple of a hash code and a dummy ref cell.  During marshaling,
 * the cell will point somewhere else, so we know that the value
 * must be reinterned.  The hash codes are preseved across
 * marshaling.
 *)

(*
 * The client needs to provide these functions.
 *)
module type HashMarshalArgSig =
sig
   type t

   (* For debugging *)
   val debug : string

   (* The client needs to provide hash and comparison functions *)
   val hash : t -> int
   val compare : t -> t -> int
   val reintern : t -> t
end

(*
 * This is what we get.
 *)
module type HashMarshalSig =
sig
   type elt
   type t

   (* Creation *)
   val create   : elt -> t

   (* The intern function fails with Not_found if the node does not already exist *)
   val intern   : elt -> t

   (* Destructors *)
   val get      : t -> elt
   val hash     : t -> int

   (* Comparison *)
   val equal    : t -> t -> bool
   val compare  : t -> t -> int

   (* Rehash the value *)
   val reintern : t -> t
end

(************************************************************************
 * A variation on the above marshalable version, with two equalities.
 *
 * Here we assume that the argument type has two notions of equality:
 * - A strong equality ("idenitity"). Two strongly equal items are considered
 *   identical and should be coalesced during cons-hashing.
 * - A weak equality ("equivalence"). The weakly equal items should be
 *   considered equivalent for the purposes of sets and tables, but they may
 *   have some individual representational characteristics that should be
 *   preserved.
 *
 * An example of this is filenames on case-insensitive case-preserving
 * filesystems. Here the strong equality is the normal string equality
 * (ensures case preservation) and the weak equality is the equality of the
 * canonical (e.g. lowercase) representations (ensures case insensitivity).
 *)

(*
 * The client needs to provide these functions.
 *)
module type HashMarshalEqArgSig =
sig
   type t

   (* For debugging *)
   val debug : string

   (*
    * The client needs to provide the hash and the two comparison functions.
    *)
   val fine_hash      : t -> int
   val fine_compare   : t -> t -> int

   val coarse_hash    : t -> int
   val coarse_compare : t -> t -> int

   (* Rehash the value *)
   val reintern       : t -> t
end

(*
 * This is what we get.
 *)
module type HashMarshalEqSig =
sig
   include HashMarshalSig (* The default equality is the coarse one *)

   val fine_hash    : t -> int
   val fine_compare : t -> t -> int
   val fine_equal   : t -> t -> bool
end

(************************************************************************
 * Better-than-usual hashes.
 *)
module type HashCodeSig =
sig
   type t

   val create     : unit -> t
   val add_bits   : t -> int -> unit (* Adds the last 11 bits *)
   val add_int    : t -> int -> unit
   val add_nativeint : t -> Nativeint.t -> unit
   val add_int32  : t -> Int32.t -> unit
   val add_int64  : t -> Int64.t -> unit
   val add_float  : t -> float -> unit
   val add_string : t -> string -> unit
   val code       : t -> int
end

module type HashDigestSig =
sig
   type t

   val create        : unit -> t
   val add_bits      : t -> int -> unit (* Adds the last 11 bits *)
   val add_bool      : t -> bool -> unit
   val add_int       : t -> int -> unit
   val add_nativeint : t -> Nativeint.t -> unit
   val add_int32  : t -> Int32.t -> unit
   val add_int64  : t -> Int64.t -> unit
   val add_float     : t -> float -> unit
   val add_char      : t -> char -> unit
   val add_string    : t -> string -> unit
   val add_substring : t -> string -> int -> int -> unit
   val digest        : t -> string
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
