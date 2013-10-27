(*x
 * Various hash functions and hash-cons tables.
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
open Lm_printf
open Lm_hash_sig

(************************************************************************
 * A basic table for adding a hash code to every element.
 * Nothing else is done, so comparisons are still slow.
 * This table is safe to marshal.
 *)
module MakeHash (Arg : HashArgSig)
: HashSig with type elt = Arg.t;;

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
module MakeHashCons (Arg : HashArgSig)
: HashConsSig
  with type elt = Arg.t
  with type hash = MakeHash(Arg).t

(************************************************************************
 * Marshalable version.
 *
 * This takes a slightly different approach, wrapping the value in
 * a triple of a hash code and a dummy ref cell.  During marshaling,
 * the cell will point somewhere else, so we know that the value
 * must be reinterned.  The hash codes are preseved across
 * marshaling.
 *
 * BUG: we break abstraction here a little because
 * it is hard to define the type recursively otherwise.
 *)
type 'a hash_marshal_item
type 'a hash_marshal_eq_item

(*
 * Make a hash item.
 *)
module MakeHashMarshal (Arg : HashMarshalArgSig)
: HashMarshalSig
   with type elt = Arg.t
   with type t = Arg.t hash_marshal_item

(*
 * A variant with two equalities (see Lm_hash_sig for detail)
 *)
module MakeHashMarshalEq (Arg : HashMarshalEqArgSig)
: HashMarshalEqSig
   with type elt = Arg.t
   with type t = Arg.t hash_marshal_eq_item

val pp_print_hash_stats : formatter -> unit

(************************************************************************
 * Better-than-usual hashes.
 *)
module HashCode : HashCodeSig
module HashDigest : HashDigestSig

(************************************************************************
 * Helper functions.
 *)
val hash_combine : int -> int -> int
val hash_int_list : int -> int list -> int
val compare_int_list : int list -> int list -> int

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
