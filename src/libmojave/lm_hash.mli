
(************************************************************************
 * A basic table for adding a hash code to every element.
 * Nothing else is done, so comparisons are still slow.
 * This table is safe to marshal.
 *)
module MakeHash (Arg : Lm_hash_sig.HashArgSig) : Lm_hash_sig.HashSig with type elt = Arg.t;;

(**
   Table-based hash-consing.
   Items are represented by their indexes into a table.

   This is the fastest implementation, but it is not safe to marshal
   unless you also marshal the table.

   If you need a version that is safe to marshal, consider using the
   HashMarshal below.  It is only slightly slower.
 *)
module MakeHashCons (Arg : Lm_hash_sig.HashArgSig)
: Lm_hash_sig.HashConsSig
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
module MakeHashMarshal (Arg : Lm_hash_sig.HashMarshalArgSig)
: Lm_hash_sig.HashMarshalSig
   with type elt = Arg.t
   with type t = Arg.t hash_marshal_item

(**  A variant with two equalities (see Lm_hash_sig for detail) *)
module MakeHashMarshalEq (Arg : Lm_hash_sig.HashMarshalEqArgSig)
: Lm_hash_sig.HashMarshalEqSig
   with type elt = Arg.t
   with type t = Arg.t hash_marshal_eq_item

val pp_print_hash_stats : Format.formatter -> unit

(************************************************************************
 * Better-than-usual hashes.
 *)
module HashCode : Lm_hash_sig.HashCodeSig
module HashDigest : Lm_hash_sig.HashDigestSig

(************************************************************************
 * Helper functions.
 *)
val hash_combine : int -> int -> int
val hash_int_list : int -> int list -> int
val compare_int_list : int list -> int list -> int
