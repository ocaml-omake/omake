
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
