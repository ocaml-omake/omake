
(** Marshalable version.

   This takes a slightly different approach, wrapping the value in
   a triple of a hash code and a dummy ref cell.  During marshaling,
   the cell will point somewhere else, so we know that the value
   must be reinterned.  The hash codes are preseved across
   marshaling.

   BUG: we break abstraction here a little because
   it is hard to define the type recursively otherwise.
 *)

module type MARSHAL = 
sig
  type t

  (* For debugging *)
  val debug : string

  (* The client needs to provide hash and comparison functions *)
  val hash : t -> int
  val compare : t -> t -> int
  val reintern : t -> t
end


module type MARSHAL_EQ = 
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


module type HashMarshalEqSig =
sig
   include HashMarshalSig (* The default equality is the coarse one *)

   val fine_hash    : t -> int
   val fine_compare : t -> t -> int
   val fine_equal   : t -> t -> bool
end


module MakeCoarse (Arg : MARSHAL)
: HashMarshalSig with type elt = Arg.t


(**  A variant with two equalities (see Lm_hash_sig for detail)
     Here we assume that the argument type has two notions of equality:
     - A strong equality ("idenitity"). Two strongly equal items are considered
     identical and should be coalesced during cons-hashing.
     - A weak equality ("equivalence"). The weakly equal items should be
     considered equivalent for the purposes of sets and tables, but they may
     have some individual representational characteristics that should be
     preserved.

     An example of this is filenames on case-insensitive case-preserving
     filesystems. Here the strong equality is the normal string equality
     (ensures case preservation) and the weak equality is the equality of the
     canonical (e.g. lowercase) representations (ensures case insensitivity).
 *)
module MakeFine (Arg : MARSHAL_EQ)
: HashMarshalEqSig
   with type elt = Arg.t


val pp_print_stats : Format.formatter -> unit

