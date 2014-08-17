
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
  val print : t Lm_printf.t
  val compare : t -> t -> int
end


(*
 * Our version.
 *)
module type LmSet =
sig
   type elt
   type t
   val empty : t
   val is_empty : t -> bool
   val mem : t -> elt -> bool
   val add : t -> elt -> t
   val singleton : elt -> t
   val remove : t -> elt -> t
   val union : t -> t -> t
   val inter : t -> t -> t
   val diff : t -> t -> t
   val compare : t -> t -> int
   val equal : t -> t -> bool
   (*
    * These two functions are identical.
    * subset s1 s2 tests whether s1 is a subset of s2.
    *)
   val subset : t -> t -> bool

   val iter : (elt -> unit) -> t -> unit
   val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
   val for_all : (elt -> bool) -> t -> bool
   val exists : (elt -> bool) -> t -> bool
   val filter : (elt -> bool) -> t -> t
   val partition : (elt -> bool) -> t -> t * t
   val cardinal : t -> int
   val elements : t -> elt list
   val min_elt : t -> elt
   val max_elt : t -> elt
   val choose : t -> elt
   val find : elt -> t -> elt
   (** compared with standard set, missing [split] *)
   val of_list : elt list -> t
   val to_list : t -> elt list
   val add_list : t -> elt list -> t
end

module type LmSetDebug =
sig
   include LmSet
   val print : t Lm_printf.t
end


