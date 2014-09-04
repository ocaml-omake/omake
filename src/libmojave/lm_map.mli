(*  Map module based on red-black trees *)
open Lm_map_sig

module Make       (Ord : OrderedType) : (S         with type key = Ord.t)
module LmMake     (Ord : OrderedType) : (LmMap     with type key = Ord.t)
module LmMakeList (Ord : OrderedType) : (LmMapList with type key = Ord.t)

(*
 * This version includes a sharing constraint so that maps can
 * be used in recursive definitions.  This exposes the internal
 * representation, should you should avoid using it unless
 * absolutely necessary (like in a recursive type definition).
 *)
type ('key, 'value) tree

module LmMakeRec (Ord : OrderedType) : (LmMap     with type key = Ord.t with type 'a t = (Ord.t, 'a) tree)
