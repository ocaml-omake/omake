(* This is for VERY SMALL bitsets only that fit into a few machine words *)

type t
val create : unit -> t
val is_set : t -> int -> bool
val set : t -> int -> t
