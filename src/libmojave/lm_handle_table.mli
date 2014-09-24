type 'a t
type handle

val create : unit -> 'a t
val add : 'a t -> 'a -> handle
val find : 'a t -> handle -> 'a
