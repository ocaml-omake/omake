type handle
type 'a t

(* Handles *)
val create_handle : 'a t -> int -> handle
val new_handle    : 'a t -> handle
val int_of_handle : handle -> int

(* Table *)
val create : unit -> 'a t
val add    : 'a t -> handle -> 'a -> unit
val remove : 'a t -> handle -> unit
val find   : 'a t -> handle -> 'a
val find_any : 'a t -> handle -> 'a
val find_any_handle : 'a t -> int -> handle
val find_value : 'a t -> int -> 'a -> handle
