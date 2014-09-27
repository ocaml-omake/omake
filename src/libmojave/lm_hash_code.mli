(************************************************************************
 * Better-than-usual hashes.
 *)
module HashCode : sig
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

module HashDigest : sig
   type t

   val create        : unit -> t
   val add_bits      : t -> int -> unit (* Adds the last 11 bits *)
   val add_int       : t -> int -> unit
   val add_nativeint : t -> Nativeint.t -> unit
   val add_int32  : t -> Int32.t -> unit
   val add_int64  : t -> Int64.t -> unit
   val add_float     : t -> float -> unit
   val add_string    : t -> string -> unit

   val add_char      : t -> char -> unit
   val add_bool      : t -> bool -> unit
   val add_substring : t -> string -> int -> int -> unit
   val digest        : t -> string
end


val hash_combine : int -> int -> int
val hash_int_list : int -> int list -> int
val hash_list : ('a -> int) -> 'a list -> int
