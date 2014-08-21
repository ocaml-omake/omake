(*  Type of job identifiers. *)
type t


module IdTable : Lm_map_sig.LmMap with type key = t



(*  "Null" id for the "master" process. *)
val null_id : t

(*  Get a new id. *)
val create : unit -> t

val pp_print_pid : t Lm_printf.t

val marshal_id : t -> Omake_marshal.msg
val unmarshal_id : Omake_marshal.msg -> t

