



(*  Run-time symbols. *)
val defined_sym : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> string -> bool
val get_sym : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> string -> Omake_value_type.t
val add_sym : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> string -> Omake_value_type.t -> Omake_env.t


(*
 * Map over a sequence and add separators.
 *)
val sequence_map : ('a -> Omake_value_type.t) -> 'a list -> Omake_value_type.t list
val sequence_list : Omake_value_type.t list -> Omake_value_type.t list

(*
 * Boolean values.
 *)
val val_true : Omake_value_type.t
val val_false : Omake_value_type.t
val val_of_bool : bool -> Omake_value_type.t

