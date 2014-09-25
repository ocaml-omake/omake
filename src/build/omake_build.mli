
val debug_rule     : bool ref
val debug_build    : bool ref
val debug_deps     : bool ref

(*
 * .omakedb save interval (0 - disable)
 *)
val save_interval  : float ref


val build : Omake_options.t -> string -> string list -> unit

(** Used in osh *)
val build_fun : Omake_env.t -> string list -> bool

