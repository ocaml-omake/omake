
val debug_rule     : bool ref
val debug_build    : bool ref
val debug_deps     : bool ref

(*
 * .omakedb save interval (0 - disable)
 *)
val save_interval  : float ref


(*
 * Build the system.
 *)
val build : Omake_options.t -> string -> string list -> unit
val build_fun : Omake_env.t -> string list -> bool

