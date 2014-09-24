
val debug_rule     : bool ref
val debug_build    : bool ref
val debug_deps     : bool ref

(*
 * .omakedb save interval (0 - disable)
 *)
val save_interval  : float ref

(*
 * Examining the state.
 * Note that in a non-standard build phase (such as .DUILD_SUCCESS),
 * this function will process _both_ the phase-specific worklist and the main worklist.
 *)
val command_fold   : 
    Omake_build_type.env -> Omake_build_type.command_tag -> 
      ('a -> Omake_build_type.command -> 'a) -> 'a -> 'a

(*
 * Build the system.
 *)
val build : Omake_options.t -> string -> string list -> unit
val build_fun : Omake_env.t -> string list -> bool

