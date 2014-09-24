

(*
 * Register some builtin info.
 *)
val register_builtin : Omake_builtin_type.builtin_info -> unit

(*
 * Add a command line variable definition.
 *)
val add_command_def : string -> string -> unit

(*
 * Check if there are command defs.
 *)
val command_defs_are_nonempty : unit -> bool

(*
 * Add all the command-line defs to the encironment.
 *)
val venv_add_command_defs : Omake_env.t -> Omake_env.t

(*
 * Builtin functions.
 *)
val venv_add_builtins : Omake_env.t -> Omake_env.t
val venv_add_pervasives : Omake_env.t -> Omake_env.t
val venv_include_rc_file : Omake_env.t -> string -> Omake_env.t

