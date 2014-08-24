

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
val venv_add_command_defs : Omake_env.venv -> Omake_env.venv

(*
 * Builtin functions.
 *)
val venv_add_builtins : Omake_env.venv -> Omake_env.venv
val venv_add_pervasives : Omake_env.venv -> Omake_env.venv
val venv_include_rc_file : Omake_env.venv -> string -> Omake_env.venv

