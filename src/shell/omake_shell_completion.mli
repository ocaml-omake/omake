


(*
 * Set the command completion fur use by readline.
 * This should be called before each call to realine,
 * or after each shell prompt.
 *)
val set_completion_functions : Omake_env.venv -> Omake_value_type.pos -> Lm_location.loc -> unit

