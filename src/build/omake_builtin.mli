open Omake_env
open Omake_builtin_type

(*
 * Register some builtin info.
 *)
val register_builtin : builtin_info -> unit

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
val venv_add_command_defs : venv -> venv

(*
 * Builtin functions.
 *)
val venv_add_builtins : venv -> venv
val venv_add_pervasives : venv -> venv
val venv_include_rc_file : venv -> string -> venv

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
