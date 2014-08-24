

open Omake_env

open Omake_node
open! Omake_value_type
open! Omake_build_type

(*
 * Run-time symbols.
 *)
val defined_sym : venv -> pos -> Lm_location.loc -> string -> bool
val get_sym : venv -> pos -> Lm_location.loc -> string -> value
val add_sym : venv -> pos -> Lm_location.loc -> string -> value -> venv

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
 * Map over a sequence and add separators.
 *)
val sequence_map : ('a -> value) -> 'a list -> value list
val sequence_list : value list -> value list

(*
 * Boolean values.
 *)
val val_true : value
val val_false : value
val val_of_bool : bool -> value

(*
 * Unfortunately, we have to specify the environment imperatively.
 *)
val set_env : env -> unit
val get_env : pos -> Lm_location.loc -> env
val is_build_phase : unit -> bool

(*
 * A node is a leaf if it has no dependencies and no commands.
 *)
val is_leaf_command : command -> bool
val is_leaf_node    : env -> Node.t -> bool

(*
 * Get the object from a file.
 *)
val object_of_file : venv -> pos -> Lm_location.loc -> string -> obj

