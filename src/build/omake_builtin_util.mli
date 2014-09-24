open Omake_node
open! Omake_value_type
open! Omake_build_type

(*
 * Run-time symbols.
 *)
val defined_sym : Omake_env.t -> pos -> Lm_location.t -> string -> bool
val get_sym : Omake_env.t -> pos -> Lm_location.t -> string -> Omake_value_type.t
val add_sym : Omake_env.t -> pos -> Lm_location.t -> string -> Omake_value_type.t -> Omake_env.t

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

(*
 * Unfortunately, we have to specify the environment imperatively.
 *)
val set_env : env -> unit
val get_env : pos -> Lm_location.t -> env
val is_build_phase : unit -> bool

(*
 * A node is a leaf if it has no dependencies and no commands.
 *)
val is_leaf_command : command -> bool
val is_leaf_node    : env -> Node.t -> bool

(*
 * Get the object from a file.
 *)
val object_of_file : Omake_env.t -> pos -> Lm_location.t -> string -> obj

