
val parse_sym : Omake_env.t ->
  Omake_value_type.pos ->
  Lm_location.t -> string -> Omake_ir.var_info * Omake_ir.var list

val is_leaf_command : Omake_build_type.command -> bool


(*
 * A node is a leaf if it has no dependencies and no commands.
 *)
val is_leaf_node    : Omake_build_type.env -> Omake_node.Node.t -> bool


(*
 * Unfortunately, we have to specify the environment imperatively.
 *)
val set_env : Omake_build_type.env -> unit
val get_env : Omake_value_type.pos -> Lm_location.t -> Omake_build_type.env
val is_build_phase : unit -> bool


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
 * Get the object from a file.
 *)
val object_of_file : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> string ->
  Omake_value_type.obj


(*
 * Test for a dependency.
 * The symbol is the name of the ordering relation.
 * The bool is whether to work in debug mode.
 *)
val sort :
  Omake_build_type.env -> 
  Omake_env.t ->
  Omake_value_type.pos ->
  Lm_symbol.t ->
  Omake_node.Node.t list ->
  Omake_node.Node.t list

val check_sort : 
  Omake_build_type.env ->
  Omake_env.t ->
  Omake_value_type.pos ->
  Lm_symbol.t ->
  Omake_node.Node.t list -> unit

