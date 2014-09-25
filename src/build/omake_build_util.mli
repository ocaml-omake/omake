
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

val command_tag : Omake_build_type.command_state -> Omake_build_type.command_tag

val get_worklist_command : 
  Omake_build_type.env_wl ->  Omake_build_type.command_tag -> Omake_build_type.command option ref

val command_worklist : 
  Omake_build_type.env ->
  Omake_build_type.command_tag -> Omake_build_type.command option ref

val create_wl : unit -> Omake_build_type.env_wl


val pp_print_command_state : Omake_build_type.command_state Lm_printf.t

val pp_print_command :  Omake_build_type.command Lm_printf.t

val pp_print_node_states : Omake_build_type.env -> Omake_node.NodeSet.t Lm_printf.t

val print_stats : Omake_build_type.env -> string -> float -> unit

val print_failed : Omake_build_type.env ->  Omake_build_type.command_tag Lm_printf.t

val print_failed_targets : Omake_build_type.env -> Format.formatter -> unit(* Omake_build_type.env Lm_printf.t *)

val command_find : Omake_build_type.env ->
  Omake_build_type.command_tag ->
  (Omake_build_type.command -> bool) -> Omake_build_type.command

val command_exists : Omake_build_type.env ->
  Omake_build_type.command_tag ->
  (Omake_build_type.command -> bool) -> bool -> bool
val command_list_head : Omake_build_type.env ->
  Omake_build_type.command_tag -> Omake_build_type.command

val command_iter : Omake_build_type.env ->
  Omake_build_type.command_tag -> (Omake_build_type.command -> 'a) -> unit

val command_list_is_empty : Omake_build_type.env -> Omake_build_type.command_tag -> bool

(*
 * Examining the state.
 * Note that in a non-standard build phase (such as .DUILD_SUCCESS),
 * this function will process _both_ the phase-specific worklist and the main worklist.
 *)
val command_fold   : 
    Omake_build_type.env -> Omake_build_type.command_tag -> 
      ('a -> Omake_build_type.command -> 'a) -> 'a -> 'a


val wait_for_lock : unit -> unit
val unlock_db : unit -> unit
