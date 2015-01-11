val target_is_buildable : Omake_cache.t -> Omake_env.t -> Omake_value_type.pos -> Omake_node.Node.t -> bool
val target_is_buildable_proper : Omake_cache.t -> Omake_env.t -> Omake_value_type.pos -> Omake_node.Node.t -> bool
val venv_find_buildable_implicit_rule : Omake_cache.t -> Omake_env.t -> Omake_value_type.pos -> Omake_node.Node.t -> Omake_env.erule option

val target_is_buildable_in_path : Omake_cache.t -> Omake_env.t -> Omake_value_type.pos -> Omake_node.Dir.t list -> string list -> Omake_node.Node.t option
val target_is_buildable_in_path_1 : Omake_cache.t -> Omake_env.t -> Omake_value_type.pos -> (Omake_node.Dir.t * Omake_env.target_dir) list -> string list -> Omake_node.Node.t option

