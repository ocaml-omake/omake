




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

