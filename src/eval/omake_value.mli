(**
  If there is only one value,
  do not create the array. *)
val concat_array : Omake_value_type.value list -> Omake_value_type.value


(*
 * Concatenate some strings.
 * If there is only 1 value, do not create the array.
 *)
val concat_strings : string list -> Omake_value_type.value

(*
 * Expand a value so that the outermost constructor
 * is not an application.
 *)
val eval_value        : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value
val eval_single_value : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value
val eval_prim_value   : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value
val eval_object_value : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.obj -> Omake_value_type.value

val add_object_value  : Omake_value_type.obj -> Omake_value_type.value -> Omake_value_type.obj

(*
 * Convert to a string.
 *)
val string_of_value  : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> string
val strings_of_value : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> string list
val values_of_value  : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value list
val vars_of_value    : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_ir.var_info list

(*
 * Coercions.
 *)
val bool_of_value       : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> bool
val int_of_value        : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> int
val float_of_value      : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> float
val number_of_value     : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value
val key_of_value        : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value
val map_of_value        : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.map

val dir_of_value        : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_node.Dir.t
val file_of_value       : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_node.Node.t
val node_value_of_value : Omake_env.venv -> Omake_value_type.pos -> ?follow_symlinks:bool -> Omake_value_type.value -> Omake_value_type.value
val dir_value_of_value  : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value
val filename_of_value   : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> string

val prim_channel_of_value    : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.prim_channel
val prim_channel_of_var      : Omake_env.venv -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var_info -> Omake_value_type.prim_channel
val channel_of_var           : Omake_env.venv -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var_info -> Lm_channel.t
val channel_of_value         : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Lm_channel.t
val in_channel_of_any_value  : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.prim_channel * bool
val out_channel_of_any_value : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.prim_channel * bool

val is_glob_value            : Lm_glob.glob_options -> Omake_value_type.value -> bool
val is_glob_value_list       : Lm_glob.glob_options -> Omake_value_type.value list -> bool

val current_lexer            : Omake_env.venv -> Omake_value_type.pos -> Omake_lexer.Lexer.t
val current_parser           : Omake_env.venv -> Omake_value_type.pos -> Omake_parser.Parser.t
val loc_of_value             : Omake_env.venv -> Omake_value_type.pos -> Omake_value_type.value -> Lm_location.t

