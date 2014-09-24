(**
  If there is only one value,
  do not create the array. *)
val concat_array : Omake_value_type.t list -> Omake_value_type.t


(*
 * Concatenate some strings.
 * If there is only 1 value, do not create the array.
 *)
val concat_strings : string list -> Omake_value_type.t

(*
 * Expand a value so that the outermost constructor
 * is not an application.
 *)
val eval_value        : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t
val eval_single_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t
val eval_prim_value   : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t
val eval_object_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.obj -> Omake_value_type.t

val add_object_value  : Omake_value_type.obj -> Omake_value_type.t -> Omake_value_type.obj

(*
 * Convert to a string.
 *)
val string_of_value  : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> string
val strings_of_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> string list
val values_of_value  : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t list
val vars_of_value    : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_ir.var_info list

(*
 * Coercions.
 *)
val bool_of_value       : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> bool
val int_of_value        : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> int
val float_of_value      : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> float
val number_of_value     : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t
val key_of_value        : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t
val map_of_value        : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.map

val dir_of_value        : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_node.Dir.t
val file_of_value       : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_node.Node.t
val node_value_of_value : Omake_env.t -> Omake_value_type.pos -> ?follow_symlinks:bool -> Omake_value_type.t -> Omake_value_type.t
val dir_value_of_value  : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t
val filename_of_value   : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> string

val prim_channel_of_value    : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.prim_channel
val prim_channel_of_var      : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var_info -> Omake_value_type.prim_channel
val channel_of_var           : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var_info -> Lm_channel.t
val channel_of_value         : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Lm_channel.t
val in_channel_of_any_value  : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.prim_channel * bool
val out_channel_of_any_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.prim_channel * bool

val is_glob_value            : Lm_glob.glob_options -> Omake_value_type.t -> bool
val is_glob_value_list       : Lm_glob.glob_options -> Omake_value_type.t list -> bool

val current_lexer            : Omake_env.t -> Omake_value_type.pos -> Omake_lexer.Lexer.t
val current_parser           : Omake_env.t -> Omake_value_type.pos -> Omake_parser.Parser.t
val loc_of_value             : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Lm_location.t

