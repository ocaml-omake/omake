(*
 * Argument parser.
 *)
type arg_buffer

val arg_buffer_empty      : arg_buffer
val arg_buffer_add_string : arg_buffer -> string -> arg_buffer
val arg_buffer_add_data   : arg_buffer -> string -> arg_buffer
val arg_buffer_contents   : arg_buffer -> Omake_command_type.arg_string list

(*
 * Parse commands.
 *)
val parse_commands : 
  'venv -> Omake_node.Dir.t -> Omake_node.Node.t -> Lm_location.loc ->
  (Omake_command_type.command_flag list * 
     ('exp, ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) Omake_shell_type.poly_pipe, 'value)
       Omake_command_type.poly_command_inst) list ->
  ('venv, 'exp, 
   ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) Omake_shell_type.poly_pipe,
   'value) Omake_command_type.poly_command_line list

(*
 * Add the output flag.
 *)
val command_allow_output :
   ('venv, 'exp, 'argv, 'value) Omake_command_type.poly_command_line ->
   ('venv, 'exp, 'argv, 'value) Omake_command_type.poly_command_line
