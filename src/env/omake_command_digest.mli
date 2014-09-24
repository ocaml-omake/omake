
(*
 * Digests.
 *)
val digest_of_exp      : 
  Omake_value_type.pos -> Omake_value_type.t list -> Omake_ir.exp -> Omake_command_type.command_digest
val digest_of_commands : 
  Omake_value_type.pos -> Omake_env.arg_command_line list -> Omake_command_type.command_digest

