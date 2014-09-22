



val loc_of_exp : Omake_ast.exp -> Lm_location.t
val key_of_exp : Omake_ast.exp -> string
val scan_body_flag : Omake_ast.body_flag -> Omake_ast.exp -> Omake_ast.body_flag
val update_body : Omake_ast.exp -> Omake_ast.body_flag -> Omake_ast.exp list -> Omake_ast.exp
val can_continue : Omake_ast.exp -> string option
val flatten_sequence_prog : Omake_ast.prog -> Omake_ast.prog
val flatten_string_prog   : Omake_ast.prog -> Omake_ast.prog

