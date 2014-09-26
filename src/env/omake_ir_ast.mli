
(*
 * Parsing environments.
 *)
type penv

(*
 * Parse a variable declaration.
 *)
val parse_declaration : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var list -> Omake_ir.method_name

(*
 * Environment for parsing AST files.
 *)
type senv_open_file  = string -> Omake_value_type.pos -> Lm_location.t -> Omake_node.Node.t * Omake_ir.senv

(*
 * Internal function for converting string expressions.
 *)
val build_string     : penv -> Omake_ast.exp -> Omake_value_type.pos -> penv * Omake_ir.string_exp

(*
 * Create a parsing environment for the given file.
 *    penv_create (file, pervasives_id)
 *)
val penv_create        : senv_open_file -> Omake_env.t -> Omake_node.Node.t -> penv
val penv_class_names   : penv -> Lm_symbol.t list * Omake_ir.senv
val penv_of_vars       : senv_open_file -> Omake_env.t -> Omake_node.Node.t -> Omake_ir.senv -> penv

(*
 * Compile an AST program.
 *)
val compile_string   : penv -> Omake_ast.exp -> Omake_value_type.pos -> penv * Omake_ir.string_exp
val compile_exp      : penv -> Omake_ast.exp        -> penv * Omake_ir.t
val compile_exp_list : penv -> Omake_ast.exp list   -> penv * Omake_ir.t
val compile_prog     : penv -> Omake_ast.prog       -> penv * Omake_ir.t
