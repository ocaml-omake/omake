(*
 * Compile (evaluate) an OMakefile.
 *)

val print_ast   : bool ref
val print_ir    : bool ref
val print_rules : bool ref
val print_files : bool ref
val debug_eval  : bool ref

val raise_uncaught_exception : Omake_value_type.pos -> exn -> 'a

(*
 * Evaluate an expression.
 *)
val eval : Omake_env.t -> Omake_ir.exp -> Omake_value_type.t

val eval_exp : 
  Omake_env.t -> Omake_value_type.t -> Omake_ir.exp
  -> Omake_env.t * Omake_value_type.t

val eval_sequence : 
  Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t ->
  Omake_ir.exp list -> Omake_env.t * Omake_value_type.t

val eval_sequence_export : Omake_env.t -> Omake_value_type.pos ->
  Omake_value_type.t -> Omake_ir.exp list -> Omake_ir.export ->
  Omake_env.t * Omake_value_type.t

val eval_sequence_exp : Omake_env.t -> Omake_value_type.pos -> 
  Omake_ir.exp list -> Omake_env.t * Omake_value_type.t

val eval_sequence_export_exp : 
  Omake_env.t -> Omake_value_type.pos ->
  Omake_ir.exp list -> Omake_ir.export -> Omake_env.t * Omake_value_type.t

(*
 * String expression evaluation.
 *)
val eval_string_exp : 
  Omake_env.t -> Omake_value_type.pos -> Omake_ir.string_exp -> Omake_value_type.t

(*
 * Include the file literally.
 *)
val find_include_file : 
  Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> string -> Omake_node.Node.t

val eval_open_file : Omake_env.t -> Omake_ir_ast.senv_open_file

val eval_include_file : 
  Omake_env.t -> Omake_env.include_scope -> Omake_value_type.pos
  -> Lm_location.t -> Omake_node.Node.t -> Omake_env.t * Omake_value_type.t

val include_file : Omake_env.t -> 
  Omake_env.include_scope -> Omake_value_type.pos ->
  Lm_location.t -> Omake_node.Node.t -> Omake_env.t

(*
 * Evaluate a file as if it were an object.
 *)
val eval_object_file : 
  Omake_env.t -> Omake_value_type.pos -> Lm_location.t ->
  Omake_node.Node.t -> Omake_value_type.obj

(*
 * Evaluate the program.
 * This modifies the environment.
 *)
val compile : Omake_env.t -> unit

(*
 * Passes the IR thru Omake_ir_semant.build_prog, printing it if print_ir is enabled.
 *)
val postprocess_ir : Omake_env.t -> Omake_ir.t -> Omake_ir.t

(*
 * Evaluate a dependency file.
 *)
val compile_deps : Omake_env.t -> Omake_node.Node.t -> string -> (string list * string list) list

(*
 * Remove outermost applications.
 *)
val eval_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t

(*
 * Evaluate ValBody expressions.
 *)
val eval_body_value : 
  Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t

val eval_body_exp   : 
  Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t ->
  Omake_value_type.t -> Omake_env.t * Omake_value_type.t

(*
 * Get the object for the Omake_value_type.t.
 *)
val eval_object : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.obj

val eval_find_field : 
  Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var_info ->
  Omake_ir.var list -> Omake_value_type.path * Omake_value_type.obj * Omake_ir.var

val eval_find_method : Omake_env.t -> Omake_value_type.pos ->
  Lm_location.t -> Omake_ir.var_info -> Omake_ir.var list -> Omake_env.t * Omake_value_type.t

val eval_defined_field : Omake_env.t -> Omake_value_type.pos ->
  Lm_location.t -> Omake_ir.var_info -> Omake_ir.var list -> bool

(*
 * Evaluate a Omake_value_type.t that should be a function.
 * Be careful with this: don't create a ValPrim using
 * this function, since marshaling will fail.
 *)
val eval_fun : 
  Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t ->
  bool * Omake_env.prim_fun_data

(*
 * Also, if the Omake_value_type.t is an array of 1 element,
 * return the element.
 *)
val eval_single_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t

(*
 * Evaluate to a primitive Omake_value_type.t.
 * That is, if the Omake_value_type.t is an object, return the
 * primitive handle associated with the object.
 * If the object has no primitive Omake_value_type.t, the object
 * itself is returned.
 *)
val eval_prim_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t

(*
 * Evaluate a function application.
 *)
val eval_apply : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> Omake_value_type.t -> Omake_value_type.t list -> Omake_value_type.keyword_value list -> Omake_env.t * Omake_value_type.t
val eval_partial_apply : Omake_env.t -> Omake_value_type.pos -> Lm_location.t -> Omake_value_type.t -> Omake_value_type.t list -> Omake_value_type.keyword_value list -> Omake_env.t * Omake_value_type.t

(*
 * Conversions.
 * The following two functions should be used with care, since
 * they fail if the Omake_value_type.t contains an array.
 *)
val string_of_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> string
val string_of_quote : Omake_env.t -> Omake_value_type.pos -> char option -> Omake_value_type.t list -> string
val file_of_value   : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_node.Node.t
val path_of_values  : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t list -> string -> (bool * Omake_node.Dir.t list) list

(*
 * These conversions are safe to use anywhere.
 *)
val tokens_of_value  : Omake_env.t -> Omake_value_type.pos -> Omake_env.lexer -> Omake_value_type.t -> Omake_env.tok list
val arg_of_values    : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t list -> Omake_command_type.arg
val argv_of_values   : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t list list -> Omake_command_type.arg list
val values_of_value  : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t list
val strings_of_value : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> string list
val bool_of_value    : Omake_env.t -> Omake_value_type.pos -> Omake_value_type.t -> bool

