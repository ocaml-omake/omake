
open Lm_symbol
open Lm_location

open! Omake_ir
open Omake_env

open Omake_node

(*
 * Parsing environments.
 *)
type penv

(*
 * Parse a variable declaration.
 *)
val parse_declaration : venv -> Omake_value_type.pos -> loc -> var list -> method_name

(*
 * Environment for parsing AST files.
 *)
type senv_open_file  = string -> Omake_value_type.pos -> loc -> Node.t * senv

(*
 * Internal function for converting string expressions.
 *)
val build_string     : penv -> Omake_ast.exp -> Omake_value_type.pos -> penv * string_exp

(*
 * Create a parsing environment for the given file.
 *    penv_create (file, pervasives_id)
 *)
val penv_create        : senv_open_file -> venv -> Node.t -> penv
val penv_class_names   : penv -> symbol list * senv
val penv_of_vars       : senv_open_file -> venv -> Node.t -> senv -> penv

(*
 * Compile an AST program.
 *)
val compile_string   : penv -> Omake_ast.exp -> Omake_value_type.pos -> penv * string_exp
val compile_exp      : penv -> Omake_ast.exp        -> penv * ir
val compile_exp_list : penv -> Omake_ast.exp list   -> penv * ir
val compile_prog     : penv -> Omake_ast.prog       -> penv * ir
