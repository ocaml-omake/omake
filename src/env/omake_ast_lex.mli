(**  Lexer for OMakefile's *)

val debug_lex : bool ref

val parse_ast    : string -> Omake_ast.prog
val parse_string : string -> Omake_ast.prog

val parse_deps   : string -> 
  (Omake_ast.exp * Omake_ast.exp * Lm_location.t )
    list

(*
 * Shell gets its own handle.
 *)
type session

val current_location : session -> Lm_location.t 
val create_shell     : unit -> session
val parse_shell      : session -> string -> Omake_ast.exp list
