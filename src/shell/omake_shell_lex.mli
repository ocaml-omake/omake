(*
 * Lex a shell line.
 *)
open Lm_glob




open Omake_shell_type
open Omake_command_type
open! Omake_value_type

(*
 * Commands with a leading \ are quoted.
 *)
val parse_command_string : string -> simple_exe

(*
 * Construct the pipe from the value.
 *)
val pipe_of_value : Omake_env.t ->
   (Omake_env.t -> pos -> Lm_location.t -> string -> (Lm_symbol.t * Omake_env.apply) option) -> glob_options ->
   pos -> Lm_location.t -> Omake_value_type.t -> command_flag list * Omake_env.arg_pipe

