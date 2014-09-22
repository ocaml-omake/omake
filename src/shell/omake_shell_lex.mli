(*
 * Lex a shell line.
 *)
open Lm_glob


open Omake_env

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
val pipe_of_value : venv ->
   (venv -> pos -> Lm_location.t -> string -> (Lm_symbol.t * apply) option) -> glob_options ->
   pos -> Lm_location.t -> value -> command_flag list * arg_pipe

