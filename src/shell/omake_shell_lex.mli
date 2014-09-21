(*
 * Lex a shell line.
 *)
open Lm_glob
open Lm_location

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
   (venv -> pos -> loc -> string -> (Lm_symbol.t * apply) option) -> glob_options ->
   pos -> loc -> value -> command_flag list * arg_pipe

