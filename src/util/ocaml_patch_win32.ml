(*
 * Patch some problems in OCaml.
 *)
open Unix

(*
 * create_process on 3.08.0 quotes its arguments.
 *)
external win_create_process : string -> string -> string option ->
                              file_descr -> file_descr -> file_descr -> int
                            = "win_create_process" "win_create_process_native"

let create_process prog args fd1 fd2 fd3 =
  win_create_process prog (String.concat " " (Array.to_list args)) None fd1 fd2 fd3

