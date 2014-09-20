
(*
 * Local server.
 *)
val create_process : 
  string -> string array -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int

