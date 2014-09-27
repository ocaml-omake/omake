(**  Shell execution. *)

(** Create a thread or process running the function. *)
val create_thread : Omake_env.t -> (Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int) ->
   Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> Omake_env.pid

(** Start a job given a pipe specification. *)
val create_job : 
  Omake_env.t -> Omake_env.string_pipe ->
  Unix.file_descr -> Unix.file_descr ->
  Unix.file_descr -> Omake_env.t * Omake_value_type.t

(** Create a process in the background. *)
val create_process : 
  Omake_env.t -> Omake_env.string_pipe -> 
  Unix.file_descr -> Unix.file_descr ->
  Unix.file_descr -> Omake_env.pid

val waitpid : 
  Omake_env.t -> Omake_value_type.pos ->
  Omake_env.pid -> int * Unix.process_status * Omake_value_type.t

(** Shell operations. *)
val jobs    : Omake_env.t -> unit
val bg      : Omake_env.t -> Omake_value_type.pos -> int -> unit
val fg      : Omake_env.t -> Omake_value_type.pos -> int -> unit
val stop    : Omake_env.t -> Omake_value_type.pos -> int -> unit
val kill    : Omake_env.t -> Omake_value_type.pos -> int -> Omake_shell_type.signal -> unit
val wait    : Omake_env.t -> Omake_value_type.pos -> int -> unit
val cleanup : Omake_env.t -> unit
