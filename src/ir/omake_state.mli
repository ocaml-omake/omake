(*
 * Configuration.
 *)

(*
 * Error codes for various actions.
 *)
val signal_error_code   : int
val fork_error_code     : int
val internal_error_code : int
val deadlock_error_code : int
val exn_error_code      : int
val scanner_error_code  : int

(*
 * Name of the database.
 *)
val db_name : string

(*
 * Name of the makefiles.
 *)
val makefile_name : string
val makeroot_name : string
val makeroot_short_name : string
val omake_file_suffix : string

(*
 * Cache management.
 *)
val always_use_dotomake : bool ref
val set_omake_dir : string -> unit

(*
 * Files.
 *)
val lib_dir         : string
val lib_dir_reason  : string
val home_dir        : string
val application_dir : string
val omake_dir       : unit -> string
val db_file         : unit -> string
val history_file    : unit -> string

val omakeinit_file : string
val omakerc_file   : string
val oshrc_file     : string

val get_cache_file  : string -> string -> string * Unix.file_descr
val lock_file       : Unix.file_descr -> Unix.lock_command -> unit
