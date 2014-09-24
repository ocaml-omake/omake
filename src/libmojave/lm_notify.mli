
(*
 * Event manager.
 *)
type t

(*
 * Events.
 *)
type code =
  |Changed
  | Deleted
  | StartExecuting
  | StopExecuting
  | Created
  | Moved
  | Acknowledge
  | Exists
  | EndExist
  | DirectoryChanged

type event =
   { notify_code : code;
     notify_name : string
   }

(*
 * Debugging.
 *)
val debug_notify      : bool ref
val string_of_code    : code -> string

(*
 * Methods.
 *)
val enabled           : bool
val create            : unit -> t
val close             : t -> unit
val file_descr        : t -> Unix.file_descr option
val monitor           : t -> string -> bool -> unit
val pending           : t -> bool
val next_event        : t -> event

val suspend           : t -> string -> unit
val resume            : t -> string -> unit
val cancel            : t -> string -> unit

val suspend_all       : t -> unit
val resume_all        : t -> unit
val cancel_all        : t -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
