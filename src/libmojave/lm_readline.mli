

val flush           : unit -> unit
val isatty          : unit -> bool
val readline        : string -> string
val readstring      : string -> string -> int -> int -> int
val set_interactive : bool -> unit
val is_interactive  : unit -> bool
val where           : unit -> int
val history         : unit -> string array
val load            : string -> unit
val save            : unit -> unit
val set_length      : int -> unit
val set_directory   : string -> unit
val prompt_invisible: (string * string) option

