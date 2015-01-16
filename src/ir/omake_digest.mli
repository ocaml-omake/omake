type digester

val init : unit -> digester
val add : digester -> string -> unit
val finish : digester -> string
val string : string -> string
val file : string -> string
