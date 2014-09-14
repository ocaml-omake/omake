external flush           : unit -> unit                          = "omake_readline_flush"
external isatty          : unit -> bool                          = "omake_isatty"
external is_interactive  : unit -> bool                          = "omake_is_interactive"
external set_interactive : bool -> unit                          = "omake_interactive"
external init            : unit -> unit                          = "omake_readline_init"
external where           : unit -> int                           = "omake_where_history"
external history         : unit -> string array                  = "omake_readline_history"
external load            : string -> unit                        = "omake_readline_load_file"
external save            : unit -> unit                          = "omake_readline_save_file"
external set_length      : int -> unit                           = "omake_readline_set_length"
external set_directory   : string -> unit                        = "omake_readline_set_directory"
external get_prompt_invs : unit -> string * string               = "omake_rl_prompt_wrappers"

let () = init ()

let prompt_invisible =
  match get_prompt_invs () with
  | "", "" -> None
  | inv -> Some inv

external ext_readline    : string -> string                      = "omake_readline"
external ext_readstring  : string -> string -> int -> int -> int = "omake_readstring"

let readline s =
   Lm_thread_pool.blocking_section ext_readline s

let readstring s buf off len =
   Lm_thread_pool.blocking_section (ext_readstring s buf off) len

