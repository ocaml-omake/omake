

(* tgetstr id
   Lookup the terminal capability with indicated id.  This assumes the
   terminfo to lookup is given in the TERM environment variable.  This
   function returns None if the terminal capability is not defined.  *)
val tgetstr : string -> string option


(* Various terminfo identifier names for use with tgetstr *)
val enter_bold_mode : string
val exit_attribute_mode : string


(* xterm_escape_begin ()
   Display XTerm title begin escape, if available.  *)
val xterm_escape_begin : unit -> string option


(* xterm_escape_begin ()
   Display XTerm title end escape, if available.  *)
val xterm_escape_end : unit -> string option


val get_number_of_cores : unit -> int
