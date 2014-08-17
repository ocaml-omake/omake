

(**
 * For now, just use normal output channels.
 * Type t of buffers.
 *)
type 'a t = Format.formatter -> 'a  -> unit
type out_channel = Format.formatter

(*
 * Normal buffers.
 *)
val stdout : Format.formatter
val stderr : Format.formatter
val stdstr : Format.formatter

(*
 * Get the string from the stdstr channel.
 *)
val flush_stdstr : unit -> string

(*
 * Open new channels.
 *)
val open_out     : string -> Format.formatter
val open_out_bin : string -> Format.formatter

(*
 * Simple printing.
 *)
val output_char    : Format.formatter -> char -> unit
val output_string  : Format.formatter -> string -> unit

(*
 * These functions are bad style for functional programs.
 *)
val prerr_char    : char -> unit
val prerr_int     : int -> unit
val prerr_string  : string -> unit

(*
 * Flush the output.
 *)
val flush  : Format.formatter -> unit
val eflush : Format.formatter -> unit

(*
 * Printing.
 *)
val eprintf : ('a, Format.formatter, unit) format -> 'a
val printf  : ('a, Format.formatter, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val fprintf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val bprintf : Buffer.t -> ('a, Format.formatter, unit) format -> 'a

(*
 * List printing helpers.
 *)
val print_any_list : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val print_string_list : Format.formatter -> string list -> unit
val print_int_list : Format.formatter -> int list -> unit

(************************************************************************
 * Formatter interface.
 *)

(*
 * Boxes.
 *)
val open_box : int -> unit
val open_vbox : int -> unit
val open_hbox : unit -> unit
val open_hvbox : int -> unit
val open_hovbox : int -> unit
val close_box : unit -> unit

(*
 * Formatting functions.
 *)
val print_string : string -> unit
val print_as : int -> string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_char : char -> unit
val print_bool : bool -> unit

(*
 * Break hints.
 *)
val print_space : unit -> unit
val print_cut : unit -> unit
val print_break : int -> int -> unit
val print_flush : unit -> unit
val print_newline : unit -> unit
val force_newline : unit -> unit
val print_if_newline : unit -> unit

(*
 * Margin.
 *)
val set_margin : int -> unit
val get_margin : unit -> int

(*
 * Indentation limit.
 *)
val set_max_indent : int -> unit
val get_max_indent : unit -> int

(*
 * Formatting depth.
 *)
val set_max_boxes : int -> unit
val get_max_boxes : unit -> int
val over_max_boxes : unit -> bool

(*
 * Tabulations.
 *)
val open_tbox : unit -> unit
val close_tbox : unit -> unit
val print_tbreak : int -> int -> unit
val set_tab : unit -> unit
val print_tab : unit -> unit

(*
 * Ellipsis.
 *)
val set_ellipsis_text : string -> unit
val get_ellipsis_text : unit -> string

(*
 * Redirecting formatter output.
 *)
val set_formatter_out_channel      : Pervasives.out_channel -> unit
val set_formatter_output_functions : (string -> int -> int -> unit) -> (unit -> unit) -> unit
val get_formatter_output_functions : unit -> (string -> int -> int -> unit) * (unit -> unit)

val set_all_formatter_output_functions :
   (string -> int -> int -> unit) ->
   (unit -> unit) ->
   (unit -> unit) ->
   (int -> unit) ->
   unit

val get_all_formatter_output_functions :
   unit ->
   (string -> int -> int -> unit) *
   (unit -> unit) *
   (unit -> unit) *
   (int -> unit)

(*
 * Multiple formatted output.
 *)
type formatter = Format.formatter

val formatter_of_out_channel     : Pervasives.out_channel -> formatter
val std_formatter                : formatter
val err_formatter                : formatter
val str_formatter                : formatter
val stdbuf                       : Buffer.t
val flush_str_formatter          : unit -> string

val formatter_of_buffer          : Buffer.t -> formatter
val make_formatter               : (string -> int -> int -> unit) -> (unit -> unit) -> formatter

val pp_open_hbox                 : formatter -> unit -> unit
val pp_open_vbox                 : formatter -> int -> unit
val pp_open_hvbox                : formatter -> int -> unit
val pp_open_hovbox               : formatter -> int -> unit
val pp_open_box                  : formatter -> int -> unit
val pp_close_box                 : formatter -> unit -> unit
val pp_print_string              : formatter -> string -> unit
val pp_print_as                  : formatter -> int -> string -> unit
val pp_print_int                 : formatter -> int -> unit
val pp_print_float               : formatter -> float -> unit
val pp_print_char                : formatter -> char -> unit
val pp_print_bool                : formatter -> bool -> unit
val pp_print_break               : formatter -> int -> int -> unit
val pp_print_cut                 : formatter -> unit -> unit
val pp_print_space               : formatter -> unit -> unit
val pp_force_newline             : formatter -> unit -> unit
val pp_print_flush               : formatter -> unit -> unit
val pp_print_newline             : formatter -> unit -> unit
val pp_print_if_newline          : formatter -> unit -> unit
val pp_open_tbox                 : formatter -> unit -> unit
val pp_close_tbox                : formatter -> unit -> unit
val pp_print_tbreak              : formatter -> int -> int -> unit
val pp_set_tab                   : formatter -> unit -> unit
val pp_print_tab                 : formatter -> unit -> unit
val pp_set_margin                : formatter -> int -> unit
val pp_get_margin                : formatter -> unit -> int
val pp_set_max_indent            : formatter -> int -> unit
val pp_get_max_indent            : formatter -> unit -> int
val pp_set_max_boxes             : formatter -> int -> unit
val pp_get_max_boxes             : formatter -> unit -> int
val pp_over_max_boxes            : formatter -> unit -> bool
val pp_set_ellipsis_text         : formatter -> string -> unit
val pp_get_ellipsis_text         : formatter -> unit -> string
val pp_set_formatter_out_channel : formatter -> Pervasives.out_channel -> unit

val pp_set_formatter_output_functions :
   formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit

val pp_get_formatter_output_functions :
   formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit)

val pp_set_all_formatter_output_functions :
   formatter ->
   (string -> int -> int -> unit) ->
   (unit -> unit) ->
   (unit -> unit) ->
   (int -> unit) ->
   unit

val pp_get_all_formatter_output_functions :
   formatter ->
   unit ->
   (string -> int -> int -> unit) *
   (unit -> unit) *
   (unit -> unit) *
   (int -> unit)

(*
 * Allow the use of the Format modules.
 *)
val out_channel_of_formatter : Format.formatter -> Format.formatter

(************************************************************************
 * Helper utilities.
 *)

(* Prints a "; "- separated list. *)
val pp_print_any_list : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
