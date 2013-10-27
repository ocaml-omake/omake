(*
 * Override the usual out_channels to use the Lm_buffer module.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Mojave Group, Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *)

(*
 * Type t of buffers.
 *)
type out_channel

(*
 * Normal buffers.
 *)
val stdout : out_channel
val stderr : out_channel
val stdstr : out_channel

(*
 * Get the string from the stdstr channel.
 *)
val flush_stdstr : unit -> string

(*
 * Open new channels.
 *)
val open_out     : string -> out_channel
val open_out_bin : string -> out_channel

(*
 * Simple printing.
 *)
val output_char    : out_channel -> char -> unit
val output_int     : out_channel -> int -> unit
val output_string  : out_channel -> string -> unit
val output_rbuffer : out_channel -> Lm_rformat.buffer -> unit

(*
 * These functions are bad style for functional programs.
 *)
val print_char    : char -> unit
val print_int     : int -> unit
val print_string  : string -> unit
val print_rbuffer : Lm_rformat.buffer -> unit

val prerr_char    : char -> unit
val prerr_int     : int -> unit
val prerr_string  : string -> unit
val prerr_rbuffer : Lm_rformat.buffer -> unit

(*
 * Flush the output.
 *)
val flush  : out_channel -> unit
val eflush : out_channel -> unit

(*
 * Printing.
 *)
val eprintf : ('a, out_channel, unit) format -> 'a
val printf  : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val bprintf : Buffer.t -> ('a, out_channel, unit) format -> 'a

(*
 * List printing helpers.
 *)
val print_any_list : (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
val print_string_list : out_channel -> string list -> unit
val print_int_list : out_channel -> int list -> unit

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
type formatter = out_channel

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
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
