(*
 * Convert to standard format.
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
val set_formatter_out_channel : out_channel -> unit
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
type formatter

val formatter_of_out_channel     : out_channel -> formatter
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
val pp_print_rbuffer             : formatter -> Lm_rformat.buffer -> unit
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
val pp_set_formatter_out_channel : formatter -> out_channel -> unit

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
 * Allow output to be diverted to a function.
 *)
val divert  : formatter -> (Lm_rformat.buffer -> unit) option -> unit

(*
 * Printf-style functions.
 *)
val fprintf : formatter -> ('a, formatter, unit) format -> 'a
val printf  : ('a, formatter, unit) format -> 'a
val bprintf : Buffer.t -> ('a, formatter, unit) format -> 'a
val eprintf : ('a, formatter, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
