(*
 * Formatter like in the standard library.
 * Output is organized into boxes, each of which has an indentation.
 *
 * Commands:
 *    format_sbreak str str': soft break is taken if necessary
 *        if taken, str is printed after the current line
 *        if not, str' is printed
 *    format_hbreak str str': hard breaks are taken in groups
 *        if taken, str is printed
 *        if not, str' is printed
 *
 *    format_lzone: begin a zone with no breaks
 *    format_szone: soft break zone (all or no hard breaks are taken)
 *    format_hzone: all hard breaks are taken.
 *    format_ezone: end the current zone.
 *
 *    format_pushm i: push left margin from here by i more spaces
 *    format_popm: pop last pushm
 *
 *    format_char: add a single char
 *    format_int: print a number
 *    format_string: add a string to the buffer
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Abstract type of buffers containing formatted text.
 *)
type buffer

(*
 * The BufferOverflow exception is raised when too much visible text is
 * put in the buffer.  You can control how much visible text is allowed
 * by using the format_bound function below.
 *)
exception RFormatOverflow

(*
 * A printer contains:
 *    print_string s : print string s to the buffer
 *    print_atomic s : print the buffer to the buffer
 *    print_invis s : print string s in invisible mode
 *    print_tab lmargin tags : tab to the specified left margin
 *    print_begin_tag : start tagging a value
 *    print_end_tag : finish tagging the value
 *)
type printer =
   { print_string    : string -> unit;
     print_invis     : string -> unit;
     print_atomic    : string -> unit;
     print_tab       : int * string -> string list -> unit;
     print_begin_tag : string -> unit;
     print_end_tag   : string -> unit
   }

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Buffer creation.
 *)
val new_buffer : unit -> buffer
val clone_buffer : buffer -> buffer
val clear_buffer : buffer -> unit
val buffer_is_empty : buffer -> bool

(*
 * Marshaling.
 * This will raise Failure if the marshal
 * version changes.
 *)
val marshal_buffers : buffer list -> string
val unmarshal_buffers : string -> buffer list

(*
 * Specify the max number of characters in the buffer.
 * This will not raise an exception even if the buffer
 * is already too large.  You will get the exception
 * the next time you insert visible text.
 *)
val format_bound : buffer -> int -> unit

(*
 * Breaks.
 *)
val format_cbreak  : buffer -> string -> string -> unit
val format_sbreak  : buffer -> string -> string -> unit
val format_hbreak  : buffer -> string -> string -> unit
val format_space   : buffer -> unit
val format_hspace  : buffer -> unit
val format_newline : buffer -> unit

(*
 * Break zones.
 *)
val zone_depth   : buffer -> int
val format_lzone : buffer -> unit
val format_szone : buffer -> unit
val format_hzone : buffer -> unit
val format_ezone : buffer -> unit
val format_izone : buffer -> unit
val format_azone : buffer -> unit

(* TeX boxes *)
val format_tzone : buffer -> string -> unit

(*
 * Margins.
 *)
val format_pushm : buffer -> int -> unit
val format_pushm_str : buffer -> string -> unit
val format_popm : buffer -> unit

(*
 * Printers.
 *)
val format_char : buffer -> char -> unit
val format_string : buffer -> string -> unit
val format_raw_string : buffer -> string -> unit
val format_quoted_string : buffer -> string -> unit
val format_int : buffer -> int -> unit
val format_num : buffer -> Lm_num.num -> unit
val format_buffer : buffer -> buffer -> unit

(*
 * Internals.
 *)

(* Get the current nesting depth *)
val format_depth : buffer -> int

(* Close all open boxes, indicriminately *)
val format_flush : buffer -> unit

(* Close all open pushm boxes *)
val format_flush_popm : buffer -> unit

(*
 * Collecting output.
 *)
val default_width : int (* 80 *)

(*
 * Final output.
 *)
val print_to_printer : buffer -> int -> printer -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
