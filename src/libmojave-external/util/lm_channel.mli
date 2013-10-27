(*
 * Generic buffered IO channel.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_location

type channel
type t = channel

(*
 * The channel may be a file, pipe, or socket.
 *)
type kind =
   FileChannel
 | PipeChannel
 | SocketChannel

type mode =
   InChannel
 | OutChannel
 | InOutChannel

(* Creation *)
val create        : string -> kind -> mode -> bool -> Unix.file_descr option -> t
val name          : t -> string
val descr         : t -> Unix.file_descr
val close         : t -> unit
val info          : t -> int * kind * mode * bool

val set_id        : t -> int -> unit

val of_string     : string -> t
val of_substring  : string -> int -> int -> t
val of_loc_string : string -> int -> int -> string -> t
val of_fun        : (string -> int -> int -> int) -> (string -> int -> int -> int) -> t

(* Output to strings *)
val create_string     : unit -> t
val create_loc_string : string -> int -> int -> t
val to_string         : t -> string

(* Set the file and line number *)
val set_line      : t -> string -> int -> unit

(*
 * Set text vs binary mode.
 * No effect unless on Win32.
 *)
val set_binary_mode : t -> bool -> unit

(* The write function is arbitrary and can be replaced *)
val set_io_functions : t ->
   (string -> int -> int -> int) ->  (* Reader *)
   (string -> int -> int -> int) ->  (* Writer *)
   unit

(* Positioning *)
val tell            : t -> int
val seek            : t -> int -> Unix.seek_command -> int
val loc             : t -> loc

(* Check if there is already input in the buffer *)
val poll          : t -> bool

(* Buffered IO *)
val input_char    : t -> char
val input_byte    : t -> int
val input_buffer  : t -> string -> int -> int -> unit
val input_line    : t -> string
val input_entire_line : t -> string
val read          : t -> string -> int -> int -> int

(* Flush data to the channel *)
val flush         : t -> unit

(* Buffered IO *)
val output_char   : t -> char -> unit
val output_byte   : t -> int -> unit
val output_buffer : t -> string -> int -> int -> unit
val output_string : t -> string -> unit
val write         : t -> string -> int -> int -> int

(* Select *)
val select        : t list -> t list -> t list -> float -> t list * t list * t list

(* Lex-mode operations *)
module LexerInput :
sig
   type t = channel

   val lex_start     : t -> int
   val lex_restart   : t -> int -> unit
   val lex_stop      : t -> int -> unit
   val lex_string    : t -> int -> string
   val lex_substring : t -> int -> int -> string
   val lex_next      : t -> int
   val lex_pos       : t -> int
   val lex_buffer    : t -> Buffer.t -> unit
   val lex_loc       : t -> int -> loc
   val bof           : int
   val eof           : int
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
