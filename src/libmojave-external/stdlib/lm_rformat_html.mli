(*
 * Text-based printing.
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
open Lm_rformat_raw
open Lm_rformat

type html_tagger_fun =
   NoTagger
 | StringTagger of string
 | FunTagger of (string -> string)

type html_tagger_pair =
   { html_tag_begin : html_tagger_fun;
     html_tag_end   : html_tagger_fun
   }

type html_tagger = html_tagger_pair option

val make_html_printer  : html_tagger -> raw_printer -> printer

val print_html_channel : int -> html_tagger -> buffer -> out_channel -> unit
val print_html_buffer  : int -> html_tagger -> buffer -> Buffer.t -> unit
val print_html_string  : int -> html_tagger -> buffer -> string

val escape : string -> string

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
