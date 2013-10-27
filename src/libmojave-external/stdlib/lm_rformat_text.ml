(*
 * Normal text-based printing.
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

(*
 * Some empty print functions.
 *)
let print_arg1_invis _ =
   ()

let print_arg2_invis _ _ =
   ()

(*
 * Channel printer.
 *)
let make_text_printer raw =
   let { raw_print_string  = output_string;
         raw_print_newline = output_newline;
         raw_print_spaces  = output_spaces
       } = raw
   in
   let print_string s =
      output_string s 0 (String.length s)
   in
   let print_tab (i, _) _ =
      output_newline ();
      output_spaces i
   in
      { print_string    = print_string;
        print_invis     = print_string;
        print_atomic    = print_string;
        print_tab       = print_tab;
        print_begin_tag = print_arg1_invis;
        print_end_tag   = print_arg1_invis
      }

(*
 * Text printing.
 *)
let print_text_raw rmargin buf raw =
   let info = make_text_printer raw in
      print_to_printer buf rmargin info;
      raw.raw_print_flush ()

let print_text_channel rmargin buf out =
   print_text_raw rmargin buf (raw_channel_printer out)

let print_text_buffer rmargin buf out =
   print_text_raw rmargin buf (raw_buffer_printer out)

let print_text_string rmargin buf =
   let out = Buffer.create 100 in
      print_text_buffer rmargin buf out;
      Buffer.contents out

(************************************************************************
 * Special case: print the first line to a string.
 *)
let line_format length fmt_fun =
   if length < 3 then
      raise (Invalid_argument "Lm_rformat.line_format");

   let buf = new_buffer () in
   let s, overflow =
      format_bound buf length;
      format_lzone buf;
      let over =
         try fmt_fun buf; false with
            RFormatOverflow ->
               true
      in
         format_ezone buf;
         print_text_string length buf, over
   in
   let len = String.length s in
   let s =
      if len > length then
         String.sub s 0 length
      else if overflow && (len <= (length - 3)) then
         s ^ "..."
      else if overflow && (len < length) then
         s ^ (String.make (length - len) '.')
      else
         s
   in
      if (overflow && len > (length - 3)) || len > length then
         begin
            s.[length - 3] <- '.';
            s.[length - 2] <- '.';
            s.[length - 1] <- '.'
         end;
      s

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
