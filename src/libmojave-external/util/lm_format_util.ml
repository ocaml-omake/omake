(*
 * JYH: we should port this to Lm_printf.
 *
 * Utilities for use with the format library.
 * Copyright (C) 2002, Justin David Smith, Caltech
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
 * Author: Justin David Smith
 * justins@chaos2.org
 *)
open Format

(* pp_print_paragraph_bare buf text
   Prints a paragraph of text.  Normally, the text is allowed to break at
   any whitespace character.  Line breaks are forced at any newline char.
   The &nbsp; sequence can be used to insert a nonbreaking space.  This
   assumes the caller has already opened an HOV box and that the caller
   will close the box when finished.  *)
let pp_print_paragraph_bare buf text =
   (* Precompute the length of the text *)
   let length = String.length text in

   (* Print out the descriptive text associated with an option *)
   let rec print_text index =
      if index + 5 < length && compare (String.sub text index 6) "&nbsp;" = 0 then begin
         (* Non-breakable space must be printed here. *)
         pp_print_string buf " ";
         print_text (index + 6)
      end else if index < length then begin
         (match text.[index] with
            ' ' | '\t' ->
               (* Spaces are normally breakable. *)
               pp_print_space buf ()
          | '\n' ->
               (* Newline characters force a new line. *)
               pp_force_newline buf ()
          | _ as c ->
               (* Ordinary character *)
               pp_print_char buf c);
         print_text (index + 1)
      end else
         ()
   in (* end of print_text *)

      (* Print out the paragraph in an existing hov box *)
      print_text 0


(* pp_print_paragraph buf text
   Prints a paragraph of text in its own HOV box.  Normally, the text is
   allowed to break at any whitespace character.  Line breaks are forced
   at any newline character.  The &nbsp; sequence can be used to insert
   a nonbreaking space.  *)
let pp_print_paragraph buf text =
   (* Print out the paragraph in a new hov box *)
   pp_open_hovbox buf 0;
   pp_print_paragraph_bare buf text;
   pp_close_box buf ()


(* y_formatter buf1 buf2
   Builds a Y formatter given two other formatters.  Data printed to the Y
   formatter will be written to *both* formatters that are given as input.
   As a result, this acts as a `Y'' which splits the output.  Pretty nice,
   eh?

   JYH: actually, to be consistent, this should be called "tee" *)
let y_formatter buf1 buf2 =
   let y_out s i j =
      let buf1_out, _ = pp_get_formatter_output_functions buf1 () in
      let buf2_out, _ = pp_get_formatter_output_functions buf2 () in
      buf1_out s i j;
      buf2_out s i j
   in
   let y_flush () =
      let _, buf1_flush = pp_get_formatter_output_functions buf1 () in
      let _, buf2_flush = pp_get_formatter_output_functions buf2 () in
      buf1_flush ();
      buf2_flush ()
   in
      make_formatter y_out y_flush
