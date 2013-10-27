(*
 * Formatting to LaTeX documents.
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
 * We hack the indentation in the TeX printer.
 * Format the data into lines, and print the tabstops in
 * the background color.
 *
 * The prefix is the white space that is inserted to
 * get the left margin right.
 *)
type tex_buffer =
   { mutable tex_current_line : (bool * string) list;
     mutable tex_prefix       : string;
     tex_print_string         : string -> unit;
     tex_print_newline        : unit -> unit
   }

(*
 * Have to escape special characters.
 *)
let tex_escape_string _linebreaks s =
   let len = String.length s in
   let rec collect i j =
      if j = len then
         if i = 0 then
             s
         else if i < j then
            String.sub s i (j - i)
         else
            ""
      else
         match s.[j] with
            ' ' ->
               collect_escape i j "\\ "
          | '_' ->
               collect_escape i j "\\_"
          | '{' ->
               collect_escape i j "\\{"
          | '}' ->
               collect_escape i j "\\}"
          | '|' ->
               collect_escape i j "\\|"
          | '<' ->
               collect_escape_space i j "\\lt"
          | '>' ->
               collect_escape_space i j "\\gt"
          | '^'
          | '&'
          | '#'
          | '['
          | ']'
          | '\\'
          | '$'
          | '%' as c->
               collect_escape_space i j (Printf.sprintf "\\char%i" (Char.code c))
          | _ ->
               collect i (succ j)
   and collect_esc i j s' =
      if i < j then
         String.sub s i (j - i) ^ s'
      else
         s'
   and collect_escape i j s =
      collect_esc i j (s ^ collect (succ j) (succ j))
   and collect_escape_space i j s =
      let s' = collect (succ j) (succ j) in
      let s'' =
         if s' = "" then
            " "
         else
            match s'.[0] with
               ' ' | '\\' | '$' | '_' | '^' | '&' | '}' | '{' -> s'
          | _ ->
               " " ^ s'
      in
         collect_esc i j (s ^ s'')
   in
      collect 0 0

(*
 * Print strings.
 *)
let tex_print_string buf s =
   buf.tex_current_line <- (true, s) :: buf.tex_current_line

let tex_print_invis buf s =
   buf.tex_current_line <- (false, s) :: buf.tex_current_line

(*
 * Extract the entire line.
 *)
let tex_line buf =
   let rec collect line = function
      (vis, h) :: t ->
         collect ((if vis then (tex_escape_string true h) else h) ^ line) t
    | [] ->
         line
   in
      collect "" buf.tex_current_line

let tex_visible buf =
   let rec collect line = function
      (vis, h) :: t ->
         collect (if vis then h ^ line else line) t
    | [] ->
         line
   in
      collect "" buf.tex_current_line

let make_tag s =
   (false, "\\" ^ s ^ "{")

let tex_push_line buf tags =
   let line = tex_line buf in
      buf.tex_print_string line;
      buf.tex_current_line <- [];
      if line <> "" && line.[String.length line - 1] != '\n' then
         begin
            buf.tex_print_string (String.make (List.length tags) '}');
            buf.tex_print_string "\\\\\n";
            buf.tex_current_line <- List.map make_tag tags
         end

let tex_flush buf =
   tex_push_line buf []

(*
 * Set up all pending tabstops.
 *)
let tex_tab_line buf =
   buf.tex_prefix ^ tex_visible buf

(*
 * Newline.
 * Compute all pending tabstops,
 * then push the line and the new tabstop.
 *)
let tex_tab buf (col, _) tags =
   if col = 0 then
      begin
         tex_push_line buf tags;
         buf.tex_prefix <- ""
      end
   else
      let tabline = tex_tab_line buf in
         tex_push_line buf tags;
         let prefix =
            if col >= String.length tabline then
               tabline
            else
               String.sub tabline 0 col
         in
         let spacer = Printf.sprintf "\\phantom{%s}" (tex_escape_string false prefix) in
            buf.tex_prefix <- prefix;
            buf.tex_current_line <- buf.tex_current_line @ [false, spacer]

let tex_tag buf s =
   buf.tex_current_line <- (make_tag s) :: buf.tex_current_line

let tex_etag buf _ =
   buf.tex_current_line <- (false, "}") :: buf.tex_current_line

(*
 * A TeX printer.
 *)
let make_tex_printer_aux raw =
   let { raw_print_string  = output_string;
         raw_print_newline = output_newline
       } = raw
   in
   let print_string s =
      output_string s 0 (String.length s)
   in
   let buf =
      { tex_current_line  = [];
        tex_prefix        = "";
        tex_print_string  = print_string;
        tex_print_newline = output_newline
      }
   in
   let info =
      { print_string    = tex_print_string buf;
        print_invis     = tex_print_invis buf;
        print_atomic    = tex_print_string buf;
        print_tab       = tex_tab buf;
        print_begin_tag = tex_tag buf;
        print_end_tag   = tex_etag buf
      }
   in
      buf, info

let make_tex_printer raw =
   snd (make_tex_printer_aux raw)

(*
 * Raw printer.
 *)
let print_tex_raw rmargin buf raw =
   let tbuf, info = make_tex_printer_aux raw in
   let print_string s =
      raw.raw_print_string s 0 (String.length s)
   in
      print_string "\\texfalse\\iftex%\n";
      print_to_printer buf rmargin info;
      tex_flush tbuf;
      print_string "\\fi\\enmptab%\n";
      raw.raw_print_flush ()

(*
 * The channel and buffer versions.
 *)
let print_tex_channel rmargin buf out =
   print_tex_raw rmargin buf (raw_channel_printer out)

let print_tex_buffer rmargin buf out =
   print_tex_raw rmargin buf (raw_buffer_printer out)

let print_tex_string rmargin buf =
   let out = Buffer.create 100 in
      print_tex_buffer rmargin buf out;
      Buffer.contents out

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
