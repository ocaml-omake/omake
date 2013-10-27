(*
 * Formatting to HTML documents.
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
 * Kinds of input.
 *)
type data =
   VisibleString of string
 | InvisibleString of string
 | AtomicString of string

(*
 * We hack the indentation in the HTML printer.
 * Format the data into lines, and print the tabstops in
 * invisible mode.
 *
 * The prefix is the white space for the left margin.
 *)
type html_buffer =
   { html_line               : data Queue.t;
     mutable html_column     : int;
     mutable html_prefix     : data list;
     mutable html_spacer     : string;
     html_print_string       : string -> unit;
     html_print_newline      : unit -> unit
   }

(*
 * Tagging functions.
 *)
type html_tagger_fun =
   NoTagger
 | StringTagger of string
 | FunTagger of (string -> string)

type html_tagger_pair =
   { html_tag_begin : html_tagger_fun;
     html_tag_end   : html_tagger_fun
   }

type html_tagger = html_tagger_pair option

(*
 * Have to escape special characters.
 *)
let html_escape_string buffer s =
   let len = String.length s in
   let rec collect i j =
      if j != len then
         match s.[j] with
            '<' ->
               collect_escape i j "&lt;"
          | '>' ->
               collect_escape i j "&gt;"
          | '&' ->
               collect_escape i j "&amp;"
          | '"' ->
               collect_escape i j "&quot;"
          | _ ->
               collect i (succ j)
      else if i = 0 then
         Buffer.add_string buffer s
      else if i < j then
         Buffer.add_substring buffer s i (j - i)
   and collect_escape i j s' =
      if i < j then
         Buffer.add_substring buffer s i (j - i);
      Buffer.add_string buffer s';
      collect (succ j) (succ j)
   in
      collect 0 0

(*
 * For external use.
 *)
let escape s =
   let buf = Buffer.create 16 in
      html_escape_string buf s;
      Buffer.contents buf

(*
 * Extract the entire line.
 *)
let html_line buf =
   let buffer = Buffer.create 100 in
      Queue.iter (function
         VisibleString s ->
            html_escape_string buffer s
       | InvisibleString s
       | AtomicString s ->
            Buffer.add_string buffer s) buf.html_line;
      Buffer.contents buffer

let html_push_line buf =
   let line = html_line buf in
      buf.html_print_string line;
      Queue.clear buf.html_line

let html_flush buf =
   html_push_line buf

(*
 * Get the spacer from the prefix.
 *)
let html_spacer buf =
   let buffer = Buffer.create 16 in
      Buffer.add_string buffer "<span style=\"visibility:hidden\">";
      List.iter (fun item ->
            match item with
               VisibleString s ->
                  html_escape_string buffer s
             | InvisibleString _ ->
                  ()
             | AtomicString s ->
                  Buffer.add_string buffer s) buf.html_prefix;
      Buffer.add_string buffer "</span>";
      Buffer.contents buffer

(*
 * Get a new prefix buffer.
 * The entire line is (buf.html_prefix @ buf.html_current_line).
 * Build the line, and truncate it.
 *)
let html_prefix buf col =
   let { html_column = cur;
         html_prefix = prefix;
         html_line = line
       } = buf
   in

   (* Add the visible elements in the current line to the prefix *)
   let prefix =
      if col < cur then
         prefix
      else
         List.rev (Queue.fold (fun prefix item ->
                         match item with
                            VisibleString _
                          | AtomicString _ ->
                               item :: prefix
                          | InvisibleString _ ->
                               prefix) (List.rev prefix) line)
   in

   (* Truncate the prefix to the current column *)
   let rec collect prefix cur items =
      match items with
         item :: items ->
            (match item with
                VisibleString s ->
                   let len = String.length s in
                   let next = cur + len in
                      if next <= col then
                         collect (item :: prefix) next items
                      else
                         let s = String.sub s 0 (col - cur) in
                            VisibleString s :: prefix
              | InvisibleString _ ->
                   collect prefix cur items
              | AtomicString _ ->
                   let next = succ cur in
                      if next <= col then
                         collect (item :: prefix) next items
                      else
                         prefix)
       | [] ->
            if cur < col then
               VisibleString (String.make (col - cur) 'm') :: prefix
            else
               prefix
   in
      buf.html_column <- col;
      buf.html_prefix <- List.rev (collect [] 0 prefix);
      buf.html_spacer <- html_spacer buf

(*
 * Newline.
 *
 * The col is the new *absolute* tabstop.
 *
 * Compute the new tabstop prefix, then push the line,
 * and save the new tabstop.
 *)
let html_tab buf (col, _) _ =
   if col = 0 then
      begin
         html_push_line buf;
         buf.html_print_newline ();
         buf.html_print_string "<br>\n";
         buf.html_column <- 0;
         buf.html_prefix <- [];
         buf.html_spacer <- ""
      end
   else
      let spacer =
         if col <> buf.html_column then
            html_prefix buf col;
         buf.html_spacer
      in
         html_push_line buf;
         buf.html_print_newline ();
         buf.html_print_string "<br>\n";
         buf.html_print_string spacer

(*
 * Print strings.
 *)
let html_print_string buf s =
   Queue.add (VisibleString s) buf.html_line

let html_print_invis buf s =
   Queue.add (InvisibleString s) buf.html_line

let html_print_atomic buf s =
   Queue.add (AtomicString s) buf.html_line

let html_tag tagger buf =
   match tagger with
      Some { html_tag_begin = FunTagger tagger } ->
         (fun s -> Queue.add (InvisibleString (tagger s)) buf.html_line)
    | Some { html_tag_begin = StringTagger tagger } ->
         (fun _s -> Queue.add (InvisibleString tagger) buf.html_line)
    | Some { html_tag_begin = NoTagger }
    | None ->
         (fun _s -> ())

let html_etag tagger buf =
   match tagger with
      Some { html_tag_end = FunTagger tagger } ->
         (fun s -> Queue.add (InvisibleString (tagger s)) buf.html_line)
    | Some { html_tag_end = StringTagger tagger } ->
         (fun _s -> Queue.add (InvisibleString tagger) buf.html_line)
    | Some { html_tag_end = NoTagger }
    | None ->
         (fun _s -> ())

(*
 * An HTML printer.
 *)
let make_html_printer_aux tagger raw =
   let { raw_print_string  = output_string;
         raw_print_newline = output_newline
       } = raw
   in
   let print_string s =
      output_string s 0 (String.length s)
   in
   let buf =
      { html_line          = Queue.create ();
        html_column        = 0;
        html_prefix        = [];
        html_spacer        = "";
        html_print_string  = print_string;
        html_print_newline = output_newline
      }
   in
   let info =
      { print_string    = html_print_string buf;
        print_invis     = html_print_invis buf;
        print_atomic    = html_print_atomic buf;
        print_tab       = html_tab buf;
        print_begin_tag = html_tag tagger buf;
        print_end_tag   = html_etag tagger buf
      }
   in
      buf, info

let make_html_printer tagger raw =
   snd (make_html_printer_aux tagger raw)

let print_html_raw rmargin tagger buf raw =
   let hbuf, info = make_html_printer_aux tagger raw in
      print_to_printer buf rmargin info;
      html_flush hbuf;
      raw.raw_print_flush ()

let print_html_channel rmargin tagger buf out =
   print_html_raw rmargin tagger buf (raw_channel_printer out)

let print_html_buffer rmargin tagger buf out =
   print_html_raw rmargin tagger buf (raw_buffer_printer out)

let print_html_string rmargin tagger buf  =
   let out = Buffer.create 100 in
      print_html_buffer rmargin tagger buf out;
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
