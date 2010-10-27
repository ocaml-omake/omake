(*
 * Bah, autogenerate the parse tables.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Printf

(************************************************************************
 * Tokens and their productions.
 *)
let tokens =
   ["TokWhite",         "StringWhiteExp";
    "TokOp",            "StringOpExp";
    "TokLeftParen",     "StringOpExp";
    "TokRightParen",    "StringOpExp";
    "TokArrow",         "StringOpExp";
    "TokComma",         "StringOpExp";
    "TokColon",         "StringOpExp";
    "TokDoubleColon",   "StringOpExp";
    "TokNamedColon",    "StringOpExp";
    "TokEq",            "StringOpExp";
    "TokArray",         "StringOpExp";
    "TokDot",           "StringOpExp";
    "TokId",            "StringIdExp";
    "TokInt",           "StringIntExp";
    "TokFloat",         "StringFloatExp";
    "TokKey",           "StringKeywordExp";
    "TokKeyword",       "StringKeywordExp";
    "TokCatch",         "StringKeywordExp";
    "TokClass",         "StringKeywordExp";
    "TokString",        "StringOtherExp"]

let named_tokens =
   ["quote",      "{ $1 }";
    "apply",      "{ $1 }"]

let tokens =
   let print_const name =
      sprintf "{ let (s, loc) = $1 in %s (s, loc), loc }" name
   in
   let tokens =
      List.map (fun (s, id) -> s, print_const id) tokens
   in
      tokens @ named_tokens

(************************************************************************
 * Sets of tokens.
 *)
let colon =
   ["TokColon";
    "TokDoubleColon";
    "TokNamedColon"]

let id =
   ["TokId";
    "TokKeyword";
    "TokCatch";
    "TokClass"]

let key =
   ["TokId";
    "TokKeyword";
    "TokCatch";
    "TokClass";
    "TokKey"]

let white =
   ["TokWhite"]

let parens =
   ["TokLeftParen";
    "TokRightParen"]

let arg =
   ["TokComma";
    "TokArrow";
    "TokIn"]

(************************************************************************
 * Productions.
 *)
let subtract l1 l2 =
   List.fold_left (fun l1 v ->
         List.remove_assoc v l1) l1 l2

let add l1 l2 =
   List.fold_left (fun l v ->
         (v, List.assoc v l1) :: l) [] l2

let text_next =
   tokens

let text_nonwhite =
   subtract tokens white

let target_next =
   subtract tokens colon

let target_start =
   subtract target_next white

let keyword_target_start =
   subtract target_start ["TokLeftParen"]

let paren_next =
   subtract tokens parens

let arg_next =
   subtract paren_next arg

let arg_any_start =
   subtract arg_next white

let arg_start =
   subtract arg_any_start key

let arg_next_noneq =
   subtract arg_any_start ["TokEq"]

let other_start =
   subtract tokens ("TokWhite" :: "quote" :: List.flatten [id; colon])

let other_method_id_white =
   subtract tokens ("TokEq" :: "TokArray" :: "TokLeftParen" :: "TokColon" :: colon)

let other_method_id =
   subtract other_method_id_white ("TokDot" :: white)

let other_method_id_prefix_white =
   subtract tokens ("TokEq" :: List.flatten [id; colon])

let other_method_id_prefix =
   subtract other_method_id_prefix_white white

let other_quote_id_white =
   subtract tokens ("TokEq" :: "TokColon" :: colon)

let other_quote_id =
   subtract other_quote_id_white white

let productions =
    ["colon",                           add tokens colon;
     "white",                           add tokens white;
     "text_next",                       text_next;
     "text_nonwhite",                   text_nonwhite;
     "target_next",                     target_next;
     "target_start",                    target_start;
     "keyword_target_start",            keyword_target_start;
     "paren_next",                      paren_next;
     "arg_next",                        arg_next;
     "arg_start",                       arg_start;
     "arg_any_start",                   arg_any_start;
     "arg_next_noneq",                  arg_next_noneq;
     "other_start",                     other_start;
     "other_method_id_white",           other_method_id_white;
     "other_method_id",                 other_method_id;
     "other_method_id_prefix_white",    other_method_id_prefix_white;
     "other_method_id_prefix",          other_method_id_prefix;
     "other_quote_id_white",            other_quote_id_white;
     "other_quote_id",                  other_quote_id]

let print_productions outx =
   List.iter (fun (v, tokens) ->
         fprintf outx "%s:\n" v;
         List.iter (fun (v, body) ->
               fprintf outx "\t| %s\n\t\t%s\n" v body) tokens;
         fprintf outx "\t;\n") productions

(************************************************************************
 * Process the input file, and write the output file.
 *)
let copy inx outx =
   let rec copy_exn () =
      let line = input_line inx in
      let line =
         let l = String.length line in
            if l > 0 then
               let l = l - 1 in
                  match line.[l] with
                     '\n' | '\r' ->
                        String.sub line 0 l
                   | _ ->
                        line
            else
               line
      in
         if line = "%%GENERATED%%" then
            print_productions outx
         else
            fprintf outx "%s\n" line;
         copy_exn ()
   in
   let () =
      try copy_exn () with
         End_of_file ->
            ()
   in
      close_in inx;
      close_out outx

let infile = ref None
let outfile = ref None

let spec =
   ["-o", Arg.String (fun s -> outfile := Some s), "set output file"]

let usage =
   "Generate parse file"

let set_input s =
    infile := Some s

let main () =
   Arg.parse spec set_input usage;
   let inx =
      match !infile with
         Some file ->
            open_in file
       | None ->
            stdin
   in
   let outx =
      match !outfile with
         Some file ->
            open_out file
       | None ->
            stdout
   in
      copy inx outx

let () =
   Printexc.catch main ()

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
