(*
 * \begin{doc}
 * \section{Higher-level IO functions}
 *
 * \subsection{Regular expressions}
 * \index{regular expressions}
 *
 * Many of the higher-level functions use regular expressions.
 * Regular expressions are defined by strings with syntax nearly identical
 * to \Cmd{awk}{1}.
 *
 * Strings may contain the following character constants.
 *
 * \begin{itemize}
 * \item \verb+\\+ : a literal backslash.
 * \item \verb+\a+ : the alert character \verb+^G+.
 * \item \verb+\b+ : the backspace character \verb+^H+.
 * \item \verb+\f+ : the formfeed character \verb+^L+.
 * \item \verb+\n+ : the newline character \verb+^J+.
 * \item \verb+\r+ : the carriage return character \verb+^M+.
 * \item \verb+\t+ : the tab character \verb+^I+.
 * \item \verb+\v+ : the vertical tab character.
 * \item \verb+\xhh...+ : the character represented by the string
 *   of hexadecimal digits \verb+h+.  All valid hexadecimal digits
 *   following the sequence are considered to be part of the sequence.
 * \item \verb+\ddd+ : the character represented by 1, 2, or 3 octal
 *   digits.
 * \end{itemize}
 *
 * Regular expressions are defined using the special characters \verb+.\^$[(){}*?++.
 *
 * \begin{itemize}
 * \item \verb+c+ : matches the literal character \verb+c+ if \verb+c+ is not
 *    a special character.
 * \item \verb+\c+ : matches the literal character \verb+c+, even if \verb+c+
 *    is a special character.
 * \item \verb+.+ : matches any character, including newline.
 * \item \verb+^+ : matches the beginning of a line.
 * \item \verb+$+ : matches the end of line.
 * \item \verb+[abc...]+ : matches any of the characters \verb+abc...+
 * \item \verb+[^abc...]+ : matches any character except \verb+abc...+
 * \item \verb+r1|r2+ : matches either \verb+r1+ or \verb+r2+.
 * \item \verb+r1r2+ : matches \verb+r1+ and then \verb+r2+.
 * \item \verb+r++ : matches one or more occurrences of \verb+r+.
 * \item \verb+r*+ : matches zero or more occurrences of \verb+r+.
 * \item \verb+r?+ : matches zero or one occurrence of \verb+r+.
 * \item \verb+(r)+ : parentheses are used for grouping; matches \verb+r+.
 * \item \verb+\(r\)+ : also defines grouping, but the expression matched
 *    within the parentheses is available to the output processor
 *    through one of the variables \verb+$1+, \verb+$2+, ...
 * \item \verb+r{n}+ : matches exactly \verb+n+ occurrences of \verb+r+.
 * \item \verb+r{n,}+ : matches \verb+n+ or more occurrences of \verb+r+.
 * \item \verb+r{n,m}+ : matches at least \verb+n+ occurrences of \verb+r+,
 *    and no more than \verb+m+ occurrences.
 * \item \verb+\y+: matches the empty string at either the beginning or
 *    end of a word.
 * \item \verb+\B+: matches the empty string within a word.
 * \item \verb+\<+: matches the empty string at the beginning of a word.
 * \item \verb+\>+: matches the empty string at the end of a word.
 * \item \verb+\w+: matches any character in a word.
 * \item \verb+\W+: matches any character that does not occur within a word.
 * \item \verb+\`+: matches the empty string at the beginning of a file.
 * \item \verb+\'+: matches the empty string at the end of a file.
 * \end{itemize}
 *
 * Character classes can be used to specify character sequences
 * abstractly.  Some of these sequences can change depending on your LOCALE.
 *
 * \begin{itemize}
 * \item \verb+[:alnum:]+ Alphanumeric characters.
 * \item \verb+[:alpha:]+ Alphabetic characters.
 * \item \verb+[:lower:]+ Lowercase alphabetic characters.
 * \item \verb+[:upper:]+ Uppercase alphabetic characters.
 * \item \verb+[:cntrl:]+ Control characters.
 * \item \verb+[:digit:]+ Numeric characters.
 * \item \verb+[:xdigit:]+ Numeric and hexadecimal characters.
 * \item \verb+[:graph:]+ Characters that are printable and visible.
 * \item \verb+[:print:]+ Characters that are printable, whether they are visible or not.
 * \item \verb+[:punct:]+ Punctuation characters.
 * \item \verb+[:blank:]+ Space or tab characters.
 * \item \verb+[:space:]+ Whitespace characters.
 * \end{itemize}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2010 Mojave Group, California Institute of Technology, and
 * HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_debug
open Lm_printf
open Lm_parser
open Lm_location
open Lm_symbol

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_eval
open Omake_node
open Omake_value
open Omake_lexer
open Omake_parser
open Omake_printf
open Omake_symbol
open Omake_builtin
open Omake_value_type
open Omake_value_print
open Omake_builtin_util
open Omake_builtin_type

module Pos = MakePos (struct let name = "Omake_builtin_io_fun" end)
open Pos

let debug_parsing =
   create_debug (**)
      { debug_name = "parsing";
        debug_description = "Debug parsing operations";
        debug_value = false
      }

(*
 * Concatenate files into a string.
 *
 * \begin{doc}
 * \fun{cat}
 *
 * \begin{verbatim}
 *     cat(files) : Sequence
 *        files : File or InChannel Sequence
 * \end{verbatim}
 *
 * The \verb+cat+ function concatenates the output from multiple files
 * and returns it as a string.
 * \end{doc}
 *)
let cat venv pos loc args =
   let pos = string_pos "cat" pos in
      match args with
         [arg] ->
            let names = values_of_value venv pos arg in
            let buf = Buffer.create 1024 in
               List.iter (fun name ->
                     try
                        let inp, close_flag = in_channel_of_any_value venv pos name in
                        let inx = venv_find_channel venv pos inp in
                        let rec copy () =
                           let c = Lm_channel.input_char inx in
                              Buffer.add_char buf c;
                              copy ()
                        in
                        let () =
                           try copy () with
                              End_of_file ->
                                 ()
                        in
                           if close_flag then
                              venv_close_channel venv pos inp
                     with
                        Sys_error _ ->
                           let print_error buf =
                              fprintf buf "unable to open file: %a" pp_print_value name
                           in
                              raise (OmakeException (loc_pos loc pos, LazyError print_error))) names;
               ValString (Buffer.contents buf)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Grep takes some flags.
 *
 * \begin{doc}
 * \fun{grep}
 *
 * \begin{verbatim}
 *    grep(pattern) : String  # input from stdin, default options
 *       pattern : String
 *    grep(pattern, files) : String  # default options
 *       pattern : String
 *       files   : File Sequence
 *    grep(options, pattern, files) : String
 *      options : String
 *      pattern : String
 *      files   : File Sequence
 * \end{verbatim}
 *
 * The \verb+grep+ function searches for occurrences of a regular
 * expression \verb+pattern+ in a set of files, and prints lines that match.
 * This is like a highly-simplified version of \Cmd{grep}{1}.
 *
 * The options are:
 * \begin{description}
 * \item[q] If specified, the output from \verb+grep+ is not displayed.
 * \item[h] If specified, output lines will not include the filename (default, when only one input
 *          file is given).
 * \item[n] If specified, output lines include the filename (default, when more than one input file
 *          is given).
 * \item[v] If specified, search for lines without a match instead of lines with a match,
 * \end{description}
 *
 * The \verb+pattern+ is a regular expression.
 *
 * If successful (\verb+grep+ found a match), the function returns \verb+true+.
 * Otherwise, it returns \verb+false+.
 * \end{doc}
 *)
type grep_flag =
   GrepQuiet
 | GrepPrint
 | GrepNoPrint
 | GrepNoMatch

let grep_flags pos loc s =
   let len = String.length s in
   let rec collect flags i =
      if i = len then
         flags
      else
         let flag =
            match s.[i] with
               'q' ->
                  GrepQuiet
             | 'n' ->
                  GrepPrint
             | 'v' ->
                  GrepNoMatch
             | 'h' ->
                  GrepNoPrint
             | c ->
                  raise (OmakeException (loc_pos loc pos, StringStringError ("illegal grep option", String.make 1 c)))
         in
            collect (flag::flags) (succ i)
   in
      collect [] 0

let grep venv pos loc args =
   let pos = string_pos "grep" pos in
   let outx = channel_of_var venv pos loc stdout_var in
   let flags, pattern, files =
      match args with
         [pattern] ->
            ValNone, pattern, ValNone
       | [pattern; files] ->
            ValNone, pattern, files
       | [flags; pattern; files] ->
            flags, pattern, files
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 3), List.length args)))
   in
   let flags = grep_flags pos loc (string_of_value venv pos flags) in
   let pattern = string_of_value venv pos pattern in
   let pattern =
      try lexer_of_string pattern
      with Failure err ->
         let msg = sprintf "Mailformed regular expression '%s'" pattern in
            raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
   in
   let files = values_of_value venv pos files in
   let flags, files =
      match files with
         [] ->
            flags, [venv_find_var venv pos loc stdin_var]
       | [_] ->
            flags, files
       | _::_::_ ->
            (if List.mem GrepNoPrint flags then flags else GrepPrint :: flags), files
   in
   let verbose = not (List.mem GrepQuiet flags) in
   let print = List.mem GrepPrint flags in
   let matches = not (List.mem GrepNoMatch flags) in

   (* Grep against a single line *)
   let grep_line file found line =
      let b = ((lexer_matches pattern line) == matches) in
         if b && verbose then
            begin
               if print then
                  begin
                     Lm_channel.output_string outx file;
                     Lm_channel.output_char outx ':'
                  end;
               Lm_channel.output_string outx line;
               Lm_channel.output_char outx '\n'
            end;
         found || b
   in

   (* Open the file *)
   let grep_file found s =
      let filename = string_of_value venv pos s in
      let inp, close_flag = in_channel_of_any_value venv pos s in
      let inx = venv_find_channel venv pos inp in
      let rec search found =
         let text =
            try Some (Lm_channel.input_line inx) with
               End_of_file ->
                  None
         in
            match text with
               Some line' ->
                  search (grep_line filename found line')
             | None ->
                  found
      in
      let found = search found in
         if close_flag then
            venv_close_channel venv pos inp;
         found
   in
   let b = List.fold_left grep_file false files in
      Lm_channel.flush outx;
      val_of_bool b

let builtin_grep venv pos loc args =
   let pos = string_pos "builtin-grep" pos in
   let args =
      match args with
         [arg] ->
            values_of_value venv pos arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in

   (* Eat options *)
   let flags, pattern, files =
      let rec collect flags args =
         match args with
            arg :: args ->
               (match string_of_value venv pos arg with
                   "-q" ->
                      collect ("q" ^ flags) args
                 | "-n" ->
                      collect ("n" ^ flags) args
                 | "-v" ->
                      collect ("v" ^ flags) args
                 | "-h" ->
                      collect ("h" ^ flags) args
                 | pattern ->
                      flags, pattern, args)
          | [] ->
               raise (OmakeException (loc_pos loc pos, StringError "no pattern specified"))
      in
         collect "" args
   in
      grep venv pos loc [ValData flags; ValData pattern; ValArray files]

(*
 * \begin{doc}
 * \fun{scan}
 *
 * \begin{verbatim}
 *    scan(input-files)
 *    case string1
 *       body1
 *    case string2
 *       body2
 *    ...
 *    default
 *       bodyd
 * \end{verbatim}
 *
 * The \verb+scan+ function provides input processing in command-line form.
 * The function takes file/filename arguments.  If called with no
 * arguments, the input is taken from \verb+stdin+. If arguments are provided,
 * each specifies an \verb+InChannel+, or the name of a file for input.
 * Output is always to \verb+stdout+.
 *
 * The \verb+scan+ function operates by reading the input one line at a time,
 * and processing it according to the following algorithm.
 *
 * For each line,
 * the record is first split into fields, and
 * the fields are bound to the variables \verb+$1, $2, ...+.  The variable
 * \verb+$0+ is defined to be the entire line, and \verb+$*+ is an array
 * of all the field values.  The \verb+$(NF)+ variable is defined to be the number
 * of fields.
 *
 * Next, a case expression is selected.  If \verb+string_i+ matches the token \verb+$1+,
 * then \verb+body_i+ is evaluated.  If the body ends in an \verb+export+, the state
 * is passed to the next clause.  Otherwise the value is discarded.
 *
 * For example, here is an \verb+scan+ function that acts as a simple command processor.
 *
 * \begin{verbatim}
 *     calc() =
 *        i = 0
 *        scan(script.in)
 *        case print
 *           println($i)
 *        case inc
 *           i = $(add $i, 1)
 *           export
 *        case dec
 *           i = $(sub $i, 1)
 *           export
 *        case addconst
 *           i = $(add $i, $2)
 *           export
 *        default
 *           eprintln($"Unknown command: $1")
 * \end{verbatim}
 *
 * The \verb+scan+ function also supports several options.
 *
 * \begin{verbatim}
 *     scan(options, files)
 *     ...
 * \end{verbatim}
 *
 * \begin{description}
 * \item[A] Parse each line as an argument list, where arguments
 *    may be quoted.  For example, the following line has three words,
 *    ``\verb+ls+'', ``\verb+-l+'', ``\verb+Program Files+''.
 *
 *    \begin{verbatim}
 *        ls -l "Program Files"
 *    \end{verbatim}
 * \item[O] Parse each line using white space as the separator, using the
 *    usual \OMake{} algorithm for string parsing.  This is the default.
 * \item[x] Once each line is split, reduce each word using the
 *    hex representation.  This is the usual hex representation used
 *    in URL specifiers, so the string ``Program Files'' may be
 *    alternately represented in the form Program%20Files or
 *    Program+Files.
 * \end{description}
 *
 * Note, if you want to redirect the output to a file, the easiest way is to
 * redefine the \verb+stdout+ variable.  The \verb+stdout+ variable is scoped the
 * same way as other variables, so this definition does not affect the meaning of
 * \verb+stdout+ outside the \verb+calc+ function.
 *
 * \begin{verbatim}
 *     calc() =
 *         stdout = $(fopen script.out, w)
 *         scan(script.in)
 *            ...
 *         close($(stdout))
 * \end{verbatim}
 * \end{doc}
 *)

(*
 * Scanner options.
 *)
type parse_option =
   ParseArgs
 | ParseWords

type rewrite_option =
   RewriteHex
 | RewriteNone

let scan_options venv pos loc options s =
   let len = String.length s in
   let rec collect ((poption, roption) as options) i =
      if i = len then
         options
      else
         let options =
            match s.[i] with
               'A' ->
                  ParseArgs, roption
             | 'O' ->
                  ParseWords, roption
             | 'x' ->
                  poption, RewriteHex
             | c ->
                  raise (OmakeException (loc_pos loc pos, StringStringError ("illegal option", s)))
         in
            collect options (succ i)
   in
      collect options 0

let scan_options venv pos loc options =
   List.fold_left (scan_options venv pos loc) (ParseWords, RewriteNone) (strings_of_value venv pos options)

(*
 * The arguments.
 *)
let scan_args venv pos loc args =
   let pos = string_pos "scan_args" pos in
   let cases, options, files =
      match args with
         [ValCases cases] ->
            cases, ValNone, venv_find_var venv pos loc stdin_var
       | [ValCases cases; files] ->
            cases, ValNone, files
       | [ValCases cases; options; files] ->
            cases, options, files
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let poptions, roptions = scan_options venv pos loc options in
      cases, poptions, roptions, values_of_value venv pos files

(*
 * Awk the value.
 *)
let scan venv pos loc args kargs =
   let pos = string_pos "scan" pos in
   let cases, token_mode, rewrite_mode, files = scan_args venv pos loc args in

   (* Get lexers for all the cases *)
   let cases, def =
      List.fold_left (fun (cases, def) (v, test, body, export) ->
            if Lm_symbol.eq v case_sym then
               let s = string_of_value venv pos test in
               let cases =
                  SymbolTable.filter_add cases (Lm_symbol.add s) (fun b ->
                        match b with
                           Some _ ->
                              raise (OmakeException (loc_pos loc pos, StringVarError ("duplicate case", v)))
                         | None ->
                              body, export)
               in
                  cases, def
            else if Lm_symbol.eq v default_sym then
               match def with
                  Some _ ->
                     raise (OmakeException (loc_pos loc pos, StringError "duplicate default case"))
                | None ->
                     cases, Some (body, export)
            else
               raise (OmakeException (loc_pos loc pos, StringVarError ("unknown case", v)))) (SymbolTable.empty, None) cases
   in

   (* Split a line into words *)
   let collect_words_argv line =
      let words =
         match token_mode with
            ParseArgs ->
               Lm_string_util.parse_args line
          | ParseWords ->
               strings_of_value venv pos (ValString line)
      in
         match rewrite_mode with
            RewriteHex ->
               List.map Lm_string_util.decode_hex_name words
          | RewriteNone ->
               words
   in

   (* Select a case and run it *)
   let eval_case venv words =
      let body =
         match words with
            command :: _ ->
               (try Some (SymbolTable.find cases (Lm_symbol.add command)) with
                   Not_found ->
                      def)
          | [] ->
               def
      in
         match body with
            Some (body, export) ->
               let venv_new, _ = eval_sequence_exp venv pos body in
                  add_exports venv venv_new pos export
          | None ->
               venv
   in

   (* Read the file a line at a time *)
   let rec line_loop venv inx =
      let text =
         try Some (Lm_channel.input_line inx) with
            End_of_file ->
               None
      in
         match text with
            Some line ->
               let words = collect_words_argv line in
               let venv = venv_add_match venv line words in
               let venv = eval_case venv words in
                  line_loop venv inx
          | None ->
               venv
   in
   let rec file_loop venv args =
      match args with
         arg :: args ->
            let inp, close_in = in_channel_of_any_value venv pos arg in
            let inx = venv_find_channel venv pos inp in
            let venv =
               try line_loop venv inx with
                  exn when close_in ->
                     venv_close_channel venv pos inp;
                     raise exn
               in
               if close_in then
                  venv_close_channel venv pos inp;
               file_loop venv args
       | [] ->
            venv
   in
      file_loop venv files, ValNone

(*
 * \begin{doc}
 * \fun{awk}
 *
 * \begin{verbatim}
 *    awk(input-files)
 *    case pattern1:
 *       body1
 *    case pattern2:
 *       body2
 *    ...
 *    default:
 *       bodyd
 *\end{verbatim}
 *
 * or
 *
 * \begin{verbatim}
 *    awk(options, input-files)
 *    case pattern1:
 *       body1
 *    case pattern2:
 *       body2
 *    ...
 *    default:
 *       bodyd
 * \end{verbatim}
 *
 * The \verb+awk+ function provides input processing similar to \Cmd{awk}{1},
 * but more limited.  The \verb+input-files+ argument is a sequence of values,
 * each specifies an \verb+InChannel+, or the name of a file for input.
 * If called with no options and no file arguments, the input is taken from \verb+stdin+.
 * Output is always to \verb+stdout+.
 *
 * The variables \verb+RS+ and \verb+FS+ define record and field separators
 * as regular expressions.
 * The default value of \verb+RS+ is the regular expression \verb+\r|\n|\r\n+.
 * The default value of \verb+FS+ is the regular expression \verb+[ \t]++.
 *
 * The \verb+awk+ function operates by reading the input one record at a time,
 * and processing it according to the following algorithm.
 *
 * For each line,
 * the record is first split into fields using the field separator \verb+FS+, and
 * the fields are bound to the variables \verb+$1, $2, ...+.  The variable
 * \verb+$0+ is defined to be the entire line, and \verb+$*+ is an array
 * of all the field values.  The \verb+$(NF)+ variable is defined to be the number
 * of fields.
 *
 * Next, the cases are evaluated in order.
 * For each case, if the regular expression \verb+pattern_i+ matches the record \verb+$0+,
 * then \verb+body_i+ is evaluated.  If the body ends in an \verb+export+, the state
 * is passed to the next clause.  Otherwise the value is discarded.  If the regular
 * expression contains \verb+\(r\)+ expression, those expression override the
 * fields \verb+$1, $2, ...+.
 *
 * For example, here is an \verb+awk+ function to print the text between two
 * delimiters \verb+\begin{<name>}+ and \verb+\end{<name>}+, where the \verb+<name>+
 * must belong to a set passed as an argument to the \verb+filter+ function.
 *
 * \begin{verbatim}
 *     filter(names) =
 *        print = false
 *
 *        awk(Awk.in)
 *        case $"^\\end\{\([:alpha:]+\)\}"
 *           if $(mem $1, $(names))
 *              print = false
 *              export
 *           export
 *        default
 *           if $(print)
 *              println($0)
 *        case $"^\\begin\{\([:alpha:]+\)\}"
 *           print = $(mem $1, $(names))
 *           export
 * \end{verbatim}
 *
 * Note, if you want to redirect the output to a file, the easiest way is to
 * redefine the \verb+stdout+ variable.  The \verb+stdout+ variable is scoped the
 * same way as other variables, so this definition does not affect the meaning of
 * \verb+stdout+ outside the \verb+filter+ function.
 *
 * \begin{verbatim}
 *     filter(names) =
 *         stdout = $(fopen file.out, w)
 *         awk(Awk.in)
 *            ...
 *         close($(stdout))
 * \end{verbatim}
 *
 * Options.
 * \begin{description}
 * \item[b] ``Break'' when evaluating cases.  Only the first case that matches will be selected.
 * \end{description}
 *
 * The \hyperfun{break} can be used to abort the loop,
 * exiting the \verb+awk+ function immediately.
 * \end{doc}
 *)

(*
 * Evaluate all the cases that match.
 *)
let rec awk_eval_cases venv pos loc break line cases =
   match cases with
      (None, body, export) :: cases ->
         let venv_new, _ = eval_sequence_exp venv pos body in
         let venv = add_exports venv venv_new pos export in
            if break then
               venv
            else
               awk_eval_cases venv pos loc break line cases
    | (Some lex, body, export) :: cases ->
         let channel = Lm_channel.of_string line in
         let venv, stop =
            match Lexer.search lex channel with
               Some (_, _, _, _, args) ->
                  let venv_new = venv_add_match_args venv args in
                  let venv_new, _ = eval_sequence_exp venv_new pos body in
                  let venv = add_exports venv venv_new pos export in
                     venv, break
             | None ->
                  venv, false
         in
            if stop then
               venv
            else
               awk_eval_cases venv pos loc break line cases
    | [] ->
         venv

(*
 * The arguments.
 *)
let awk_args venv pos loc args =
   let pos = string_pos "awk_args" pos in
      match args with
         [ValCases cases] ->
            cases, [venv_find_var venv pos loc stdin_var]
       | [ValCases cases; files] ->
            cases, values_of_value venv pos files
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))

let awk_option_args venv pos loc args =
   let pos = string_pos "awk_args" pos in
      match args with
         [ValCases cases] ->
            cases, "", [venv_find_var venv pos loc stdin_var]
       | [ValCases cases; files] ->
            cases, "", values_of_value venv pos files
       | [ValCases cases; options; files] ->
            cases, string_of_value venv pos options, values_of_value venv pos files
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 3), List.length args)))

type awk_flag =
   AwkBreak

let awk_flags pos loc s =
   let len = String.length s in
   let rec collect flags i =
      if i = len then
         flags
      else
         let flag =
            match s.[i] with
               'b' ->
                  AwkBreak
             | c ->
                  raise (OmakeException (loc_pos loc pos, StringStringError ("illegal awk option", String.make 1 c)))
         in
            collect (flag :: flags) (succ i)
   in
      collect [] 0

(*
 * Awk the value.
 *)
let awk venv pos loc args kargs =
   let pos = string_pos "awk" pos in
   let cases, flags, files = awk_option_args venv pos loc args in
   let flags = awk_flags pos loc flags in
   let break = List.mem AwkBreak flags in

   (* Separator expressions *)
   let rs =
      try string_of_value venv pos (venv_find_var_exn venv rs_var) with
         Not_found ->
            "\r|\n|\r\n"
   in
   let fs =
      try string_of_value venv pos (venv_find_var_exn venv fs_var) with
         Not_found ->
            "[ \t]+"
   in
   let rs_lex =
      try lexer_of_string rs with
         Failure err ->
            let msg = sprintf "Malformed regular expression '%s'" rs in
               raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
   in
   let fs_lex =
      try lexer_of_string fs with
         Failure err ->
            let msg = sprintf "Malformed regular expression '%s'" fs in
               raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
   in

   (* Get lexers for all the cases *)
   let cases =
      List.map (fun (v, test, body, export) ->
            if Lm_symbol.eq v case_sym then
               let s = string_of_value venv pos test in
               let _, lex =
                  try Lexer.add_clause Lexer.empty v s with
                     Failure err ->
                        let msg = sprintf "Malformed regular expression '%s'" s in
                           raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
               in
                  Some lex, body, export
            else if Lm_symbol.eq v default_sym then
               None, body, export
            else
               raise (OmakeException (loc_pos loc pos, StringVarError ("unknown case", v)))) cases
   in

   (* Split a line into words *)
   let collect_words line =
      let channel = Lm_channel.of_string line in
      let rec collect words =
         match Lexer.searchto fs_lex channel with
            Lexer.LexEOF ->
               List.rev words
          | Lexer.LexSkipped (_, skipped)
          | Lexer.LexMatched (_, _, skipped, _, _) ->
               collect (skipped :: words)
      in
         collect []
   in

   (* Read the file a line at a time *)
   let rec line_loop venv inx lineno =
      match Lexer.searchto rs_lex inx with
         Lexer.LexEOF ->
            venv
       | Lexer.LexSkipped (_, line)
       | Lexer.LexMatched (_, _, line, _, _) ->
            (* Split into words *)
            let words = collect_words line in
            let venv = venv_add_match venv line words in
            let venv = venv_add_var venv fnr_var (ValInt lineno) in
            let venv = awk_eval_cases venv pos loc break line cases in
               line_loop venv inx (lineno + 1)
   in
   let rec file_loop venv args =
      match args with
         arg :: args ->
            let inp, close_in = in_channel_of_any_value venv pos arg in
            let inx = venv_find_channel venv pos inp in
            let venv = venv_add_var venv filename_var (ValData (Lm_channel.name inx)) in
            let venv =
               try line_loop venv inx 1 with
                  exn when close_in ->
                     venv_close_channel venv pos inp;
                     raise exn
            in
               if close_in then
                  venv_close_channel venv pos inp;
               file_loop venv args
       | [] ->
            venv
   in
   let venv =
      try file_loop venv files with
         Break (_, venv) ->
            venv
   in
      venv, ValNone

(*
 * \begin{doc}
 * \fun{fsubst}
 *
 * \begin{verbatim}
 *    fsubst(files)
 *    case pattern1 [options]
 *       body1
 *    case pattern2 [options]
 *       body2
 *    ...
 *    default
 *       bodyd
 * \end{verbatim}
 *
 * The \verb+fsubst+ function provides a \Cmd{sed}{1}-like substitution
 * function.  Similar to \verb+awk+, if \verb+fsubst+ is called with no
 * arguments, the input is taken from \verb+stdin+.  If arguments are provided,
 * each specifies an \verb+InChannel+, or the name of a file for input.
 *
 * The \verb+RS+ variable defines a regular expression that determines a record separator,
 * The default value of \verb+RS+ is the regular expression \verb+\r|\n|\r\n+.
 *
 * The \verb+fsubst+ function reads the file one record at a time.
 *
 * For each record, the cases are evaluated in order.  Each case defines
 * a substitution from a substring matching the \verb+pattern+ to
 * replacement text defined by the body.
 *
 * Currently, there is only one option: \verb+g+.
 * If specified, each clause specifies a global replacement,
 * and all instances of the pattern define a substitution.
 * Otherwise, the substitution is applied only once.
 *
 * Output can be redirected by redefining the \verb+stdout+ variable.
 *
 * For example, the following program replaces all occurrences of
 * an expression \verb+word.+ with its capitalized form.
 *
 * \begin{verbatim}
 *     section
 *        stdout = $(fopen Subst.out, w)
 *        fsubst(Subst.in)
 *        case $"\<\([[:alnum:]]+\)\." g
 *           value $(capitalize $1).
 *        close($(stdout))
 * \end{verbatim}
 * \end{doc}
 *)

(*
 * Substitution options.
 *)
let subst_global_opt = 1

let subst_options venv pos loc options s =
   let len = String.length s in
   let rec collect options i =
      if i = len then
         options
      else
         let flag =
            match s.[i] with
               'g' ->
                  subst_global_opt
             | c ->
                  raise (OmakeException (loc_pos loc pos, StringStringError ("illegal option", s)))
         in
            collect (options lor flag) (succ i)
   in
      collect options 0

(*
 * Sed function performs a substitution line-by-line.
 *)
let rec subst_eval_case venv pos loc buf channel lex options body =
   match Lexer.searchto lex channel with
      Lexer.LexEOF ->
         ()
    | Lexer.LexSkipped (_, skipped) ->
         Buffer.add_string buf skipped
    | Lexer.LexMatched (_, _, skipped, matched, args) ->
         let venv' = venv_add_match venv matched args in
         let _, result = eval_sequence_exp venv' pos body in
            Buffer.add_string buf skipped;
            Buffer.add_string buf (string_of_value venv pos result);
            if (options land subst_global_opt) <> 0 then
               subst_eval_case venv pos loc buf channel lex options body
            else
               Lm_channel.LexerInput.lex_buffer channel buf

let subst_eval_line venv pos loc line cases =
   let buffer = Buffer.create (String.length line) in
      List.fold_left (fun line (lex, options, body) ->
            let channel = Lm_channel.of_string line in
               Buffer.clear buffer;
               subst_eval_case venv pos loc buffer channel lex options body;
               Buffer.contents buffer) line cases

let fsubst venv pos loc args kargs =
   let pos = string_pos "fsubst" pos in
   let cases, files = awk_args venv pos loc args in
   let outp = prim_channel_of_var venv pos loc stdout_var in
   let outx = venv_find_channel venv pos outp in

   (* Record separator *)
   let rs =
      try string_of_value venv pos (venv_find_var_exn venv rs_var) with
         Not_found ->
            "\r|\n|\r\n"
   in
   let rs_lex =
      try lexer_of_string rs with
         Failure err ->
            let msg = sprintf "Malformed regular expression '%s'" rs in
               raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
   in

   (* Get lexers for all the cases *)
   let cases =
      List.map (fun (v, test, body, _) ->
            let args = values_of_value venv pos test in
            let pattern, options =
               match args with
                  pattern :: options ->
                     string_of_value venv pos pattern, options
                | [] ->
                     "", []
            in
            let pattern, options =
               if Lm_symbol.eq v case_sym then
                  pattern, options
               else if Lm_symbol.eq v default_sym then
                  ".*", []
               else
                  raise (OmakeException (loc_pos loc pos, StringVarError ("unknown case", v)))
            in
            let options =
               List.fold_left (fun options arg ->
                     subst_options venv pos loc options (string_of_value venv pos arg)) 0 options
            in
            let _, lex =
               try Lexer.add_clause Lexer.empty v pattern with
                  Failure err ->
                     let msg = sprintf "Malformed regular expression '%s'" pattern in
                        raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
            in
               lex, options, body) cases
   in

   (* Read the file a line at a time *)
   let rec line_loop inx =
      match Lexer.searchto rs_lex inx with
         Lexer.LexEOF ->
            ()
       | Lexer.LexSkipped (_, line) ->
            let line = subst_eval_line venv pos loc line cases in
               Lm_channel.output_string outx line
       | Lexer.LexMatched (_, _, line, term, _) ->
            let line = subst_eval_line venv pos loc line cases in
               Lm_channel.output_string outx line;
               Lm_channel.output_string outx term;
               line_loop inx
   in
   let rec file_loop files =
      match files with
         file :: files ->
            let inp, close_in = in_channel_of_any_value venv pos file in
            let inx = venv_find_channel venv pos inp in
            let () =
               try line_loop inx with
                  exn when close_in ->
                     venv_close_channel venv pos inp;
                     raise exn
            in
               if close_in then
                  venv_close_channel venv pos inp;
               file_loop files
       | [] ->
            ()
   in
   let venv =
      try file_loop files; venv with
         Break (_, venv) ->
            venv
   in
      Lm_channel.flush outx;
      venv, ValNone

(*
 * \begin{doc}
 * \fun{lex}
 *
 * \begin{verbatim}
 *    lex(files)
 *    case pattern1
 *       body1
 *    case pattern2
 *       body2
 *    ...
 *    default
 *       bodyd
 * \end{verbatim}
 *
 * The \verb+lex+ function provides a simple lexical-style scanner
 * function.  The input is a sequence of files or channels.  The cases
 * specify regular expressions.  Each time the input is read, the regular
 * expression that matches the \emph{longest prefix} of the input is selected,
 * and the body is evaluated.
 *
 * If two clauses both match the same input, the \emph{last} one is selected
 * for execution.  The \verb+default+ case matches the regular expression \verb+.+;
 * you probably want to place it first in the pattern list.
 *
 * If the body end with an \verb+export+ directive,
 * the state is passed to the next clause.
 *
 * For example, the following program collects all occurrences of alphanumeric
 * words in an input file.
 *
 * \begin{verbatim}
 *     collect-words($(files)) =
 *        words[] =
 *        lex($(files))
 *        default
 *           # empty
 *        case $"[[:alnum:]]+" g
 *           words[] += $0
 *           export
 * \end{verbatim}
 *
 * The \verb+default+ case, if one exists, matches single characters.  Since
 *
 * It is an error if the input does not match any of the regular expressions.
 *
 * The \hyperfun{break} can be used to abort the loop.
 * \end{doc}
 *)
let eof_sym = Lm_symbol.add "eof"

let lex venv pos loc args kargs =
   let pos = string_pos "lex" pos in
   let cases, files = awk_args venv pos loc args in

   (* Add a clause for EOF *)
   let _, lex = Lexer.add_clause Lexer.empty eof_sym "\\'" in

   (* Get lexers for all the cases *)
   let lex, cases, _ =
      List.fold_left (fun (lex, cases, index) (v, test, body, export) ->
            let args = values_of_value venv pos test in
            let pattern =
               match args with
                  pattern :: _ ->
                     string_of_value venv pos pattern
                | [] ->
                     ""
            in
            let pattern =
               if Lm_symbol.eq v case_sym then
                  pattern
               else if Lm_symbol.eq v default_sym then
                  "."
               else
                  raise (OmakeException (loc_pos loc pos, StringVarError ("unknown case", v)))
            in
               let action_sym = Lm_symbol.make "action" index in
               let _, lex =
                  try Lexer.add_clause lex action_sym pattern with
                     Failure err ->
                        let msg = sprintf "Malformed regular expression '%s'" pattern in
                           raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
               in
               let cases = SymbolTable.add cases action_sym (body, export) in
                  lex, cases, succ index) (lex, SymbolTable.empty, 0) cases
   in

   (* Process the files *)
   let rec input_loop venv inx =
      let action_sym, lexeme_loc, lexeme, args = Lexer.lex lex inx in
         if Lm_symbol.eq action_sym eof_sym then
            venv
         else
            let venv_new = venv_add_match venv lexeme args in
            let venv_new = venv_add_var venv_new parse_loc_var (ValOther (ValLocation lexeme_loc)) in
            let body, export =
               try SymbolTable.find cases action_sym with
                  Not_found ->
                     raise (Invalid_argument "lex")
            in
            let venv_new, _ = eval_sequence_exp venv_new pos body in
            let venv = add_exports venv venv_new pos export in
               input_loop venv inx
   in
   let rec file_loop venv files =
      match files with
         file :: files ->
            let inp, close_in = in_channel_of_any_value venv pos file in
            let inx = venv_find_channel venv pos inp in
            let venv =
               try input_loop venv inx with
                  (Break _ | Return _ ) as exn ->
                     if close_in then
                        venv_close_channel venv pos inp;
                     raise exn
                | exn ->
                     if close_in then
                        venv_close_channel venv pos inp;
                     raise_uncaught_exception pos exn
            in
               if close_in then
                  venv_close_channel venv pos inp;
               file_loop venv files
       | [] ->
            venv
   in
   let venv =
      try file_loop venv files with
         Break (_, venv) ->
            venv
   in
      venv, ValNone

(*
 * \begin{doc}
 * \fun{lex-search}
 *
 * \begin{verbatim}
 *    lex-search(files)
 *    case pattern1
 *       body1
 *    case pattern2
 *       body2
 *    ...
 *    default
 *       bodyd
 * \end{verbatim}
 *
 * The \verb+lex-search+ function is like the \verb+lex+ function, but input that
 * does not match any of the regular expressions is skipped.  If the clauses include
 * a \verb+default+ case, then the \verb+default+ matches any skipped text.
 *
 * For example, the following program collects all occurrences of alphanumeric
 * words in an input file, skipping any other text.
 *
 * \begin{verbatim}
 *     collect-words($(files)) =
 *        words[] =
 *        lex-search($(files))
 *        default
 *           eprintln(Skipped $0)
 *        case $"[[:alnum:]]+" g
 *           words[] += $0
 *           export
 * \end{verbatim}
 *
 * The \verb+default+ case, if one exists, matches single characters.  Since
 *
 * It is an error if the input does not match any of the regular expressions.
 *
 * The \hyperfun{break} can be used to abort the loop.
 * \end{doc}
 *)
let lex_search venv pos loc args kargs =
   let pos = string_pos "lex-search" pos in
   let cases, files = awk_args venv pos loc args in

   (* Get lexers for all the cases *)
   let lex, cases, default, _ =
      List.fold_left (fun (lex, cases, default, index) (v, test, body, export) ->
            let args = values_of_value venv pos test in
            let pattern =
               match args with
                  pattern :: _ ->
                     string_of_value venv pos pattern
                | [] ->
                     ""
            in
               if Lm_symbol.eq v case_sym then
                  let action_sym = Lm_symbol.make "action" index in
                  let _, lex =
                     try Lexer.add_clause lex action_sym pattern with
                        Failure err ->
                           let msg = sprintf "Malformed regular expression '%s'" pattern in
                              raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
                  in
                  let cases = SymbolTable.add cases action_sym (body, export) in
                     lex, cases, default, succ index
               else if Lm_symbol.eq v default_sym then
                  lex, cases, Some (body, export), index
               else
                  raise (OmakeException (loc_pos loc pos, StringVarError ("unknown case", v)))) (**)
         (Lexer.empty, SymbolTable.empty, None, 0) cases
   in

   (* What to do for skipped text *)
   let skip venv lexeme_loc lexeme =
      match lexeme, default with
         "", _
       | _, None ->
            venv
       | _, Some (body, export) ->
            let venv_new = venv_add_match venv lexeme [] in
            let venv_new = venv_add_var venv_new parse_loc_var (ValOther (ValLocation lexeme_loc)) in
            let venv_new, _ = eval_sequence_exp venv_new pos body in
               add_exports venv venv_new pos export
   in

   (* Process the files *)
   let rec input_loop venv inx =
      match Lexer.searchto lex inx with
         Lexer.LexEOF ->
            venv
       | Lexer.LexSkipped (lexeme_loc, lexeme) ->
            skip venv loc lexeme
       | Lexer.LexMatched (action_sym, lexeme_loc, skipped, lexeme, args) ->
            (* Process skipped text *)
            let venv = skip venv lexeme_loc skipped in

            (* Process the matched text *)
            let venv_new = venv_add_match venv lexeme args in
            let venv_new = venv_add_var venv_new parse_loc_var (ValOther (ValLocation lexeme_loc)) in
            let body, export =
               try SymbolTable.find cases action_sym with
                  Not_found ->
                     raise (Invalid_argument "lex")
            in
            let venv_new, _ = eval_sequence_exp venv_new pos body in
            let venv = add_exports venv venv_new pos export in
               input_loop venv inx
   in

   (* Process each file *)
   let rec file_loop venv files =
      match files with
         file :: files ->
            let inp, close_in = in_channel_of_any_value venv pos file in
            let inx = venv_find_channel venv pos inp in
            let venv =
               try input_loop venv inx with
                  (Break _ | Return _) as exn ->
                     if close_in then
                        venv_close_channel venv pos inp;
                     raise exn
                | exn ->
                     if close_in then
                        venv_close_channel venv pos inp;
                     raise_uncaught_exception pos exn
            in
               if close_in then
                  venv_close_channel venv pos inp;
               file_loop venv files
       | [] ->
            venv
   in
   let venv =
      try file_loop venv files with
         Break (_, venv) ->
            venv
   in
      venv, ValNone

(*
 * \begin{doc}
 * \obj{Lexer}
 *
 * The \verb+Lexer+ object defines a facility for lexical analysis, similar to the
 * \Cmd{lex}{1} and \Cmd{flex}{1} programs.
 *
 * In \Prog{omake}, lexical analyzers can be constructed dynamically by extending
 * the \verb+Lexer+ class.  A lexer definition consists of a set of directives specified
 * with method calls,  and set of clauses specified as rules.
 *
 * For example, consider the following lexer definition, which is intended
 * for lexical analysis of simple arithmetic expressions for a desktop
 * calculator.
 *
 * \begin{verbatim}
 *    lexer1. =
 *       extends $(Lexer)
 *
 *       other: .
 *          eprintln(Illegal character: $* )
 *          lex()
 *
 *       white: $"[[:space:]]+"
 *          lex()
 *
 *       op: $"[-+*/()]"
 *          switch $*
 *          case +
 *             Token.unit($(loc), plus)
 *          case -
 *             Token.unit($(loc), minus)
 *          case *
 *             Token.unit($(loc), mul)
 *          case /
 *             Token.unit($(loc), div)
 *          case $"("
 *             Token.unit($(loc), lparen)
 *          case $")"
 *             Token.unit($(loc), rparen)
 *
 *       number: $"[[:digit:]]+"
 *          Token.pair($(loc), exp, $(int $* ))
 *
 *       eof: $"\'"
 *          Token.unit($(loc), eof)
 * \end{verbatim}
 *
 * This program defines an object \verb+lexer1+ the extends the \verb+Lexer+
 * object, which defines lexing environment.
 *
 * The remainder of the definition consists of a set of clauses,
 * each with a method name before the colon; a regular expression
 * after the colon; and in this case, a body.  The body is optional,
 * if it is not specified, the method with the given name should
 * already exist in the lexer definition.
 *
 * \emph{NB} The clause that matches the \emph{longest} prefix of the input
 * is selected.  If two clauses match the same input prefix, then the \emph{last}
 * one is selected.  This is unlike most standard lexers, but makes more sense
 * for extensible grammars.
 *
 * The first clause matches any input that is not matched by the other clauses.
 * In this case, an error message is printed for any unknown character, and
 * the input is skipped.  Note that this clause is selected only if no other
 * clause matches.
 *
 * The second clause is responsible for ignoring white space.
 * If whitespace is found, it is ignored, and the lexer is called
 * recursively.
 *
 * The third clause is responsible for the arithmetic operators.
 * It makes use of the \verb+Token+ object, which defines three
 * fields: a \verb+loc+ field that represents the source location;
 * a \verb+name+; and a \verb+value+.
 *
 * The lexer defines the \verb+loc+ variable to be the location
 * of the current lexeme in each of the method bodies, so we can use
 * that value to create the tokens.
 *
 * The \verb+Token.unit($(loc), name)+
 * method constructs a new \verb+Token+ object with the given name,
 * and a default value.
 *
 * The \verb+number+ clause matches nonnegative integer constants.
 * The \verb+Token.pair($(loc), name, value)+ constructs a token with the
 * given name and value.
 *
 * Lexer object operate on \verb+InChannel+ objects.
 * The method \verb+lexer1.lex-channel(channel)+ reads the next
 * token from the channel argument.
 *
 * \subsection{Lexer matching}
 *
 * During lexical analysis, clauses are selected by longest match.
 * That is, the clause that matches the longest sequence of input
 * characters is chosen for evaluation.  If no clause matches, the
 * lexer raises a \verb+RuntimeException+.  If more than one clause
 * matches the same amount of input, the first one is chosen
 * for evaluation.
 *
 * \subsection{Extending lexer definitions}
 *
 * Suppose we wish to augment the lexer example so that it ignores
 * comments.  We will define comments as any text that begins with
 * the string \verb+(*+, ends with \verb+*)+, and comments may
 * be nested.
 *
 * One convenient way to do this is to define a separate lexer
 * just to skip comments.
 *
 * \begin{verbatim}
 *    lex-comment. =
 *       extends $(Lexer)
 *
 *       level = 0
 *
 *       other: .
 *          lex()
 *
 *       term: $"[*][)]"
 *          if $(not $(eq $(level), 0))
 *             level = $(sub $(level), 1)
 *             lex()
 *
 *       next: $"[(][*]"
 *          level = $(add $(level), 1)
 *          lex()
 *
 *       eof: $"\'"
 *          eprintln(Unterminated comment)
 * \end{verbatim}
 *
 * This lexer contains a field \verb+level+ that keeps track of the nesting
 * level.  On encountering a \verb+(*+ string, it increments the level,
 * and for \verb+*)+, it decrements the level if nonzero, and continues.
 *
 * Next, we need to modify our previous lexer to skip comments.
 * We can do this by extending the lexer object \verb+lexer1+
 * that we just created.
 *
 * \begin{verbatim}
 *    lexer1. +=
 *       comment: $"[(][*]"
 *          lex-comment.lex-channel($(channel))
 *          lex()
 * \end{verbatim}
 *
 * The body for the comment clause calls the \verb+lex-comment+ lexer when
 * a comment is encountered, and continues lexing when that lexer returns.
 *
 * \subsection{Threading the lexer object}
 *
 * Clause bodies may also end with an \verb+export+ directive.  In this case
 * the lexer object itself is used as the returned token.  If used with
 * the \verb+Parser+ object below, the lexer should define the \verb+loc+, \verb+name+
 * and \verb+value+ fields in each \verb+export+ clause.  Each time
 * the \verb+Parser+ calls the lexer, it calls it with the lexer returned
 * from the previous lex invocation.
 * \end{doc}
 *)

(*
 * Add a lexer clause.
 *)
let lex_rule venv pos loc args kargs =
   let pos = string_pos "lex-rule" pos in
      match args, kargs with
         [_; action; _; pattern; _; ValBody (body, export)], [] ->
            let lexer = current_lexer venv pos in
            let action_name = string_of_value venv pos action in
            let action_sym = Lm_symbol.add action_name in
            let pattern = string_of_value venv pos pattern in
            let _, lexer =
               try Lexer.add_clause lexer action_sym pattern with
                  Failure err ->
                     let msg = sprintf "Malformed regular expression '%s'" pattern in
                        raise (OmakeException (loc_pos loc pos, StringStringError (msg, err)))
            in

            (* Add the method *)
            let action_var = VarThis (loc, action_sym) in
            let venv = venv_add_var venv action_var (ValFun (venv_get_env venv, [], [], body, export)) in
            let venv = venv_add_var venv builtin_field_var (ValOther (ValLexer lexer)) in
               venv, ValNone

       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 6, List.length args)))

(*
 * Perform the lexing.
 *)
let lex_engine venv pos loc args kargs =
   let pos = string_pos "lex" pos in
      match args, kargs with
         [arg], [] ->
            let lexer = current_lexer venv pos in
            let inp, close_flag = in_channel_of_any_value venv pos arg in
            let inx = venv_find_channel venv pos inp in
            let action, lexeme_loc, lexeme, args =
               try Lexer.lex lexer inx with
                  Failure _ as exn ->
                     let loc = Lm_channel.loc inx in
                     let pos = loc_pos loc pos in
                        if close_flag then
                           venv_close_channel venv pos inp;
                        raise (UncaughtException (pos, exn))
            in
            let () =
               if close_flag then
                  venv_close_channel venv pos inp
            in
            let venv = venv_add_match venv lexeme args in
            let venv = venv_add_var venv parse_loc_var (ValOther (ValLocation lexeme_loc)) in
            let action = venv_find_var venv pos loc (VarThis (loc, action)) in
               eval_apply venv pos loc action [] []
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \obj{Parser}
 *
 * The \verb+Parser+ object provides a facility for syntactic analysis based
 * on context-free grammars.
 *
 * \verb+Parser+ objects are specified as a sequence of directives,
 * specified with method calls; and productions, specified as rules.
 *
 * For example, let's finish building the desktop calculator started
 * in the \verb+Lexer+ example.
 *
 * \begin{verbatim}
 *    parser1. =
 *       extends $(Parser)
 *
 *       #
 *       # Use the main lexer
 *       #
 *       lexer = $(lexer1)
 *
 *       #
 *       # Precedences, in ascending order
 *       #
 *       left(plus minus)
 *       left(mul div)
 *       right(uminus)
 *
 *       #
 *       # A program
 *       #
 *       start(prog)
 *
 *       prog: exp eof
 *          return $1
 *
 *       #
 *       # Simple arithmetic expressions
 *       #
 *       exp: minus exp :prec: uminus
 *          neg($2)
 *
 *       exp: exp plus exp
 *          add($1, $3)
 *
 *       exp: exp minus exp
 *          sub($1, $3)
 *
 *       exp: exp mul exp
 *          mul($1, $3)
 *
 *       exp: exp div exp
 *          div($1, $3)
 *
 *       exp: lparen exp rparen
 *          return $2
 * \end{verbatim}
 *
 * Parsers are defined as extensions of the \verb+Parser+ class.
 * A \verb+Parser+ object must have a \verb+lexer+ field.  The \verb+lexer+
 * is not required to be a \verb+Lexer+ object, but it must provide
 * a \verb+lexer.lex()+ method that returns a token object with
 * \verb+name+ and \verb+value+ fields.  For this example, we use the
 * \verb+lexer1+ object that we defined previously.
 *
 * The next step is to define precedences for the terminal symbols.
 * The precedences are defined with the \verb+left+, \verb+right+,
 * and \verb+nonassoc+ methods in order of increasing precedence.
 *
 * The grammar must have at least one start symbol, declared with
 * the \verb+start+ method.
 *
 * Next, the productions in the grammar are listed as rules.
 * The name of the production is listed before the colon, and
 * a sequence of variables is listed to the right of the colon.
 * The body is a semantic action to be evaluated when the production
 * is recognized as part of the input.
 *
 * In this example, these are the productions for the arithmetic
 * expressions recognized by the desktop calculator.  The semantic
 * action performs the calculation.  The variables \verb+$1, $2, ...+
 * correspond to the values associated with each of the variables
 * on the right-hand-side of the production.
 *
 * \subsection{Calling the parser}
 *
 * The parser is called with the \verb+$(parser1.parse-channel start, channel)+
 * or \verb+$(parser1.parse-file start, file)+ functions.  The \verb+start+
 * argument is the start symbol, and the \verb+channel+ or \verb+file+
 * is the input to the parser.
 *
 * \subsection{Parsing control}
 *
 * The parser generator generates a pushdown automation based on LALR(1)
 * tables.  As usual, if the grammar is ambiguous, this may generate shift/reduce
 * or reduce/reduce conflicts.  These conflicts are printed to standard
 * output when the automaton is generated.
 *
 * By default, the automaton is not constructed until the parser is
 * first used.
 *
 * The \verb+build(debug)+ method forces the construction of the automaton.
 * While not required, it is wise to finish each complete parser with
 * a call to the \verb+build(debug)+ method.  If the \verb+debug+ variable
 * is set, this also prints with parser table together with any conflicts.
 *
 * The \verb+loc+ variable is defined within action bodies, and represents
 * the input range for all tokens on the right-hand-side of the production.
 *
 * \subsection{Extending parsers}
 *
 * Parsers may also be extended by inheritance.
 * For example, let's extend the grammar so that it also recognizes
 * the \verb+<<+ and \verb+>>+ shift operations.
 *
 * First, we extend the lexer so that it recognizes these tokens.
 * This time, we choose to leave \verb+lexer1+ intact, instead of
 * using the += operator.
 *
 * \begin{verbatim}
 *    lexer2. =
 *       extends $(lexer1)
 *
 *       lsl: $"<<"
 *          Token.unit($(loc), lsl)
 *
 *       asr: $">>"
 *          Token.unit($(loc), asr)
 * \end{verbatim}
 *
 * Next, we extend the parser to handle these new operators.
 * We intend that the bitwise operators have lower precedence
 * than the other arithmetic operators.  The two-argument form
 * of the \verb+left+ method accomplishes this.
 *
 * \begin{verbatim}
 *    parser2. =
 *       extends $(parser1)
 *
 *       left(plus, lsl lsr asr)
 *
 *       lexer = $(lexer2)
 *
 *       exp: exp lsl exp
 *          lsl($1, $3)
 *
 *       exp: exp asr exp
 *          asr($1, $3)
 * \end{verbatim}
 *
 * In this case, we use the new lexer \verb+lexer2+, and we add productions
 * for the new shift operations.
 * \end{doc}
 *)

(*
 * Add start symbols.
 *)
let parse_start venv pos loc args kargs =
   let pos = string_pos "parse-start" pos in
   let parse = current_parser venv pos in
   let args =
      match args, kargs with
         [arg], [] ->
            strings_of_value venv pos arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let parse =
      List.fold_left (fun parse s ->
            Parser.add_start parse (Lm_symbol.add s)) parse args
   in

   (* Redefine the parser *)
   let venv = venv_add_var venv builtin_field_var (ValOther (ValParser parse)) in
      venv, ValNone

(*
 * Precedence operations.
 *)
let parse_prec venv pos loc args kargs assoc =
   let pos = string_pos "parse-prec" pos in
   let this = venv_this venv in
   let parse = current_parser venv pos in
   let parse, level, args =
      match args, kargs with
         [before; args], [] ->
            let current_prec = Lm_symbol.add (string_of_value venv pos before) in
            let level =
               try Parser.find_prec parse current_prec with
                  Not_found ->
                     raise (OmakeException (loc_pos loc pos, StringVarError ("no such precedence", current_prec)))
            in
            let parse, level = Parser.create_prec_lt parse level assoc in
               parse, level, args
       | [args], [] ->
            let current_prec = Lm_symbol.add (string_of_value venv pos (venv_find_field_internal this pos current_prec_sym)) in
            let level =
               try Parser.find_prec parse current_prec with
                  Not_found ->
                     raise (OmakeException (loc_pos loc pos, StringVarError ("current precedence is not found", current_prec)))
            in
            let parse, level = Parser.create_prec_gt parse level assoc in
               parse, level, args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))
   in
   let args = strings_of_value venv pos args in
   let parse =
      List.fold_left (fun parse s ->
            Parser.add_prec parse level (Lm_symbol.add s)) parse args
   in

   (* Reset the current precedence *)
   let venv =
      match args with
         arg :: _ ->
            venv_add_var venv current_prec_field_var (ValString arg)
       | [] ->
            venv
   in

   (* Redefine the parser *)
   let venv = venv_add_var venv builtin_field_var (ValOther (ValParser parse)) in
      venv, ValNone

let parse_left venv pos loc args kargs =
   let pos = string_pos "parse-left" pos in
      parse_prec venv pos loc args kargs LeftAssoc

let parse_right venv pos loc args kargs =
   let pos = string_pos "parse-right" pos in
      parse_prec venv pos loc args kargs RightAssoc

let parse_nonassoc venv pos loc args kargs =
   let pos = string_pos "parse-nonassoc" pos in
      parse_prec venv pos loc args kargs NonAssoc

(*
 * Build the parser.
 *)
let parse_build venv pos loc args =
   let pos = string_pos "parse-build" pos in
      match args with
         [arg] ->
            let par = current_parser venv pos in
            let debug = bool_of_value venv pos arg in
               Parser.build par debug;
               ValNone
       | _ ->
          raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Get the precedence option.
 *)
let prec_option venv pos loc options =
   venv_map_fold (fun pre optname optval ->
         let s = string_of_value venv pos optname in
            if s = ":prec:" then
               Some (Lm_symbol.add (string_of_value venv pos optval))
            else
               raise (OmakeException (pos, StringValueError ("illegal option", optname)))) None options

(*
 * Compute an action name that is not defined in the current object.
 *)
let action_sym = Lm_symbol.add "action"

let find_action_name venv loc =
   Lm_symbol.new_name action_sym (fun v -> venv_defined venv (VarThis (loc, v)))

(*
 * Add a parser clause.
 *)
let parse_rule venv pos loc args kargs =
   let pos = string_pos "parse-rule" pos in
   let action, head, rhs, options, body, export =
      match args, kargs with
         [_; action; head; rhs; ValMap options; ValBody (body, export)], [] ->
            let action = string_of_value venv pos action in
            let head = string_of_value venv pos head in
               if head = "" then   (* Action name was omitted *)
                  find_action_name venv loc, Lm_symbol.add action, rhs, options, body, export
               else
                  Lm_symbol.add action, Lm_symbol.add head, rhs, options, body, export
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 6, List.length args)))
   in
   let par = current_parser venv pos in
   let rhs = List.map Lm_symbol.add (strings_of_value venv pos rhs) in
   let pre = prec_option venv pos loc options in
   let par = Parser.add_production par action head rhs pre in

   (* Add the method if there is a body *)
   let venv =
      match body with
         _ :: _ ->
            let body =
               LetVarExp (loc, VarThis (loc, val_sym), [], VarDefNormal, ConstString (loc, "")) :: body
            in
               venv_add_var venv (VarThis (loc, action)) (ValFun (venv_get_env venv, [], [], body, export))
       | [] ->
            venv
   in

   (* Add back the parser *)
   let venv = venv_add_var venv builtin_field_var (ValOther (ValParser par)) in
      venv, ValNone

(*
 * Perform the lexing.
 *)
let parse_engine venv pos loc args =
   let pos = string_pos "parse-engine" pos in
      match args with
         [start] ->
            let dfa = current_parser venv pos in
            let start = Lm_symbol.add (string_of_value venv pos start) in
            let lexer = venv_find_var venv pos loc lexer_field_var in
            let lexer = eval_object venv pos lexer in
            let parser_obj = venv_this venv in
            let lex (venv, parser_obj, lexer) =
               let lex = venv_find_field_internal lexer pos lex_sym in
               let venv = venv_with_object venv lexer in
               let venv, result = eval_apply venv pos loc lex [] [] in
               let obj = eval_object venv pos result in
                  try
                     let lex_loc = venv_find_field_internal_exn obj loc_sym in
                     let lex_loc = loc_of_value venv pos lex_loc in
                     let name = venv_find_field_internal_exn obj name_sym in
                     let name = Lm_symbol.add (string_of_value venv pos name) in
                     let value = venv_find_field_internal_exn obj val_sym in
                        name, lex_loc, (venv, parser_obj, lexer), value
                  with
                     Not_found ->
                        let print_error buf =
                           fprintf buf "@[<v 3>The lexer returned a malformed object.\
@ @[<v 3>The result of a lexer action should be an object with at least 3 fields:\
@ loc: the location of the token\
@ name: the name of the token\
@ val: the value of the token@]\
@ %a@]" pp_print_value (ValObject obj)
                        in
                           raise (OmakeException (pos, LazyError print_error))
            in
            let eval (venv, parser_obj, lexer) action loc args =
               let pos = loc_pos loc pos in
               let venv = venv_add_match_values venv args in
               let action = venv_find_field_internal parser_obj pos action in
               let venv = venv_with_object venv parser_obj in
               let venv = venv_add_var venv parse_loc_var (ValOther (ValLocation loc)) in
               let venv, result = eval_apply venv pos loc action [] [] in
                  (venv, parser_obj, lexer), result
            in
            let _, value =
               try Parser.parse dfa start lex eval (venv, parser_obj, lexer) with
                  Failure _ as exn ->
                     raise (UncaughtException (pos, exn))
                | Lm_parser.ParseError (loc, s) ->
                     raise (OmakeException (loc_pos loc pos, StringError s))
            in
               value
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(************************************************************************
 * External interface.
 *)
let () =
   let builtin_funs =
      [true, "grep",                  grep,                 ArityRange (1, 3);
       true, "builtin-grep",          builtin_grep,         ArityExact 1;
       true, "cat",                   cat,                  ArityExact 1;
       true, "parse-engine",          parse_engine,         ArityExact 1;
       true, "parse-build",           parse_build,          ArityExact 1;
      ]
   in
   let builtin_kfuns =
      [true, "lex-rule",              lex_rule,             ArityRange (3, 4);
       true, "lex-engine",            lex_engine,           ArityExact 1;
       true, "parse-rule",            parse_rule,           ArityRange (3, 5);
       true, "parse-start",           parse_start,          ArityExact 1;
       true, "parse-left",            parse_left,           ArityExact 1;
       true, "parse-right",           parse_right,          ArityExact 1;
       true, "parse-nonassoc",        parse_nonassoc,       ArityExact 1;
       true, "scan",                  scan,                 ArityRange (1, 3);
       true, "awk",                   awk,                  ArityExact 3;
       true, "fsubst",                fsubst,               ArityExact 3;
       true, "lex",                   lex,                  ArityExact 3;
       true, "lex-search",            lex_search,           ArityExact 3;
      ]
   in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs;
                           builtin_kfuns = builtin_kfuns
      }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
