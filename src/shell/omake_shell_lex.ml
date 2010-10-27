(*
 * A token-preserving lexer for the shell.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006-2007 Mojave Group, Caltech
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
open Lm_printf

open Omake_env
open Omake_pos
open Omake_eval
open Omake_value_type
open Omake_shell_type
open Omake_shell_parse
open Omake_value_print
open Omake_command_type

module Pos = MakePos (struct let name = "Omake_shell_lex" end);;
open Pos;;

(************************************************************************
 * Lexing.
 *)

(*
 * Locations.
 *)
let shell_sym = Lm_symbol.add "shell"

let syntax_error s loc =
   raise (OmakeException (loc_exp_pos loc, SyntaxError s))

(*
 * For debugging, try to print the first token.
 *)
let rec pp_print_token buf tok =
   match tok with
      Omake_env.TokString v  ->
         pp_print_value buf v
    | Omake_env.TokToken s  ->
         pp_print_string buf s
    | Omake_env.TokGroup toks ->
         fprintf buf "(%a)" pp_print_token_list toks

and pp_print_token_list buf toks =
   match toks with
      [tok] ->
         pp_print_token buf tok
    | tok :: toks ->
         pp_print_token buf tok;
         pp_print_char buf ' ';
         pp_print_token_list buf toks
    | [] ->
         ()

(*
 * Tokenizer.
 *)
let flatten_items items loc =
   let rec collect tokens vl vll =
      match vl, vll with
         v :: vl, _ ->
            (match v with
                Omake_env.TokGroup vl' ->
                   collect tokens vl' (vl :: vll)
              | Omake_env.TokString v ->
                   collect (v :: tokens) vl vll
              | Omake_env.TokToken s ->
                   syntax_error ("illegal token: " ^ s) loc)
       | [], vl :: vll ->
            collect tokens vl vll
       | [], [] ->
            List.rev tokens
   in
      collect [] items []

let lex_tok token loc =
   match token with
      Omake_env.TokToken s ->
         (match s with
             "<"  -> TokLessThan (s, loc)
           | ">"  -> TokGreaterThan (s, loc)
           | ">>" -> TokGreaterGreaterThan (s, loc)
           | "&"  -> TokAmp (s, loc)
           | ";"  -> TokSemiColon (s, loc)
           | "&&" -> TokAnd (s, loc)
           | "|"  -> TokPipe (s, loc)
           | "||" -> TokOr (s, loc)
           | "("  -> TokLeftParen (s, loc)
           | ")"  -> TokRightParen (s, loc)
           | _    -> syntax_error ("illegal operator: " ^ s) loc)
    | Omake_env.TokGroup items ->
         TokValues (flatten_items items loc, loc)
    | Omake_env.TokString v ->
         TokValues ([v], loc)

(*
 * Token buffer.
 *)
type lexinfo =
   { mutable lex_tokens : Omake_env.tok list;
     mutable lex_pos    : int;
     lex_loc            : Lm_location.loc
   }

let create_lexinfo loc tokens =
   { lex_tokens = tokens;
     lex_pos    = 1;
     lex_loc    = loc
   }

let lex_main lexinfo _lexbuf =
   let { lex_tokens = tokens;
         lex_pos    = pos;
         lex_loc    = loc
       } = lexinfo
   in
      match tokens with
         [] ->
            TokEof loc
       | token :: tokens ->
            let tok = lex_tok token loc in
               lexinfo.lex_tokens <- tokens;
               lexinfo.lex_pos <- pos + 1;
               tok

(*
 * Lexer from a token list.
 *)
let lexbuf = Lexing.from_string "dummy lexbuf"

let parse loc tokens =
   let lexinfo = create_lexinfo loc tokens in
      try Omake_shell_parse.prog (lex_main lexinfo) lexbuf with
         Parsing.Parse_error ->
            syntax_error "parse error" loc

(************************************************************************
 * Token lexer.
 *)
let check_next c s off len =
   let off = succ off in
      if off < len && s.[off] = c then
         2
      else
         1

let lexer s off len =
   match s.[off] with
      '<'
    | '('
    | ')'
    | ';' ->
         Some 1
    | '&' ->
         Some (check_next '&' s off len)
    | '|' ->
         Some (check_next '|' s off len)
    | '>' ->
         Some (check_next '>' s off len)
    | _ ->
         None

(************************************************************************
 * Strip the initial flags.
 *)
let collect_flags toks =
   let rec collect_flags flags toks =
      if false then
         eprintf "Command: %a@." pp_print_token_list toks;
      match toks with
         tok :: toks' ->
            (match tok with
                TokString (ValString s) ->
                   let len = String.length s in
                   let rec scan flags i =
                      if i = len then
                         collect_flags flags toks'
                      else
                         match s.[i] with
                            '@'  -> scan (QuietFlag :: flags) (succ i)
                          | '-'  -> scan (AllowFailureFlag :: flags) (succ i)
                          | '+'  -> scan flags (succ i)
                          | _    ->
                               let toks =
                                  if succ i = len then
                                     toks'
                                  else
                                     TokString (ValString (String.sub s i (len - i))) :: toks'
                               in
                                  flags, toks
                   in
                      scan flags 0
              | _ ->
                   flags, toks)
       | [] ->
            flags, toks
   in
      collect_flags [] toks

(************************************************************************
 * Command-line parsing.
 *)
let rec flatten_value v =
   match v with
      ValArray [v]
    | ValSequence [v] ->
         flatten_value v
    | v ->
         v

let flatten_values vl =
   match vl with
      [v] ->
         [flatten_value v]
    | _ ->
         vl

let arg_of_redirect venv pos v =
   match v with
      RedirectArg v ->
         (match flatten_values v with
             [ValNode node] ->
                RedirectNode node
           | v ->
                RedirectArg (arg_of_values venv pos v))
    | RedirectNode _
    | RedirectNone as v ->
         v

(*
 * When parsing the command line, collect all environment definitions.
 *)
let scan_define arg =
   match arg with
      ArgString s :: args ->
         (try
             let i = String.index s '=' in
             let v = Lm_symbol.add (String.sub s 0 i) in
             let i = succ i in
             let len = String.length s in
             let args =
                if i = len then
                   args
                else
                   ArgString (String.sub s i (len - i)) :: args
             in
                Some (v, args)
          with
             Not_found ->
                None)
    | _ ->
         None

(*
 * For a command, scan forward, collecting the env.
 *)
let rec scan_argv venv pos env argv =
   match argv with
      arg :: argv ->
         (match flatten_values arg with
             [ValNode node] ->
               env, CmdNode node, argv
           | v ->
                let arg = arg_of_values venv pos v in
                   (match scan_define arg with
                       Some (v, s) ->
                          scan_argv venv pos ((v, s) :: env) argv
                     | None ->
                          env, CmdArg arg, argv))
    | [] ->
         raise (OmakeException (pos, NullCommand))

(*
 * A pipe might actually refer to an alias.
 *)
let pre_pipe_command venv find_alias options pos info =
   let { cmd_loc     = loc;
         cmd_argv    = argv;
         cmd_stdin   = stdin;
         cmd_stdout  = stdout;
         cmd_stderr  = stderr;
         cmd_append  = append
       } = info
   in
   let stdin = arg_of_redirect venv pos stdin in
   let stdout = arg_of_redirect venv pos stdout in

   (* Scan the argument list *)
   let env, exe, argv = scan_argv venv pos [] argv in
   let env = List.rev env in

   (* Detect whether this is an alias *)
   let f =
      match exe with
         CmdNode _ ->
            None
       | CmdArg arg ->
            if is_quoted_arg arg || is_glob_arg options arg then
               None
            else
               let s = simple_string_of_arg arg in
                  find_alias venv pos loc s
   in
      match f with
         Some (name, f) ->
            (* This is an alias *)
            let apply =
               { apply_loc = loc;
                 apply_env = env;
                 apply_name = name;
                 apply_fun = f;
                 apply_args = List.map (fun vl -> ValSequence vl) argv;
                 apply_stdin = stdin;
                 apply_stdout = stdout;
                 apply_stderr = stderr;
                 apply_append = append
               }
            in
               PipeApply (loc, apply)
       | None ->
            (* This is a normal command *)
            let command =
               { info with cmd_env = env;
                           cmd_exe = exe;
                           cmd_argv = argv_of_values venv pos argv;
                           cmd_stdin = stdin;
                           cmd_stdout = stdout
               }
            in
               PipeCommand (loc, command)

(*
 * The parser never produces aliases,
 * so this code is dead.
 *)
let pre_pipe_apply venv pos info =
   let { apply_env    = env;
         apply_args   = args;
         apply_stdin  = stdin;
         apply_stdout = stdout
       } = info
   in
      { info with apply_env = List.map (fun (x, v) -> x, arg_of_values venv pos v) env;
                  apply_args = List.map (fun vl -> ValSequence vl) args;
                  apply_stdin = arg_of_redirect venv pos stdin;
                  apply_stdout = arg_of_redirect venv pos stdout
      }

(*
 * Parse all the components of the pipe.
 *)
let rec pre_pipe venv find_alias options pos pipe =
   match pipe with
      PipeApply (loc, info) ->
         PipeApply (loc, pre_pipe_apply venv pos info)
    | PipeCommand (_, info) ->
         pre_pipe_command venv find_alias options pos info
    | PipeCond (loc, op, pipe1, pipe2) ->
         PipeCond (loc, op, pre_pipe venv find_alias options pos pipe1, pre_pipe venv find_alias options pos pipe2)
    | PipeCompose (loc, b, pipe1, pipe2) ->
         PipeCompose (loc, b, pre_pipe venv find_alias options pos pipe1, pre_pipe venv find_alias options pos pipe2)
    | PipeGroup (loc, info) ->
         PipeGroup (loc, pre_pipe_group venv find_alias options pos info)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, pre_pipe venv find_alias options pos pipe)

and pre_pipe_group venv find_alias options pos info =
   let { group_stdin   = stdin;
         group_stdout  = stdout;
         group_pipe    = pipe
       } = info
   in
      { info with group_stdin  = arg_of_redirect venv pos stdin;
                  group_stdout = arg_of_redirect venv pos stdout;
                  group_pipe   = pre_pipe venv find_alias options pos pipe
      }

(*
 * Do the whole command-line parsing process.
 *)
let pipe_of_value venv find_alias options pos loc v =
   let pos = string_pos "pipe_of_value" pos in
   let argv = tokens_of_value venv pos lexer v in
   let flags, argv = collect_flags argv in
   let pipe = parse loc argv in
   let pipe = pre_pipe venv find_alias options pos pipe in
      flags, pipe

(*
 * Commands with a leading \ are quoted.
 *)
let parse_command_string s =
   let len = String.length s in
      if len <> 0 && s.[0] = '\\' then
         ExeQuote (String.sub s 1 (pred len))
      else
         ExeString s

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
