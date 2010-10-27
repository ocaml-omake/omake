(*
 * Secondary lexer for expressions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2007 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURLOCE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_ast
open Omake_pos
open Omake_symbol
open Omake_ast_util
open Omake_ast_print
open Omake_exp_parse
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_shell_lex" end);;
open Pos;;

(************************************************************************
 * Types.
 *)

(*
 * Token buffer.
 *)
type lexinfo =
   { mutable lex_exp_list : Omake_ast.exp list;
     mutable lex_loc      : Lm_location.loc
   }

let create_lexinfo loc tokens =
   { lex_exp_list = tokens;
     lex_loc      = loc
   }

(************************************************************************
 * Utilities.
 *)

(*
 * Locations.
 *)
let shell_sym = Lm_symbol.add "shell"

let syntax_error s loc =
   raise (OmakeException (loc_exp_pos loc, SyntaxError s))

(************************************************************************
 * Lexing.
 *)

(*
 * Various operators.
 *)
let lex_op s loc =
   match s with
      "(" -> TokLeftParen loc
    | ")" -> TokRightParen loc
    | "," -> TokComma loc
    | ";" -> TokSemi loc
    | "=" -> TokEq loc
    | "." -> TokDot loc
    | "+" -> TokPlus loc
    | "-" -> TokMinus loc
    | "*" -> TokStar loc
    | "/" -> TokSlash loc
    | "<" -> TokLt loc
    | ">" -> TokGt loc
    | "^" -> TokHat loc
    | "&" -> TokAmp loc
    | "|" -> TokPipe loc
    | "::" -> TokColonColon loc
    | "<<" -> TokLsl loc
    | ">>" -> TokAsr loc
    | ">>>" -> TokLsr loc
    | "&&" -> TokAnd loc
    | "||" -> TokOr loc
    | "=>" -> TokArrow loc
    | _ ->
         syntax_error ("unexpected operator: " ^ s) loc

(*
 * Some identifier are operators.
 *)
let lex_id s loc =
   try TokInt (int_of_string s, loc) with
      Failure _ ->
         try TokFloat (float_of_string s, loc) with
            Failure _ ->
               match s with
                  "" -> raise (Invalid_argument "Omake_exp_lex.lex_id")
                | "-" -> TokMinus loc
                | "[" -> TokLeftBrack loc
                | "]" -> TokRightBrack loc
                | _ ->
                     match s.[0] with
                        '~' | '?' -> TokKey (Lm_symbol.add (String.sub s 1 (String.length s - 1)), loc)
                      | _ -> TokId (Lm_symbol.add s, loc)

(*
 * Translate an expression to a token.
 *)
let rec lex_tok lexinfo e =
   match e with
      IntExp (i, loc) ->
         TokInt (i, loc)
    | FloatExp (x, loc) ->
         TokFloat (x, loc)
    | StringOpExp (s, loc) ->
         lex_op s loc
    | StringIdExp (s, loc) ->
         lex_id s loc
    | StringIntExp (s, loc) ->
         TokInt (int_of_string s, loc)
    | StringFloatExp (s, loc) ->
         TokFloat (float_of_string s, loc)
    | StringKeywordExp (s, loc) ->
         TokId (Lm_symbol.add s, loc)
    | StringWhiteExp _ ->
         lex_main lexinfo
    | StringOtherExp (s, loc) ->
         syntax_error s loc
    | SequenceExp (el, _) ->
         lexinfo.lex_exp_list <- el @ lexinfo.lex_exp_list;
         lex_main lexinfo
    | NullExp _
    | QuoteExp _
    | QuoteStringExp _
    | ArrayExp _
    | ApplyExp _
    | SuperApplyExp _
    | MethodApplyExp _
    | CommandExp _
    | VarDefExp _
    | VarDefBodyExp _
    | ObjectDefExp _
    | FunDefExp _
    | RuleExp _
    | BodyExp _
    | ShellExp _
    | CatchExp _
    | ClassExp _
    | KeyExp _
    | KeyDefExp _
    | KeyDefBodyExp _ ->
         TokExp e


and lex_main lexinfo =
   match lexinfo.lex_exp_list with
      [] ->
         TokEof
    | e :: el ->
         lexinfo.lex_loc <- loc_of_exp e;
         lexinfo.lex_exp_list <- el;
         lex_tok lexinfo e

(*
 * Ignore the lexbuf.
 *)
let lex_main lexinfo _lexbuf =
   lex_main lexinfo

(*
 * Lexer from a token list.
 *)
let lexbuf = Lexing.from_string "dummy lexbuf"

let parse loc tokens =
   let lexinfo = create_lexinfo loc tokens in
      try Omake_exp_parse.ast_exp (lex_main lexinfo) lexbuf with
         Parsing.Parse_error ->
            syntax_error "parse error" lexinfo.lex_loc

(************************************************************************
 * Translation.
 *)

type mode =
   ProgramMode
 | NormalMode

let apply_mode mode = function
   CommandApply ->
      mode
 | NormalApply
 | EagerApply
 | LazyApply ->
      NormalMode

let languages = "legal languages are (program, make); you said"

let language_mode loc pattern source =
   let pos = string_pos "language_mode" (loc_exp_pos loc) in
      if SymbolTable.cardinal source <> 1 || not (SymbolTable.mem source normal_sym) then
         raise (OmakeException (pos, StringError "illegal language"));
      match SymbolTable.find source normal_sym with
         StringIdExp (s, _) ->
            (match s with
                "program" -> ProgramMode
              | "make" -> NormalMode
              | _ -> raise (OmakeException (pos, StringStringError (languages, s))))
       | e ->
            raise (OmakeException (pos, StringAstError (languages, e)))

(*
 * Perform the ast->ast translation.
 *)
let rec translate_exp mode e =
   match e with
      (* This is not an identifier *)
      NullExp _
    | IntExp _
    | FloatExp _
    | QuoteExp _
    | QuoteStringExp _
    | ClassExp _
    | KeyExp _ ->
         e

      (* Single-token processing *)
    | StringOpExp (_, loc)
    | StringIdExp (_, loc)
    | StringIntExp (_, loc)
    | StringWhiteExp (_, loc)
    | StringFloatExp (_, loc)
    | StringOtherExp (_, loc)
    | StringKeywordExp (_, loc) ->
         (match mode with
             ProgramMode ->
                translate_exp mode (parse loc [e])
           | NormalMode ->
                e)

      (* Sequences *)
    | SequenceExp (el, loc) ->
         (match mode with
             ProgramMode ->
                translate_exp mode (parse loc el)
           | NormalMode ->
                SequenceExp (translate_exp_list mode el, loc))

    | ArrayExp (el, loc) ->
         ArrayExp (translate_exp_list mode el, loc)
    | ApplyExp (strategy, v, args, loc) ->
         ApplyExp (strategy, v, translate_arg_list (apply_mode mode strategy) args, loc)
    | SuperApplyExp (strategy, v1, v2, args, loc) ->
         SuperApplyExp (strategy, v1, v2, translate_arg_list (apply_mode mode strategy) args, loc)
    | MethodApplyExp (strategy, vl, args, loc) ->
         MethodApplyExp (strategy, vl, translate_arg_list (apply_mode mode strategy) args, loc)
    | CommandExp (v, e, el, loc) ->
         CommandExp (v, translate_exp mode e, translate_body mode el, loc)
    | VarDefExp (vl, kind, flag, e, loc) ->
         VarDefExp (vl, kind, flag, translate_exp mode e, loc)
    | VarDefBodyExp (vl, kind, flag, el, loc) ->
         VarDefBodyExp (vl, kind, flag, translate_body mode el, loc)
    | ObjectDefExp (vl, flag, el, loc) ->
         ObjectDefExp (vl, flag, translate_body mode el, loc)
    | FunDefExp (vl, params, el, loc) ->
         FunDefExp (vl, translate_param_list mode params, translate_body mode el, loc)
    | RuleExp (multiple, target, pattern, options, body, loc) ->
         RuleExp (multiple,
                  translate_exp NormalMode target,
                  translate_exp NormalMode pattern,
                  translate_table_exp NormalMode options,
                  translate_exp_list mode body,
                  loc)
    | BodyExp (el, loc) ->
         BodyExp (translate_body mode el, loc)
    | CatchExp (v1, v2, el, loc) ->
         CatchExp (v1, v2, translate_body mode el, loc)
    | KeyDefExp (s, kind, flag, e, loc) ->
         KeyDefExp (s, kind, flag, translate_exp mode e, loc)
    | KeyDefBodyExp (s, kind, flag, el, loc) ->
         KeyDefBodyExp (s, kind, flag, translate_body mode el, loc)
    | ShellExp (e, loc) ->
         ShellExp (translate_exp NormalMode e, loc)

and translate_exp_list mode el =
   List.map (translate_exp mode) el

(* make-style applications are always in NormalMode *)
and translate_arg mode = function
   KeyArg (v, e) ->
      KeyArg (v, translate_exp mode e)
 | ExpArg e ->
      ExpArg (translate_exp mode e)
 | ArrowArg (params, e) ->
      ArrowArg (translate_param_list mode params, translate_exp mode e)

and translate_arg_list mode args =
   List.map (translate_arg mode) args

and translate_param mode = function
   OptionalParam (v, e, loc) ->
      OptionalParam (v, translate_exp mode e, loc)
 | RequiredParam _
 | NormalParam _ as param ->
      param

and translate_param_list mode params =
   List.map (translate_param mode) params

and translate_table_exp mode table =
   SymbolTable.map (translate_exp mode) table

and translate_body mode el =
   match el with
      [] ->
         []
    | e :: el ->
         match e with
            (* JYH: this kind of matching is very fragile *)
            RuleExp (_, SequenceExp ([StringOpExp (".", _); StringIdExp ("LANGUAGE", _)], _), pattern, source, body, loc) ->
               let new_mode = language_mode loc pattern source in
                  (match body with
                      [] ->
                         translate_body new_mode el
                    | _ :: _ ->
                         CommandExp (section_sym, e, translate_body new_mode body, loc) :: translate_body mode el)
          | _ ->
               translate_exp mode e :: translate_body mode el

(************************************************************************
 * Main function.
 *)
let compile_prog el =
   let el = flatten_sequence_prog el in
   let el = translate_body NormalMode el in
   let el = flatten_string_prog el in
      el

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
