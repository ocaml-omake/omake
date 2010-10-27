(*
 * General utilities on the AST.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
open Lm_symbol
open Lm_location

open Omake_ast

let loc_of_exp = function
   NullExp loc
 | IntExp (_, loc)
 | FloatExp (_, loc)
 | StringOpExp (_, loc)
 | StringIdExp (_, loc)
 | StringIntExp (_, loc)
 | StringFloatExp (_, loc)
 | StringWhiteExp (_, loc)
 | StringOtherExp (_, loc)
 | StringKeywordExp (_, loc)
 | QuoteExp (_, loc)
 | QuoteStringExp (_, _, loc)
 | SequenceExp (_, loc)
 | ArrayExp (_, loc)
 | ApplyExp (_, _, _, loc)
 | SuperApplyExp (_, _, _, _, loc)
 | MethodApplyExp (_, _, _, loc)
 | CommandExp (_, _, _, loc)
 | VarDefExp (_, _, _, _, loc)
 | VarDefBodyExp (_, _, _, _, loc)
 | KeyExp (_, _, loc)
 | KeyDefExp (_, _, _, _, loc)
 | KeyDefBodyExp (_, _, _, _, loc)
 | FunDefExp (_, _, _, loc)
 | ObjectDefExp (_, _, _, loc)
 | RuleExp (_, _, _, _, _, loc)
 | BodyExp (_, loc)
 | ShellExp (_, loc)
 | CatchExp (_, _, _, loc)
 | ClassExp (_, loc) ->
      loc

(*
 * Get a key word that describes the expression.
 *)
let rec last vl =
   match vl with
      [v] ->
         v
    | _ :: vl ->
         last vl
    | [] ->
         raise (Invalid_argument "last")

let key_of_exp = function
   NullExp _ ->
     "null"
 | IntExp _
 | FloatExp _
 | StringOpExp _
 | StringIdExp _
 | StringIntExp _
 | StringFloatExp _
 | StringWhiteExp _
 | StringOtherExp _
 | StringKeywordExp _
 | QuoteExp _
 | QuoteStringExp _
 | SequenceExp _
 | ArrayExp _ ->
     "string"
 | ApplyExp (_, v, _, _)
 | CommandExp (v, _, _, _)
 | SuperApplyExp (_, v, _, _, _) ->
      Lm_symbol.to_string v
 | VarDefExp (vl, _, _, _, _)
 | VarDefBodyExp (vl, _, _, _, _)
 | ObjectDefExp (vl, _, _, _)
 | FunDefExp (vl, _, _, _)
 | MethodApplyExp (_, vl, _, _) ->
      Lm_symbol.to_string (last vl)
 | KeyExp _
 | KeyDefExp _
 | KeyDefBodyExp _ ->
      "key"
 | RuleExp _ ->
      "rule"
 | BodyExp _ ->
      "body"
 | ShellExp _ ->
      "shell"
 | CatchExp _ ->
      "catch"
 | ClassExp _ ->
      "class"

(*
 * In an argument list, each ... is replaced by the body.
 * If there is no elision, then the body is added as the
 * first argument.
 *)
let is_elide_exp = function
   StringOpExp ("...", _)
 | StringOpExp ("[...]", _) ->
      true
 | _ ->
      false

let add_elide_code loc code1 code2 =
   match code1, code2 with
      NoBody, code
    | code, NoBody ->
         code
    | OptBody, code
    | code, OptBody ->
         code
    | _ ->
         if code1 = code2 then
            code1
         else
            raise (Invalid_argument "conflicting elisions")

let scan_elide_args code args =
   List.fold_left (fun code arg ->
         let arg =
            match arg with
               KeyArg (_, e)
             | ExpArg e
             | ArrowArg (_, e) ->
                  Some e
         in
            match arg with
               Some (StringOpExp ("...", loc)) ->
                  add_elide_code loc code ColonBody
             | Some (StringOpExp ("[...]", loc)) ->
                  add_elide_code loc code ArrayBody
             | _ ->
                  code) code args

let scan_body_flag code e =
   match e with
      ApplyExp (_, _, args, _)
    | SuperApplyExp (_, _, _, args, _)
    | MethodApplyExp (_, _, args, _) ->
         scan_elide_args code args
    | _ ->
         code

let update_body_args loc code body args =
   let body =
      match code with
         NoBody
       | OptBody
       | ColonBody ->
            BodyExp (body, loc)
       | ArrayBody ->
            ArrayExp (body, loc)
   in
   let rev_args, found =
      List.fold_left (fun (args, found) arg ->
            let arg, found =
               match arg with
                  KeyArg (v, e) ->
                     if is_elide_exp e then
                        KeyArg (v, body), true
                     else
                        arg, found
                | ExpArg e ->
                     if is_elide_exp e then
                        ExpArg body, true
                     else
                        arg, found
                | ArrowArg (params, e) ->
                     if is_elide_exp e then
                        ArrowArg (params, body), true
                     else
                        arg, found
            in
               arg :: args, found) ([], false) args
   in
   let args = List.rev rev_args in
      if found then
         args
      else
         ExpArg body :: args

(*
 * In an argument list, each ... is replaced by the body.
 * If there is no elision, then the body is added as the
 * first argument.
 *)
let update_body_exp e code body =
   match e with
      NullExp _
    | IntExp _
    | FloatExp _
    | StringOpExp _
    | StringIdExp _
    | StringIntExp _
    | StringFloatExp _
    | StringWhiteExp _
    | StringOtherExp _
    | StringKeywordExp _
    | QuoteExp _
    | QuoteStringExp _
    | SequenceExp _
    | ArrayExp _
    | VarDefExp _
    | KeyExp _
    | KeyDefExp _
    | BodyExp _
    | ShellExp _
    | ClassExp _ ->
         raise (Invalid_argument "update_body")
    | ApplyExp (strategy, v, args, loc) ->
         ApplyExp (strategy, v, update_body_args loc code body args, loc)
    | SuperApplyExp (strategy, super, v, args, loc) ->
         SuperApplyExp (strategy, super, v, update_body_args loc code body args, loc)
    | MethodApplyExp (strategy, vl, args, loc) ->
         MethodApplyExp (strategy, vl, update_body_args loc code body args, loc)
    | CommandExp (v, e, _, loc) ->
         CommandExp (v, e, body, loc)
    | VarDefBodyExp (v, kind, flag, _, loc) ->
         VarDefBodyExp (v, kind, flag, body, loc)
    | KeyDefBodyExp (v, kind, flag, _, loc) ->
         KeyDefBodyExp (v, kind, flag, body, loc)
    | ObjectDefExp (v, flag, _, loc) ->
         ObjectDefExp (v, flag, body, loc)
    | FunDefExp (v, params, _, loc) ->
         FunDefExp (v, params, body, loc)
    | RuleExp (flag, target, pattern, sources, _, loc) ->
         RuleExp (flag, target, pattern, sources, body, loc)
    | CatchExp (name, v, _, loc) ->
         CatchExp (name, v, body, loc)

let update_body e code body =
   match code, body with
      NoBody, []
    | OptBody, []
    | ColonBody, [] ->
         e
    | ArrayBody, _
    | _, _ :: _ ->
         update_body_exp e code body

(*
 * Indicate whether the command may have remaining parts.
 *)
let continue_commands =
   ["if",        "else";
    "elseif",    "else";
    "switch",    "case";
    "match",     "case";
    "lexer",     "case";
    "case",      "case";
    "default",   "case";
    "try",       "catch";
    "catch",     "catch"]

let continue_syms =
   List.fold_left (fun set (s1, s2) ->
         SymbolTable.add set (Lm_symbol.add s1) s2) SymbolTable.empty continue_commands

let can_continue e =
   match e with
      NullExp _
    | IntExp _
    | FloatExp _
    | StringIdExp _
    | StringOpExp _
    | StringIntExp _
    | StringFloatExp _
    | StringWhiteExp _
    | StringOtherExp _
    | StringKeywordExp _
    | QuoteExp _
    | QuoteStringExp _
    | SequenceExp _
    | ArrayExp _
    | ApplyExp _
    | SuperApplyExp _
    | MethodApplyExp _
    | VarDefExp _
    | VarDefBodyExp _
    | KeyExp _
    | KeyDefExp _
    | KeyDefBodyExp _
    | ObjectDefExp _
    | FunDefExp _
    | RuleExp _
    | BodyExp _
    | ShellExp _
    | ClassExp _ ->
         None
    | CatchExp _ ->
         Some "catch"
    | CommandExp (v, _, _, _) ->
         try Some (SymbolTable.find continue_syms v) with
            Not_found ->
               None

(************************************************************************
 * Sequence flattening.
 *)
let rec flatten_exp e =
   match e with
      NullExp _
    | IntExp _
    | FloatExp _
    | ClassExp _
    | KeyExp _
    | StringOpExp _
    | StringIdExp _
    | StringIntExp _
    | StringWhiteExp _
    | StringFloatExp _
    | StringOtherExp _
    | StringKeywordExp _ ->
         e

      (* Sequences *)
    | QuoteExp (el, loc) ->
         QuoteExp (flatten_body el, loc)
    | QuoteStringExp (c, el, loc) ->
         QuoteStringExp (c, flatten_body el, loc)
    | SequenceExp (el, loc) ->
         SequenceExp (flatten_body el, loc)

      (* Descend into the terms *)
    | ArrayExp (el, loc) ->
         ArrayExp (flatten_exp_list el, loc)
    | ApplyExp (strategy, v, args, loc) ->
         ApplyExp (strategy, v, flatten_arg_list args, loc)
    | SuperApplyExp (strategy, v1, v2, args, loc) ->
         SuperApplyExp (strategy, v1, v2, flatten_arg_list args, loc)
    | MethodApplyExp (strategy, vl, args, loc) ->
         MethodApplyExp (strategy, vl, flatten_arg_list args, loc)
    | CommandExp (v, e, el, loc) ->
         CommandExp (v, flatten_exp e, flatten_body el, loc)
    | VarDefExp (vl, kind, flag, e, loc) ->
         VarDefExp (vl, kind, flag, flatten_exp e, loc)
    | VarDefBodyExp (vl, kind, flag, el, loc) ->
         VarDefBodyExp (vl, kind, flag, flatten_body_kind kind el, loc)
    | ObjectDefExp (vl, flag, el, loc) ->
         ObjectDefExp (vl, flag, flatten_body el, loc)
    | FunDefExp (vl, params, el, loc) ->
         FunDefExp (vl, flatten_param_list params, flatten_body el, loc)
    | RuleExp (multiple, target, pattern, options, body, loc) ->
         RuleExp (multiple,
                  flatten_exp target,
                  flatten_exp pattern,
                  flatten_table_exp options,
                  flatten_body body,
                  loc)
    | BodyExp (el, loc) ->
         BodyExp (flatten_body el, loc)
    | CatchExp (v1, v2, el, loc) ->
         CatchExp (v1, v2, flatten_body el, loc)
    | KeyDefExp (s, kind, flag, e, loc) ->
         KeyDefExp (s, kind, flag, flatten_exp e, loc)
    | KeyDefBodyExp (s, kind, flag, el, loc) ->
         KeyDefBodyExp (s, kind, flag, flatten_body_kind kind el, loc)
    | ShellExp (e, loc) ->
         ShellExp (flatten_exp e, loc)

and flatten_exp_list el =
   List.map flatten_exp el

and flatten_arg = function
   KeyArg (v, e) ->
      KeyArg (v, flatten_exp e)
 | ExpArg e ->
      ExpArg (flatten_exp e)
 | ArrowArg (params, e) ->
      ArrowArg (flatten_param_list params, flatten_exp e)

and flatten_arg_list args =
   List.map flatten_arg args

and flatten_param = function
   OptionalParam (v, e, loc) ->
      OptionalParam (v, flatten_exp e, loc)
 | RequiredParam _
 | NormalParam _ as param ->
      param

and flatten_param_list params =
   List.map flatten_param params

and flatten_table_exp table =
   SymbolTable.map flatten_exp table

and flatten_body_kind kind el =
   match kind with
      DefineString ->
         flatten_body el
    | DefineArray ->
         flatten_exp_list el

and flatten_body el =
   flatten_body_aux [] el []

and flatten_body_aux items el ell =
   match el, ell with
      [], [] ->
         List.rev items
    | [], el :: ell ->
         flatten_body_aux items el ell
    | e :: el, _ ->
         match e with
            SequenceExp (el2, loc) ->
               flatten_body_aux items el2 (el :: ell)
          | NullExp _ ->
               flatten_body_aux items el ell
          | _ ->
               let items = flatten_exp e :: items in
                  flatten_body_aux items el ell

let flatten_sequence_prog = flatten_body

(************************************************************************
 * String flattening.
 *)
let rec string_exp e =
   match e with
      NullExp _
    | IntExp _
    | FloatExp _
    | ClassExp _
    | KeyExp _
    | StringWhiteExp _ ->
         e
    | StringOpExp (s, loc)
    | StringIdExp (s, loc)
    | StringIntExp (s, loc)
    | StringFloatExp (s, loc)
    | StringOtherExp (s, loc)
    | StringKeywordExp (s, loc) ->
         StringOtherExp (s, loc)

      (* Sequences *)
    | QuoteExp (el, loc) ->
         QuoteExp (flatten_string_list_exp (string_exp_list el), loc)
    | QuoteStringExp (c, el, loc) ->
         QuoteStringExp (c, flatten_string_list_exp (string_exp_list el), loc)
    | SequenceExp (el, loc) ->
         string_sequence_exp (string_exp_list el) loc

      (* Descend into the terms *)
    | ArrayExp (el, loc) ->
         ArrayExp (string_exp_list el, loc)
    | ApplyExp (strategy, v, args, loc) ->
         ApplyExp (strategy, v, string_arg_list args, loc)
    | SuperApplyExp (strategy, v1, v2, args, loc) ->
         SuperApplyExp (strategy, v1, v2, string_arg_list args, loc)
    | MethodApplyExp (strategy, vl, args, loc) ->
         MethodApplyExp (strategy, vl, string_arg_list args, loc)
    | CommandExp (v, e, el, loc) ->
         CommandExp (v, string_exp e, string_body el, loc)
    | VarDefExp (vl, kind, flag, e, loc) ->
         VarDefExp (vl, kind, flag, string_exp e, loc)
    | VarDefBodyExp (vl, kind, flag, el, loc) ->
         VarDefBodyExp (vl, kind, flag, string_body el, loc)
    | ObjectDefExp (vl, flag, el, loc) ->
         ObjectDefExp (vl, flag, string_body el, loc)
    | FunDefExp (vl, params, el, loc) ->
         FunDefExp (vl, string_param_list params, string_body el, loc)
    | RuleExp (multiple, target, pattern, options, body, loc) ->
         RuleExp (multiple,
                  string_exp target,
                  string_exp pattern,
                  string_table_exp options,
                  string_body body,
                  loc)
    | BodyExp (el, loc) ->
         BodyExp (string_body el, loc)
    | CatchExp (v1, v2, el, loc) ->
         CatchExp (v1, v2, string_body el, loc)
    | KeyDefExp (s, kind, flag, e, loc) ->
         KeyDefExp (s, kind, flag, string_exp e, loc)
    | KeyDefBodyExp (s, kind, flag, el, loc) ->
         KeyDefBodyExp (s, kind, flag, string_body el, loc)
    | ShellExp (e, loc) ->
         ShellExp (string_exp e, loc)

and string_exp_list el =
   List.map string_exp el

and string_body el =
   string_exp_list el

and string_arg = function
   KeyArg (v, e) ->
      KeyArg (v, string_exp e)
 | ExpArg e ->
      ExpArg (string_exp e)
 | ArrowArg (params, e) ->
      ArrowArg (string_param_list params, string_exp e)

and string_arg_list args =
   List.map string_arg args

and string_param = function
   OptionalParam (v, e, loc) ->
      OptionalParam (v, string_exp e, loc)
 | RequiredParam _
 | NormalParam _ as param ->
      param

and string_param_list params =
   List.map string_param params

and string_table_exp table =
   SymbolTable.map string_exp table

and string_sequence_exp el loc =
   match flatten_string_list_exp el with
      [] ->
         NullExp loc
    | [e] ->
         e
    | el ->
         SequenceExp (el, loc)

and flatten_string_list_exp el =
   let buf = Buffer.create 32 in

   (* Flush the buffer *)
   let flush_buffer buf_opt args =
      match buf_opt with
         Some loc ->
            let args = StringOtherExp (Buffer.contents buf, loc) :: args in
               Buffer.clear buf;
               args
       | None ->
            args
   in

   (* Add a constant string to the buffer *)
   let add_string buf_opt s loc =
      Buffer.add_string buf s;
      match buf_opt with
         Some loc' ->
            let loc = union_loc loc' loc in
               Some loc
       | None ->
            Some loc
   in

   (* Collect all the strings in the sequence *)
   let rec collect buf_opt args el ell =
      match el, ell with
         [], [] ->
            List.rev (flush_buffer buf_opt args)
       | [], el :: ell ->
            collect buf_opt args el ell
       | e :: el, ell ->
            match e with
               NullExp _ ->
                  collect buf_opt args el ell
             | StringOpExp (s, loc)
             | StringIdExp (s, loc)
             | StringIntExp (s, loc)
             | StringFloatExp (s, loc)
             | StringOtherExp (s, loc)
             | StringKeywordExp (s, loc) ->
                  let buf_opt = add_string buf_opt s loc in
                     collect buf_opt args el ell
             | SequenceExp (el2, loc) ->
                  collect buf_opt args el2 (el :: ell)
             | StringWhiteExp _
             | IntExp _
             | FloatExp _
             | KeyExp _
             | ClassExp _
             | QuoteStringExp _
             | QuoteExp _
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
             | CatchExp _
             | KeyDefExp _
             | KeyDefBodyExp _
             | ShellExp _ ->
                  let args = flush_buffer buf_opt args in
                     collect None (e :: args) el ell
   in
      collect None [] el []

let flatten_string_prog = string_exp_list

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
