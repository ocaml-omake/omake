(*
 * The type of values.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006-2010 Mojave Group, Caltech and HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_ir
open Omake_node
open Omake_wild
open Omake_lexer
open Omake_parser
open Omake_node_sig
open Omake_ir_free_vars
open Omake_handle_table

(* %%MAGICBEGIN%% *)
(*
 * Various kinds of handles.
 *)
type handle_env = HandleTable.handle

(*
 * I/O channels.
 *)
type channel_mode = Lm_channel.mode =
   InChannel
 | OutChannel
 | InOutChannel

type prim_channel = IntHandleTable.handle

(*
 * Possible values.
 * Someday we may want to include rules and functions.
 * For the function, the obj is the static scope.
 *)
type value =
   ValNone
 | ValInt         of int
 | ValFloat       of float
 | ValSequence    of value list
 | ValArray       of value list
 | ValWhite       of string
 | ValString      of string
 | ValData        of string
 | ValQuote       of value list
 | ValQuoteString of char * value list
 | ValRules       of Node.t list
 | ValNode        of Node.t
 | ValDir         of Dir.t
 | ValObject      of obj
 | ValMap         of map
 | ValChannel     of channel_mode * prim_channel
 | ValClass       of obj SymbolTable.t

   (* Raw expressions *)
 | ValStringExp   of env * string_exp
 | ValBody        of exp list * export
 | ValCases       of (var * value * exp list * export) list

   (* Functions *)
 | ValFun         of env * keyword_param_value list * param list * exp list * export
 | ValFunCurry    of env * param_value list * keyword_param_value list * param list * exp list * export * keyword_value list

   (* Closed values *)
 | ValPrim        of arity * bool * apply_empty_strategy * prim_fun

   (* The args, kargs are kept in -reverse- order *)
 | ValPrimCurry   of arity * bool * prim_fun * value list * keyword_value list

   (* Implicit value dependencies *)
 | ValMaybeApply  of loc * var_info

   (* Variables that are not applications *)
 | ValVar         of loc * var_info

   (* Other values *)
 | ValOther       of value_other

   (* Delayed values *)
 | ValDelayed     of value_delayed ref

(*
 * Put all the other stuff here, to keep the primary value type
 * smaller.
 *)
and value_other =
   ValLexer       of Lexer.t
 | ValParser      of Parser.t
 | ValLocation    of loc
 | ValExitCode    of int
 | ValEnv         of handle_env * export

and value_delayed =
   ValValue of value

   (* Value in a static block *)
 | ValStaticApply of value * var

(*
 * Arguments have an optional keyword.
 *)
and param_value = param * value
and keyword_value = var * value
and keyword_param_value = var * param * value option

(*
 * Primitives are option refs.
 * We do this so that we can marshal these values.
 * Just before marshaling, all the options are set to None.
 *)
and prim_fun = symbol

(*
 * An object is just an environment.
 *)
and obj = value SymbolTable.t
and env = value SymbolTable.t
and map = (value, value) Lm_map.tree
(* %%MAGICEND%% *)

(************************************************************************
 * Non-marshaled values.
 *)

(*
 * A method path.
 *)
type path =
   PathVar   of var_info
 | PathField of path * obj * var

(*
 * Command lists are used for rule bodies.
 * They have their environment, a list of sources,
 * and the actual body.  The body is polymorphic
 * for various kinds of commands.
 *)
type command =
   CommandSection of value * free_vars * exp list   (* Name of the section, its free variables, and the expression *)
 | CommandValue of loc * env * string_exp

(*
 * Kinds of rules.
 *)
type rule_multiple =
   RuleSingle
 | RuleMultiple
 | RuleScannerSingle
 | RuleScannerMultiple

type rule_kind =
   RuleNormal
 | RuleScanner

(*
 * A target value that represents a node in a rule.
 *)
type target =
   TargetNode of Node.t
 | TargetString of string

(*
 * A source is either
 *   1. A wild string
 *   2. A node
 *   3. An optional source
 *   4. A squashed source
 *)
type source_core =
   SourceWild of wild_out_patt
 | SourceNode of Node.t

type 'a source = node_kind * 'a

(************************************************************************
 * Exceptions.
 *)
type item =
   Symbol        of symbol
 | String        of string
 | AstExp        of Omake_ast.exp
 | IrExp         of Omake_ir.exp
 | Location      of loc
 | Value         of value
 | Error         of omake_error

and pos = item Lm_position.pos

and omake_error =
   SyntaxError        of string
 | StringError        of string
 | StringAstError     of string * Omake_ast.exp
 | StringStringError  of string * string
 | StringDirError     of string * Dir.t
 | StringNodeError    of string * Node.t
 | StringVarError     of string * var
 | StringIntError     of string * int
 | StringMethodError  of string * var list
 | StringValueError   of string * value
 | StringTargetError  of string * target
 | LazyError          of (formatter -> unit)
 | UnboundVar         of var
 | UnboundVarInfo     of var_info
 | UnboundFun         of var
 | UnboundMethod      of var list
 | UnboundFieldVar    of obj * var
 | ArityMismatch      of arity * int
 | NotImplemented     of string
 | UnboundKey         of string
 | UnboundValue       of value
 | NullCommand

(*
 * Standard exceptions.
 *)
exception OmakeException    of pos * omake_error
exception UncaughtException of pos * exn
exception RaiseException    of pos * obj
exception ExitException     of pos * int
exception ExitParentException     of pos * int
exception Return            of loc * value * return_id

(*
 * Omake's internal version of the Invalid_argument
 *)
exception OmakeFatal of string
exception OmakeFatalErr of pos * omake_error

(*
 * Position printer.
 *)
module type PosSig =
sig
   val loc_exp_pos    : loc -> pos
   val loc_pos        : loc -> pos -> pos

   val ast_exp_pos    : Omake_ast.exp -> pos
   val ir_exp_pos     : Omake_ir.exp -> pos
   val var_exp_pos    : var -> pos
   val string_exp_pos : string -> pos
   val value_exp_pos  : value -> pos

   val string_pos     : string -> pos -> pos
   val pos_pos        : pos -> pos -> pos
   val int_pos        : int -> pos -> pos
   val var_pos        : var -> pos -> pos
   val error_pos      : omake_error -> pos -> pos

   val del_pos        : (formatter -> unit) -> loc -> pos
   val del_exp_pos    : (formatter -> unit) -> pos -> pos

   (* Utilities *)
   val loc_of_pos     : pos -> loc
   val pp_print_pos   : formatter -> pos -> unit
end

(************************************************************************
 * Basic values and functions.
 *)

(*
 * Empty object.
 *)
let empty_obj = SymbolTable.empty

(*
 * Get the class identifiers from the object.
 *)
let class_sym = Lm_symbol.add "$class"

let venv_get_class obj =
   match
      try SymbolTable.find obj class_sym with
         Not_found ->
            ValNone
   with
      ValClass table ->
         table
    | _ ->
         SymbolTable.empty

(************************************************************************
 * Value table.
 *)
module ValueCompare =
struct
   type t = value

   (*
    * Check for simple values.
    * Arrays cannot be nested.
    *)
   let check_simple pos v =
      match v with
         ValNone
       | ValInt _
       | ValFloat _
       | ValData _
       | ValNode _
       | ValDir _
       | ValOther (ValLocation _)
       | ValOther (ValExitCode _)
       | ValVar _ ->
            ()
       | _ ->
            raise (OmakeException (pos, StringValueError ("illegal Map key", v)))

   let check pos v =
      (match v with
          ValArray vl ->
             List.iter (check_simple pos) vl
        | _ ->
             check_simple pos v);
      v

   (*
    * Compare two simple values.
    *)
   let tag = function
      ValNone                  -> 0
    | ValInt _                 -> 1
    | ValFloat _               -> 2
    | ValArray _               -> 3
    | ValData _                -> 4
    | ValNode _                -> 5
    | ValDir _                 -> 6
    | ValOther (ValExitCode _) -> 7
    | ValOther (ValLocation _) -> 8
    | ValVar _                 -> 9
    | _ ->
         raise (Invalid_argument "ValueCompare: value not supported")

   let rec compare v1 v2 =
      match v1, v2 with
         ValNone, ValNone ->
            0
       | ValInt i1, ValInt i2
       | ValOther (ValExitCode i1), ValOther (ValExitCode i2) ->
            if i1 < i2 then
               -1
            else if i1 > i2 then
               1
            else
               0
       | ValFloat x1, ValFloat x2 ->
            if x1 < x2 then
               -1
            else if x1 > x2 then
               1
            else
               0
       | ValArray a1, ValArray a2 ->
            compare_list a1 a2
       | ValData s1, ValData s2 ->
            Pervasives.compare s1 s2
       | ValNode node1, ValNode node2 ->
            Node.compare node1 node2
       | ValDir dir1, ValDir dir2 ->
            Dir.compare dir1 dir2
       | ValOther (ValLocation loc1), ValOther (ValLocation loc2) ->
            Lm_location.compare loc1 loc2
       | ValVar (_, v1), ValVar (_, v2) ->
            VarInfoCompare.compare v1 v2
       | _ ->
            tag v1 - tag v2

   and compare_list l1 l2 =
      match l1, l2 with
         v1 :: l1, v2 :: l2 ->
            let cmp = compare v1 v2 in
               if cmp = 0 then
                  compare_list l1 l2
               else
                  cmp
       | [], [] ->
            0
       | [], _ :: _ ->
            -1
       | _ :: _, [] ->
            1
end;;

module ValueTable = Lm_map.LmMakeRec (ValueCompare);;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
