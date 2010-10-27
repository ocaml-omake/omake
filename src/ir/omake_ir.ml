(*
 * Define an intermediate representation that is a little
 * easier to work with than the AST.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Jason Hickey, California Institute of Technology
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
open Lm_location
open Lm_symbol

open Omake_node_sig
open Omake_node

(*
 * %%MAGICBEGIN%%
 * Last manual IR versioning: 12/09/07 by Aleksey Nogin
 *)
type var = symbol
type keyword = symbol
type curry_flag = bool

(*
 * Whether a function of zero arguments should be applied.
 *)
type apply_empty_strategy =
   ApplyEmpty
 | ApplyNonEmpty

(*
 * Arity of functions.
 *)
type arity =
   ArityRange of int * int
 | ArityExact of int
 | ArityNone
 | ArityAny

(*
 * Kinds of matches.
 *)
type match_kind =
   MatchWild
 | MatchRegex

(*
 * Variable definitions have several forms.
 *    VarDefNormal: normal definition
 *    VarDefAppend: append the text
 *)
type var_def_kind =
   VarDefNormal
 | VarDefAppend

(*
 * Simple version of variables includes the kind of
 * scope, the location, and the variable name.
 *)
type var_info =
   VarPrivate        of loc * var
 | VarThis           of loc * var
 | VarVirtual        of loc * var
 | VarGlobal         of loc * var

type param = var_info

(*
 * A symbol table maps variables to their info.
 *)
type senv = var_info SymbolTable.t

(*
 * Exporting.
 *)
type export_item =
   ExportRules
 | ExportPhonies
 | ExportVar of var_info

type export =
   ExportNone
 | ExportAll
 | ExportList of export_item list

(*
 * A return identifier is a unique id for the function to return from.
 * NOTE: this is a unique string, compared with pointer equality.
 *)
type return_id = loc * string

(*
 * Expression that results in a string.
 *
 * Functions: a function takes a triple:
 *    keyword_param list : the optional parameters
 *    keyword_set : the set of keywords (for checking against the keyword arguments)
 *    param list : the names of the required parameters
 *
 * The ordering of keyword arguments in the source is irrelevant.
 * Internally, we sort them by symbol name, for easy checking.
 *)
type string_exp =
   NoneString        of loc
 | IntString         of loc * int
 | FloatString       of loc * float
 | WhiteString       of loc * string
 | ConstString       of loc * string
 | FunString         of loc * keyword_param list * param list * exp list * export
 | ApplyString       of loc * var_info * string_exp list * keyword_arg list
 | SuperApplyString  of loc * var * var * string_exp list * keyword_arg list
 | MethodApplyString of loc * var_info * var list * string_exp list * keyword_arg list
 | SequenceString    of loc * string_exp list
 | ArrayString       of loc * string_exp list
 | ArrayOfString     of loc * string_exp
 | QuoteString       of loc * string_exp list
 | QuoteStringString of loc * char * string_exp list
 | ObjectString      of loc * exp list * export
 | BodyString        of loc * exp list * export
 | ExpString         of loc * exp list * export
 | CasesString       of loc * (var * string_exp * exp list * export) list
 | KeyApplyString    of loc * string
 | VarString         of loc * var_info
 | ThisString        of loc
 | LazyString        of loc * string_exp
 | LetVarString      of loc * var_info * string_exp * string_exp

and source_exp = node_kind * string_exp

and source_table = string_exp SymbolTable.t

(*
 * Optional function arguments.
 *)
and keyword_param = var * param * string_exp option

(*
 * Arguments are a pair of normal arguments and keyword arguments.
 *)
and keyword_arg = var * string_exp

(*
 * Commands.
 *)
and rule_command =
   RuleSection of string_exp * exp
 | RuleString of string_exp

and exp =
   (* Definitions *)
   LetVarExp        of loc * var_info * var list * var_def_kind * string_exp
 | LetFunExp        of loc * var_info * var list * curry_flag * keyword_param list * param list * exp list * export
 | LetObjectExp     of loc * var_info * var list * string_exp * exp list * export
 | LetThisExp       of loc * string_exp
 | LetKeyExp        of loc * string * var_def_kind * string_exp

   (* Applications *)
 | ApplyExp         of loc * var_info * string_exp list * keyword_arg list
 | SuperApplyExp    of loc * var * var * string_exp list * keyword_arg list
 | MethodApplyExp   of loc * var_info * var list * string_exp list * keyword_arg list
 | KeyExp           of loc * string

   (* Sequences *)
 | SequenceExp      of loc * exp list
 | SectionExp       of loc * string_exp * exp list * export

   (* StaticExp (loc, filename, id, el) *)
 | StaticExp        of loc * Node.t * symbol * exp list

   (* Conditional *)
 | IfExp            of loc * (string_exp * exp list * export) list

   (* Shell command *)
 | ShellExp         of loc * string_exp

   (*
    * StringExp (loc, s)
    *    This is just an identity, evaluating to s
    * ReturnExp (loc, s)
    *    This is a control operation, branching to the innermost ReturnBodyExp
    * ReturnBodyExp (loc, e)
    *    Return to here.
    *)
 | StringExp        of loc * string_exp
 | ReturnExp        of loc * string_exp * return_id
 | ReturnBodyExp    of loc * exp list * return_id

   (*
    * LetOpenExp (loc, v, id, file, link)
    *    id    : the current object
    *    file  : name of the file/object to open
    *    link  : link information for the rest of the variables in scope.
    *)
 | OpenExp          of loc * Node.t list
 | IncludeExp       of loc * string_exp * string_exp list

   (* Return the current object *)
 | ReturnObjectExp  of loc * symbol list
 | ReturnSaveExp    of loc

(*
 * The IR stored in a file.
 *    ir_classnames   : class names of the file
 *    ir_vars         : variables defined by this file
 *    ir_exp          : the expression
 *)
type ir =
   { ir_classnames   : symbol list;
     ir_vars         : senv;
     ir_exp          : exp
   }
(* %%MAGICEND%% *)

(*
 * Variable classes.
 *    private: variables local to the file, statically scoped.
 *    this: object fields, dynamically scoped.
 *    virtual: file fields, dynamically scoped.
 *    global: search each of the scopes in order (ZZZ: 0.9.8 only)
 *)
type var_scope =
   VarScopePrivate
 | VarScopeThis
 | VarScopeVirtual
 | VarScopeGlobal

(************************************************************************
 * Simplified variables.
 *)
type simple_var_info = var_scope * var

module SimpleVarCompare =
struct
   type t = simple_var_info

   let compare (s1, v1) (s2, v2) =
      match s1, s2 with
         VarScopePrivate, VarScopePrivate
       | VarScopeThis, VarScopeThis
       | VarScopeVirtual, VarScopeVirtual
       | VarScopeGlobal, VarScopeGlobal ->
            Lm_symbol.compare v1 v2
       | VarScopePrivate, VarScopeThis
       | VarScopePrivate, VarScopeVirtual
       | VarScopePrivate, VarScopeGlobal
       | VarScopeThis, VarScopeVirtual
       | VarScopeThis, VarScopeGlobal
       | VarScopeVirtual, VarScopeGlobal ->
            -1
       | VarScopeThis, VarScopePrivate
       | VarScopeVirtual, VarScopePrivate
       | VarScopeVirtual, VarScopeThis
       | VarScopeGlobal, VarScopePrivate
       | VarScopeGlobal, VarScopeThis
       | VarScopeGlobal, VarScopeVirtual ->
            1
end;;

module SimpleVarSet = Lm_set.LmMake (SimpleVarCompare);;
module SimpleVarTable = Lm_map.LmMake (SimpleVarCompare);;

(************************************************************************
 * Variable tables.  The const_flag and protected_flag are just
 * comments, and aren't part of the comparison.
 *)
module VarInfoCompare =
struct
   type t = var_info

   let compare info1 info2 =
      match info1, info2 with
         VarPrivate   (_, v1), VarPrivate   (_, v2)
       | VarThis (_, v1),      VarThis (_, v2)
       | VarVirtual (_, v1),   VarVirtual (_, v2)
       | VarGlobal (_, v1),    VarGlobal (_, v2) ->
            Lm_symbol.compare v1 v2
       | VarPrivate _,        VarThis _
       | VarPrivate _,        VarVirtual _
       | VarPrivate _,        VarGlobal _
       | VarThis _,           VarVirtual _
       | VarThis _,           VarGlobal _
       | VarVirtual _,        VarGlobal _ ->
            -1
       | VarThis _,           VarPrivate _
       | VarVirtual _,        VarPrivate _
       | VarVirtual _,        VarThis _
       | VarGlobal _,         VarPrivate _
       | VarGlobal _,         VarThis _
       | VarGlobal _,         VarVirtual _ ->
            1
end;;

module VarInfoSet = Lm_set.LmMake (VarInfoCompare);;
module VarInfoTable = Lm_map.LmMake (VarInfoCompare);;

let var_equal v1 v2 =
   VarInfoCompare.compare v1 v2 = 0

let var_of_var_info = function
   VarPrivate (loc, v)
 | VarThis (loc, v)
 | VarVirtual (loc, v)
 | VarGlobal (loc, v) ->
      loc, v

(************************************************************************
 * Path definitions.
 *)
type name_info =
   { name_static     : bool;
     name_curry      : bool;
     name_scope      : var_scope option
   }

type method_name =
   NameEmpty   of name_info
 | NameMethod  of name_info * var * var list

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
