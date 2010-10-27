(*
 * Compute the digest of a value.  This works the naive way:
 *    1. Convert the value to a string
 *    2. Compute its MD5 digest
 * This can be fairly expensive if the value is big.  The
 * current implementation is designed so that we can at least
 * compress the string a bit.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2010 Mojave Group, California Insitute of Technology and
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
 * Modified by: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol
open Lm_string_set
open Lm_hash_sig
open Lm_hash

open Omake_ir
open Omake_env
open Omake_pos
open Omake_node
open Omake_shell_type
open Omake_value_type
open Omake_value_print
open Omake_command_type

(*
 * Codes.
 *)
(* %%MAGICBEGIN%% *)
type code =
   CodeApplyExp
 | CodeApplyString
 | CodeFunString
 | CodeArgv
 | CodeArrayOfString
 | CodeArrayString
 | CodeArrow
 | CodeBegin
 | CodeObjectString
 | CodeBodyString
 | CodeCase
 | CodeCaseExp
 | CodeCases
 | CodeCasesExp
 | CodeCasesString
 | CodeCaseString
 | CodeCommaExp
 | CodeCommand
 | CodeCommands
 | CodeNoneString
 | CodeIntString
 | CodeFloatString
 | CodeWhiteString
 | CodeConstString
 | CodeVarString
 | CodeEnd
 | CodeExpString
 | CodeIfExp
 | CodeOpenExp
 | CodeIncludeExp
 | CodeLetFunExp
 | CodeLetObjectExp
 | CodeLetThisExp
 | CodeLetVarExp
 | CodeMethodApplyExp
 | CodeMethodApplyString
 | CodeQuoteString
 | CodeQuoteStringString
 | CodeExportExp
 | CodeExportNone
 | CodeExportAll
 | CodeExportList
 | CodeExportRules
 | CodeExportPhonies
 | CodeExportVar
 | CodeCancelExportExp
 | CodeReturnBodyExp
 | CodeStringExp
 | CodeReturnExp
 | CodeReturnObjectExp
 | CodeReturnSaveExp
 | CodeVarScopePrivate
 | CodeVarScopeThis
 | CodeVarScopeVirtual
 | CodeVarScopeGlobal
 | CodeVarPrivate
 | CodeVarThis
 | CodeVarVirtual
 | CodeVarGlobal
 | CodeSectionExp
 | CodeSequenceExp
 | CodeSequenceString
 | CodeShellExp
 | CodeStaticExp
 | CodeSpace
 | CodeSuperApplyExp
 | CodeSuperApplyString
 | CodeThisString
 | CodeValArray
 | CodeValStringExp
 | CodeValBody
 | CodeValData
 | CodeValDir
 | CodeValFloat
 | CodeValFun
 | CodeValFunCurry
 | CodeValInt
 | CodeValMap
 | CodeValNode
 | CodeValNone
 | CodeValObject
 | CodeValPrim
 | CodeValPrimCurry
 | CodeValQuote
 | CodeValQuoteString
 | CodeValSequence
 | CodeValWhite
 | CodeValString
 | CodeValVar
 | CodeValMaybeApply
 | CodeVarDefApply
 | CodeVarDefNormal
 | CodeLetKeyExp
 | CodeKeyApplyString
 | CodeKeyExp
 | CodeRequiredParam
 | CodeOptionalParam
 | CodeValStaticApply
 | CodeArg
 | CodeArgString
 | CodeArgData
 | CodeArgNone
 | CodePipeAnd
 | CodePipeOr
 | CodePipeSequence
 | CodePipeCommand
 | CodePipeApply
 | CodePipeCond
 | CodePipeCompose
 | CodePipeGroup
 | CodePipeBackground
 | CodeCommandEnvItem
 | CodeCommandEnv
 | CodeTrue
 | CodeFalse
 | CodeQuietFlag
 | CodeAllowFailureFlag
 | CodeAllowOutputFlag
 | CodeCommandFlags
 | CodeCmdArg
 | CodeCmdNode
 | CodePipe
 | CodeRedirectNode
 | CodeRedirectArg
 | CodeRedirectNone
 | CodeKeywordSpec
 | CodeKeywordSpecList
 | CodeNone
 | CodeSome
 | CodeLazyString
 | CodeLetVarString
(* %%MAGICEND%% *)

module type HashSig =
sig
   include Lm_hash_sig.HashDigestSig

   val add_code   : t -> code -> unit
end;;

(* %%MAGICBEGIN%% *)
module Hash : HashSig =
struct
   include Lm_hash.HashDigest

   (*
    * Add a code.
    *)
   let add_code buf (code : code) =
      add_bits buf (Obj.magic code)

end;;
(* %%MAGICEND%% *)

(*
 * Variable squashing.
 *)
let squash_var buf v =
   Hash.add_string buf (string_of_symbol v)

let rec squash_vars buf vars =
   match vars with
      [v] ->
         squash_var buf v
    | v :: vars ->
         squash_var buf v;
         Hash.add_code buf CodeSpace;
         squash_vars buf vars
    | [] ->
         ()

let squash_var_set buf vars =
   SymbolSet.iter (fun v ->
         Hash.add_code buf CodeSpace;
         squash_var buf v) vars

let squash_var_info buf v =
   match v with
      VarPrivate (_, v) ->
         Hash.add_code buf CodeVarPrivate;
         Hash.add_code buf CodeSpace;
         squash_var buf v
    | VarThis (_, v) ->
         Hash.add_code buf CodeVarThis;
         Hash.add_code buf CodeSpace;
         squash_var buf v
    | VarVirtual (_, v) ->
         Hash.add_code buf CodeVarVirtual;
         Hash.add_code buf CodeSpace;
         squash_var buf v
    | VarGlobal (_, v) ->
         Hash.add_code buf CodeVarGlobal;
         Hash.add_code buf CodeSpace;
         squash_var buf v

let rec squash_var_info_list buf vars =
   match vars with
      [v] ->
         squash_var_info buf v
    | v :: vars ->
         squash_var_info buf v;
         Hash.add_code buf CodeSpace;
         squash_var_info_list buf vars
    | [] ->
         ()

let squash_params = squash_var_info_list

let squash_keyword_spec buf (v, required) =
   Hash.add_code buf CodeKeywordSpec;
   squash_var buf v;
   Hash.add_bool buf required

let squash_keyword_spec_list buf keywords =
   Hash.add_code buf CodeKeywordSpecList;
   List.iter (squash_keyword_spec buf) keywords

(*
 * File.
 *)
let squash_node buf node =
   Hash.add_string buf (Node.absname node)

(*
 * String representations.
 *)
let squash_var_scope buf scope =
   let code =
      match scope with
         VarScopePrivate -> CodeVarScopePrivate
       | VarScopeThis    -> CodeVarScopeThis
       | VarScopeVirtual -> CodeVarScopeVirtual
       | VarScopeGlobal  -> CodeVarScopeGlobal
   in
      Hash.add_code buf code

let squash_def_kind buf kind =
   let s =
      match kind with
         VarDefNormal ->
            CodeVarDefNormal
       | VarDefAppend ->
            CodeVarDefApply
   in
      Hash.add_code buf s

(*
 * Export info.
 *)
let squash_export_info buf info =
   match info with
      Omake_ir.ExportNone ->
         Hash.add_code buf CodeExportNone
    | Omake_ir.ExportAll ->
         Hash.add_code buf CodeExportAll
    | Omake_ir.ExportList items ->
         Hash.add_code buf CodeExportList;
         List.iter (fun item ->
               match item with
                  Omake_ir.ExportRules ->
                     Hash.add_code buf CodeExportRules
                | Omake_ir.ExportPhonies ->
                     Hash.add_code buf CodeExportPhonies
                | Omake_ir.ExportVar v ->
                     Hash.add_code buf CodeExportVar;
                     squash_var_info buf v) items

(*
 * Just squash the string part of the return is.
 *)
let squash_return_id buf (_, s) =
   Hash.add_string buf s

(*
 * Squash string expressions.
 *)
let rec squash_string_exp pos buf e =
   Hash.add_code buf CodeBegin;
   begin
      match e with
         NoneString _ ->
            Hash.add_code buf CodeNoneString
       | IntString (_, i) ->
            Hash.add_code buf CodeIntString;
            Hash.add_int buf i
       | FloatString (_, x) ->
            Hash.add_code buf CodeFloatString;
            Hash.add_float buf x
       | WhiteString (_, s) ->
            Hash.add_code buf CodeWhiteString;
            Hash.add_string buf s
       | ConstString (_, s) ->
            Hash.add_code buf CodeConstString;
            Hash.add_string buf s
       | VarString (_, v) ->
            Hash.add_code buf CodeVarString;
            squash_var_info buf v
       | KeyApplyString (_, s) ->
            Hash.add_code buf CodeKeyApplyString;
            Hash.add_string buf s
       | FunString (_, opt_params, params, s, export) ->
            Hash.add_code buf CodeFunString;
            squash_params buf params;
            Hash.add_code buf CodeArrow;
            squash_keyword_param_list pos buf opt_params;
            Hash.add_code buf CodeArrow;
            squash_exp_list pos buf s;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | ApplyString (_, v, args, kargs) ->
            Hash.add_code buf CodeApplyString;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | SuperApplyString (_, v1, v2, args, kargs) ->
            Hash.add_code buf CodeSuperApplyString;
            squash_var buf v1;
            Hash.add_code buf CodeSpace;
            squash_var buf v2;
            Hash.add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | MethodApplyString (_, v, vars, args, kargs) ->
            Hash.add_code buf CodeMethodApplyString;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_vars buf vars;
            Hash.add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | SequenceString (_, sl) ->
            Hash.add_code buf CodeSequenceString;
            squash_string_exp_list pos buf sl
       | ArrayString (_, sl) ->
            Hash.add_code buf CodeArrayString;
            squash_string_exp_list pos buf sl
       | ArrayOfString (_, s) ->
            Hash.add_code buf CodeArrayOfString;
            squash_string_exp pos buf s
       | QuoteString (_, sl) ->
            Hash.add_code buf CodeQuoteString;
            squash_string_exp_list pos buf sl
       | QuoteStringString (_, c, sl) ->
            Hash.add_code buf CodeQuoteStringString;
            Hash.add_char buf c;
            squash_string_exp_list pos buf sl
       | ObjectString (_, el, export) ->
            Hash.add_code buf CodeObjectString;
            squash_exp_list pos buf el;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | BodyString (_, el, export) ->
            Hash.add_code buf CodeBodyString;
            squash_exp_list pos buf el;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | ExpString (_, el, export) ->
            Hash.add_code buf CodeExpString;
            squash_exp_list pos buf el;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | CasesString (_, cases) ->
            Hash.add_code buf CodeCasesString;
            squash_cases_exp pos buf cases
       | ThisString _ ->
            Hash.add_code buf CodeThisString
       | LazyString (_, s) ->
            Hash.add_code buf CodeLazyString;
            squash_string_exp pos buf s
       | LetVarString (_, v, s1, s2) ->
            Hash.add_code buf CodeLetVarString;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_string_exp pos buf s1;
            Hash.add_code buf CodeSpace;
            squash_string_exp pos buf s2
   end;
   Hash.add_code buf CodeEnd

and squash_opt_string_exp pos buf = function
   Some s ->
      Hash.add_code buf CodeSome;
      squash_string_exp pos buf s
 | None ->
      Hash.add_code buf CodeNone

and squash_string_exp_list pos buf sl =
   match sl with
      [s] ->
         squash_string_exp pos buf s
    | s :: sl ->
         squash_string_exp pos buf s;
         Hash.add_code buf CodeSpace;
         squash_string_exp_list pos buf sl
    | [] ->
         ()

and squash_keyword_exp_list pos buf kargs =
   match kargs with
      (v, arg) :: kargs ->
         Hash.add_code buf CodeSpace;
         squash_var buf v;
         Hash.add_code buf CodeSpace;
         squash_string_exp pos buf arg;
         squash_keyword_exp_list pos buf kargs
    | [] ->
         ()

and squash_keyword_param_list pos buf kargs =
   match kargs with
      (v, v_info, opt_arg) :: kargs ->
         Hash.add_code buf CodeSpace;
         squash_var buf v;
         Hash.add_code buf CodeSpace;
         squash_var_info buf v_info;
         Hash.add_code buf CodeSpace;
         squash_opt_string_exp pos buf opt_arg;
         squash_keyword_param_list pos buf kargs
    | [] ->
         ()

and squash_case_exp pos buf (v, s, el, export) =
   Hash.add_code buf CodeCaseString;
   squash_var buf v;
   Hash.add_code buf CodeSpace;
   squash_string_exp pos buf s;
   Hash.add_code buf CodeSpace;
   squash_exp_list pos buf el;
   Hash.add_code buf CodeSpace;
   squash_export_info buf export;
   Hash.add_code buf CodeEnd

and squash_cases_exp pos buf cases =
   Hash.add_code buf CodeCasesString;
   List.iter (squash_case_exp pos buf) cases;
   Hash.add_code buf CodeEnd

(*
 * Squash an expression.
 *)
and squash_exp pos buf e =
   Hash.add_code buf CodeBegin;
   begin
      match e with
         LetVarExp (_, v, vl, def, s) ->
            Hash.add_code buf CodeLetVarExp;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_vars buf vl;
            Hash.add_code buf CodeSpace;
            squash_def_kind buf def;
            Hash.add_code buf CodeSpace;
            squash_string_exp pos buf s
       | KeyExp (_, v) ->
            Hash.add_code buf CodeKeyExp;
            Hash.add_string buf v
       | LetKeyExp (_, v, def, s) ->
            Hash.add_code buf CodeLetKeyExp;
            Hash.add_string buf v;
            Hash.add_code buf CodeSpace;
            squash_def_kind buf def;
            Hash.add_code buf CodeSpace;
            squash_string_exp pos buf s
       | LetFunExp (_, v, vl, curry, opt_params, params, s, export) ->
            Hash.add_code buf CodeLetFunExp;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_vars buf vl;
            Hash.add_code buf CodeSpace;
            Hash.add_bool buf curry;
            squash_keyword_param_list pos buf opt_params;
            Hash.add_code buf CodeSpace;
            squash_params buf params;
            Hash.add_code buf CodeSpace;
            squash_exp_list pos buf s;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | LetObjectExp (_, v, vl, s, el, export) ->
            Hash.add_code buf CodeLetObjectExp;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_vars buf vl;
            Hash.add_code buf CodeSpace;
            squash_string_exp pos buf s;
            Hash.add_code buf CodeSpace;
            squash_exp_list pos buf el;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | LetThisExp (_, s) ->
            Hash.add_code buf CodeLetThisExp;
            squash_string_exp pos buf s
       | ShellExp (_, s) ->
            Hash.add_code buf CodeShellExp;
            squash_string_exp pos buf s
       | IfExp (_, cases) ->
            Hash.add_code buf CodeIfExp;
            squash_if_cases pos buf cases
       | SequenceExp (_, el) ->
            Hash.add_code buf CodeSequenceExp;
            squash_exp_list pos buf el
       | SectionExp (_, s, el, export) ->
            Hash.add_code buf CodeSectionExp;
            squash_string_exp pos buf s;
            Hash.add_code buf CodeArrow;
            squash_exp_list pos buf el;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | OpenExp (_, nodes) ->
            Hash.add_code buf CodeOpenExp;
            List.iter (fun node ->
                  Hash.add_code buf CodeCommaExp;
                  squash_node buf node) nodes
       | IncludeExp (_, s, sl) ->
            Hash.add_code buf CodeIncludeExp;
            squash_string_exp pos buf s;
            Hash.add_code buf CodeCommaExp;
            squash_string_exp_list pos buf sl
       | ApplyExp (_, v, args, kargs) ->
            Hash.add_code buf CodeApplyExp;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | SuperApplyExp (_, v1, v2, args, kargs) ->
            Hash.add_code buf CodeSuperApplyExp;
            squash_var buf v1;
            Hash.add_code buf CodeSpace;
            squash_var buf v2;
            Hash.add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | MethodApplyExp (_, v, vars, args, kargs) ->
            Hash.add_code buf CodeMethodApplyExp;
            squash_var_info buf v;
            Hash.add_code buf CodeSpace;
            squash_vars buf vars;
            Hash.add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | ReturnBodyExp (_, el, id) ->
            Hash.add_code buf CodeReturnBodyExp;
            squash_exp_list pos buf el;
            squash_return_id buf id
       | StringExp (_, s) ->
            Hash.add_code buf CodeStringExp;
            squash_string_exp pos buf s
       | ReturnExp (_, s, id) ->
            Hash.add_code buf CodeReturnExp;
            squash_string_exp pos buf s;
            squash_return_id buf id
       | ReturnObjectExp (_, vars) ->
            Hash.add_code buf CodeReturnObjectExp;
            squash_vars buf vars
       | ReturnSaveExp _ ->
            Hash.add_code buf CodeReturnSaveExp
       | StaticExp (_, node, key, el) ->
            Hash.add_code buf CodeStaticExp;
            squash_node buf node;
            Hash.add_code buf CodeSpace;
            squash_var buf key;
            Hash.add_code buf CodeSpace;
            squash_exp_list pos buf el
   end;
   Hash.add_code buf CodeEnd

and squash_exp_list pos buf el =
   match el with
      [e] ->
         squash_exp pos buf e
    | e :: el ->
         squash_exp pos buf e;
         Hash.add_code buf CodeSpace;
         squash_exp_list pos buf el
    | [] ->
         ()

and squash_if_case pos buf (s, el, export) =
   Hash.add_code buf CodeCaseExp;
   squash_string_exp pos buf s;
   Hash.add_code buf CodeSpace;
   squash_exp_list pos buf el;
   Hash.add_code buf CodeSpace;
   squash_export_info buf export;
   Hash.add_code buf CodeEnd

and squash_if_cases pos buf cases =
   Hash.add_code buf CodeCasesExp;
   List.iter (squash_if_case pos buf) cases;
   Hash.add_code buf CodeEnd

(*
 * Compute the digest of a value.
 *)
let rec squash_value pos buf v =
   Hash.add_code buf CodeBegin;
   begin
      match v with
         ValNone ->
            Hash.add_code buf CodeValNone;
       | ValInt i ->
            Hash.add_code buf CodeValInt;
            Hash.add_int buf i
       | ValFloat x ->
            Hash.add_code buf CodeValFloat;
            Hash.add_float buf x
       | ValSequence vl ->
            Hash.add_code buf CodeValSequence;
            squash_values pos buf vl
       | ValArray vl ->
            Hash.add_code buf CodeValArray;
            squash_values pos buf vl
       | ValWhite s ->
            Hash.add_code buf CodeValWhite;
            Hash.add_string buf s
       | ValString s ->
            Hash.add_code buf CodeValString;
            Hash.add_string buf s
       | ValData s ->
            Hash.add_code buf CodeValData;
            Hash.add_string buf s
       | ValQuote vl ->
            Hash.add_code buf CodeValQuote;
            squash_values pos buf vl
       | ValQuoteString (c, vl) ->
            Hash.add_code buf CodeValQuoteString;
            Hash.add_char buf c;
            squash_values pos buf vl
       | ValMaybeApply (_, v) ->
            Hash.add_code buf CodeValMaybeApply;
            squash_var_info buf v
       | ValFun (_, keywords, params, body, export) ->
            Hash.add_code buf CodeValFun;
            squash_keyword_param_values pos buf keywords;
            Hash.add_code buf CodeSpace;
            squash_params buf params;
            Hash.add_code buf CodeArrow;
            squash_exp_list pos buf body;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | ValFunCurry (_, args, keywords, params, body, export, kargs) ->
            Hash.add_code buf CodeValFunCurry;
            squash_param_values pos buf args;
            Hash.add_code buf CodeSpace;
            squash_keyword_param_values pos buf keywords;
            Hash.add_code buf CodeSpace;
            squash_params buf params;
            Hash.add_code buf CodeArrow;
            squash_exp_list pos buf body;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export;
            squash_keyword_values pos buf kargs
       | ValPrim (_, _, _, f) ->
            Hash.add_code buf CodeValPrim;
            squash_var buf (squash_prim_fun f)
       | ValPrimCurry (_, _, f, args, kargs) ->
            Hash.add_code buf CodeValPrimCurry;
            squash_var buf (squash_prim_fun f);
            Hash.add_code buf CodeSpace;
            squash_values pos buf args;
            squash_keyword_values pos buf kargs
       | ValNode node ->
            Hash.add_code buf CodeValNode;
            Hash.add_string buf (Node.fullname node)
       | ValDir dir ->
            Hash.add_code buf CodeValDir;
            Hash.add_string buf (Dir.fullname dir)
       | ValStringExp (_, e) ->
            Hash.add_code buf CodeValStringExp;
            squash_string_exp pos buf e
       | ValBody (e, export) ->
            Hash.add_code buf CodeValBody;
            squash_exp_list pos buf e;
            Hash.add_code buf CodeSpace;
            squash_export_info buf export
       | ValObject obj ->
            Hash.add_code buf CodeValObject;
            squash_object pos buf obj
       | ValMap obj ->
            Hash.add_code buf CodeValMap;
            squash_map pos buf obj
       | ValCases cases ->
            squash_cases pos buf cases
       | ValVar (_, v) ->
            Hash.add_code buf CodeValVar;
            squash_var_info buf v;
       | ValDelayed { contents = ValValue v } ->
            squash_value pos buf v
       | ValDelayed { contents = ValStaticApply (node, v) } ->
            Hash.add_code buf CodeValStaticApply;
            squash_value pos buf node;
            Hash.add_code buf CodeSpace;
            squash_var buf v
       | ValRules _
       | ValChannel _
       | ValClass _
       | ValOther _ as v ->
            let print_error buf =
               fprintf buf "@[<v 3>Non digestable value:@ @[<hv 3>%a@]@ Contact the OMake team at omake@@metaprl.org if you think this should be supported@]@." pp_print_value v
            in
               raise (OmakeFatalErr (pos, LazyError print_error))
   end;
   Hash.add_code buf CodeEnd

and squash_opt_value pos buf = function
   Some v ->
      Hash.add_code buf CodeSome;
      squash_value pos buf v
 | None ->
      Hash.add_code buf CodeNone

and squash_values pos buf vl =
   match vl with
      [v] ->
         squash_value pos buf v
    | v :: vl ->
         squash_value pos buf v;
         Hash.add_code buf CodeSpace;
         squash_values pos buf vl
    | [] ->
         ()

and squash_param_values pos buf kargs =
   match kargs with
      (v, arg) :: kargs ->
         Hash.add_code buf CodeSpace;
         squash_var_info buf v;
         Hash.add_code buf CodeSpace;
         squash_value pos buf arg;
         squash_param_values pos buf kargs
    | [] ->
         ()

and squash_keyword_values pos buf kargs =
   match kargs with
      (v, arg) :: kargs ->
         Hash.add_code buf CodeSpace;
         squash_var buf v;
         Hash.add_code buf CodeSpace;
         squash_value pos buf arg;
         squash_keyword_values pos buf kargs
    | [] ->
         ()

and squash_keyword_param_values pos buf kargs =
   match kargs with
      (v, v_info, opt_arg) :: kargs ->
         Hash.add_code buf CodeSpace;
         squash_var buf v;
         Hash.add_code buf CodeSpace;
         squash_var_info buf v_info;
         Hash.add_code buf CodeSpace;
         squash_opt_value pos buf opt_arg;
         squash_keyword_param_values pos buf kargs
    | [] ->
         ()

and squash_object pos buf obj =
   SymbolTable.iter (fun x v ->
         Hash.add_code buf CodeBegin;
         squash_var buf x;
         Hash.add_code buf CodeArrow;
         squash_value pos buf v;
         Hash.add_code buf CodeEnd) (Omake_env.squash_object obj)

and squash_map pos buf map =
   venv_map_iter (fun x v ->
         Hash.add_code buf CodeBegin;
         squash_value pos buf x;
         Hash.add_code buf CodeArrow;
         squash_value pos buf v;
         Hash.add_code buf CodeEnd) map

and squash_case pos buf (x, v1, x2, export) =
   Hash.add_code buf CodeCase;
   squash_var buf x;
   Hash.add_code buf CodeSpace;
   squash_value pos buf v1;
   Hash.add_code buf CodeSpace;
   squash_value pos buf v1;
   Hash.add_code buf CodeSpace;
   squash_export_info buf export;
   Hash.add_code buf CodeEnd

and squash_cases pos buf cases =
   Hash.add_code buf CodeCases;
   List.iter (squash_case pos buf) cases;
   Hash.add_code buf CodeEnd

(*
 * Commands.
 *)
let squash_command_flag buf flag =
   let code =
      match flag with
         QuietFlag ->
            CodeQuietFlag
       | AllowFailureFlag ->
            CodeAllowFailureFlag
       | AllowOutputFlag ->
            CodeAllowOutputFlag
   in
      Hash.add_code buf code

let squash_command_flags buf flags =
   Hash.add_code buf CodeCommandFlags;
   List.iter (squash_command_flag buf) flags;
   Hash.add_code buf CodeEnd

let squash_arg_string buf arg =
   match arg with
      ArgString s ->
         Hash.add_code buf CodeArgString;
         Hash.add_string buf s
    | ArgData s ->
         Hash.add_code buf CodeArgData;
         Hash.add_string buf s

let squash_arg buf arg =
   Hash.add_code buf CodeArg;
   List.iter (squash_arg_string buf) arg;
   Hash.add_code buf CodeEnd

let squash_redirect buf chan =
   match chan with
      RedirectNode node ->
         Hash.add_code buf CodeRedirectNode;
         squash_node buf node
    | RedirectArg arg ->
         Hash.add_code buf CodeRedirectArg;
         squash_arg buf arg
    | RedirectNone ->
         Hash.add_code buf CodeRedirectNone

let squash_argv buf argv =
   Hash.add_code buf CodeArgv;
   List.iter (squash_arg buf) argv;
   Hash.add_code buf CodeEnd

let squash_command_env_item buf (v, arg) =
   Hash.add_code buf CodeCommandEnvItem;
   squash_var buf v;
   Hash.add_code buf CodeSpace;
   squash_arg buf arg;
   Hash.add_code buf CodeEnd

let squash_command_env buf env =
   Hash.add_code buf CodeCommandEnv;
   List.iter (squash_command_env_item buf) env;
   Hash.add_code buf CodeEnd

let squash_exe buf exe =
   match exe with
      CmdArg arg ->
         Hash.add_code buf CodeCmdArg;
         squash_arg buf arg
    | CmdNode node ->
         Hash.add_code buf CodeCmdNode;
         squash_node buf node

let squash_pipe_op buf op =
   let code =
      match op with
         PipeAnd -> CodePipeAnd
       | PipeOr  -> CodePipeOr
       | PipeSequence -> CodePipeSequence
   in
      Hash.add_code buf code

let squash_pipe_command pos buf (info : arg_cmd) =
   let { cmd_env   = env;
         cmd_exe   = exe;
         cmd_argv  = argv;
         cmd_stdin = stdin;
         cmd_stdout = stdout;
         cmd_stderr = stderr;
         cmd_append = append
       } = info
   in
      Hash.add_code buf CodePipeCommand;
      squash_command_env buf env;
      Hash.add_code buf CodeSpace;
      squash_exe buf exe;
      Hash.add_code buf CodeSpace;
      squash_argv buf argv;
      Hash.add_code buf CodeSpace;
      squash_redirect buf stdin;
      Hash.add_code buf CodeSpace;
      squash_redirect buf stdout;
      Hash.add_code buf CodeSpace;
      Hash.add_bool buf stderr;
      Hash.add_code buf CodeSpace;
      Hash.add_bool buf append;
      Hash.add_code buf CodeEnd

let squash_pipe_apply pos buf (info : arg_apply) =
   let { apply_name = name;
         apply_args = args;
         apply_stdin = stdin;
         apply_stdout = stdout;
         apply_stderr = stderr;
         apply_append = append
       } = info
   in
      Hash.add_code buf CodePipeApply;
      squash_var buf name;
      Hash.add_code buf CodeSpace;
      squash_values pos buf args;
      Hash.add_code buf CodeSpace;
      squash_redirect buf stdin;
      Hash.add_code buf CodeSpace;
      squash_redirect buf stdout;
      Hash.add_code buf CodeSpace;
      Hash.add_bool buf stderr;
      Hash.add_code buf CodeSpace;
      Hash.add_bool buf append;
      Hash.add_code buf CodeEnd

let rec squash_pipe pos buf (pipe : arg_pipe) =
   (match pipe with
       PipeApply (_, info) ->
          squash_pipe_apply pos buf info
     | PipeCommand (_, info) ->
          squash_pipe_command pos buf info
     | PipeCond (_, op, pipe1, pipe2) ->
          Hash.add_code buf CodePipeCond;
          squash_pipe_op buf op;
          squash_pipe pos buf pipe1;
          squash_pipe pos buf pipe2
     | PipeCompose (_, b, pipe1, pipe2) ->
          Hash.add_code buf CodePipeCompose;
          Hash.add_bool buf b;
          squash_pipe pos buf pipe1;
          squash_pipe pos buf pipe2
     | PipeGroup (_, info) ->
          squash_pipe_group pos buf info
     | PipeBackground (_, pipe) ->
          Hash.add_code buf CodePipeBackground;
          squash_pipe pos buf pipe);
   Hash.add_code buf CodeEnd

and squash_pipe_group pos buf info =
   let { group_stdin = stdin;
         group_stdout = stdout;
         group_stderr = stderr;
         group_append = append;
         group_pipe   = pipe
       } = info
   in
      Hash.add_code buf CodePipeGroup;
      squash_redirect buf stdin;
      Hash.add_code buf CodeSpace;
      squash_redirect buf stdout;
      Hash.add_code buf CodeSpace;
      Hash.add_bool buf stderr;
      Hash.add_code buf CodeSpace;
      Hash.add_bool buf append;
      Hash.add_code buf CodeSpace;
      squash_pipe pos buf pipe;
      Hash.add_code buf CodeEnd

let squash_command_line pos buf (command : arg_command_inst) =
   match command with
      CommandPipe argv ->
         Hash.add_code buf CodePipe;
         squash_pipe pos buf argv;
         Hash.add_code buf CodeEnd
    | CommandEval e ->
         squash_exp_list pos buf e
    | CommandValues values ->
         squash_values pos buf values

let squash_command pos buf (command : arg_command_line) =
   let { command_dir = dir;
         command_inst = inst
       } = command
   in
      Hash.add_code buf CodeCommand;
      Hash.add_string buf (Dir.fullname dir);
      squash_command_line pos buf inst;
      Hash.add_code buf CodeEnd

let squash_commands pos buf commands =
   Hash.add_code buf CodeCommands;
   List.iter (squash_command pos buf) commands;
   Hash.add_code buf CodeEnd

(*
 * Get the digest of some commands.
 *)
let digest_of_exp pos values e =
   let buf = Hash.create () in
      squash_values pos buf values;
      Hash.add_code buf CodeSpace;
      squash_exp pos buf e;
      Some (Hash.digest buf)

let digest_of_commands pos commands =
   match commands with
      [] ->
         None
    | _ ->
         let buf = Hash.create () in
         let () = squash_commands pos buf commands in
            Some (Hash.digest buf)

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
