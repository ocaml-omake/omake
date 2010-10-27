/*
 * Parser for OMakefiles.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2007 Jason Hickey, Caltech
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
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 */
%{
open Lm_location

open Omake_pos
open Omake_ast
open Omake_symbol
open Omake_ast_util
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_exp_parse" end)
open Pos;;

(*
 * Different types of identifiers.
 *)
type id =
   SimpleId of var
 | SuperId  of var * var
 | MethodId of var list

(*
 * Identifier stands for an application.
 *)
let make_id_exp (id, loc) =
   let e =
      match id with
         SimpleId v -> ApplyExp (NormalApply, v, [], loc)
       | SuperId (v1, v2) -> SuperApplyExp (NormalApply, v1, v2, [], loc)
       | MethodId vars -> MethodApplyExp (NormalApply, vars, [], loc)
   in
      e, loc

(*
 * Unary operations.
 *)
let make_unary_exp v (e, loc) =
   ApplyExp (NormalApply, v, [ExpArg e], loc), loc

let make_binary_exp v (e1, loc1) (e2, loc2) =
   let loc = union_loc loc1 loc2 in
      ApplyExp (NormalApply, v, [ExpArg e1; ExpArg e2], loc), loc

(*
 * If the function is a null application, add the args.
 *)
let apply_var = Lm_symbol.add ".fun"

let make_apply_exp (e, loc) args =
   match e with
      ApplyExp (strategy, v, [], _) ->
         ApplyExp (strategy, v, args, loc), loc
    | _ ->
         (* Create a temporary private variable *)
         SequenceExp ([VarDefExp ([apply_var], DefineString, DefineNormal, e, loc);
                       ApplyExp (NormalApply, apply_var, args, loc)], loc), loc

(*
 * Function parameters from an argument list.
 *)
let get_fun_param = function
    ExpArg (ApplyExp (NormalApply, v, [], loc)) ->
        NormalParam (v, loc)
  | ExpArg e
  | KeyArg (_, e)
  | ArrowArg (_, e) ->
        raise (OmakeException (loc_exp_pos (loc_of_exp e), StringError "illegal parameter"))

let get_fun_params = List.map get_fun_param
%}

/*
 * Terminators
 */
%token TokEof

/*
 * Characters.
 */
%token <Lm_location.loc> TokLeftParen
%token <Lm_location.loc> TokRightParen
%token <Lm_location.loc> TokLeftBrack
%token <Lm_location.loc> TokRightBrack
%token <Lm_location.loc> TokPlus
%token <Lm_location.loc> TokMinus
%token <Lm_location.loc> TokStar
%token <Lm_location.loc> TokSlash
%token <Lm_location.loc> TokMod
%token <Lm_location.loc> TokHat
%token <Lm_location.loc> TokPipe
%token <Lm_location.loc> TokAmp
%token <Lm_location.loc> TokLsl
%token <Lm_location.loc> TokLsr
%token <Lm_location.loc> TokAsr
%token <Lm_location.loc> TokAnd
%token <Lm_location.loc> TokOr
%token <Lm_location.loc> TokDot
%token <Lm_location.loc> TokComma
%token <Lm_location.loc> TokSemi
%token <Lm_location.loc> TokLe
%token <Lm_location.loc> TokLt
%token <Lm_location.loc> TokEq
%token <Lm_location.loc> TokNeq
%token <Lm_location.loc> TokGt
%token <Lm_location.loc> TokGe
%token <Lm_location.loc> TokColonColon
%token <Lm_location.loc> TokArrow

/*
 * Words.
 */
%token <Lm_symbol.symbol * Lm_location.loc> TokId
%token <Lm_symbol.symbol * Lm_location.loc> TokKey
%token <Lm_location.loc> TokCatch

/*
 * Values.
 */
%token <int * Lm_location.loc> TokInt
%token <float * Lm_location.loc> TokFloat
%token <Omake_ast.exp> TokExp

/*
 * Precedences.
 */
%left TokSemi
%left TokComma
%left TokAnd TokOr
%left TokPipe
%left TokAmp
%left TokHat
%left TokEq TokNeq
%left TokLe TokLt TokGe TokGt
%left TokLsl TokLsr TokAsr
%left TokPlus TokMinus
%left TokStar TokSlash TokMod
%right prec_uminus
%left TokDot TokLeftBrack TokLeftParen

/*
 * A complete program.
 */
%start ast_exp
%type <Omake_ast.exp> ast_exp
%type <Omake_ast.exp * Lm_location.loc> exp

%%

ast_exp: exp TokEof
	   { let e, _ = $1 in e }
	 ;

exp:
	  TokInt
	  { let i, loc = $1 in
	       IntExp (i, loc), loc
	  }
	| TokFloat
	  { let x, loc = $1 in
	       FloatExp (x, loc), loc
	  }
        | TokExp
          { let e = $1 in
               e, loc_of_exp e
          }
	| id
	  { make_id_exp $1 }
	| TokMinus exp %prec prec_uminus
	  { make_unary_exp neg_fun_sym $2 }
	| exp TokPlus exp
	  { make_binary_exp add_fun_sym $1 $3 }
	| exp TokMinus exp
	  { make_binary_exp sub_fun_sym $1 $3 }
	| exp TokStar exp
	  { make_binary_exp mul_fun_sym $1 $3 }
	| exp TokSlash exp
	  { make_binary_exp div_fun_sym $1 $3 }
	| exp TokMod exp
	  { make_binary_exp mod_fun_sym $1 $3 }
	| exp TokHat exp
	  { make_binary_exp lxor_fun_sym $1 $3 }
	| exp TokPipe exp
	  { make_binary_exp lor_fun_sym $1 $3 }
	| exp TokAmp exp
	  { make_binary_exp land_fun_sym $1 $3 }
	| exp TokLsl exp
	  { make_binary_exp lsl_fun_sym $1 $3 }
	| exp TokLsr exp
	  { make_binary_exp lsr_fun_sym $1 $3 }
	| exp TokAsr exp
	  { make_binary_exp asr_fun_sym $1 $3 }
	| exp TokAnd exp
	  { make_binary_exp and_fun_sym $1 $3 }
	| exp TokOr exp
	  { make_binary_exp or_fun_sym $1 $3 }
        | exp TokLe exp
          { make_binary_exp le_fun_sym $1 $3 }
        | exp TokLt exp
          { make_binary_exp lt_fun_sym $1 $3 }
        | exp TokEq exp
          { make_binary_exp equal_fun_sym $1 $3 }
        | exp TokNeq exp
          { make_binary_exp nequal_fun_sym $1 $3 }
        | exp TokGt exp
          { make_binary_exp gt_fun_sym $1 $3 }
        | exp TokGe exp
          { make_binary_exp ge_fun_sym $1 $3 }
	| exp TokLeftParen opt_args TokRightParen
          { make_apply_exp $1 $3 }
	| exp TokLeftBrack exp TokRightBrack
	  { make_binary_exp nth_fun_sym $3 $1 }
	| TokLeftParen exp TokRightParen
	  { $2 }
	| TokLeftBrack opt_exp_list TokRightBrack
	  { let loc = union_loc $1 $3 in
	       ArrayExp ($2, loc), loc
	  }
	;

id:
	  TokId
	  { let id, loc = $1 in
               SimpleId id, loc
          }
	| TokId TokColonColon TokId
	  { let v1, loc1 = $1 in
            let v2, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               SuperId (v1, v2), loc
          }
	| TokId TokDot rev_path_id
	  { let v1, loc1 = $1 in
            let vars, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               MethodId (v1 :: vars), loc
          }
	;

rev_path_id:
	  TokId
	  { let v, loc = $1 in
               [v], loc
          }
	| rev_path_id TokDot TokId
	  { let path, loc1 = $1 in
            let v, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               v :: path, loc
          }
	;

/*
 * Expression lists, separated by commas.
 */
opt_exp_list:
	  /* empty */
	  { [] }
	| rev_exp_list opt_semi_or_comma
	  { List.rev $1 }
	;

rev_exp_list:
	  exp
	  { let e, _ = $1 in [e] }
	| rev_exp_list semi_or_comma exp
          { let e, _ = $3 in
               e :: $1
          }
	;

opt_semi_or_comma:
	    /* empty */
	    { () }
	  | semi_or_comma
	    { () }
	  ;

semi_or_comma:
	    TokSemi
	    { $1 }
	  | TokComma
	    { $1 }
	  ;

/*
 * Argument lists.
 */
opt_args:
	  /* empty */
	  { [] }
	| args
	  { $1 }
	;

args:     rev_args
          { List.rev $1 }
        | rev_arrow_args
          { List.rev $1 }
        | rev_arrow_args TokComma rev_args
          { List.rev_append $1 (List.rev $3) }
        ;

rev_arrow_args:
          arrow_arg
          { [$1] }
        | rev_arrow_args TokComma arrow_arg
          { $3 :: $1 }
        ;

arrow_arg:
          rev_args TokArrow exp
          { let e, _ = $3 in
               ArrowArg (get_fun_params (List.rev $1), e)
          }
        ;

rev_args:
	  arg
	  { [$1] }
	| rev_args TokComma arg
          { $3 :: $1 }
	;

arg:	  exp
	  { let e, _ = $1 in
               ExpArg e
          }
        | TokKey
	  { let key, loc = $1 in
	        KeyArg (key, NullExp loc)
          }
	| TokKey TokEq exp
	  { let key, _ = $1 in
            let e, _ = $3 in
	        KeyArg (key, e)
          }
	;
