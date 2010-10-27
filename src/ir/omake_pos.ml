(*
 * Standard exceptions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_ir
open Omake_node
open Omake_ir_util
open Omake_ir_print
open Omake_print_util
open Omake_value_type
open Omake_value_print

type pos = Omake_value_type.pos

(************************************************************************
 * Utilities.
 *)
let string_loc = bogus_loc "<Omake_env>"

let rec loc_of_item x =
   match x with
      AstExp e ->
         Omake_ast_util.loc_of_exp e
    | IrExp e ->
         Omake_ir_util.loc_of_exp e
    | Location loc ->
         loc
    | Value _
    | Symbol _
    | String _
    | Error _ ->
         string_loc

(*
 * Value printing.
 *)
let rec pp_print_item buf x =
   match x with
      AstExp e ->
         Omake_ast_print.pp_print_exp buf e

    | IrExp e ->
         Omake_ir_print.pp_print_exp buf e

    | Location _ ->
         ()

    | Symbol v ->
         pp_print_symbol buf v

    | String s ->
         pp_print_string buf s

    | Value v ->
         pp_print_value buf v

    | Error e ->
         pp_print_exn buf e

(*
 * Exception printer.
 *)
and pp_print_exn buf = function
   SyntaxError s ->
      fprintf buf "syntax error: %s" s
 | StringAstError (s, e) ->
      fprintf buf "@[<hv 3>%s:@ %a@]" s Omake_ast_print.pp_print_simple_exp e
 | StringError s ->
      pp_print_string buf s
 | StringIntError (s, i) ->
      fprintf buf "%s: %d" s i
 | StringStringError (s1, s2) ->
      fprintf buf "%s: %s" s1 s2
 | StringVarError (s, v) ->
      fprintf buf "%s: %a" s pp_print_symbol v
 | StringMethodError (s, v) ->
      fprintf buf "%s: %a" s pp_print_method_name v
 | StringDirError (s, n)->
      fprintf buf "%s: %a" s pp_print_dir n
 | StringNodeError (s, n)->
      fprintf buf "%s: %a" s pp_print_node n
 | StringValueError (s, v) ->
      fprintf buf "@[<hv 3>%s:@ %a@]" s pp_print_value v
 | StringTargetError (s, t) ->
      fprintf buf "%s: %a" s pp_print_target t
 | LazyError printer ->
      printer buf
 | UnboundVar v ->
      fprintf buf "unbound variable: %a" pp_print_symbol v
 | UnboundVarInfo v ->
      fprintf buf "unbound variable: %a" pp_print_var_info v
 | UnboundKey v ->
      fprintf buf "unbound key: %s" v
 | UnboundValue v ->
      fprintf buf "unbound value: %a" pp_print_value v
 | UnboundFun v ->
      fprintf buf "unbound function: %a" pp_print_symbol v
 | UnboundMethod vl ->
      fprintf buf "unbound method: %a" pp_print_method_name vl
 | UnboundFieldVar (obj, v) ->
      fprintf buf "@[<v 3>unbound method '%a', object classes:@ @[<b 3>" pp_print_symbol v;
      SymbolTable.iter (fun v _ ->
            fprintf buf "@ %a" pp_print_symbol v) (venv_get_class obj);
      fprintf buf "@]@]"
 | ArityMismatch (len1, len2) ->
      fprintf buf "arity mismatch: expected %a args, got %d" pp_print_arity len1 len2
 | NotImplemented s ->
      fprintf buf "not implemented: %s" s
 | NullCommand ->
      pp_print_string buf "invalid null command"

(************************************************************************
 * Positions.
 *)
module type NameSig =
sig
   val name : string
end

module MakePos (Name : NameSig) : PosSig =
struct
   module Name' =
   struct
      type t = item

      let name = Name.name

      let loc_of_value = loc_of_item
      let pp_print_value = pp_print_item
   end

   module Pos = Lm_position.MakePos (Name')

   include Pos

   let loc_pos_pos loc pos =
      cons_pos (Location loc) pos

   let ast_exp_pos e    = base_pos (AstExp e)
   let ir_exp_pos e     = base_pos (IrExp e)
   let var_exp_pos v    = base_pos (Symbol v)
   let string_exp_pos s = base_pos (String s)
   let value_exp_pos v  = base_pos (Value v)
   let var_pos          = symbol_pos
   let value_pos v pos  = cons_pos (Value v) pos
   let error_pos e pos  = cons_pos (Error e) pos
end

module Pos = MakePos (struct let name = "Omake_env" end)
             open Pos;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
