(*
 * Print the AST (for debugging).
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
open Lm_debug
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_ast
open Omake_ast_util
open Omake_print_util

let print_location =
   create_debug (**)
      { debug_name = "print-loc";
        debug_description = "Print locations";
        debug_value = false
      }

(*
 * Application strategy.
 *)
let pp_print_strategy buf s =
   match s with
      LazyApply -> pp_print_char buf '\''
    | EagerApply -> pp_print_char buf ','
    | NormalApply -> ()
    | CommandApply -> pp_print_char buf '#'

(*
 * Definitions.
 *)
let pp_print_define_kind buf flag =
   match flag with
      DefineString ->
         ()
    | DefineArray ->
         pp_print_string buf "[]"

let pp_print_define_flag buf flag =
   let s =
      match flag with
         DefineNormal -> "="
       | DefineAppend -> "+="
   in
      pp_print_string buf s

(*
 * Print an expression.
 *)
let rec pp_print_exp buf e =
   if !print_location then
      fprintf buf "<%a>" pp_print_location (loc_of_exp e);
   match e with
      NullExp _ ->
         pp_print_string buf "<null>"
    | IntExp (i, _) ->
         fprintf buf "(int %d)" i
    | FloatExp (x, _) ->
         fprintf buf "(float %f)" x
    | StringOpExp (s, _) ->
         fprintf buf "(string-op \"%s\")" (String.escaped s)
    | StringIdExp (s, _) ->
         fprintf buf "(string-id \"%s\")" (String.escaped s)
    | StringIntExp (s, _) ->
         fprintf buf "(string-int \"%s\")" (String.escaped s)
    | StringFloatExp (s, _) ->
         fprintf buf "(string-float \"%s\")" (String.escaped s)
    | StringWhiteExp (s, _) ->
         fprintf buf "(string-white \"%s\")" (String.escaped s)
    | StringOtherExp (s, _) ->
         fprintf buf "(string-other \"%s\")" (String.escaped s)
    | StringKeywordExp (s, _) ->
         fprintf buf "(string-keyword \"%s\")" (String.escaped s)
    | QuoteExp (el, _) ->
         fprintf buf "@[<hv 3>(quote";
         List.iter (fun e ->
               fprintf buf "@ %a" pp_print_exp e) el;
         fprintf buf ")@]"
    | QuoteStringExp (c, el, _) ->
         fprintf buf "@[<hv 3>(quoted-string %c" c;
         List.iter (fun e ->
               fprintf buf "@ %a" pp_print_exp e) el;
         fprintf buf "%c)@]" c
    | SequenceExp (el, _) ->
         fprintf buf "@[<hv 3>(sequence";
         List.iter (fun e ->
               fprintf buf "@ %a" pp_print_exp e) el;
         fprintf buf ")@]"
    | ArrayExp (el, _) ->
         fprintf buf "@[<hv 3>(array";
         List.iter (fun e ->
               fprintf buf "@ %a" pp_print_exp e) el;
         fprintf buf ")@]"
    | ApplyExp (LazyApply, v, [], _) ->
         fprintf buf "$%a" pp_print_symbol v
    | ApplyExp (s, v, args, _) ->
         fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
            pp_print_symbol v
            pp_print_strategy s
            pp_print_args args
    | SuperApplyExp (s, super, v, args, _) ->
         fprintf buf "@[<hv 3>%a%a::%a(%a)@]" (**)
            pp_print_symbol super
            pp_print_strategy s
            pp_print_symbol v
            pp_print_args args
    | MethodApplyExp (s, vl, args, _) ->
         fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
            pp_print_method_name vl
            pp_print_strategy s
            pp_print_args args
    | CommandExp (v, arg, commands, _) ->
         fprintf buf "@[<hv 0>@[<hv 3>command %a(%a) {%a@]@ }@]" (**)
            pp_print_symbol v
            pp_print_exp arg
            pp_print_exp_list commands
    | VarDefExp (v, kind, flag, e, _) ->
         fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
            pp_print_method_name v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_exp e
    | VarDefBodyExp (v, kind, flag, el, _) ->
         fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
            pp_print_method_name v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_exp_list el
    | KeyExp (strategy, v, _) ->
         fprintf buf "$%a|%s|" pp_print_strategy strategy v
    | KeyDefExp (v, kind, flag, e, _) ->
         fprintf buf "@[<hv 3>\"%s\"%a %a@ %a@]" (**)
            v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_exp e
    | KeyDefBodyExp (v, kind, flag, el, _) ->
         fprintf buf "@[<hv 3>key \"%s\"%a %a@ %a@]" (**)
            v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_exp_list el
    | ObjectDefExp (v, flag, el, _) ->
         fprintf buf "@[<hv 3>let %a. %a@ %a@]" (**)
            pp_print_method_name v
            pp_print_define_flag flag
            pp_print_exp_list el;
    | FunDefExp (v, vars, el, _) ->
         fprintf buf "@[<hv 3>let %a(%a) =" (**)
            pp_print_params vars
            pp_print_method_name v;
         List.iter (fun e -> fprintf buf "@ %a" pp_print_exp e) el;
         fprintf buf "@]"
    | RuleExp (multiple, target, pattern, source, commands, _) ->
         fprintf buf "@[<hv 0>@[<hv 3>rule {@ multiple = %b;@ @[<hv 3>target =@ %a;@]@ @[<hv 3>pattern =@ %a;@]@ @[<hv 3>source =@ %a@]@ %a@]@ }@]" (**)
            multiple
            pp_print_exp target
            pp_print_exp pattern
            pp_print_table_exp source
            pp_print_exp_list commands
    | BodyExp (body, _) ->
         fprintf buf "@[<v 3>body";
         List.iter (fun e -> fprintf buf "@ %a" pp_print_exp e) body;
         fprintf buf "@]"
    | ShellExp (e, _) ->
         fprintf buf "@[<hv 3>shell %a@]" pp_print_exp e
    | CatchExp (name, v, body, _) ->
         fprintf buf "@[<v 3>catch %a(%a)@ %a@]" (**)
            pp_print_symbol name
            pp_print_symbol v
            pp_print_exp_list body
    | ClassExp (names, _) ->
         fprintf buf "@[<hv 3>class";
         List.iter (fun v -> fprintf buf "@ %a" pp_print_symbol v) names;
         fprintf buf "@]"

(*
 * Parameters.
 *)
and pp_print_param buf = function
   OptionalParam (v, e, _) ->
      fprintf buf "@[<hv 3>?%a =@ %a@]" pp_print_symbol v pp_print_exp e
 | RequiredParam (v, _) ->
      fprintf buf "~%a" pp_print_symbol v
 | NormalParam (v, _) ->
      pp_print_symbol buf v

and pp_print_params buf vars =
   match vars with
      [v] ->
         pp_print_param buf v
    | v :: vars ->
         fprintf buf "%a,@ " pp_print_param v;
         pp_print_params buf vars
    | [] ->
         ()

and pp_print_arrow_arg buf params e =
   fprintf buf "@[<hv 3>%a =>@ %a@]" pp_print_params params pp_print_exp e

and pp_print_arg buf = function
   KeyArg (v, e) ->
      fprintf buf "@[<hv 3>~%a =@ %a@]" pp_print_symbol v pp_print_exp e
 | ExpArg e ->
      pp_print_exp buf e
 | ArrowArg (params, e) ->
      pp_print_arrow_arg buf params e

and pp_print_args buf args =
   match args with
      [arg] ->
         pp_print_arg buf arg
    | arg :: args ->
         pp_print_arg buf arg;
         fprintf buf ",@ ";
         pp_print_args buf args
    | [] ->
         ()

and pp_print_exp_list buf commands =
   List.iter (fun e -> fprintf buf "@ %a" pp_print_exp e) commands

and pp_print_exp_option buf e_opt =
   match e_opt with
      Some e -> pp_print_exp buf e
    | None -> pp_print_string buf "<none>"

and pp_print_table_exp buf source =
   fprintf buf "@[<hv 0>@[<hv 3>{";
   SymbolTable.iter (fun v e ->
         fprintf buf "@ %a = %a" pp_print_symbol v pp_print_exp e) source;
   fprintf buf "@]@ }@]"

(*
 * A program is a list of expressions.
 *)
let pp_print_prog buf prog =
   fprintf buf "@[<v 0>Prog:";
   List.iter (fun e -> fprintf buf "@ %a" pp_print_exp e) prog;
   fprintf buf "@]"

(*
 * Simplified printing.
 *)
let rec pp_print_simple_exp buf e =
   if !print_location then
      fprintf buf "<%a>" pp_print_location (loc_of_exp e);
   match e with
      NullExp _ ->
         pp_print_string buf "<null>"
    | IntExp (i, _) ->
         fprintf buf "%d" i
    | FloatExp (x, _) ->
         fprintf buf "%f" x
    | StringOpExp (s, _)
    | StringIdExp (s, _)
    | StringIntExp (s, _)
    | StringFloatExp (s, _)
    | StringWhiteExp (s, _)
    | StringOtherExp (s, _)
    | StringKeywordExp (s, _) ->
         pp_print_string buf s
    | QuoteExp (el, _) ->
         fprintf buf "$'%a'" pp_print_simple_exp_list el
    | QuoteStringExp (c, el, _) ->
         fprintf buf "%c%a%c" c pp_print_simple_exp_list el c
    | SequenceExp (el, _) ->
         pp_print_simple_exp_list buf el
    | ArrayExp (el, _) ->
         fprintf buf "@[<hv 3>(array";
         List.iter (fun e ->
               fprintf buf "@ %a" pp_print_exp e) el;
         fprintf buf ")@]"
    | ApplyExp (LazyApply, v, [], _) ->
         fprintf buf "$%a" pp_print_symbol v
    | ApplyExp (s, v, args, _) ->
         fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
            pp_print_strategy s
            pp_print_symbol v
            pp_print_simple_args args
    | SuperApplyExp (s, super, v, args, _) ->
         fprintf buf "@[<hv 3>%a%a::%a(%a)@]" (**)
            pp_print_symbol super
            pp_print_strategy s
            pp_print_symbol v
            pp_print_simple_args args
    | MethodApplyExp (s, vl, args, _) ->
         fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
            pp_print_method_name vl
            pp_print_strategy s
            pp_print_simple_args args
    | CommandExp (v, arg, commands, _) ->
         fprintf buf "@[<hv 0>@[<hv 3>command %a(%a) {%a@]@ }@]" (**)
            pp_print_symbol v
            pp_print_simple_exp arg
            pp_print_simple_exp_list commands
    | VarDefExp (v, kind, flag, e, _) ->
         fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
            pp_print_method_name v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_simple_exp e
    | VarDefBodyExp (v, kind, flag, el, _) ->
         fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
            pp_print_method_name v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_simple_exp_list el
    | KeyExp (strategy, v, _) ->
         fprintf buf "$%a|%s|" pp_print_strategy strategy v
    | KeyDefExp (v, kind, flag, e, _) ->
         fprintf buf "@[<hv 3>\"%s\"%a %a@ %a@]" (**)
            v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_simple_exp e
    | KeyDefBodyExp (v, kind, flag, el, _) ->
         fprintf buf "@[<hv 3>key \"%s\"%a %a@ %a@]" (**)
            v
            pp_print_define_kind kind
            pp_print_define_flag flag
            pp_print_simple_exp_list el
    | ObjectDefExp (v, flag, el, _) ->
         fprintf buf "@[<hv 3>let %a. %a@ %a@]" (**)
            pp_print_method_name v
            pp_print_define_flag flag
            pp_print_simple_exp_list el
    | FunDefExp (v, vars, el, _) ->
         fprintf buf "@[<hv 3>let %a(%a) =" (**)
            pp_print_params vars
            pp_print_method_name v;
         List.iter (fun e -> fprintf buf "@ %a" pp_print_exp e) el;
         fprintf buf "@]"
    | RuleExp (multiple, target, pattern, source, commands, _) ->
         fprintf buf "@[<hv 0>@[<hv 3>rule {@ multiple = %b;@ @[<hv 3>target =@ %a;@]@ @[<hv 3>pattern =@ %a;@]@ @[<hv 3>source =@ %a@]@ %a@]@ }@]" (**)
            multiple
            pp_print_simple_exp target
            pp_print_simple_exp pattern
            pp_print_table_exp source
            pp_print_simple_exp_list commands
    | BodyExp (body, _) ->
         fprintf buf "@[<v 3>body";
         List.iter (fun e -> fprintf buf "@ %a" pp_print_simple_exp e) body;
         fprintf buf "@]"
    | ShellExp (e, _) ->
         fprintf buf "@[<hv 3>shell %a@]" pp_print_simple_exp e
    | CatchExp (name, v, body, _) ->
         fprintf buf "@[<v 3>catch %a(%a)@ %a@]" (**)
            pp_print_symbol name
            pp_print_symbol v
            pp_print_simple_exp_list body
    | ClassExp (names, _) ->
         fprintf buf "@[<hv 3>class";
         List.iter (fun v -> fprintf buf "@ %a" pp_print_symbol v) names;
         fprintf buf "@]"

and pp_print_simple_exp_list buf el =
   List.iter (pp_print_simple_exp buf) el

and pp_print_simple_args buf args =
   match args with
      [arg] ->
         pp_print_simple_arg buf arg
    | arg :: args ->
         pp_print_simple_arg buf arg;
         fprintf buf ",@ ";
         pp_print_simple_args buf args
    | [] ->
         ()

and pp_print_simple_arg buf = function
   KeyArg (v, e) ->
      fprintf buf "@[<hv 3>~%a =@ %a@]" pp_print_symbol v pp_print_exp e
 | ExpArg e ->
      pp_print_simple_exp buf e
 | ArrowArg (params, e) ->
      pp_print_arrow_arg buf params e

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
