
let pp_print_location = Lm_location.pp_print_location
let pp_print_symbol = 
  Lm_symbol.pp_print_symbol
module SymbolTable = Lm_symbol.SymbolTable
let create_debug = Lm_debug.create_debug

let pp_print_method_name = 
  Lm_symbol.pp_print_method_name

let print_location =
  create_debug (**)
    { debug_name = "print-loc";
      debug_description = "Print locations";
      debug_value = false
    }

(*
 * Application strategy.
 *)
let pp_print_strategy buf (s : Omake_ast.apply_strategy) =
  match s with
  | LazyApply -> Format.pp_print_char buf '\''
  | EagerApply -> Format.pp_print_char buf ','
  | NormalApply -> ()
  | CommandApply -> Format.pp_print_char buf '#'

(*
 * Definitions.
 *)
let pp_print_define_kind buf (flag : Omake_ast.define_kind) =
  match flag with
  | DefineString ->
    ()
  | DefineArray ->
    Format.pp_print_string buf "[]"

let pp_print_define_flag buf (flag : Omake_ast.define_flag) =
  let s =
    match flag with
    | DefineNormal -> "="
    | DefineAppend -> "+=" in
  Format.pp_print_string buf s

(*
 * Print an expression.
 *)
let rec pp_print_exp buf (e : Omake_ast.exp)=
  if !print_location then
    Format.fprintf buf "<%a>" pp_print_location (Omake_ast_util.loc_of_exp e);
  match e with
  | NullExp _ ->
    Format.pp_print_string buf "<null>"
  | IntExp (i, _) ->
    Format.fprintf buf "(int %d)" i
  | FloatExp (x, _) ->
    Format.fprintf buf "(float %f)" x
  | StringOpExp (s, _) ->
    Format.fprintf buf "(string-op \"%s\")" (String.escaped s)
  | StringIdExp (s, _) ->
    Format.fprintf buf "(string-id \"%s\")" (String.escaped s)
  | StringIntExp (s, _) ->
    Format.fprintf buf "(string-int \"%s\")" (String.escaped s)
  | StringFloatExp (s, _) ->
    Format.fprintf buf "(string-float \"%s\")" (String.escaped s)
  | StringWhiteExp (s, _) ->
    Format.fprintf buf "(string-white \"%s\")" (String.escaped s)
  | StringOtherExp (s, _) ->
    Format.fprintf buf "(string-other \"%s\")" (String.escaped s)
  | StringKeywordExp (s, _) ->
    Format.fprintf buf "(string-keyword \"%s\")" (String.escaped s)
  | QuoteExp (el, _) ->
    Format.fprintf buf "@[<hv 3>(quote";
    List.iter (fun e ->
        Format.fprintf buf "@ %a" pp_print_exp e) el;
    Format.fprintf buf ")@]"
  | QuoteStringExp (c, el, _) ->
    Format.fprintf buf "@[<hv 3>(quoted-string %c" c;
    List.iter (fun e ->
        Format.fprintf buf "@ %a" pp_print_exp e) el;
    Format.fprintf buf "%c)@]" c
  | SequenceExp (el, _) ->
    Format.fprintf buf "@[<hv 3>(sequence";
    List.iter (fun e ->
        Format.fprintf buf "@ %a" pp_print_exp e) el;
    Format.fprintf buf ")@]"
  | ArrayExp (el, _) ->
    Format.fprintf buf "@[<hv 3>(array";
    List.iter (fun e ->
        Format.fprintf buf "@ %a" pp_print_exp e) el;
    Format.fprintf buf ")@]"
  | ApplyExp (LazyApply, v, [], _) ->
    Format.fprintf buf "$%a" pp_print_symbol v
  | ApplyExp (s, v, args, _) ->
    Format.fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
      pp_print_symbol v
      pp_print_strategy s
      pp_print_args args
  | SuperApplyExp (s, super, v, args, _) ->
    Format.fprintf buf "@[<hv 3>%a%a::%a(%a)@]" (**)
      pp_print_symbol super
      pp_print_strategy s
      pp_print_symbol v
      pp_print_args args
  | MethodApplyExp (s, vl, args, _) ->
    Format.fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
      pp_print_method_name vl
      pp_print_strategy s
      pp_print_args args
  | CommandExp (v, arg, commands, _) ->
    Format.fprintf buf "@[<hv 0>@[<hv 3>command %a(%a) {%a@]@ }@]" (**)
      pp_print_symbol v
      pp_print_exp arg
      pp_print_exp_list commands
  | VarDefExp (v, kind, flag, e, _) ->
    Format.fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
      pp_print_method_name v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_exp e
  | VarDefBodyExp (v, kind, flag, el, _) ->
    Format.fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
      pp_print_method_name v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_exp_list el
  | KeyExp (strategy, v, _) ->
    Format.fprintf buf "$%a|%s|" pp_print_strategy strategy v
  | KeyDefExp (v, kind, flag, e, _) ->
    Format.fprintf buf "@[<hv 3>\"%s\"%a %a@ %a@]" (**)
      v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_exp e
  | KeyDefBodyExp (v, kind, flag, el, _) ->
    Format.fprintf buf "@[<hv 3>key \"%s\"%a %a@ %a@]" (**)
      v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_exp_list el
  | ObjectDefExp (v, flag, el, _) ->
    Format.fprintf buf "@[<hv 3>let %a. %a@ %a@]" (**)
      pp_print_method_name v
      pp_print_define_flag flag
      pp_print_exp_list el;
  | FunDefExp (v, vars, el, _) ->
    Format.fprintf buf "@[<hv 3>let %a(%a) =" (**)
      pp_print_params vars
      pp_print_method_name v;
    List.iter (fun e -> Format.fprintf buf "@ %a" pp_print_exp e) el;
    Format.fprintf buf "@]"
  | RuleExp (multiple, target, pattern, source, commands, _) ->
    Format.fprintf buf "@[<hv 0>@[<hv 3>rule {@ multiple = %b;@ @[<hv 3>target =@ %a;@]@ @[<hv 3>pattern =@ %a;@]@ @[<hv 3>source =@ %a@]@ %a@]@ }@]" (**)
      multiple
      pp_print_exp target
      pp_print_exp pattern
      pp_print_table_exp source
      pp_print_exp_list commands
  | BodyExp (body, _) ->
    Format.fprintf buf "@[<v 3>body";
    List.iter (fun e -> Format.fprintf buf "@ %a" pp_print_exp e) body;
    Format.fprintf buf "@]"
  | ShellExp (e, _) ->
    Format.fprintf buf "@[<hv 3>shell %a@]" pp_print_exp e
  | CatchExp (name, v, body, _) ->
    Format.fprintf buf "@[<v 3>catch %a(%a)@ %a@]" (**)
      pp_print_symbol name
      pp_print_symbol v
      pp_print_exp_list body
  | ClassExp (names, _) ->
    Format.fprintf buf "@[<hv 3>class";
    List.iter (fun v -> Format.fprintf buf "@ %a" pp_print_symbol v) names;
    Format.fprintf buf "@]"

(*
 * Parameters.
 *)
and pp_print_param buf param =
  match (param : Omake_ast.param) with 
  | OptionalParam (v, e, _) ->
    Format.fprintf buf "@[<hv 3>?%a =@ %a@]" pp_print_symbol v pp_print_exp e
  | RequiredParam (v, _) ->
    Format.fprintf buf "~%a" pp_print_symbol v
  | NormalParam (v, _) ->
    pp_print_symbol buf v

and pp_print_params buf vars =
  match vars with
  | [v] ->
    pp_print_param buf v
  | v :: vars ->
    Format.fprintf buf "%a,@ " pp_print_param v;
    pp_print_params buf vars
  | [] ->
    ()

and pp_print_arrow_arg buf params e =
  Format.fprintf buf "@[<hv 3>%a =>@ %a@]" pp_print_params params pp_print_exp e

and pp_print_arg buf (arg :  Omake_ast.arg) = 
  match arg with 
  | KeyArg (v, e) ->
    Format.fprintf buf "@[<hv 3>~%a =@ %a@]" pp_print_symbol v pp_print_exp e
  | ExpArg e ->
    pp_print_exp buf e
  | ArrowArg (params, e) ->
    pp_print_arrow_arg buf params e

and pp_print_args buf (args : Omake_ast.arg list) =
  match args with
  | [arg] ->
    pp_print_arg buf arg
  | arg :: args ->
    pp_print_arg buf arg;
    Format.fprintf buf ",@ ";
    pp_print_args buf args
  | [] ->
    ()

and pp_print_exp_list buf commands =
  List.iter (fun e -> Format.fprintf buf "@ %a" pp_print_exp e) commands

(* and pp_print_exp_option buf e_opt = *)
(*   match e_opt with *)
(*   | Some e -> pp_print_exp buf e *)
(*   | None -> Format.pp_print_string buf "<none>" *)

and pp_print_table_exp buf source =
  Format.fprintf buf "@[<hv 0>@[<hv 3>{";
  SymbolTable.iter (fun v e ->
      Format.fprintf buf "@ %a = %a" pp_print_symbol v pp_print_exp e) source;
  Format.fprintf buf "@]@ }@]"

(*
 * A program is a list of expressions.
 *)
let pp_print_prog buf prog =
  Format.fprintf buf "@[<v 0>Prog:";
  List.iter (fun e -> Format.fprintf buf "@ %a" pp_print_exp e) prog;
  Format.fprintf buf "@]"

(*
 * Simplified printing.
 *)
let rec pp_print_simple_exp buf (e : Omake_ast.exp) =
  if !print_location then
    Format.fprintf buf "<%a>" pp_print_location (Omake_ast_util.loc_of_exp e);
  match e with
  | NullExp _ -> Format.pp_print_string buf "<null>"
  | IntExp (i, _) ->
    Format.fprintf buf "%d" i
  | FloatExp (x, _) ->
    Format.fprintf buf "%f" x
  | StringOpExp (s, _)
  | StringIdExp (s, _)
  | StringIntExp (s, _)
  | StringFloatExp (s, _)
  | StringWhiteExp (s, _)
  | StringOtherExp (s, _)
  | StringKeywordExp (s, _) ->    Format.pp_print_string buf s
  | QuoteExp (el, _) ->
    Format.fprintf buf "$'%a'" pp_print_simple_exp_list el
  | QuoteStringExp (c, el, _) ->
    Format.fprintf buf "%c%a%c" c pp_print_simple_exp_list el c
  | SequenceExp (el, _) ->
    pp_print_simple_exp_list buf el
  | ArrayExp (el, _) ->
    Format.fprintf buf "@[<hv 3>(array";
    List.iter (fun e ->
        Format.fprintf buf "@ %a" pp_print_exp e) el;
    Format.fprintf buf ")@]"
  | ApplyExp (LazyApply, v, [], _) ->
    Format.fprintf buf "$%a" pp_print_symbol v
  | ApplyExp (s, v, args, _) ->
    Format.fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
      pp_print_strategy s
      pp_print_symbol v
      pp_print_simple_args args
  | SuperApplyExp (s, super, v, args, _) ->
    Format.fprintf buf "@[<hv 3>%a%a::%a(%a)@]" (**)
      pp_print_symbol super
      pp_print_strategy s
      pp_print_symbol v
      pp_print_simple_args args
  | MethodApplyExp (s, vl, args, _) ->
    Format.fprintf buf "@[<hv 3>%a%a(%a)@]" (**)
      pp_print_method_name vl
      pp_print_strategy s
      pp_print_simple_args args
  | CommandExp (v, arg, commands, _) ->
    Format.fprintf buf "@[<hv 0>@[<hv 3>command %a(%a) {%a@]@ }@]" (**)
      pp_print_symbol v
      pp_print_simple_exp arg
      pp_print_simple_exp_list commands
  | VarDefExp (v, kind, flag, e, _) ->
    Format.fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
      pp_print_method_name v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_simple_exp e
  | VarDefBodyExp (v, kind, flag, el, _) ->
    Format.fprintf buf "@[<hv 3>let %a%a %a@ %a@]" (**)
      pp_print_method_name v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_simple_exp_list el
  | KeyExp (strategy, v, _) ->
    Format.fprintf buf "$%a|%s|" pp_print_strategy strategy v
  | KeyDefExp (v, kind, flag, e, _) ->
    Format.fprintf buf "@[<hv 3>\"%s\"%a %a@ %a@]" (**)
      v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_simple_exp e
  | KeyDefBodyExp (v, kind, flag, el, _) ->
    Format.fprintf buf "@[<hv 3>key \"%s\"%a %a@ %a@]" (**)
      v
      pp_print_define_kind kind
      pp_print_define_flag flag
      pp_print_simple_exp_list el
  | ObjectDefExp (v, flag, el, _) ->
    Format.fprintf buf "@[<hv 3>let %a. %a@ %a@]" (**)
      pp_print_method_name v
      pp_print_define_flag flag
      pp_print_simple_exp_list el
  | FunDefExp (v, vars, el, _) ->
    Format.fprintf buf "@[<hv 3>let %a(%a) =" (**)
      pp_print_params vars
      pp_print_method_name v;
    List.iter (fun e -> Format.fprintf buf "@ %a" pp_print_exp e) el;
    Format.fprintf buf "@]"
  | RuleExp (multiple, target, pattern, source, commands, _) ->
    Format.fprintf buf "@[<hv 0>@[<hv 3>rule {@ multiple = %b;@ @[<hv 3>target =@ %a;@]@ @[<hv 3>pattern =@ %a;@]@ @[<hv 3>source =@ %a@]@ %a@]@ }@]" (**)
      multiple
      pp_print_simple_exp target
      pp_print_simple_exp pattern
      pp_print_table_exp source
      pp_print_simple_exp_list commands
  | BodyExp (body, _) ->
    Format.fprintf buf "@[<v 3>body";
    List.iter (fun e -> Format.fprintf buf "@ %a" pp_print_simple_exp e) body;
    Format.fprintf buf "@]"
  | ShellExp (e, _) ->
    Format.fprintf buf "@[<hv 3>shell %a@]" pp_print_simple_exp e
  | CatchExp (name, v, body, _) ->
    Format.fprintf buf "@[<v 3>catch %a(%a)@ %a@]" (**)
      pp_print_symbol name
      pp_print_symbol v
      pp_print_simple_exp_list body
  | ClassExp (names, _) ->
    Format.fprintf buf "@[<hv 3>class";
    List.iter (fun v -> Format.fprintf buf "@ %a" pp_print_symbol v) names;
    Format.fprintf buf "@]"

and pp_print_simple_exp_list buf el =
  List.iter (pp_print_simple_exp buf) el

and pp_print_simple_args buf args =
  match args with
  | [arg] ->
    pp_print_simple_arg buf arg
  | arg :: args ->
    pp_print_simple_arg buf arg;
    Format.fprintf buf ",@ ";
    pp_print_simple_args buf args
  | [] ->
    ()

and pp_print_simple_arg buf (arg :  Omake_ast.arg) = 
  match arg with 
  | KeyArg (v, e) ->
    Format.fprintf buf "@[<hv 3>~%a =@ %a@]" pp_print_symbol v pp_print_exp e
  | ExpArg e ->
    pp_print_simple_exp buf e
  | ArrowArg (params, e) ->
    pp_print_arrow_arg buf params e
