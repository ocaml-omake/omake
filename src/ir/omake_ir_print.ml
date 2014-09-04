let print_location = Omake_ast_print.print_location

let string_override s pp_fun complete buf arg =
   if complete then
      pp_fun true buf arg
   else
      Lm_printf.pp_print_string buf s

(*
 * Match kind.
 *)
(* let pp_print_match_kind out kind = *)
(*    let s = *)
(*       match kind with *)
(*          Omake_ir.MatchWild -> "switch" *)
(*        | MatchRegex -> "match" *)
(*    in *)
(*       Lm_printf.pp_print_string out s *)

(*
 * Arities.
 *)
let pp_print_arity buf arity =
  match arity with
  | Omake_ir.ArityRange (lower, upper) ->
    Format.fprintf buf "%d..%d" lower upper
  | ArityExact i ->
    Lm_printf.pp_print_int buf i
  | ArityAny ->
    Lm_printf.pp_print_string buf "varargs"
  | ArityNone ->
    Lm_printf.pp_print_int buf 0

(*
 * Print a list of symbols.
 *)
(* let rec pp_print_symbol_list buf sl = *)
(*    match sl with *)
(*       [s] -> *)
(*          Lm_symbol.pp_print_symbol buf s *)
(*     | [] -> *)
(*          () *)
(*     | s :: sl -> *)
(*          Format.fprintf buf "%a, %a" Lm_symbol.pp_print_symbol s pp_print_symbol_list sl *)

(*
 * Print a variable definition kind.
 *)
let pp_print_var_def_kind buf kind =
  let s =
    match kind with
    | Omake_ir.VarDefNormal ->
      "="
    | VarDefAppend ->
      "+=" in
  Lm_printf.pp_print_string buf s

(*
 * Scope.
 *)
let pp_print_var_scope buf kind =
  let s =
    match kind with
      Omake_ir.VarScopePrivate   -> "private."
    | VarScopeThis      -> "this."
    | VarScopeVirtual   -> "public."
    | VarScopeGlobal    -> "global."
  in
  Lm_printf.pp_print_string buf s

(*
 * Variables.
 *)
let pp_print_var_info buf v =
  match v with
    Omake_ir.VarPrivate (_, v) ->
    Format.fprintf buf "private.%a" (**)
      Lm_symbol.pp_print_symbol v
  | VarThis (_, v) ->
    Format.fprintf buf "this.%a" (**)
      Lm_symbol.pp_print_symbol v
  | VarVirtual (_, v) ->
    Format.fprintf buf "public.%a" (**)
      Lm_symbol.pp_print_symbol v
  | VarGlobal (_, v) ->
    Format.fprintf buf "global.%a" (**)
      Lm_symbol.pp_print_symbol v

let pp_print_param = pp_print_var_info

(*
 * Print the export info.
 *)
let pp_print_export_item buf item =
  match item with
    Omake_ir.ExportRules ->
    Lm_printf.pp_print_string buf ".RULE"
  | ExportPhonies ->
    Lm_printf.pp_print_string buf ".PHONY"
  | ExportVar v ->
    pp_print_var_info buf v

let rec pp_print_export_items buf items =
   match items with
      [item] ->
         pp_print_export_item buf item
    | item :: items ->
         Format.fprintf buf "%a@ %a" pp_print_export_item item pp_print_export_items items
    | [] ->
         ()

let pp_print_export_info buf info =
  match info with
    Omake_ir.ExportNone ->
    ()
  | ExportAll ->
    Format.fprintf buf "@ export <all>"
  | ExportList items ->
    Format.fprintf buf "@ @[<b 3>export %a@]" pp_print_export_items items

(*
 * Return identifiers.
 *)
let pp_print_return_id buf (loc, s) =
   Format.fprintf buf "%s (%a)" s Lm_location.pp_print_location loc

(*
 * Print a string expression.
 *)
let rec pp_print_string_exp complete buf s =
  match s with
    Omake_ir.NoneString _ ->
    Format.fprintf buf "<none>"
  | IntString (_, i) ->
    Format.fprintf buf "%d" i
  | FloatString (_, x) ->
    Format.fprintf buf "%g" x
  | WhiteString (_, s) ->
    Format.fprintf buf "'%s'" (String.escaped s)
  | ConstString (_, s) ->
    Format.fprintf buf "\"%s\"" (String.escaped s)
  | KeyApplyString (_, s) ->
    Format.fprintf buf "$|%s|" s
  | FunString (_, opt_params, params, e, export) ->
    Format.fprintf buf "@[<hv 3>(fun %a =>@ %a%a)@]" (**)
      (pp_print_all_params complete) (opt_params, params)
      (pp_print_exp_list complete) e
      pp_print_export_info export
  | ApplyString (_, v, [], []) ->
    Format.fprintf buf "@[<hv 3>$(%a)@]" (**)
      pp_print_var_info v
  | VarString (_, v) ->
    Format.fprintf buf "`%a" pp_print_var_info v
  | ApplyString (_, v, args, kargs) ->
    Format.fprintf buf "@[<hv 3>$(%a %a)@]" (**)
      pp_print_var_info v
      (pp_print_args complete) (args, kargs)
  | SuperApplyString (_, super, v, [], []) ->
    Format.fprintf buf "@[<hv 3>$(%a::%a)@]" (**)
      Lm_symbol.pp_print_symbol super
      Lm_symbol.pp_print_symbol v
  | SuperApplyString (_, super, v, args, kargs) ->
    Format.fprintf buf "@[<hv 3>$(%a::%a %a)@]" (**)
      Lm_symbol.pp_print_symbol super
      Lm_symbol.pp_print_symbol v
      (pp_print_args complete) (args, kargs)
  | MethodApplyString (_, v, vl, [], []) ->
    Format.fprintf buf "@[<hv 3>$(%a.%a)@]" (**)
      pp_print_var_info v
      Omake_print_util.pp_print_method_name vl
  | MethodApplyString (_, v, vl, args, kargs) ->
    Format.fprintf buf "@[<hv 3>$(%a.%a %a)@]" (**)
      pp_print_var_info v
      Omake_print_util.pp_print_method_name vl
      (pp_print_args complete) (args, kargs)
  | SequenceString (_, sl) ->
    Format.fprintf buf "@[<hv 1>(%a)@]" (**)
      (pp_print_string_exp_list complete) sl
  | ArrayOfString (_, s) ->
    Format.fprintf buf "@[<hv 1>(array-of-string@ %a)@]" (**)
      (pp_print_string_exp complete) s
  | ArrayString (_, sl) ->
    Format.fprintf buf "@[<hv 1>[|%a|]@]" (**)
      (pp_print_string_exp_list complete) sl
  | QuoteString (_, sl) ->
    Format.fprintf buf "@[<hv 1>(quote %a)@]" (**)
      (pp_print_string_exp_list complete) sl
  | QuoteStringString (_, c, sl) ->
    Format.fprintf buf "@[<hv 1>(quote %c%a%c)@]" (**)
      c (pp_print_string_exp_list complete) sl c
  | ObjectString (_, e, export) ->
    if complete then
      Format.fprintf buf "@[<hv 3>object@ %a%a@]" (**)
        (pp_print_exp_list complete) e
        pp_print_export_info export
    else
      Lm_printf.pp_print_string buf "<object...>"
  | BodyString (_, e, export) ->
    if complete then
      Format.fprintf buf "@[<hv 3>body@ %a%a@]" (**)
        (pp_print_exp_list complete) e
        pp_print_export_info export
    else
      Lm_printf.pp_print_string buf "<body...>"
  | ExpString (_, e, export) ->
    if complete then
      Format.fprintf buf "@[<hv 3>exp@ %a%a@]" (**)
        (pp_print_exp_list complete) e
        pp_print_export_info export
    else
      Lm_printf.pp_print_string buf "<exp...>"
  | CasesString (_, cases) ->
    if complete then begin
      Format.fprintf buf "@[<hv 3>cases:";
      List.iter (fun (v, e1, e2, export) ->
        Format.fprintf buf "@ @[<hv 3>%a %a:@ %a%a@]" (**)
          Lm_symbol.pp_print_symbol v
          (pp_print_string_exp complete) e1
          (pp_print_exp_list complete) e2
          pp_print_export_info export) cases;
      Format.fprintf buf "@]"
    end else
      Lm_printf.pp_print_string buf "<cases...>"
  | ThisString _ ->
    Lm_printf.pp_print_string buf "$<this>"
  | LazyString (_, e) ->
    Format.fprintf buf "$`[%a]" (pp_print_string_exp complete) e
  | LetVarString (_, v, e1, e2) ->
    Format.fprintf buf "@[<hv 2>let %a = %a in@ %a@]" (**)
      pp_print_var_info v
      (pp_print_string_exp complete) e1
      (pp_print_string_exp complete) e2

and pp_print_string_exp_list complete buf sl =
  match sl with
    [s] ->
    pp_print_string_exp complete buf s
  | [] ->
    ()
  | s :: sl ->
    Format.fprintf buf "%a,@ %a" (pp_print_string_exp complete) s (pp_print_string_exp_list complete) sl

(*
 * Print a list of symbols.
 *)
and pp_print_curry buf flag =
  if flag then
    Lm_printf.pp_print_string buf "curry."

and pp_print_params_inner buf sl =
  match sl with
    [v] ->
    pp_print_param buf v
  | [] ->
    ()
  | v :: sl ->
    Format.fprintf buf "%a, " pp_print_param v;
    pp_print_params_inner buf sl

and pp_print_params buf sl =
  pp_print_params_inner buf sl

and pp_print_keyword_param complete buf param =
  match param with
    (v1, v2, Some s) ->
    Format.fprintf buf "@[<hv 3>?%a (%a) =@ %a@]" Lm_symbol.pp_print_symbol v1 pp_print_param v2 (pp_print_string_exp complete) s
  | (v1, v2, None) ->
    Format.fprintf buf "@[<hv 3>~%a (%a)@]" Lm_symbol.pp_print_symbol v1 pp_print_param v2

and pp_print_keyword_params complete buf params =
  match params with
    [p] ->
    pp_print_keyword_param complete buf p
  | p :: params ->
    Format.fprintf buf "%a,@ " (pp_print_keyword_param complete) p;
    pp_print_keyword_params complete buf params
  | [] ->
    ()

and pp_print_all_params complete buf = function
    [], params -> pp_print_params buf params
  | opt_params, [] -> pp_print_keyword_params complete buf opt_params
  | opt_params, params -> Format.fprintf buf "%a,@ %a" (pp_print_keyword_params complete) opt_params pp_print_params params

and pp_print_normal_args complete buf first args =
  match args with
    arg :: args ->
    if not first then
      Format.fprintf buf ",@ ";
    pp_print_string_exp complete buf arg;
    pp_print_normal_args complete buf false args
  | [] ->
    first

and pp_print_keyword_args complete buf first args =
  match args with
    (v, arg) :: args ->
    if not first then
      Format.fprintf buf ",@ ";
    Format.fprintf buf "%a =@ %a" Lm_symbol.pp_print_symbol v (pp_print_string_exp complete) arg;
    pp_print_keyword_args complete buf false args
  | [] ->
    ()

and pp_print_args complete buf (args, kargs) =
  pp_print_keyword_args complete buf (pp_print_normal_args complete buf true args) kargs

(*
 * Print an expression.
 *)
and pp_print_exp complete buf e =
  if complete && !print_location then
    Format.fprintf buf "<%a>" Lm_location.pp_print_location (Omake_ir_util.loc_of_exp e);
  match e with
    LetVarExp (_, v, vl, kind, s) ->
    Format.fprintf buf "@[<hv 3>%a%a %a@ %a@]" (**)
      pp_print_var_info v
      Omake_print_util.pp_print_method_name vl
      pp_print_var_def_kind kind
      (pp_print_string_exp complete) s
  | LetFunExp (_, v, vl, curry, opt_params, params, el, export) ->
    Format.fprintf buf "@[<hv 3>%a%a%a(%a) =@ %a%a@]" (**)
      pp_print_curry curry
      pp_print_var_info v
      Omake_print_util.pp_print_method_name vl
      (pp_print_all_params complete) (opt_params, params)
      (string_override "<...>" pp_print_exp_list complete) el
      pp_print_export_info export
  | LetObjectExp (_, v, vl, s, el, export) ->
    Format.fprintf buf "@[<v 3>%a%a. =@ extends %a@ %a%a@]" (**)
      pp_print_var_info v
      Omake_print_util.pp_print_method_name vl
      (pp_print_string_exp complete) s
      (string_override "<...>" pp_print_exp_list complete) el
      pp_print_export_info export
  | LetThisExp (_, e) ->
    Format.fprintf buf "@[<hv 3><this> =@ %a@]" (pp_print_string_exp complete) e
  | ShellExp (_, e) ->
    Format.fprintf buf "@[<hv 3>shell(%a)@]" (pp_print_string_exp complete) e
  | IfExp (_, cases) ->
    if complete then begin
      Format.fprintf buf "@[<hv 0>if";
      List.iter (fun (s, el, export) ->
        Format.fprintf buf "@ @[<hv 3>| %a ->@ %a%a@]" (**)
          (pp_print_string_exp complete) s
          (pp_print_exp_list complete) el
          pp_print_export_info export) cases;
      Format.fprintf buf "@]"
    end else
      Lm_printf.pp_print_string buf "<if ... then ... [else ...]>"
  | SequenceExp (_, el) ->
    Format.fprintf buf "@[<hv 3>sequence@ %a@]" (**)
      (pp_print_exp_list complete) el
  | SectionExp (_, s, el, export) ->
    Format.fprintf buf "@[<hv 3>section %a@ %a%a@]" (**)
      (pp_print_string_exp complete) s
      (string_override "<...>" pp_print_exp_list complete) el
      pp_print_export_info export
  | OpenExp (_, nodes) ->
    Format.fprintf buf "@[<hv 3>open";
    List.iter (fun node -> Format.fprintf buf "@ %a" Omake_node.pp_print_node node) nodes;
    Format.fprintf buf "@]"
  | IncludeExp (_, s, commands) ->
    Format.fprintf buf "@[<hv 3>include %a:%a@]" (**)
      (pp_print_string_exp complete) s
      (pp_print_commands complete) commands
  | ApplyExp (_, v, args, kargs) ->
    Format.fprintf buf "@[<hv 3>%a(%a)@]" (**)
      pp_print_var_info v
      (pp_print_args complete) (args, kargs)
  | SuperApplyExp (_, super, v, args, kargs) ->
    Format.fprintf buf "@[<hv 0>%a::%a(%a)@]" (**)
      Lm_symbol.pp_print_symbol super
      Lm_symbol.pp_print_symbol v
      (pp_print_args complete) (args, kargs)
  | MethodApplyExp (_, v, vl, args, kargs) ->
    Format.fprintf buf "@[<hv 3>%a.%a(%a)@]" (**)
      pp_print_var_info v
      Omake_print_util.pp_print_method_name vl
      (pp_print_args complete) (args, kargs)
  | ReturnBodyExp (_, el, id) ->
    Format.fprintf buf "@[<hv 3>return-body %a@ %a@]" (**)
      pp_print_return_id id
      (string_override "<...>" pp_print_exp_list complete) el
  | StringExp (_, s) ->
    Format.fprintf buf "string(%a)" (pp_print_string_exp complete) s
  | ReturnExp (_, s, id) ->
    Format.fprintf buf "return(%a) from %a" (**)
      (pp_print_string_exp complete) s
      pp_print_return_id id
  | ReturnSaveExp _ ->
    Lm_printf.pp_print_string buf "return-current-file"
  | ReturnObjectExp (_, names) ->
    Format.fprintf buf "@[<b 3>return-current-object";
    List.iter (fun v -> Format.fprintf buf "@ %a" Lm_symbol.pp_print_symbol v) names;
    Format.fprintf buf "@]"
  | KeyExp (_, v) ->
    Format.fprintf buf "$|%s|" v
  | LetKeyExp (_, v, kind, s) ->
    Format.fprintf buf "@[<hv 3>$|%s| %a@ %a@]" (**)
      v
      pp_print_var_def_kind kind
      (pp_print_string_exp complete) s
  | StaticExp (_, node, key, el) ->
    Format.fprintf buf "@[<hv 3>static(%a.%a):@ %a@]" (**)
      Omake_node.pp_print_node node
      Lm_symbol.pp_print_symbol key
      (string_override "<...>" pp_print_exp_list complete) el

and pp_print_exp_list complete buf el =
  match el with
    [e] ->
    pp_print_exp complete buf e
  | e :: el ->
    pp_print_exp complete buf e;
    Lm_printf.pp_print_space buf ();
    pp_print_exp_list complete buf el
  | [] ->
    ()

and pp_print_commands complete buf el =
  List.iter (fun e -> Format.fprintf buf "@ %a" (pp_print_string_exp complete) e) el

(*
 * Print simple parts, abbreviating others as "<exp>"
 *)
let pp_print_exp_list_simple = pp_print_exp_list false

(*
 * The complete printers.
 *)
let pp_print_exp = pp_print_exp true
let pp_print_string_exp = pp_print_string_exp true
let pp_print_string_exp_list = pp_print_string_exp_list true

let pp_print_exp_list = pp_print_exp_list true

(* let pp_print_prog buf el = *)
(*    Format.fprintf buf "@[<v 0>%a@]" pp_print_exp_list el *)

