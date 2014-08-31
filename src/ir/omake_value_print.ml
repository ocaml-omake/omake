
(************************************************************************
 * Simple printing.
 *)
let pp_print_string_list buf sl =
   List.iter (fun s -> Format.fprintf buf "@ %s" s) sl

let pp_print_node_list buf l =
   List.iter (fun s -> Format.fprintf buf "@ %a" Omake_node.pp_print_node s) l

let pp_print_node_set buf set =
   Omake_node.NodeSet.iter (fun s -> Format.fprintf buf "@ %a" Omake_node.pp_print_node s) set

let pp_print_wild_list buf wl =
   List.iter (fun w -> Format.fprintf buf "@ %a" Omake_wild.pp_print_wild_in w) wl

let pp_print_source buf (_, source) =
  match source with
  | Omake_value_type.SourceWild wild ->
    Omake_wild.pp_print_wild_out buf wild
  | SourceNode node ->
    Omake_node.pp_print_node buf node

let pp_print_source_list buf sources =
   List.iter (fun source -> Format.fprintf buf "@ %a" pp_print_source source) sources

let pp_print_target buf target =
   match target with
   | Omake_value_type.TargetNode node ->
     Omake_node.pp_print_node buf node
   | TargetString s ->
     Lm_printf.pp_print_string buf s

let pp_print_required buf b =
   if b then
      Lm_printf.pp_print_char buf '~'
   else
      Lm_printf.pp_print_char buf '?'

(************************************************************************
 * Path printing.
 *)
let rec pp_print_path buf = function
  | Omake_value_type.PathVar info ->
    Omake_ir_print.pp_print_var_info buf info
  | PathField (path, _obj, v) ->
    Format.fprintf buf "%a.%a" Lm_symbol.pp_print_symbol v pp_print_path path

(************************************************************************
 * Arity approximation.
 *)
(*
 * XXX: TODO: currently keyword args are ignored, we should probably include
 * them, and also return an ArityRange when some keyword arguments have a default
 * value defined. See also Bugzilla bug 731.
 *)
let fun_arity _keywords params =
   Omake_ir.ArityExact (List.length params)

let curry_fun_arity curry_args _keywords params _curry_kargs =
   Omake_ir.ArityExact ((List.length params) - (List.length curry_args))

(************************************************************************
 * Value printing.
 *)
let rec pp_print_value buf v =
  match v with
    Omake_value_type.ValNone ->
    Lm_printf.pp_print_string buf "<none>"
  | ValInt i ->
    Format.fprintf buf "%d : Int" i
  | ValFloat x ->
    Format.fprintf buf "%g : Float" x
  | ValData s ->
    Format.fprintf buf "@[<v 3><data \"%s\"> : String@]" (String.escaped s)
  | ValQuote vl ->
    Format.fprintf buf "@[<v 3><string%a>@ : String@]" pp_print_value_list vl
  | ValWhite s ->
    Format.fprintf buf "'%s' : White" (String.escaped s)
  | ValString s ->
    Format.fprintf buf "\"%s\" : Sequence" (String.escaped s)
  | ValQuoteString (c, vl) ->
    Format.fprintf buf "@[<v 3><string %c%a%c>@ : String@]" c pp_print_value_list vl c
  | ValSequence [v] ->
    pp_print_value buf v
  | ValSequence vl ->
    Format.fprintf buf "@[<hv 3><sequence%a>@ : Sequence@]" pp_print_value_list vl
  | ValArray vl ->
    Format.fprintf buf "@[<v 3><array%a>@ : Array@]" pp_print_value_list vl
  | ValMaybeApply (_, v) ->
    Format.fprintf buf "@[<hv 3>ifdefined(%a)@]" (**)
      Omake_ir_print.pp_print_var_info v
  | ValFun (_, keywords, params, _, _) ->
    Format.fprintf buf "<fun %a>" Omake_ir_print.pp_print_arity (fun_arity keywords params)
  | ValFunCurry (_, curry_args, keywords, params, _, _, curry_kargs) ->
    Format.fprintf buf "<curry %a>" 
      Omake_ir_print.pp_print_arity (curry_fun_arity curry_args keywords params curry_kargs)
  | ValPrim (_, special, _, name)
  | ValPrimCurry (_, special, name, _, _) ->
    if special then
      Format.fprintf buf "<special-function %a>" Lm_symbol.pp_print_symbol name
    else
      Format.fprintf buf "<prim-function %a>" Lm_symbol.pp_print_symbol name
  | ValRules rules ->
    Format.fprintf buf "<@[<hv 3>rules:";
    List.iter (fun erule -> Format.fprintf buf "@ %a" Omake_node.pp_print_node erule) rules;
    Format.fprintf buf "@]>"
  | ValDir dir ->
    Format.fprintf buf "%a : Dir" Omake_node.pp_print_dir dir
  | ValNode node ->
    Format.fprintf buf "%a : File" Omake_node.pp_print_node node
  | ValStringExp (_, e) ->
    Format.fprintf buf "@[<hv 0>%a : Exp@]" Omake_ir_print.pp_print_string_exp e
  | ValBody (el, export) ->
    Format.fprintf buf "@[<v 0>%a%a@ : Body@]" 
      Omake_ir_print.pp_print_exp_list el Omake_ir_print.pp_print_export_info export
  | ValObject env ->
    pp_print_env buf env
  | ValMap map ->
    Format.fprintf buf "@[<hv 3>map";
    Omake_value_type.ValueTable.iter (fun v e -> Format.fprintf buf "@ %a@ = %a" pp_print_value v pp_print_value e) map;
    Format.fprintf buf "@]"
  | ValChannel (InChannel, _) ->
    Format.fprintf buf "<channel> : InChannel"
  | ValChannel (OutChannel, _) ->
    Format.fprintf buf "<channel> : OutChannel"
  | ValChannel (InOutChannel, _) ->
    Format.fprintf buf "<channel> : InOutChannel"
  | ValClass c ->
    Format.fprintf buf "@[<hv 3>class";
    Lm_symbol.SymbolTable.iter (fun v _ ->
      Format.fprintf buf "@ %a" Lm_symbol.pp_print_symbol v) c;
    Format.fprintf buf "@]"
  | ValCases cases ->
    Format.fprintf buf "@[<hv 3>cases";
    List.iter (fun (v, e1, e2, export) ->
      Format.fprintf buf "@[<hv 3>%a %a:@ %a%a@]" (**)
        Lm_symbol.pp_print_symbol v
        pp_print_value e1
        Omake_ir_print.pp_print_exp_list e2
        Omake_ir_print.pp_print_export_info export) cases;
    Format.fprintf buf "@]"
  | ValVar (_, v) ->
    Format.fprintf buf "`%a" Omake_ir_print.pp_print_var_info v
  | ValOther (ValLexer _) ->
    Format.fprintf buf "<lexer> : Lexer"
  | ValOther (ValParser _) ->
    Format.fprintf buf "<parser> : Parser"
  | ValOther (ValLocation loc) ->
    Format.fprintf buf "<location %a> : Location" Lm_location.pp_print_location loc
  | ValOther (ValExitCode code) ->
    Format.fprintf buf "<exit-code %d> : Int" code
  | ValOther (ValEnv _) ->
    Format.fprintf buf "<env>"
  | ValDelayed { contents = ValValue v } ->
    Format.fprintf buf "<delayed:value %a>" pp_print_value v
  | ValDelayed { contents = ValStaticApply (key, v) } ->
    Format.fprintf buf "<delayed:memo %a::%a>" pp_print_value key Lm_symbol.pp_print_symbol v

and pp_print_value_list buf vl =
   List.iter (fun v -> Format.fprintf buf "@ %a" pp_print_value v) vl

and pp_print_normal_args buf first args =
   match args with
      arg :: args ->
         if not first then
            Format.fprintf buf ",@ ";
         pp_print_value buf arg;
         pp_print_normal_args buf false args
    | [] ->
         first

and pp_print_keyword_args buf first kargs =
   match kargs with
      (v, arg) :: kargs ->
         if not first then
            Format.fprintf buf ",@ ";
         Format.fprintf buf "@[<hv 3>%a =@ %a@]" Lm_symbol.pp_print_symbol v pp_print_value arg;
         pp_print_keyword_args buf false kargs
    | [] ->
         ()

and pp_print_value_args buf (args, kargs) =
   pp_print_keyword_args buf (pp_print_normal_args buf true args) kargs

and pp_print_env buf env =
  let tags = Omake_value_type.venv_get_class env in
  let env = Lm_symbol.SymbolTable.remove env Omake_value_type.class_sym in
  Format.fprintf buf "@[<v 3>@[<hv 3>class";
  Lm_symbol.SymbolTable.iter 
    (fun v _ -> Format.fprintf buf "@ %a" 
        Lm_symbol.pp_print_symbol v) tags;
  Format.fprintf buf "@]";
  Lm_symbol.SymbolTable.iter (fun v e -> Format.fprintf buf "@ %a = %a" Lm_symbol.pp_print_symbol v pp_print_value e) env;
  Format.fprintf buf "@]"

(************************************************************************
 * Simplified printing.
 *)
let rec pp_print_simple_value buf v =
  match v with
    Omake_value_type.ValNone ->
    Lm_printf.pp_print_string buf "<none>"
  | ValInt i ->
    Lm_printf.pp_print_int buf i
  | ValFloat x ->
    Lm_printf.pp_print_float buf x
  | ValData s ->
    Omake_command_type.pp_print_arg buf [ArgData s]
  | ValWhite s
  | ValString s ->
    Omake_command_type.pp_print_arg buf [ArgString s]
  | ValQuote vl ->
    Format.fprintf buf "\"%a\"" pp_print_simple_value_list vl
  | ValQuoteString (c, vl) ->
    Format.fprintf buf "%c%a%c" c pp_print_simple_value_list vl c
  | ValSequence vl ->
    pp_print_simple_value_list buf vl
  | ValArray vl ->
    pp_print_simple_arg_list buf vl
  | ValMaybeApply (_, v) ->
    Format.fprintf buf "$?(%a)" (**)
      Omake_ir_print.pp_print_var_info v
  | ValFun _ ->
    Lm_printf.pp_print_string buf "<fun>"
  | ValFunCurry _ ->
    Lm_printf.pp_print_string buf "<curry>"
  | ValPrim _
  | ValPrimCurry _ ->
    Lm_printf.pp_print_string buf "<prim>"
  | ValRules _ ->
    Lm_printf.pp_print_string buf "<rules>"
  | ValDir dir ->
    Omake_node.pp_print_dir buf dir
  | ValNode node ->
    Omake_node.pp_print_node buf node
  | ValStringExp _ ->
    Lm_printf.pp_print_string buf "<exp>"
  | ValBody _ ->
    Lm_printf.pp_print_string buf "<body>"
  | ValObject _ ->
    Lm_printf.pp_print_string buf "<object>"
  | ValMap _ ->
    Lm_printf.pp_print_string buf "<map>"
  | ValChannel _ ->
    Lm_printf.pp_print_string buf "<channel>"
  | ValClass _ ->
    Lm_printf.pp_print_string buf "<class>"
  | ValCases _ ->
    Lm_printf.pp_print_string buf "<cases>"
  | ValVar (_, v) ->
    Format.fprintf buf "`%a" Omake_ir_print.pp_print_var_info v
  | ValOther (ValLexer _) ->
    Lm_printf.pp_print_string buf "<lexer>"
  | ValOther (ValParser _) ->
    Lm_printf.pp_print_string buf "<parser>"
  | ValOther (ValLocation _) ->
    Lm_printf.pp_print_string buf "<location>"
  | ValOther (ValExitCode i) ->
    Lm_printf.pp_print_int buf i
  | ValOther (ValEnv _) ->
    Lm_printf.pp_print_string buf "<env>"
  | ValDelayed { contents = ValValue v } ->
    pp_print_simple_value buf v
  | ValDelayed { contents = ValStaticApply _ } ->
    Lm_printf.pp_print_string buf "<static>"

and pp_print_simple_value_list buf vl =
   List.iter (pp_print_simple_value buf) vl

and pp_print_simple_arg_list buf vl =
  match vl with
  | [] -> ()
  | [v] ->
    pp_print_simple_value buf v
  | v :: vl ->
    pp_print_simple_value buf v;
    Lm_printf.pp_print_char buf ' ';
    pp_print_simple_arg_list buf vl

