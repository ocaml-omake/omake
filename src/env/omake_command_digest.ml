(*
 * Compute the digest of a value.  This works the naive way:
 *    1. Convert the value to a string
 *    2. Compute its MD5 digest
 * This can be fairly expensive if the value is big.  The
 * current implementation is designed so that we can at least
 * compress the string a bit.
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

 | CodeExportNone
 | CodeExportAll
 | CodeExportList
 | CodeExportRules
 | CodeExportPhonies
 | CodeExportVar

 | CodeReturnBodyExp
 | CodeStringExp
 | CodeReturnExp
 | CodeReturnObjectExp
 | CodeReturnSaveExp
 (* | CodeVarScopePrivate *) (* report Mantis location incorrect {|File "/Users/hongbozhang/omake-fork/src/env/omake_command_digest.ml", line 12, characters 5-2211:
Warning 37: unused constructor CodeVarScopePrivate.
|}*)
 (* | CodeVarScopeThis *)
 (* | CodeVarScopeVirtual *)
 (* | CodeVarScopeGlobal *)
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


 | CodeValStaticApply
 | CodeArg
 | CodeArgString
 | CodeArgData

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
 (* | CodeQuietFlag *)
 (* | CodeAllowFailureFlag *)
 (* | CodeAllowOutputFlag *)
 (* | CodeCommandFlags *)
 | CodeCmdArg
 | CodeCmdNode
 | CodePipe
 | CodeRedirectNode
 | CodeRedirectArg
 | CodeRedirectNone
 (* | CodeKeywordSpec *)
 (* | CodeKeywordSpecList *)
 | CodeNone
 | CodeSome
 | CodeLazyString
 | CodeLetVarString
(* %%MAGICEND%% *)


(* %%MAGICBEGIN%% *)
module Hash = Lm_hash_code.HashDigest
let add_code buf (code : code) =
  Hash.add_bits buf (Obj.magic code)
(* %%MAGICEND%% *)

(*
 * Variable squashing.
 *)
let squash_var buf v =
   Hash.add_string buf (Lm_symbol.string_of_symbol v)

let rec squash_vars buf vars =
   match vars with
      [v] ->
         squash_var buf v
    | v :: vars ->
         squash_var buf v;
         add_code buf CodeSpace;
         squash_vars buf vars
    | [] ->
         ()

(* let squash_var_set buf vars = *)
(*    Lm_symbol.SymbolSet.iter (fun v -> *)
(*          add_code buf CodeSpace; *)
(*          squash_var buf v) vars *)

let squash_var_info buf v =
   match v with
      Omake_ir.VarPrivate (_, v) ->
         add_code buf CodeVarPrivate;
         add_code buf CodeSpace;
         squash_var buf v
    | VarThis (_, v) ->
         add_code buf CodeVarThis;
         add_code buf CodeSpace;
         squash_var buf v
    | VarVirtual (_, v) ->
         add_code buf CodeVarVirtual;
         add_code buf CodeSpace;
         squash_var buf v
    | VarGlobal (_, v) ->
         add_code buf CodeVarGlobal;
         add_code buf CodeSpace;
         squash_var buf v

let rec squash_var_info_list buf vars =
   match vars with
      [v] ->
         squash_var_info buf v
    | v :: vars ->
         squash_var_info buf v;
         add_code buf CodeSpace;
         squash_var_info_list buf vars
    | [] ->
         ()

let squash_params = squash_var_info_list

(* let squash_keyword_spec buf (v, required) = *)
(*    add_code buf CodeKeywordSpec; *)
(*    squash_var buf v; *)
(*    Hash.add_bool buf required *)

(* let squash_keyword_spec_list buf keywords = *)
(*    add_code buf CodeKeywordSpecList; *)
(*    List.iter (squash_keyword_spec buf) keywords *)

(*
 * File.
 *)
let squash_node buf node =
   Hash.add_string buf (Omake_node.Node.absname node)

(*
 * String representations.
 *)
(* let squash_var_scope buf scope = *)
(*    let code = *)
(*       match scope with *)
(*          Omake_ir.VarScopePrivate -> CodeVarScopePrivate *)
(*        | VarScopeThis    -> CodeVarScopeThis *)
(*        | VarScopeVirtual -> CodeVarScopeVirtual *)
(*        | VarScopeGlobal  -> CodeVarScopeGlobal *)
(*    in *)
(*       add_code buf code *)

let squash_def_kind buf kind =
   let s =
      match kind with
         Omake_ir.VarDefNormal ->
            CodeVarDefNormal
       | VarDefAppend ->
            CodeVarDefApply
   in
      add_code buf s

(*
 * Export info.
 *)
let squash_export_info buf info =
   match info with
      Omake_ir.ExportNone ->
         add_code buf CodeExportNone
    | Omake_ir.ExportAll ->
         add_code buf CodeExportAll
    | Omake_ir.ExportList items ->
         add_code buf CodeExportList;
         List.iter (fun item ->
               match item with
                  Omake_ir.ExportRules ->
                     add_code buf CodeExportRules
                | Omake_ir.ExportPhonies ->
                     add_code buf CodeExportPhonies
                | Omake_ir.ExportVar v ->
                     add_code buf CodeExportVar;
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
   add_code buf CodeBegin;
   begin
      match e with
         Omake_ir.NoneString _ ->
            add_code buf CodeNoneString
       | IntString (_, i) ->
            add_code buf CodeIntString;
            Hash.add_int buf i
       | FloatString (_, x) ->
            add_code buf CodeFloatString;
            Hash.add_float buf x
       | WhiteString (_, s) ->
            add_code buf CodeWhiteString;
            Hash.add_string buf s
       | ConstString (_, s) ->
            add_code buf CodeConstString;
            Hash.add_string buf s
       | VarString (_, v) ->
            add_code buf CodeVarString;
            squash_var_info buf v
       | KeyApplyString (_, s) ->
            add_code buf CodeKeyApplyString;
            Hash.add_string buf s
       | FunString (_, opt_params, params, s, export) ->
            add_code buf CodeFunString;
            squash_params buf params;
            add_code buf CodeArrow;
            squash_keyword_param_list pos buf opt_params;
            add_code buf CodeArrow;
            squash_exp_list pos buf s;
            add_code buf CodeSpace;
            squash_export_info buf export
       | ApplyString (_, v, args, kargs) ->
            add_code buf CodeApplyString;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | SuperApplyString (_, v1, v2, args, kargs) ->
            add_code buf CodeSuperApplyString;
            squash_var buf v1;
            add_code buf CodeSpace;
            squash_var buf v2;
            add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | MethodApplyString (_, v, vars, args, kargs) ->
            add_code buf CodeMethodApplyString;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_vars buf vars;
            add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | SequenceString (_, sl) ->
            add_code buf CodeSequenceString;
            squash_string_exp_list pos buf sl
       | ArrayString (_, sl) ->
            add_code buf CodeArrayString;
            squash_string_exp_list pos buf sl
       | ArrayOfString (_, s) ->
            add_code buf CodeArrayOfString;
            squash_string_exp pos buf s
       | QuoteString (_, sl) ->
            add_code buf CodeQuoteString;
            squash_string_exp_list pos buf sl
       | QuoteStringString (_, c, sl) ->
            add_code buf CodeQuoteStringString;
            Hash.add_char buf c;
            squash_string_exp_list pos buf sl
       | ObjectString (_, el, export) ->
            add_code buf CodeObjectString;
            squash_exp_list pos buf el;
            add_code buf CodeSpace;
            squash_export_info buf export
       | BodyString (_, el, export) ->
            add_code buf CodeBodyString;
            squash_exp_list pos buf el;
            add_code buf CodeSpace;
            squash_export_info buf export
       | ExpString (_, el, export) ->
            add_code buf CodeExpString;
            squash_exp_list pos buf el;
            add_code buf CodeSpace;
            squash_export_info buf export
       | CasesString (_, cases) ->
            add_code buf CodeCasesString;
            squash_cases_exp pos buf cases
       | ThisString _ ->
            add_code buf CodeThisString
       | LazyString (_, s) ->
            add_code buf CodeLazyString;
            squash_string_exp pos buf s
       | LetVarString (_, v, s1, s2) ->
            add_code buf CodeLetVarString;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_string_exp pos buf s1;
            add_code buf CodeSpace;
            squash_string_exp pos buf s2
   end;
   add_code buf CodeEnd

and squash_opt_string_exp pos buf = function
   Some s ->
      add_code buf CodeSome;
      squash_string_exp pos buf s
 | None ->
      add_code buf CodeNone

and squash_string_exp_list pos buf sl =
   match sl with
      [s] ->
         squash_string_exp pos buf s
    | s :: sl ->
         squash_string_exp pos buf s;
         add_code buf CodeSpace;
         squash_string_exp_list pos buf sl
    | [] ->
         ()

and squash_keyword_exp_list pos buf kargs =
   match kargs with
      (v, arg) :: kargs ->
         add_code buf CodeSpace;
         squash_var buf v;
         add_code buf CodeSpace;
         squash_string_exp pos buf arg;
         squash_keyword_exp_list pos buf kargs
    | [] ->
         ()

and squash_keyword_param_list pos buf kargs =
   match kargs with
      (v, v_info, opt_arg) :: kargs ->
         add_code buf CodeSpace;
         squash_var buf v;
         add_code buf CodeSpace;
         squash_var_info buf v_info;
         add_code buf CodeSpace;
         squash_opt_string_exp pos buf opt_arg;
         squash_keyword_param_list pos buf kargs
    | [] ->
         ()

and squash_case_exp pos buf (v, s, el, export) =
   add_code buf CodeCaseString;
   squash_var buf v;
   add_code buf CodeSpace;
   squash_string_exp pos buf s;
   add_code buf CodeSpace;
   squash_exp_list pos buf el;
   add_code buf CodeSpace;
   squash_export_info buf export;
   add_code buf CodeEnd

and squash_cases_exp pos buf cases =
   add_code buf CodeCasesString;
   List.iter (squash_case_exp pos buf) cases;
   add_code buf CodeEnd

(*
 * Squash an expression.
 *)
and squash_exp pos buf e =
   add_code buf CodeBegin;
   begin
      match e with
         Omake_ir.LetVarExp (_, v, vl, def, s) ->
            add_code buf CodeLetVarExp;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_vars buf vl;
            add_code buf CodeSpace;
            squash_def_kind buf def;
            add_code buf CodeSpace;
            squash_string_exp pos buf s
       | KeyExp (_, v) ->
            add_code buf CodeKeyExp;
            Hash.add_string buf v
       | LetKeyExp (_, v, def, s) ->
            add_code buf CodeLetKeyExp;
            Hash.add_string buf v;
            add_code buf CodeSpace;
            squash_def_kind buf def;
            add_code buf CodeSpace;
            squash_string_exp pos buf s
       | LetFunExp (_, v, vl, curry, opt_params, params, s, export) ->
            add_code buf CodeLetFunExp;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_vars buf vl;
            add_code buf CodeSpace;
            Hash.add_bool buf curry;
            squash_keyword_param_list pos buf opt_params;
            add_code buf CodeSpace;
            squash_params buf params;
            add_code buf CodeSpace;
            squash_exp_list pos buf s;
            add_code buf CodeSpace;
            squash_export_info buf export
       | LetObjectExp (_, v, vl, s, el, export) ->
            add_code buf CodeLetObjectExp;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_vars buf vl;
            add_code buf CodeSpace;
            squash_string_exp pos buf s;
            add_code buf CodeSpace;
            squash_exp_list pos buf el;
            add_code buf CodeSpace;
            squash_export_info buf export
       | LetThisExp (_, s) ->
            add_code buf CodeLetThisExp;
            squash_string_exp pos buf s
       | ShellExp (_, s) ->
            add_code buf CodeShellExp;
            squash_string_exp pos buf s
       | IfExp (_, cases) ->
            add_code buf CodeIfExp;
            squash_if_cases pos buf cases
       | SequenceExp (_, el) ->
            add_code buf CodeSequenceExp;
            squash_exp_list pos buf el
       | SectionExp (_, s, el, export) ->
            add_code buf CodeSectionExp;
            squash_string_exp pos buf s;
            add_code buf CodeArrow;
            squash_exp_list pos buf el;
            add_code buf CodeSpace;
            squash_export_info buf export
       | OpenExp (_, nodes) ->
            add_code buf CodeOpenExp;
            List.iter (fun node ->
                  add_code buf CodeCommaExp;
                  squash_node buf node) nodes
       | IncludeExp (_, s, sl) ->
            add_code buf CodeIncludeExp;
            squash_string_exp pos buf s;
            add_code buf CodeCommaExp;
            squash_string_exp_list pos buf sl
       | ApplyExp (_, v, args, kargs) ->
            add_code buf CodeApplyExp;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | SuperApplyExp (_, v1, v2, args, kargs) ->
            add_code buf CodeSuperApplyExp;
            squash_var buf v1;
            add_code buf CodeSpace;
            squash_var buf v2;
            add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | MethodApplyExp (_, v, vars, args, kargs) ->
            add_code buf CodeMethodApplyExp;
            squash_var_info buf v;
            add_code buf CodeSpace;
            squash_vars buf vars;
            add_code buf CodeSpace;
            squash_string_exp_list pos buf args;
            squash_keyword_exp_list pos buf kargs
       | ReturnBodyExp (_, el, id) ->
            add_code buf CodeReturnBodyExp;
            squash_exp_list pos buf el;
            squash_return_id buf id
       | StringExp (_, s) ->
            add_code buf CodeStringExp;
            squash_string_exp pos buf s
       | ReturnExp (_, s, id) ->
            add_code buf CodeReturnExp;
            squash_string_exp pos buf s;
            squash_return_id buf id
       | ReturnObjectExp (_, vars) ->
            add_code buf CodeReturnObjectExp;
            squash_vars buf vars
       | ReturnSaveExp _ ->
            add_code buf CodeReturnSaveExp
       | StaticExp (_, node, key, el) ->
            add_code buf CodeStaticExp;
            squash_node buf node;
            add_code buf CodeSpace;
            squash_var buf key;
            add_code buf CodeSpace;
            squash_exp_list pos buf el
   end;
   add_code buf CodeEnd

and squash_exp_list pos buf el =
   match el with
      [e] ->
         squash_exp pos buf e
    | e :: el ->
         squash_exp pos buf e;
         add_code buf CodeSpace;
         squash_exp_list pos buf el
    | [] ->
         ()

and squash_if_case pos buf (s, el, export) =
   add_code buf CodeCaseExp;
   squash_string_exp pos buf s;
   add_code buf CodeSpace;
   squash_exp_list pos buf el;
   add_code buf CodeSpace;
   squash_export_info buf export;
   add_code buf CodeEnd

and squash_if_cases pos buf cases =
   add_code buf CodeCasesExp;
   List.iter (squash_if_case pos buf) cases;
   add_code buf CodeEnd

(*
 * Compute the digest of a value.
 *)
let rec squash_value pos buf v =
  add_code buf CodeBegin;
  begin
    match v with
    |Omake_value_type.ValNone ->
      add_code buf CodeValNone;
    | ValInt i ->
      add_code buf CodeValInt;
      Hash.add_int buf i
    | ValFloat x ->
      add_code buf CodeValFloat;
      Hash.add_float buf x
    | ValSequence vl ->
      add_code buf CodeValSequence;
      squash_values pos buf vl
    | ValArray vl ->
      add_code buf CodeValArray;
      squash_values pos buf vl
    | ValWhite s ->
      add_code buf CodeValWhite;
      Hash.add_string buf s
    | ValString s ->
      add_code buf CodeValString;
      Hash.add_string buf s
    | ValData s ->
      add_code buf CodeValData;
      Hash.add_string buf s
    | ValQuote vl ->
      add_code buf CodeValQuote;
      squash_values pos buf vl
    | ValQuoteString (c, vl) ->
      add_code buf CodeValQuoteString;
      Hash.add_char buf c;
      squash_values pos buf vl
    | ValMaybeApply (_, v) ->
      add_code buf CodeValMaybeApply;
      squash_var_info buf v
    | ValFun (_, keywords, params, body, export) ->
      add_code buf CodeValFun;
      squash_keyword_param_values pos buf keywords;
      add_code buf CodeSpace;
      squash_params buf params;
      add_code buf CodeArrow;
      squash_exp_list pos buf body;
      add_code buf CodeSpace;
      squash_export_info buf export
    | ValFunCurry (_, args, keywords, params, body, export, kargs) ->
      add_code buf CodeValFunCurry;
      squash_param_values pos buf args;
      add_code buf CodeSpace;
      squash_keyword_param_values pos buf keywords;
      add_code buf CodeSpace;
      squash_params buf params;
      add_code buf CodeArrow;
      squash_exp_list pos buf body;
      add_code buf CodeSpace;
      squash_export_info buf export;
      squash_keyword_values pos buf kargs
    | ValPrim (_, _, _, f) ->
      add_code buf CodeValPrim;
      squash_var buf (Omake_env.squash_prim_fun f)
    | ValPrimCurry (_, _, f, args, kargs) ->
      add_code buf CodeValPrimCurry;
      squash_var buf (Omake_env.squash_prim_fun f);
      add_code buf CodeSpace;
      squash_values pos buf args;
      squash_keyword_values pos buf kargs
    | ValNode node ->
      add_code buf CodeValNode;
      Hash.add_string buf (Omake_node.Node.fullname node)
    | ValDir dir ->
      add_code buf CodeValDir;
      Hash.add_string buf (Omake_node.Dir.fullname dir)
    | ValStringExp (_, e) ->
      add_code buf CodeValStringExp;
      squash_string_exp pos buf e
    | ValBody (keywords, params, e, export) ->
      add_code buf CodeValBody;
      squash_keyword_param_values pos buf keywords;
      add_code buf CodeSpace;
      squash_params buf params;
      add_code buf CodeArrow;
      squash_exp_list pos buf e;
      add_code buf CodeSpace;
      squash_export_info buf export
    | ValObject obj ->
      add_code buf CodeValObject;
      squash_object pos buf obj
    | ValMap obj ->
      add_code buf CodeValMap;
      squash_map pos buf obj
    | ValCases cases ->
      squash_cases pos buf cases
    | ValVar (_, v) ->
      add_code buf CodeValVar;
      squash_var_info buf v;
    | ValDelayed { contents = ValValue v } ->
      squash_value pos buf v
    | ValDelayed { contents = ValStaticApply (node, v) } ->
      add_code buf CodeValStaticApply;
      squash_value pos buf node;
      add_code buf CodeSpace;
      squash_var buf v
    | ValRules _
    | ValChannel _
    | ValClass _
    | ValOther _ as v ->
      let print_error buf =
        Format.fprintf buf "@[<v 3>Non digestable value:@ @[<hv 3>%a@]@ Contact the OMake team at omake@@metaprl.org if you think this should be supported@]@." Omake_value_print.pp_print_value v
      in
      raise (Omake_value_type.OmakeFatalErr (pos, LazyError print_error))
  end;
  add_code buf CodeEnd

and squash_opt_value pos buf = function
    Some v ->
    add_code buf CodeSome;
    squash_value pos buf v
  | None ->
    add_code buf CodeNone

and squash_values pos buf vl =
  match vl with
    [v] ->
    squash_value pos buf v
  | v :: vl ->
    squash_value pos buf v;
    add_code buf CodeSpace;
    squash_values pos buf vl
  | [] ->
    ()

and squash_param_values pos buf kargs =
  match kargs with
    (v, arg) :: kargs ->
    add_code buf CodeSpace;
    squash_var_info buf v;
    add_code buf CodeSpace;
    squash_value pos buf arg;
    squash_param_values pos buf kargs
  | [] ->
    ()

and squash_keyword_values pos buf kargs =
  match kargs with
    (v, arg) :: kargs ->
    add_code buf CodeSpace;
    squash_var buf v;
    add_code buf CodeSpace;
    squash_value pos buf arg;
    squash_keyword_values pos buf kargs
  | [] ->
    ()

and squash_keyword_param_values pos buf kargs =
  match kargs with
    (v, v_info, opt_arg) :: kargs ->
    add_code buf CodeSpace;
    squash_var buf v;
    add_code buf CodeSpace;
    squash_var_info buf v_info;
    add_code buf CodeSpace;
    squash_opt_value pos buf opt_arg;
    squash_keyword_param_values pos buf kargs
  | [] ->
    ()

and squash_object pos buf obj =
  Lm_symbol.SymbolTable.iter (fun x v ->
    add_code buf CodeBegin;
    squash_var buf x;
    add_code buf CodeArrow;
    squash_value pos buf v;
    add_code buf CodeEnd) (Omake_env.squash_object obj)

and squash_map pos buf map =
  Omake_env.venv_map_iter (fun x v ->
    add_code buf CodeBegin;
    squash_value pos buf x;
    add_code buf CodeArrow;
    squash_value pos buf v;
    add_code buf CodeEnd) map

and squash_case pos buf (x, v1, _x2, export) =
  add_code buf CodeCase;
  squash_var buf x;
  add_code buf CodeSpace;
  squash_value pos buf v1;
  add_code buf CodeSpace;
  squash_value pos buf v1;
  add_code buf CodeSpace;
  squash_export_info buf export;
  add_code buf CodeEnd

and squash_cases pos buf cases =
  add_code buf CodeCases;
  List.iter (squash_case pos buf) cases;
  add_code buf CodeEnd

(*
 * Commands.
 *)
(* let squash_command_flag buf flag = *)
(*   let code = *)
(*     match flag with *)
(*     |Omake_command_type.QuietFlag -> *)
(*       CodeQuietFlag *)
(*     | AllowFailureFlag -> *)
(*       CodeAllowFailureFlag *)
(*     | AllowOutputFlag -> *)
(*       CodeAllowOutputFlag *)
(*   in *)
(*   add_code buf code *)

(* let squash_command_flags buf flags = *)
(*    add_code buf CodeCommandFlags; *)
(*    List.iter (squash_command_flag buf) flags; *)
(*    add_code buf CodeEnd *)

let squash_arg_string buf arg =
  match arg with
  | Omake_command_type.ArgString s ->
    add_code buf CodeArgString;
    Hash.add_string buf s
  | ArgData s ->
    add_code buf CodeArgData;
    Hash.add_string buf s

let squash_arg buf arg =
   add_code buf CodeArg;
   List.iter (squash_arg_string buf) arg;
   add_code buf CodeEnd

let squash_redirect buf chan =
  match chan with
  |Omake_shell_type.RedirectNode node ->
    add_code buf CodeRedirectNode;
    squash_node buf node
  | RedirectArg arg ->
    add_code buf CodeRedirectArg;
    squash_arg buf arg
  | RedirectNone ->
    add_code buf CodeRedirectNone

let squash_argv buf argv =
   add_code buf CodeArgv;
   List.iter (squash_arg buf) argv;
   add_code buf CodeEnd

let squash_command_env_item buf (v, arg) =
   add_code buf CodeCommandEnvItem;
   squash_var buf v;
   add_code buf CodeSpace;
   squash_arg buf arg;
   add_code buf CodeEnd

let squash_command_env buf env =
   add_code buf CodeCommandEnv;
   List.iter (squash_command_env_item buf) env;
   add_code buf CodeEnd

let squash_exe buf exe =
  match exe with
  |Omake_shell_type.CmdArg arg ->
    add_code buf CodeCmdArg;
    squash_arg buf arg
  | CmdNode node ->
    add_code buf CodeCmdNode;
    squash_node buf node

let squash_pipe_op buf op =
  let code =
    match op with
    |Omake_shell_type.PipeAnd -> CodePipeAnd
    | PipeOr  -> CodePipeOr
    | PipeSequence -> CodePipeSequence
  in
  add_code buf code

let squash_pipe_command _pos buf (info : Omake_env.arg_cmd) =
   let { Omake_shell_type.cmd_env   = env;
         cmd_exe   = exe;
         cmd_argv  = argv;
         cmd_stdin = stdin;
         cmd_stdout = stdout;
         cmd_stderr = stderr;
         cmd_append = append;
         _
       } = info
   in
      add_code buf CodePipeCommand;
      squash_command_env buf env;
      add_code buf CodeSpace;
      squash_exe buf exe;
      add_code buf CodeSpace;
      squash_argv buf argv;
      add_code buf CodeSpace;
      squash_redirect buf stdin;
      add_code buf CodeSpace;
      squash_redirect buf stdout;
      add_code buf CodeSpace;
      Hash.add_bool buf stderr;
      add_code buf CodeSpace;
      Hash.add_bool buf append;
      add_code buf CodeEnd

let squash_pipe_apply pos buf (info : Omake_env.arg_apply) =
   let {Omake_shell_type.apply_name = name;
         apply_args = args;
         apply_stdin = stdin;
         apply_stdout = stdout;
         apply_stderr = stderr;
         apply_append = append;
         _
       } = info
   in
      add_code buf CodePipeApply;
      squash_var buf name;
      add_code buf CodeSpace;
      squash_values pos buf args;
      add_code buf CodeSpace;
      squash_redirect buf stdin;
      add_code buf CodeSpace;
      squash_redirect buf stdout;
      add_code buf CodeSpace;
      Hash.add_bool buf stderr;
      add_code buf CodeSpace;
      Hash.add_bool buf append;
      add_code buf CodeEnd

let rec squash_pipe pos buf (pipe : Omake_env.arg_pipe) =
   (match pipe with
       PipeApply (_, info) ->
          squash_pipe_apply pos buf info
     | PipeCommand (_, info) ->
          squash_pipe_command pos buf info
     | PipeCond (_, op, pipe1, pipe2) ->
          add_code buf CodePipeCond;
          squash_pipe_op buf op;
          squash_pipe pos buf pipe1;
          squash_pipe pos buf pipe2
     | PipeCompose (_, b, pipe1, pipe2) ->
          add_code buf CodePipeCompose;
          Hash.add_bool buf b;
          squash_pipe pos buf pipe1;
          squash_pipe pos buf pipe2
     | PipeGroup (_, info) ->
          squash_pipe_group pos buf info
     | PipeBackground (_, pipe) ->
          add_code buf CodePipeBackground;
          squash_pipe pos buf pipe);
   add_code buf CodeEnd

and squash_pipe_group pos buf info =
   let { Omake_shell_type.group_stdin = stdin;
         group_stdout = stdout;
         group_stderr = stderr;
         group_append = append;
         group_pipe   = pipe
       } = info
   in
      add_code buf CodePipeGroup;
      squash_redirect buf stdin;
      add_code buf CodeSpace;
      squash_redirect buf stdout;
      add_code buf CodeSpace;
      Hash.add_bool buf stderr;
      add_code buf CodeSpace;
      Hash.add_bool buf append;
      add_code buf CodeSpace;
      squash_pipe pos buf pipe;
      add_code buf CodeEnd

let squash_command_line pos buf (command : Omake_env.arg_command_inst) =
   match command with
      CommandPipe argv ->
         add_code buf CodePipe;
         squash_pipe pos buf argv;
         add_code buf CodeEnd
    | CommandEval e ->
         squash_exp_list pos buf e
    | CommandValues values ->
         squash_values pos buf values

let squash_command pos buf (command : Omake_env.arg_command_line) =
   let {Omake_command_type.command_dir = dir;
         command_inst = inst;
         _
       } = command
   in
      add_code buf CodeCommand;
      Hash.add_string buf (Omake_node.Dir.fullname dir);
      squash_command_line pos buf inst;
      add_code buf CodeEnd

let squash_commands pos buf commands =
   add_code buf CodeCommands;
   List.iter (squash_command pos buf) commands;
   add_code buf CodeEnd

(*
 * Get the digest of some commands.
 *)
let digest_of_exp pos values e =
   let buf = Hash.create () in
      squash_values pos buf values;
      add_code buf CodeSpace;
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

