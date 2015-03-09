module I = Lm_instrument

(*  Predefined set of functions. *)

include Omake_pos.Make (struct let name = "Omake_eval" end);;


let debug_eval =
  Lm_debug.create_debug (**)
    { debug_name = "debug-eval";
      debug_description = "Debug the evaluator";
      debug_value = false
    }

let print_ast =
  Lm_debug.create_debug (**)
    { debug_name = "print-ast";
      debug_description = "Print the AST after parsing";
      debug_value = false
    }

let print_ir =
  Lm_debug.create_debug (**)
    { debug_name = "print-ir";
      debug_description = "Print the IR after parsing";
      debug_value = false
    }

let print_rules =
  Lm_debug.create_debug (**)
    { debug_name = "print-rules";
      debug_description = "Print the rules after evaluation";
      debug_value = false
    }

let print_files =
  Lm_debug.create_debug (**)
    { debug_name = "print-files";
      debug_description = "Print the files as they are read";
      debug_value = false
    }

let bool_of_string s =
  match String.lowercase s with
  | ""
  | "0"
  | "no"
  | "nil"
  | "false"
  | "undefined" ->
    false
  | _ ->
    true

(*
 * For now, use a bogu location for parameters.
 *)
(* let param_loc = Lm_location.bogus_loc "Omake_eval.param" *)

(*
 * Including files.
 *)

(************************************************************************
 * Utilities.
*)
let raise_uncaught_exception pos = function
  | Sys.Break
  | Omake_value_type.OmakeException _
  | Omake_value_type.OmakeFatal _
  | Omake_value_type.OmakeFatalErr _
  | Omake_value_type.UncaughtException _ as exn ->
    raise exn
  | exn ->
    raise (Omake_value_type.UncaughtException (pos, exn))

(*
 * Add an optional quote.
 *)
let buffer_add_quote buf = function
    Some c -> Buffer.add_char buf c
  | None -> ()

(*
 * The various forms of empty values.
 *)
let rec is_empty_value ( v : Omake_value_type.t) =
  match v with
  | ValNone
  | ValWhite _
  | ValString ""
  | ValData ""
  | ValQuote []
  | ValArray []
  | ValRules [] ->
    true
  | ValSequence vl ->
    List.for_all is_empty_value vl
  | ValObject obj ->
    (try is_empty_value 
        (Omake_env.venv_find_field_internal_exn obj Omake_symbol.builtin_sym) with
      Not_found ->
      false)
  | ValInt _
  | ValFloat _
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValString _
  | ValArray _
  | ValMaybeApply _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValRules _
  | ValNode _
  | ValDir _
  | ValStringExp _
  | ValBody _
  | ValMap _
  | ValChannel _
  | ValClass _
  | ValCases _
  | ValOther _
  | ValDelayed _
  | ValVar _ ->
    false

(*
 * Check whether a value has an embedded array.
 *)
let rec is_array_value (v : Omake_value_type.t) =
  match v with
  | ValArray _ ->
    true
  | ValSequence [v]
  | ValQuote [v] ->
    is_array_value v
  | ValObject obj ->
    (try
      match Omake_env.venv_find_field_internal_exn obj Omake_symbol.builtin_sym with
        ValArray _ -> true
      | _ -> false
    with
      Not_found ->
      false)
  | ValNone
  | ValInt _
  | ValFloat _
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValWhite _
  | ValString _
  | ValMaybeApply _
  | ValSequence _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValRules _
  | ValNode _
  | ValDir _
  | ValStringExp _
  | ValBody _
  | ValMap _
  | ValChannel _
  | ValClass _
  | ValCases _
  | ValOther _
  | ValVar _
  | ValDelayed _ ->
    false

(*
 * Determine when an application is ready from its arity.
 *)
type partial_arity =
  | FullArity    of Omake_value_type.t list * Omake_value_type.t list
  | PartialArity of Omake_ir.arity * Omake_value_type.t list

let rec concat_n_args args1 args2 n =
  if n = 0 then
    FullArity (List.rev args1, args2)
  else
    match args2 with
      arg :: args2 ->
      concat_n_args (arg :: args1) args2 (n - 1)
    | [] ->
      raise (Invalid_argument "concat_n_args")

let arity_apply_args ( arity : Omake_ir.arity ) args1 args2 =
  let len = List.length args2 in
  match arity with
  | ArityRange (min, max) ->
    if len < min then
      let args = List.rev_append args2 args1 in
      PartialArity (ArityRange (min - len, max - len), args)
    else if len < max then
      let args = List.rev_append args1 args2 in
      FullArity (args, [])
    else
      concat_n_args args1 args2 max
  | ArityExact i ->
    if len < i then
      let args = List.rev_append args2 args1 in
      PartialArity ( ArityExact (i - len), args)
    else
      concat_n_args args1 args2 i
  | ArityNone ->
    FullArity ([], List.rev_append args1 args2)
  | ArityAny ->
    FullArity (List.rev_append args1 args2, [])

(************************************************************************
 * Compiling utilities.
*)
let postprocess_ir venv ( ir : Omake_ir.t) =
  let () =
    if Lm_debug.debug print_ir then
      Format.eprintf "@[<v 3>IR1:@ %a@]@." 
        Omake_ir_print.pp_print_exp ir.ir_exp
  in
  let ir = { ir with ir_exp = Omake_ir_semant.build_prog venv ir.ir_exp } in
  let () =
    if Lm_debug.debug print_ir then
      Format.eprintf "@[<v 3>IR2:@ %a@]@." Omake_ir_print.pp_print_exp ir.ir_exp
  in
  ir

(**  Parse and evaluate a file. *)
let rec parse_ir 
    (venv : Omake_env.t) 
    (scope : Omake_env.include_scope) 
    (node : Omake_node.Node.t) : Omake_ir.t =
  let filename = Omake_node.Node.fullname node in
  let ast = Omake_ast_lex.parse_ast filename in
  let () =
    if Lm_debug.debug print_ast then
      Format.eprintf "@[<v 3>AST (initial):@ %a@]@." Omake_ast_print.pp_print_prog ast
  in
  let ast = Omake_exp_lex.compile_prog ast in
  let () =
    if Lm_debug.debug print_ast then
      Format.eprintf "@[<v 3>AST %a:@ %a@]@." Omake_node.pp_print_node node Omake_ast_print.pp_print_prog ast
  in
  let vars = Omake_env.venv_include_scope venv scope in
  let _senv, ir = Omake_ir_ast.compile_prog (Omake_ir_ast.penv_of_vars (open_ir venv) venv node vars) ast in
  postprocess_ir venv ir

(*
 * When constructing a path, the relative filenames
 * should be auto-rehash.
 *
 *    values  : the path
 *    dirname : the subdirectory to search (often ".")
 *)
and path_of_values_select venv pos (values : Omake_value_type.t list) dirname =
  let rec collect groups auto_rehash items (values : Omake_value_type.t list) =
    match values with
    | v :: values ->
      let rehash_flag, dir =
        match v with
        | ValDir dir ->
          false, dir
        | ValNode _ ->
          let dir = Omake_env.venv_intern_dir venv (string_of_value venv pos v) in
          false, dir
        | _ ->
          let s = string_of_value venv pos v in
          let rehash_flag = not (Lm_filename_util.is_absolute s) in
          let dir = Omake_env.venv_intern_dir venv s in
          rehash_flag, dir
      in
      let dir = Omake_node.Dir.chdir dir dirname in
      let groups, items =
        if rehash_flag <> auto_rehash && items <> [] then
          (auto_rehash, List.rev items) :: groups, [dir]
        else
          groups, dir :: items
      in
      collect groups rehash_flag items values
    | [] ->
      if items <> [] then
        (auto_rehash, List.rev items) :: groups
      else
        groups
  in
  List.rev (collect [] false [] values)

and path_of_values_rehash venv pos values dirname =
  let dir_of_value (v : Omake_value_type.t) =
    let dir =
      match v with
      | ValDir dir ->
        dir
      | _ ->
        Omake_env.venv_intern_dir venv (string_of_value venv pos v)
    in
    Omake_node.Dir.chdir dir dirname
  in
  [true, List.map dir_of_value values]

and path_of_values venv pos values dirname =
  let auto_rehash =
    try bool_of_value venv pos 
        (Omake_env.venv_find_var_exn venv Omake_var.auto_rehash_var) with
      Not_found ->
      false
  in
  let f =
    if auto_rehash then
      path_of_values_rehash
    else
      path_of_values_select
  in
  f venv pos values dirname

(*
 * Open the file.
 * Get the IR and return the vars.
 *)
and find_include_file venv pos loc filename =
  let pos = string_pos "find_include_file" pos in
  let cache = Omake_env.venv_cache venv in
  if not (Filename.is_relative filename) || not (Filename.is_implicit filename) then
    let fullname = filename ^ Omake_state.omake_file_suffix in
    let node1 = Omake_env.venv_intern venv PhonyProhibited fullname in
    if Omake_cache.exists cache node1 then
      node1
    else
      let node2 = Omake_env.venv_intern venv PhonyProhibited filename in
      if Omake_cache.exists cache node2 then
        node2
      else
        let print_error buf =
          Format.fprintf buf "@[<hv 3>include file not found, neither file exists:@ %a@ %a@]" (**)
            Omake_node.pp_print_node node1
            Omake_node.pp_print_node node2
        in
        raise (Omake_value_type.OmakeException (loc_pos loc pos, LazyError print_error))
  else
    let dirname = Filename.dirname filename in
    let basename = Filename.basename filename in
    let fullname = basename ^ Omake_state.omake_file_suffix in
    let path = Omake_env.venv_find_var venv pos loc Omake_var.omakepath_var in
    let full_path = values_of_value venv pos path in
    let path = path_of_values venv pos full_path dirname in
    let cache = Omake_env.venv_cache venv in
    let listing = Omake_cache.ls_path cache path in
    try
      match Omake_cache.listing_find cache listing fullname with
        DirEntry dir ->
        raise (Omake_value_type.OmakeException (loc_pos loc pos, StringDirError ("is a directory", dir)))
      | NodeEntry node ->
        node
    with
      Not_found ->
      try
        match Omake_cache.listing_find cache listing basename with
          DirEntry dir ->
          raise (Omake_value_type.OmakeException (loc_pos loc pos, StringDirError ("is a directory", dir)))
        | NodeEntry node ->
          node
      with
        Not_found ->
        let print_error buf =
          Format.fprintf buf "@[<hv 3>include file %s not found in OMAKEPATH@ (@[<hv3>OMAKEPATH[] =%a@])@]" (**)
            filename
            Omake_value_print.pp_print_value_list full_path
        in
        raise (Omake_value_type.OmakeException (loc_pos loc pos, LazyError print_error))

and open_ir venv filename pos loc =
  let pos = string_pos "open_ir" pos in
  let source = find_include_file venv pos loc filename in
  let ir  : Omake_ir.t = compile_ir venv Omake_env.IncludePervasives pos loc source in
  if !print_ir then begin
    Format.eprintf "@[<v 3>Vars: %a" Omake_node.pp_print_node source;
    Lm_symbol.SymbolTable.iter (fun v info ->
      Format.eprintf "@ %a = %a" Lm_symbol.pp_print_symbol v Omake_ir_print.pp_print_var_info info) ir.ir_vars;
    Format.eprintf "@]@."
  end;
  source, ir.ir_vars

(*
 * The include file contains the IR for the file.
 * Try to load the old entry.
 * If it fails, compile the file and save the new entry.
 *)
and compile_add_ir_info venv scope pos _ source info =
  let _pos = string_pos "compile_add_ir_info" pos in
  try Omake_env.Static.get_ir info with
    Not_found ->
    let ir = parse_ir venv scope source in
    Omake_env.Static.add_ir info ir;
    ir

and compile_ir_info venv scope pos loc source info =
  let _pos = string_pos "compile_ir_info" pos in
  try Omake_env.Static.find_ir info with
    Not_found ->
    Omake_env.Static.rewrite info (compile_add_ir_info venv scope pos loc source)

and compile_ir venv scope pos loc source =
  let pos = string_pos "compile_ir" pos in
  (*
       * Try to get a cached copy.
       *)
  try Omake_env.venv_find_ir_file_exn venv source with
    Not_found ->
    let ir =
      (*
                * Open the database.
                *)
      try Omake_env.Static.read venv source (compile_ir_info venv scope pos loc source)
      with Not_found ->
        raise (Omake_value_type.OmakeException (loc_pos loc pos, StringNodeError ("can't open IR", source)))
    in
    Omake_env.venv_add_ir_file venv source ir;
    ir

(*
 * The object file contains the evaluated file.
 *)
and compile_add_object_info compile _ pos source info =
  let _pos = string_pos "compile_add_object_info_info" pos in
  try Omake_env.Static.get_object info with
    Not_found ->
    let obj = compile info source in
    Omake_env.Static.add_object info obj;
    obj

(*
 * Try to load the old entry.
 * If it fails, compile the file and save the new entry.
 *)
and compile_object_info compile venv pos source info =
  let _pos = string_pos "compile_object_info" pos in
  try Omake_env.Static.find_object info with
    Not_found ->
    Omake_env.Static.rewrite info (compile_add_object_info compile venv pos source)

and compile_object compile venv pos loc source =
  let pos = string_pos "compile_ast" pos in

  (*
       * Try to get a cached copy.
       *)
  try Omake_env.venv_find_object_file_exn venv source with
    Not_found ->
    let obj =
      (*
                * Open the database.
                *)
      try Omake_env.Static.read venv source (compile_object_info compile venv pos source)
      with Not_found ->
        raise (Omake_value_type.OmakeException (loc_pos loc pos, StringNodeError ("can't open object", source)))
    in
    Omake_env.venv_add_object_file venv source obj;
    obj

(************************************************************************
 * Value operations.
*)

(*
 * Get the string representation of a value.
 * It not legal to convert an array to a string.
 *)
and string_of_value venv pos (v : Omake_value_type.t) =
  let pos = string_pos "string_of_value" pos in
  let scratch_buf = Buffer.create 32 in
  let rec collect (v : Omake_value_type.t) =
    match eval_prim_value venv pos v with
    (* Values that expand to nothing *)
    | ValNone
    | ValFun _
    | ValFunCurry _
    | ValPrim _
    | ValPrimCurry _
    | ValRules _
    | ValBody _
    | ValMap _
    | ValObject _
    | ValChannel _
    | ValClass _
    | ValCases _
    | ValOther _
    | ValArray []
    | ValVar _ ->
      ()
    | ValSequence vl ->
      List.iter collect vl
    | ValQuote vl ->
      string_of_quote_buf scratch_buf venv pos vl
    | ValQuoteString (c, vl) ->
      Buffer.add_char scratch_buf c;
      string_of_quote_buf scratch_buf venv pos vl;
      Buffer.add_char scratch_buf c
    | ValArray [v] ->
      collect v
    | ValArray vl ->
      let print_error buf =
        Format.fprintf buf "@[<v 3>Array value where string expected:";
        Format.fprintf buf "@ Use the $(string ...) function if you really want to do this";
        Format.fprintf buf "@ @[<v 3>The array has length %d:" (List.length vl);
        ignore (List.fold_left (fun index v ->
            Format.fprintf buf "@ @[<hv 3>[%d] =@ %a@]" index Omake_value_print.pp_print_value v;
            succ index) 0 vl);
        Format.fprintf buf "@]@]@."
      in
      raise (Omake_value_type.OmakeException (pos, LazyError print_error))
    | ValInt i ->
      Buffer.add_string scratch_buf (string_of_int i)
    | ValFloat x ->
      Buffer.add_string scratch_buf (string_of_float x)
    | ValData s
    | ValWhite s
    | ValString s ->
      Buffer.add_string scratch_buf s
    | ValDir dir2 ->
      Buffer.add_string scratch_buf (Omake_env.venv_dirname venv dir2)
    | ValNode node ->
      Buffer.add_string scratch_buf (Omake_env.venv_nodename venv node)
    | ValStringExp _
    | ValMaybeApply _
    | ValDelayed _ ->
      raise (Invalid_argument "string_of_value")  in
  collect v;
  Buffer.contents scratch_buf

(*
 * Collect the values in a quotation into a string.
 * Even array values are flattened without warning.
 *)
and string_of_quote venv pos c vl =
  let pos = string_pos "string_of_quote" pos in
  let scratch_buf = Buffer.create 32 in
  buffer_add_quote scratch_buf c;
  string_of_quote_buf scratch_buf venv pos vl;
  buffer_add_quote scratch_buf c;
  Buffer.contents scratch_buf

and string_of_quote_buf scratch_buf venv pos vl =
  let pos = string_pos "string_of_quote_buf" pos in
  let rec collect v =
    match (eval_value venv pos v : Omake_value_type.t) with
      (* Values that expand to nothing *)
    | ValNone
    | ValFun _
    | ValFunCurry _
    | ValPrim _
    | ValPrimCurry _
    | ValRules _
    | ValBody _
    | ValMap _
    | ValObject _
    | ValChannel _
    | ValClass _
    | ValCases _
    | ValOther _
    | ValArray []
    | ValVar _ ->
      ()
    | ValSequence vl
    | ValQuote vl ->
      List.iter collect vl
    | ValQuoteString (c, vl) ->
      Buffer.add_char scratch_buf c;
      List.iter collect vl;
      Buffer.add_char scratch_buf c
    | ValArray [v] ->
      collect v
    | ValArray vl ->
      collect_array vl
    | ValInt i ->
      Buffer.add_string scratch_buf (string_of_int i)
    | ValFloat x ->
      Buffer.add_string scratch_buf (string_of_float x)
    | ValData s
    | ValWhite s
    | ValString s ->
      Buffer.add_string scratch_buf s
    | ValDir dir2 ->
      Buffer.add_string scratch_buf (Omake_env.venv_dirname venv dir2)
    | ValNode node ->
      Buffer.add_string scratch_buf (Omake_env.venv_nodename venv node)
    | ValStringExp _
    | ValMaybeApply _
    | ValDelayed _ ->
      raise (Invalid_argument "string_of_value")
  and collect_array vl =
    match vl with
      [v] ->
      collect v
    | v :: vl ->
      collect v;
      Buffer.add_char scratch_buf ' ';
      collect_array vl
    | [] ->
      ()
  in
  List.iter collect vl

(*
 * Get a list of values from the value.
 * Array elements are always special, and returned as an element.
 * We divide values into two classes:
 *    The "catenable" values are the values that can be concatenated to
 *    form a string.  These include: string, node, dir, int, float.
 *
 *    Nothing else can be concatenated with a string, and is always preserved
 *    in the value list.
 *)
and values_of_value venv pos v =
  let pos = string_pos "values_of_value" pos in

  (*
    * Convert a catenable value to a string
    *)
  let group tokens : Omake_value_type.t  = ValSequence tokens in
  let wrap_string s : Omake_value_type.t = ValString s in
  let wrap_data s   : Omake_value_type.t = ValData s in
  let wrap_token s  : Omake_value_type.t = ValData s in
  let lexer _ _ _   = None in
  let tokens = Lm_string_util.tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group in

  (*
    * Array elements are always separate values.
    * The arrays are flattened.
    *)
  let rec collect_array (vl : Omake_value_type.t list) vll =
    match vl, vll with
    | v :: vl, _ ->
      begin match eval_value venv pos v with
        ValArray el ->
        collect_array el (vl :: vll)
      | ValSequence [v] ->
        collect_array (v :: vl) vll
      | v ->
        Lm_string_util.tokens_atomic tokens v;
        collect_array vl vll
      end
    | [], vl :: vll ->
      collect_array vl vll
    | [], [] ->
      ()
  in

  (*
    * Collect_string is used when we have seen whitespace
    * in a sequence.  Collect the values into the string buffer,
    * then parse the string into separate tokens.
    *)
  let rec collect vl vll =
    match vl, vll with
    | v :: vl, _ ->
      let v : Omake_value_type.t = eval_catenable_value venv pos v in
      begin match v with
      | ValNone ->
        collect vl vll

      (* Strings *)
      | ValWhite s
      | ValString s ->
        Lm_string_util.tokens_string tokens s;
        collect vl vll
      | ValSequence el ->
        collect el (vl :: vll)

      (* Other catenable values *)
      | ValData _
      | ValInt _
      | ValFloat _
      | ValDir _
      | ValNode _
      | ValQuote _
      | ValQuoteString _ ->
        Lm_string_util.tokens_add tokens v;
        collect vl vll

      (* Atomic values *)
      | ValArray el ->
        Lm_string_util.tokens_break tokens;
        collect_array el [];
        collect vl vll
      | ValFun _
      | ValFunCurry _
      | ValPrim _
      | ValPrimCurry _
      | ValRules _
      | ValBody _
      | ValMap _
      | ValObject _
      | ValChannel _
      | ValClass _
      | ValCases _
      | ValOther _
      | ValVar _ ->
        Lm_string_util.tokens_atomic tokens v;
        collect vl vll
      | ValStringExp _
      | ValMaybeApply _
      | ValDelayed _ ->
        raise (Omake_value_type.OmakeException (pos, StringValueError ("illegal application", v)))
      end
    | [], vl :: vll ->
      collect vl vll
    | [], [] ->
      Lm_string_util.tokens_flush tokens
  in
  collect [v] []

(*
 * Get a string list from the value.
 * This is always legal because arrays have been flattened.
 *)
and strings_of_value venv pos v =
  let values = values_of_value venv pos v in
  List.map (string_of_value venv pos) values

(*
 * Get a list of tokens from the value.
 * This is a lot like the previous function, but we use a lexer
 * for parsing special character sequences.
 *)
and tokens_of_value venv pos lexer v =
  let pos = string_pos "tokens_of_value" pos in

  (*
    * Convert a catenable value to a string
    *)
  let group tokens  = Omake_env.TokGroup tokens in
  let wrap_string s = Omake_env.TokString (ValString s) in
  let wrap_data s   = Omake_env.TokString (ValData s) in
  let wrap_token s  = Omake_env.TokToken s in
  let tokens = Lm_string_util.tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group in

  (*
    * Array elements are always separate values.
    * The arrays are flattened.
    *)
  let rec collect_array vl vll =
    match vl, vll with
      v :: vl, _ ->
      (match eval_value venv pos v with
        ValArray el ->
        collect_array el (vl :: vll)
      | ValSequence [v] ->
        collect_array (v :: vl) vll
      | v ->
        Lm_string_util.tokens_atomic tokens (TokString v);
        collect_array vl vll)
    | [], vl :: vll ->
      collect_array vl vll
    | [], [] ->
      ()
  in

  (*
    * Collect_string is used when we have seen whitespace
    * in a sequence.  Collect the values into the string buffer,
    * then parse the string into separate tokens.
    *)
  let rec collect vl vll =
    match vl, vll with
      v :: vl, _ ->
      let v = eval_catenable_value venv pos v in
      (match v with
        ValNone ->
        collect vl vll

      (* Strings *)
      | ValWhite s
      | ValString s ->
        Lm_string_util.tokens_lex tokens s;
        collect vl vll
      | ValSequence el ->
        collect el (vl :: vll)

      (* Other catenable values *)
      | ValData _
      | ValInt _
      | ValFloat _
      | ValDir _
      | ValNode _
      | ValQuote _ ->
        Lm_string_util.tokens_add tokens (TokString v);
        collect vl vll
      | ValQuoteString (_, v) ->
        Lm_string_util.tokens_add tokens (TokString (ValQuote v));
        collect vl vll

      (* Atomic values *)
      | ValArray el ->
        Lm_string_util.tokens_break tokens;
        collect_array el [];
        collect vl vll
      | ValFun _
      | ValFunCurry _
      | ValPrim _
      | ValPrimCurry _
      | ValRules _
      | ValBody _
      | ValMap _
      | ValObject _
      | ValChannel _
      | ValClass _
      | ValCases _
      | ValOther _
      | ValVar _ ->
        Lm_string_util.tokens_atomic tokens (TokString v);
        collect vl vll
      | ValStringExp _
      | ValMaybeApply _
      | ValDelayed _ ->
        raise (Omake_value_type.OmakeException (pos, StringValueError ("illegal application", v))))
    | [], vl :: vll ->
      collect vl vll
    | [], [] ->
      Lm_string_util.tokens_flush tokens
  in
  collect [v] []

(*
 * Flatten the value list into a arg_string list.
 * Basically just concatenate all the values, being
 * careful to preserve quoting.  In addition, we want to
 * concatenate adjacent strings of the same type.
 *)
and arg_of_values venv pos vl =
  let pos = string_pos "arg_of_values" pos in

  (*
    * Flatten all sequences.
    *)
  let rec collect is_quoted tokens vl vll =
    match vl, vll with
      v :: vl, _ ->
      let v = eval_value venv pos v in
      (match v with
        ValNone ->
        collect is_quoted tokens vl vll

      (* Strings *)
      | ValWhite s
      | ValString s ->
        let tokens =
          if is_quoted then
            Omake_command.arg_buffer_add_data tokens s
          else
            Omake_command.arg_buffer_add_string tokens s
        in
        collect is_quoted tokens vl vll
      | ValData s ->
        collect is_quoted (Omake_command.arg_buffer_add_data tokens s) vl vll
      | ValSequence el ->
        collect is_quoted tokens el (vl :: vll)
      | ValArray el ->
        collect true tokens el (vl :: vll)

      (* Other quoted values *)
      | ValInt _
      | ValFloat _
      | ValDir _
      | ValNode _
      | ValQuote _
      | ValQuoteString _
      | ValFun _
      | ValFunCurry _
      | ValPrim _
      | ValPrimCurry _
      | ValRules _
      | ValBody _
      | ValMap _
      | ValObject _
      | ValChannel _
      | ValClass _
      | ValCases _
      | ValOther _
      | ValVar _ ->
        let tokens = Omake_command.arg_buffer_add_data tokens (string_of_value venv pos v) in
        collect is_quoted tokens vl vll

      (* Illegal values *)
      | ValStringExp _
      | ValMaybeApply _
      | ValDelayed _ ->
        raise (Omake_value_type.OmakeException (pos, StringValueError ("illegal application", v))))
    | [], vl :: vll ->
      collect is_quoted tokens vl vll
    | [], [] ->
      Omake_command.arg_buffer_contents tokens
  in
  collect false Omake_command.arg_buffer_empty vl []

and argv_of_values venv pos vll =
  List.map (arg_of_values venv pos) vll

(*
 * Boolean test.
 * Arrays are always true.
 *)
and bool_of_value venv pos v =
  let values = values_of_value venv pos v in
  match values with
    []
  | [ValNone]
  | [ValWhite _] ->
    false
  | [ValInt i] ->
    i <> 0
  | [ValFloat x] ->
    x <> 0.0
  | [ValData s]
  | [ValString s] ->
    bool_of_string s
  | [ValQuote vl] ->
    bool_of_string (string_of_quote venv pos None vl)
  | _ ->
    true

(*
 * The value should be a directory.
 *)
and file_of_value venv pos file =
  let pos = string_pos "file_of_value" pos in
  let file = eval_prim_value venv pos file in
  match file with
    ValNode node ->
    node
  | ValDir dir ->
    Omake_node.Node.node_of_dir dir
  | ValData _
  | ValString _
  | ValSequence _
  | ValQuote _
  | ValQuoteString _
  | ValInt _
  | ValFloat _ ->
    Omake_env.venv_intern venv PhonyExplicit (string_of_value venv pos file)
  | ValArray _
  | ValNone
  | ValWhite _
  | ValMaybeApply _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValRules _
  | ValStringExp _
  | ValBody _
  | ValMap _
  | ValObject _
  | ValChannel _
  | ValClass _
  | ValCases _
  | ValVar _
  | ValDelayed _
  | ValOther _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "illegal value"))

(*
 * Be lazy about concatenating arrays, to
 * avoid quadratic behavior.
 *)
and append_arrays venv pos a1 a2 : Omake_value_type.t  =
  if is_array_value a1 then
    if is_array_value a2 then
      ValArray [a1; a2]
    else
      let al = values_of_value venv pos a2 in
      ValArray (a1 :: al)
  else if is_array_value a2 then
    let al = values_of_value venv pos a1 in
    ValArray [ValArray al; a2]
  else if is_empty_value a1 then
    a2
  else if is_empty_value a2 then
    a1
  else
    ValSequence [a1; ValWhite " "; a2]

(************************************************************************
 * Evaluation.
*)

(*
 * Eval a static value.
 *)
and eval_value_static venv pos key v =
  let pos = string_pos "eval_value_static" pos in
  let obj =
    match Omake_env.venv_find_static_info venv pos key with
      StaticValue obj ->
      obj
    | StaticRule 
        { srule_env  = venv;
          srule_deps = deps;
          srule_vals = values;
          srule_exp  = e;
          srule_static ; 
          _
        } -> 
      let values = List.flatten (List.map (values_of_value venv pos) values) in
      let values = List.map (eval_prim_value venv pos) values in
      let digest = Omake_command_digest.digest_of_exp pos values e in
      let cache = Omake_env.venv_cache venv in
      let obj =
        (* Try to fetch the value from the memo *)
        try Omake_cache.find_value cache key srule_static deps digest with
          Not_found ->
          (* Finally, if we don't have a value, evaluate the rule.
           * Prevent recursive calls *)
          let () = Omake_env.venv_set_static_info venv key (StaticValue Omake_value_util.empty_obj) in
          let venv, v = eval_exp venv Omake_value_type.ValNone e in
          let obj = eval_object venv pos v in
          Omake_cache.add_value cache key srule_static deps digest (MemoSuccess obj);
          obj
      in
      Omake_env.venv_set_static_info venv key (StaticValue obj);
      obj
  in
  Omake_env.venv_find_field_internal obj pos v

and eval_value_delayed venv pos (p : Omake_value_type.value_delayed ref) =
  match !p with
  | ValValue v ->
    eval_value_core venv pos v
  | ValStaticApply (key, v) ->
    let v = eval_value_static venv pos key v in
    p := ValValue v;
    eval_value_core venv pos v

(*
 * Unfold the outermost application to get a real value.
 *)
and eval_value_core venv pos v : Omake_value_type.t =
  match v with
  | ValMaybeApply (loc, v) ->
    let v =
      try Some (Omake_env.venv_find_var_exn venv v) with
        Not_found ->
        None
    in
    begin match v with
    | Some v -> ValArray [eval_value_core venv pos (eval_var venv pos loc v)]
    | None -> ValNone
    end
  | ValDelayed p ->
    eval_value_delayed venv pos p
  | ValSequence [v] ->
    eval_value_core venv pos v
  | ValStringExp (env, e) ->
    let v = eval_string_exp (Omake_env.venv_with_env venv env) pos e in
    eval_value_core venv pos v
  | _ ->
    v

and eval_value venv pos v =
  let pos = string_pos "eval_value" pos in
  eval_value_core venv pos v

and eval_single_value venv pos v =
  let pos = string_pos "eval_single_value" pos in
  match eval_value venv pos v with
    ValArray [v] ->
    eval_single_value venv pos v
  | _ ->
    v

and eval_prim_value venv pos v : Omake_value_type.t =
  let pos = string_pos "eval_prim_value" pos in
  let v = eval_value venv pos v in
  match v with
    ValArray [v] ->
    eval_prim_value venv pos v
  | ValObject obj ->
    (try Omake_env.venv_find_field_internal_exn obj Omake_symbol.builtin_sym with
      Not_found ->
      v)
  | _ ->
    v

(*
 * The values are being flattened, so expand all sequences.
 *)
and eval_catenable_value venv pos v =
  let pos = string_pos "eval_catenable_value" pos in
  let v = eval_value venv pos v in
  match v with
    ValObject obj ->
    (try
      match Omake_env.venv_find_field_internal_exn obj Omake_symbol.builtin_sym with
        ValNone
      | ValWhite _
      | ValString _
      | ValSequence _
      | ValData _
      | ValInt _
      | ValFloat _
      | ValDir _
      | ValNode _
      | ValArray _
      | ValRules _ as v ->
        v
      | _ ->
        v
    with
      Not_found ->
      v)
  | _ ->
    v

(*
 * Evaluate the value in a function body.
 * Expand all applications.
 *)
and eval_body_value venv pos v : Omake_value_type.t =
  match (eval_value venv pos v : Omake_value_type.t) with
  | ValSequence sl ->
    ValSequence (List.map (eval_body_value venv pos) sl)
  | ValArray sl ->
    ValArray (List.map (eval_body_value venv pos) sl)
  | ValBody (body, _) ->
    snd (eval_sequence_exp venv pos body)
  | ValNone
  | ValInt _
  | ValFloat _
  | ValData _
  | ValWhite _
  | ValString _
  | ValQuote _
  | ValQuoteString _
  | ValDir _
  | ValNode _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValRules _
  | ValMap _
  | ValObject _
  | ValChannel _
  | ValClass _
  | ValCases _
  | ValVar _
  | ValOther _ as result ->
    result
  | ValStringExp _
  | ValMaybeApply _
  | ValDelayed _ ->
    raise (Invalid_argument "eval_body_value")

and eval_body_exp venv pos x v : (Omake_env.t * Omake_value_type.t) =
  match (eval_value venv pos v : Omake_value_type.t) with
  | ValSequence sl ->
    venv, ValSequence (List.map (eval_body_value venv pos) sl)
  | ValArray sl ->
    venv, ValArray (List.map (eval_body_value venv pos) sl)
  | ValBody (body, export) ->
    eval_sequence_export venv pos x body export
  | ValNone
  | ValInt _
  | ValFloat _
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValWhite _
  | ValString _
  | ValDir _
  | ValNode _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValRules _
  | ValMap _
  | ValObject _
  | ValChannel _
  | ValClass _
  | ValCases _
  | ValVar _
  | ValOther _ as result ->
    venv, result
  | ValStringExp _
  | ValMaybeApply _
  | ValDelayed _ ->
    raise (Invalid_argument "eval_body_exp")

(*
 * Evaluate a variable.
 * It is fine for the variable to evaluate to a function.
 * But if the function has arity 0, then evaluate it.
 *)
and eval_var venv pos loc v =
  match v with
  | ValFun (env, _, [], body, _) ->
    let venv = Omake_env.venv_with_env venv env in
    let _, result = eval_sequence venv pos Omake_value_type.ValNone body in
    result
  | ValFunCurry (env, args, _, [], body, _, []) ->
    let venv = Omake_env.venv_with_partial_args venv env args in
    let _, result = eval_sequence venv pos ValNone body in
    result
  | ValFunCurry (env, args, _, [], body, export, kargs) ->
    (* XXX: verify that we should pass forward the exports *)
    let venv_new = Omake_env.venv_with_partial_args venv env args in
    let venv_new, v = eval_sequence venv_new pos ValNone body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    eval_apply venv pos loc v [] kargs
  | ValPrim (_, _, ApplyEmpty, f) ->
    snd (Omake_env.venv_apply_prim_fun f venv pos loc [] [])
  | _ ->
    v

(*
 * Evaluate a key.
 *)
and eval_key venv pos loc v =
  try
    let map = eval_map venv pos (Omake_env.venv_find_var_exn venv Omake_var.map_field_var) in
    Omake_env.venv_map_find map pos (ValData v)
  with
    Not_found ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, UnboundKey v))

(*
 * Evaluate an application.
 *)
and eval_apply venv pos loc v args kargs =
  let pos = string_pos "eval_apply" pos in
  match eval_value venv pos v with
    ValFun (env, keywords, params, body, _) ->
    let venv = Omake_env.venv_add_args venv pos loc env params args keywords kargs in
    let _, result = eval_sequence_exp venv pos body in
    result
  | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
    let venv_new, args, kargs = Omake_env.venv_add_curry_args venv pos loc env pargs params args keywords kargs1 kargs in
    let venv_new, v = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    eval_apply venv pos loc v args kargs
  | ValPrim (_, _, _, f) ->
    snd (Omake_env.venv_apply_prim_fun f venv pos loc args kargs)
  | ValPrimCurry (_, _, f, args1, kargs1) ->
    snd (Omake_env.venv_apply_prim_fun f venv pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs))
  | v ->
    if args = [] && kargs = [] then
      v
    else
      let print_error buf =
        Format.fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" Omake_value_print.pp_print_value v;
        List.iter (fun arg ->
          Format.fprintf buf "@ @[<hv 3>arg = %a@]" Omake_value_print.pp_print_value arg) args;
        List.iter (fun (v, arg) ->
          Format.fprintf buf "@ @[<hv 3>%a = %a@]" Lm_symbol.pp_print_symbol v Omake_value_print.pp_print_value arg) kargs;
        Format.fprintf buf "@]"
      in
      raise (Omake_value_type.OmakeException (pos, LazyError print_error))

(*
 * Evaluate an application with string arguments.
 *)
and eval_apply_string_exp venv venv_obj pos loc v args kargs =
  let pos = string_pos "eval_apply_string_exp" pos in
  match eval_value venv pos v with
    ValFun (env, keywords, params, body, _) ->
    let args = List.map (eval_string_exp venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
    let venv_new = Omake_env.venv_add_args venv_obj pos loc env params args keywords kargs in
    let _, result = eval_sequence_exp venv_new pos body in
    result
  | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
    let args = List.map (eval_string_exp venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
    let venv_new, args, kargs = Omake_env.venv_add_curry_args venv_obj pos loc env pargs params args keywords kargs1 kargs in
    let venv_new, v = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    eval_apply venv pos loc v args kargs
  | ValPrim (_, be_eager, _, f) ->
    let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp true venv pos s) kargs in
    snd (Omake_env.venv_apply_prim_fun f venv_obj pos loc args kargs)
  | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
    let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp true venv pos s) kargs in
    snd (Omake_env.venv_apply_prim_fun f venv_obj pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs))
  | v ->
    if args = [] && kargs = [] then
      v
    else
      let print_error buf =
        Format.fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" Omake_value_print.pp_print_value v;
        List.iter (fun arg ->
          Format.fprintf buf "@ @[<hv 3>arg = %a@]" Omake_ir_print.pp_print_string_exp arg) args;
        List.iter (fun (v, arg) ->
          Format.fprintf buf "@ @[<hv 3>%a = %a@]" Lm_symbol.pp_print_symbol v Omake_ir_print.pp_print_string_exp arg) kargs;
        Format.fprintf buf "@]"
      in
      raise (Omake_value_type.OmakeException (pos, LazyError print_error))

(*
 * Get a function from a value.
 *)
and eval_fun venv pos v =
  match eval_value venv pos v with
    ValFun (env, keywords, params, body, export) ->
    let f venv pos loc args kargs =
      let venv_new = Omake_env.venv_add_args venv pos loc env params args keywords kargs in
      let venv_new, result = eval_sequence_exp venv_new pos body in
      let venv = Omake_env.add_exports venv venv_new pos export in
      venv, result
    in
    true, f
  | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
    let f venv pos loc args kargs =
      let venv_new, args, kargs = Omake_env.venv_add_curry_args venv pos loc env pargs params args keywords kargs1 kargs in
      let venv_new, v = eval_sequence_exp venv_new pos body in
      let venv = Omake_env.add_exports venv venv_new pos export in
      eval_apply_export venv pos loc v args kargs
    in
    true, f
  | ValPrim (_, be_eager, _, f) ->
    be_eager, Omake_env.venv_apply_prim_fun f
  | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
    let f venv pos loc args2 kargs2 =
      Omake_env.venv_apply_prim_fun f venv pos loc (List.rev_append args1 args2) (List.rev_append kargs1 kargs2)
    in
    be_eager, f
  | ValBody (body, export) ->
    let f venv pos loc args kargs =
      if args <> [] || kargs <> [] then
        raise (Omake_value_type.OmakeException (loc_pos loc pos, 
            ArityMismatch (ArityExact 0, List.length args)));
      eval_sequence_export_exp venv pos body export
    in
    true, f
  | _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "not a function"))

(*
 * Get an object from a variable.
 *)
and eval_map venv pos x =
  match eval_value venv pos x with
    ValMap map ->
    map
  | _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "not a map"))

and eval_object venv pos x =
  try eval_object_exn venv pos x with
    Not_found ->
    raise (Omake_value_type.OmakeException (pos, StringError "not an object"))

and eval_object_exn venv pos x =
  let x = eval_value venv pos x in
  match x with
    ValObject env ->
    env
  | ValInt _
  | ValOther (ValExitCode _) ->
    create_object venv x Omake_var.int_object_var
  | ValFloat _ ->
    create_object venv x Omake_var.float_object_var
  | ValData _
  | ValQuote _
  | ValQuoteString _ ->
    create_object venv x Omake_var.string_object_var
  | ValSequence _
  | ValWhite _
  | ValString _
  | ValNone ->
    create_object venv x Omake_var.sequence_object_var
  | ValArray _ ->
    create_object venv x Omake_var.array_object_var
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _ ->
    create_object venv x Omake_var.fun_object_var
  | ValRules _ ->
    create_object venv x Omake_var.rule_object_var
  | ValNode _ ->
    create_object venv x Omake_var.file_object_var
  | ValDir _ ->
    create_object venv x Omake_var.dir_object_var
  | ValBody _ ->
    create_object venv x Omake_var.body_object_var
  | ValChannel (InChannel, _) ->
    create_object venv x Omake_var.in_channel_object_var
  | ValChannel (OutChannel, _) ->
    create_object venv x Omake_var.out_channel_object_var
  | ValChannel (InOutChannel, _) ->
    create_object venv x Omake_var.in_out_channel_object_var
  | ValOther (ValLexer _) ->
    create_object venv x Omake_var.lexer_object_var
  | ValOther (ValParser _) ->
    create_object venv x Omake_var.parser_object_var
  | ValOther (ValLocation _) ->
    create_object venv x Omake_var.location_object_var
  | ValOther (ValEnv _) ->
    raise (Omake_value_type.OmakeException (pos, StringError "dereferenced <env>"))
  | ValClass _ ->
    raise (Invalid_argument "internal error: dereferenced $class")
  | ValCases _ ->
    raise (Invalid_argument "internal error: dereferenced cases")
  | ValMap _ ->
    create_map venv x Omake_var.map_object_var
  | ValVar _ ->
    create_object venv x Omake_var.var_object_var
  | ValStringExp _
  | ValMaybeApply _
  | ValDelayed _ ->
    raise (Invalid_argument "find_object")

and create_object venv x v =
  let obj = Omake_env.venv_find_var_exn venv v in
  match obj with
    ValObject env ->
    Omake_env.venv_add_field_internal env Omake_symbol.builtin_sym x
  | _ ->
    raise Not_found

and create_map venv x v =
  let obj = Omake_env.venv_find_var_exn venv v in
  match obj with
    ValObject env ->
    Omake_env.venv_add_field_internal env Omake_symbol.map_sym x
  | _ ->
    raise Not_found

(*
 * Field operations.
 *)
and eval_find_field_exn venv path obj pos vl =
  match vl with
    [v] ->
    path, obj, v
  | v :: vl ->
    let path, v = Omake_env.venv_find_field_path_exn venv path obj pos v in
    let obj = eval_object_exn venv pos v in
    eval_find_field_exn venv path obj pos vl
  | [] ->
    raise (Omake_value_type.OmakeException (pos, StringError "empty method name"))

and eval_find_field_aux venv envl pos v vl =
  match envl with
  | [env] ->
    let env = eval_object_exn venv pos env in
    let path : Omake_value_type.path = PathVar v in
    eval_find_field_exn venv path env pos vl
  | env :: envl ->
    let env = eval_object_exn venv pos env in
    (try eval_find_field_exn venv (PathVar v) env pos vl with
      Not_found ->
      eval_find_field_aux venv envl pos v vl)
  | [] ->
    raise Not_found

and eval_find_field venv pos _ v vl =
  let envl = Omake_env.venv_current_objects venv pos v in
  try eval_find_field_aux venv envl pos v vl with
    Not_found ->
    let pos = string_pos "eval_find_field" pos in
    raise (Omake_value_type.OmakeException (pos, UnboundMethod vl))

(*
 * Method paths.
 *)
and eval_with_method_exn venv path obj pos vl =
  match vl with
    [v] ->
    let v = Omake_env.venv_find_field_exn venv obj pos v in
    let venv = Omake_env.venv_with_object venv obj in
    venv, path, v
  | v :: vl ->
    let path, v = Omake_env.venv_find_field_path_exn venv path obj pos v in
    let obj = eval_object_exn venv pos v in
    eval_with_method_exn venv path obj pos vl
  | [] ->
    raise (Omake_value_type.OmakeException (pos, StringError "empty method name"))

and eval_with_method_aux venv envl pos v vl =
  match envl with
  | [env] ->
    let env = eval_object_exn venv pos env in
    eval_with_method_exn venv (PathVar v) env pos vl
  | env :: envl ->
    let env = eval_object_exn venv pos env in
    (try eval_with_method_exn venv (PathVar v) env pos vl with
      Not_found ->
      eval_with_method_aux venv envl pos v vl)
  | [] ->
    raise Not_found

and eval_with_method venv pos loc v vl =
  let envl = Omake_env.venv_current_objects venv pos v in
  try eval_with_method_aux venv envl pos v vl with
    Not_found ->
    let pos = string_pos "eval_with_method" (loc_pos loc pos) in
    raise (Omake_value_type.OmakeException (pos, UnboundMethod vl))

(*
 * Method paths.
 *)
and eval_find_method_exn venv obj pos vl =
  match vl with
    [v] ->
    let v = Omake_env.venv_find_field_exn venv obj pos v in
    let venv = Omake_env.venv_with_object venv obj in
    venv, v
  | v :: vl ->
    let v = Omake_env.venv_find_field_exn venv obj pos v in
    let obj = eval_object_exn venv pos v in
    eval_find_method_exn venv obj pos vl
  | [] ->
    raise (Omake_value_type.OmakeException (pos, StringError "empty method name"))

and eval_find_method_aux venv envl pos vl =
  match envl with
    [env] ->
    let env = eval_object_exn venv pos env in
    eval_find_method_exn venv env pos vl
  | env :: envl ->
    let env = eval_object_exn venv pos env in
    (try eval_find_method_exn venv env pos vl with
      Not_found ->
      eval_find_method_aux venv envl pos vl)
  | [] ->
    raise Not_found

and eval_find_method venv pos loc v vl =
  let envl = Omake_env.venv_current_objects venv pos v in
  try eval_find_method_aux venv envl pos vl with
    Not_found ->
    let pos = string_pos "eval_find_method" (loc_pos loc pos) in
    raise (Omake_value_type.OmakeException (pos, UnboundMethod vl))

(*
 * Check whether a field is defined.
 *)
and eval_defined_field_exn venv env pos vl =
  match vl with
    [v] ->
    Omake_env.venv_defined_field venv env v
  | v :: vl ->
    let v = Omake_env.venv_find_field_exn venv env pos v in
    let obj = eval_object_exn venv pos v in
    eval_defined_field_exn venv obj pos vl
  | [] ->
    raise (Omake_value_type.OmakeException (pos, StringError "empty method name"))

and eval_defined_field_aux venv envl pos vl =
  match envl with
    [env] ->
    let env = eval_object_exn venv pos env in
    eval_defined_field_exn venv env pos vl
  | env :: envl ->
    let env = eval_object_exn venv pos env in
    (try eval_defined_field_exn venv env pos vl with
      Not_found ->
      eval_defined_field_aux venv envl pos vl)
  | [] ->
    raise Not_found

and eval_defined_field venv pos _ v vl =
  let envl = Omake_env.venv_current_objects venv pos v in
  try eval_defined_field_aux venv envl pos vl with
    Not_found ->
    false

(*
 * Simplify a quoted value if possible.
 * Strings are concatenated.
 *)
and simplify_quote_val venv pos c (el : Omake_value_type.t list) : Omake_value_type.t  =
  match el with
  | [ValWhite s]
  | [ValString s]
  | [ValData s] ->
    begin match c with
    | None ->
      ValData s
    | Some c ->
      ValQuoteString (c, [ValData s])
    end
  | _ ->
    let buf = Buffer.create 32 in
    let flush vl : Omake_value_type.t list =
      if Buffer.length buf = 0 then
        vl
      else
        let s = Buffer.contents buf in
        Buffer.clear buf;
        ValData s :: vl in
    let rec collect vl el =
      match el with
      | e :: el ->
        begin match eval_value venv pos e with
          ValWhite s
        | ValString s
        | ValData s ->
          Buffer.add_string buf s;
          collect vl el
        | v ->
          collect (v :: flush vl) el
        end
      | [] ->
        List.rev (flush vl)
    in
    match collect [] el with
      [ValWhite s]
    | [ValString s]
    | [ValData s] ->
      ValData s
    | el ->
      match c with
        None ->
        ValQuote el
      | Some c ->
        ValQuoteString (c, el)

(*
 * Evaluate a string expression.
 *)
and eval_string_exp venv pos s =
  let pos = string_pos "eval_string_exp" pos in
  match s with
    NoneString _ ->
    ValNone
  | IntString (_, i) ->
    ValInt i
  | FloatString (_, x) ->
    ValFloat x
  | WhiteString (_, s) ->
    ValWhite s
  | ConstString (_, s) ->
    ValString s
  | KeyApplyString (loc, v) ->
    eval_key venv pos loc v
  | FunString (_, opt_params, params, body, export) ->
    let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
    let env = Omake_env.venv_get_env venv in
    ValFun (env, opt_params, params, body, export)
  | ApplyString (loc, v, [], []) ->
    eval_var venv pos loc (Omake_env.venv_find_var venv pos loc v)
  | ApplyString (loc, v, args, kargs) ->
    eval_apply_string_exp venv venv pos loc (Omake_env.venv_find_var venv pos loc v) args kargs
  | SuperApplyString (loc, super, v, args, kargs) ->
    let v = Omake_env.venv_find_super_field venv pos loc super v in
    eval_apply_string_exp venv venv pos loc v args kargs
  | MethodApplyString (loc, v, vl, args, kargs) ->
    let venv_obj, v = eval_find_method venv pos loc v vl in
    eval_apply_string_exp venv venv_obj pos loc v args kargs
  | SequenceString (_, sl) ->
    ValSequence (List.map (eval_string_exp venv pos) sl)
  | ObjectString (_, e, export)
  | BodyString (_, e, export) ->
    ValBody (e, export)
  | ArrayString (_, el) ->
    ValArray (List.map (eval_string_exp venv pos) el)
  | ArrayOfString (_, e) ->
    let v = eval_string_exp venv pos e in
    ValArray (values_of_value venv pos v)
  | ExpString (_, e, _) ->
    let _, result = eval_sequence_exp venv pos e in
    result
  | CasesString (_, cases) ->
    let cases =
      List.map (fun (v, e1, e2, export) ->
        v, eval_string_exp venv pos e1, e2, export) cases
    in
    ValCases cases
  | QuoteString (_, el) ->
    simplify_quote_val venv pos None (List.map (eval_string_exp venv pos) el)
  | QuoteStringString (_, c, el) ->
    simplify_quote_val venv pos (Some c) (List.map (eval_string_exp venv pos) el)
  | VarString (loc, v) ->
    ValVar (loc, v)
  | ThisString _ ->
    ValObject (Omake_env.venv_this venv)
  | LazyString (_, s) ->
    ValStringExp (Omake_env.venv_get_env venv, s)
  | LetVarString (_, v, s1, s2) ->
    let x = eval_string_exp venv pos s1 in
    let venv = Omake_env.venv_add_var venv v x in
    eval_string_exp venv pos s2

(* and eval_keyword_string_exp venv pos (v, s) = *)
(*   v, eval_string_exp venv pos s *)

and eval_keyword_param_value_list_exp venv pos opt_params =
  List.map (eval_keyword_param_value_exp venv pos) opt_params

and eval_keyword_param_value_exp venv pos = function
    v, v_info, Some s ->
    v, v_info, Some (eval_string_exp venv pos s)
  | _, _, None as param ->
    param

and eval_prim_arg_exp be_eager venv pos s =
  if be_eager then
    eval_string_exp venv pos s
  else
    ValStringExp (Omake_env.venv_get_env venv, s)

(************************************************************************
 * Export versions.
 *
 * These functions with the _export suffix also allow modifications
 * to the environment.
*)
and eval_var_export venv pos loc (v : Omake_value_type.t) =
  let pos = string_pos "eval_var_export" pos in

  (* Do not use eval_value; we don't want to force evaluation *)
  match v with
  | ValFun (env, _, [], body, export) ->
    let venv_new = Omake_env.venv_with_env venv env in
    let venv_new, result = eval_sequence venv_new pos ValNone body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    venv, result
  | ValFunCurry (env, pargs, _, [], body, export, []) ->
    let venv_new = Omake_env.venv_with_partial_args venv env pargs in
    let venv_new, result = eval_sequence venv_new pos ValNone body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    venv, result
  | ValFunCurry (env, pargs, _, [], body, export, kargs) ->
    let venv_new = Omake_env.venv_with_partial_args venv env pargs in
    let venv_new, v = eval_sequence venv_new pos ValNone body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    eval_apply_export venv pos loc v [] kargs
  | ValPrim (_, _, ApplyEmpty, f) ->
    Omake_env.venv_apply_prim_fun f venv pos loc [] []
  | _ ->
    venv, v

(*
 * Evaluate an application.
 *)
and eval_apply_export venv pos loc v args kargs =
  let pos = string_pos "eval_apply_export" pos in
  match (eval_value venv pos v : Omake_value_type.t) with
  | ValFun (env, keywords, params, body, export) ->
    let venv_new = Omake_env.venv_add_args venv pos loc env params args keywords kargs in
    let venv_new, result = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    venv, result
  | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
    let venv_new, args, kargs = Omake_env.venv_add_curry_args venv pos loc env pargs params args keywords kargs1 kargs in
    let venv_new, v = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    eval_apply_export venv pos loc v args kargs
  | ValPrim (_, _, _, f) ->
    Omake_env.venv_apply_prim_fun f venv pos loc args kargs
  | ValPrimCurry (_, _, f, args1, kargs1) ->
    Omake_env.venv_apply_prim_fun f venv pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs)
  | v ->
    if args = [] && kargs = [] then
      venv, v
    else
      let print_error buf =
        Format.fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" Omake_value_print.pp_print_value v;
        List.iter (fun arg ->
          Format.fprintf buf "@ @[<hv 3>arg = %a@]" Omake_value_print.pp_print_value arg) args;
        List.iter (fun (v, arg) ->
          Format.fprintf buf "@ @[<hv 3>%a = %a@]" Lm_symbol.pp_print_symbol v Omake_value_print.pp_print_value arg) kargs;
        Format.fprintf buf "@]"
      in
      raise (Omake_value_type.OmakeException (pos, LazyError print_error))

and eval_partial_apply venv pos loc v args kargs :  (Omake_env.t * Omake_value_type.t )=
  match eval_value venv pos v with
  | ValFun (env, keywords, params, body, export) ->
    begin match 
      (Omake_env.venv_add_partial_args venv pos loc env [] params args keywords [] kargs
      ) with
    | PartialApply (env, pargs, keywords, params, kargs) ->
      venv, ValFunCurry (env, pargs, keywords, params, body, export, kargs)
    | FullApply (venv, args, kargs) ->
      let venv_new, v = eval_sequence_exp venv pos body in
      let venv = Omake_env.add_exports venv venv_new pos export in
      eval_partial_apply venv pos loc v args kargs
    end
  | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
    (match Omake_env.venv_add_partial_args venv pos loc env pargs params args keywords kargs1 kargs with
      PartialApply (env, pargs, keywords, params, kargs) ->
      venv, ValFunCurry (env, pargs, keywords, params, body, export, kargs)
    | FullApply (venv, args, kargs) ->
      let venv_new, v = eval_sequence_exp venv pos body in
      let venv = Omake_env.add_exports venv venv_new pos export in
      eval_partial_apply venv pos loc v args kargs)
  | ValPrim (arity, eager, _, f) ->
    (match arity_apply_args arity [] args with
      FullArity (current_args, rest_args) ->
      (* We assume the primitive takes all the keyword args *)
      let venv, v = Omake_env.venv_apply_prim_fun f venv pos loc current_args kargs in
      eval_partial_apply venv pos loc v rest_args []
    | PartialArity (arity, args) ->
      venv, ValPrimCurry (arity, eager, f, args, List.rev kargs))
  | ValPrimCurry (arity, eager, f, args1, kargs1) ->
    (match arity_apply_args arity args1 args with
      FullArity (current_args, rest_args) ->
      (* We assume the primitive takes all the keyword args *)
      let venv, v = Omake_env.venv_apply_prim_fun f venv pos loc current_args kargs in
      eval_partial_apply venv pos loc v rest_args []
    | PartialArity (arity, args) ->
      venv, ValPrimCurry (arity, eager, f, args, List.rev_append kargs kargs1))
  | v ->
    if args = [] && kargs = [] then
      venv, v
    else
      let print_error buf =
        Format.fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" Omake_value_print.pp_print_value v;
        List.iter (fun arg ->
          Format.fprintf buf "@ @[<hv 3>arg = %a@]" Omake_value_print.pp_print_value arg) args;
        List.iter (fun (v, arg) ->
          Format.fprintf buf "@ @[<hv 3>%a = %a@]" Lm_symbol.pp_print_symbol v Omake_value_print.pp_print_value arg) kargs;
        Format.fprintf buf "@]"
      in
      raise (Omake_value_type.OmakeException (pos, LazyError print_error))

and eval_apply_string_export_exp venv venv_new pos loc v args kargs =
  let pos = string_pos "eval_apply_string_export_exp" pos in
  match eval_value venv pos v with
    ValFun (env, keywords, params, body, export) ->
    let args = List.map (eval_string_exp venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
    let venv_new = Omake_env.venv_add_args venv_new pos loc env params args keywords kargs in
    let venv_new, result = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    venv, result
  | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
    let args = List.map (eval_string_exp venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
    let venv_new, args, kargs = Omake_env.venv_add_curry_args venv_new pos loc env pargs params args keywords kargs1 kargs in
    let venv_new, v = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_exports venv venv_new pos export in
    eval_apply_export venv pos loc v args kargs
  | ValPrim (_, be_eager, _, f) ->
    let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
    Omake_env.venv_apply_prim_fun f venv_new pos loc args kargs
  | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
    let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
    Omake_env.venv_apply_prim_fun f venv_new pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs)
  | v ->
    if args = [] && kargs = [] then
      venv, v
    else
      let print_error buf =
        Format.fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" Omake_value_print.pp_print_value v;
        List.iter (fun arg ->
          Format.fprintf buf "@ @[<hv 3>arg = %a@]" Omake_ir_print.pp_print_string_exp arg) args;
        List.iter (fun (v, arg) ->
          Format.fprintf buf "@ @[<hv 3>%a = %a@]" Lm_symbol.pp_print_symbol v Omake_ir_print.pp_print_string_exp arg) kargs;
        Format.fprintf buf "@]"
      in
      raise (Omake_value_type.OmakeException (pos, LazyError print_error))

and eval_apply_method_export_exp venv venv_obj pos loc path v args kargs =
  let pos = string_pos "eval_apply_method_export_exp" pos in
  match eval_value venv pos v with
    ValFun (env, keywords, params, body, export) ->
    let args = List.map (eval_string_exp venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
    let venv_new = Omake_env.venv_add_args venv_obj pos loc env params args keywords kargs in
    let venv_new, result = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_path_exports venv venv_obj venv_new pos path export in
    venv, result
  | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
    (* XXX: JYH: this, need to think about *)
    let args = List.map (eval_string_exp venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
    let venv_new, args, kargs = Omake_env.venv_add_curry_args venv_obj pos loc env pargs params args keywords kargs1 kargs in
    let venv_new, v = eval_sequence_exp venv_new pos body in
    let venv = Omake_env.add_path_exports venv venv_obj venv_new pos path export in
    eval_apply_export venv pos loc v args kargs
  | ValPrim (_, be_eager, _, f) ->
    let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
    let venv_new, result = Omake_env.venv_apply_prim_fun f venv_obj pos loc args kargs in
    let venv = Omake_env.hoist_this venv venv_new path in
    venv, result
  | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
    let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
    let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
    let venv_new, result = Omake_env.venv_apply_prim_fun f venv_obj pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs) in
    let venv = Omake_env.hoist_this venv venv_new path in
    venv, result
  | v ->
    if args = [] && kargs = [] then
      venv, v
    else
      let print_error buf =
        Format.fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" Omake_value_print.pp_print_value v;
        List.iter (fun arg ->
          Format.fprintf buf "@ @[<hv 3>arg = %a@]" Omake_ir_print.pp_print_string_exp arg) args;
        List.iter (fun (v, arg) ->
          Format.fprintf buf "@ @[<hv 3>%a = %a@]" Lm_symbol.pp_print_symbol v Omake_ir_print.pp_print_string_exp arg) kargs;
        Format.fprintf buf "@]"
      in
      raise (Omake_value_type.OmakeException (pos, LazyError print_error))

(*
 * Evaluate a string expression, and allow exports.
 *)
and eval_string_export_exp venv pos ( s : Omake_ir.string_exp)
  : (Omake_env.t * Omake_value_type.t)=
  let pos = string_pos "eval_string_export_exp" pos in
  match s with
  | NoneString _ ->
    venv, ValNone
  | IntString (_, i) ->
    venv, ValInt i
  | FloatString (_, x) ->
    venv, ValFloat x
  | WhiteString (_, s) ->
    venv, ValWhite s
  | ConstString (_, s) ->
    venv, ValString s
  | KeyApplyString (loc, v) ->
    venv, eval_key venv pos loc v
  | FunString (_, opt_params, params, body, export) ->
    let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
    let env = Omake_env.venv_get_env venv in
    venv, ValFun (env, opt_params, params, body, export)
  | ApplyString (loc, v, [], []) ->
    eval_var_export venv pos loc (Omake_env.venv_find_var venv pos loc v)
  | ApplyString (loc, v, args, kargs) ->
    eval_apply_string_export_exp venv venv pos loc (Omake_env.venv_find_var venv pos loc v) args kargs
  | SuperApplyString (loc, super, v, args, kargs) ->
    let v = Omake_env.venv_find_super_field venv pos loc super v in
    eval_apply_string_export_exp venv venv pos loc v args kargs
  | MethodApplyString (loc, v, vl, args, kargs) ->
    let venv_obj, path, v = eval_with_method venv pos loc v vl in
    eval_apply_method_export_exp venv venv_obj pos loc path v args kargs
  | SequenceString (_, sl) ->
    venv, ValSequence (List.map (eval_string_exp venv pos) sl)
  | ObjectString (_, e, export)
  | BodyString (_, e, export) ->
    venv, ValBody (e, export)
  | ArrayString (_, el) ->
    venv, ValArray (List.map (eval_string_exp venv pos) el)
  | ArrayOfString (_, e) ->
    let v = eval_string_exp venv pos e in
    venv, ValArray (values_of_value venv pos v)
  | ExpString (_, e, export) ->
    eval_sequence_export_exp venv pos e export
  | CasesString (_, cases) ->
    let cases =
      List.map (fun (v, e1, e2, export) ->
        v, eval_string_exp venv pos e1, e2, export) cases
    in
    venv, ValCases cases
  | QuoteString (_, el) ->
    venv, simplify_quote_val venv pos None (List.map (eval_string_exp venv pos) el)
  | QuoteStringString (_, c, el) ->
    venv, simplify_quote_val venv pos (Some c) (List.map (eval_string_exp venv pos) el)
  | VarString (loc, v) ->
    venv, ValVar (loc, v)
  | ThisString _ ->
    venv, ValObject (Omake_env.venv_this venv)
  | LazyString (_, s) ->
    venv, ValStringExp (Omake_env.venv_get_env venv, s)
  | LetVarString (_, v, s1, s2) ->
    let venv, x = eval_string_export_exp venv pos s1 in
    let venv = Omake_env.venv_add_var venv v x in
    eval_string_export_exp venv pos s2

(************************************************************************
 * Evaluate an expression.
*)
and eval_exp venv _ e =
  let pos = string_pos "eval_exp" (ir_exp_pos e) in
  match e with
    LetVarExp (_, v, [], flag, s) ->
    eval_let_var_exp venv pos v flag s
  | LetVarExp (loc, v, vl, flag, s) ->
    eval_let_var_field_exp venv pos loc v vl flag s
  | LetKeyExp (_, v, flag, s) ->
    eval_let_key_exp venv pos v flag s
  | LetFunExp (loc, v, [], curry, opt_params, params, body, export) ->
    eval_let_fun_exp venv pos loc v curry opt_params params body export
  | LetFunExp (loc, v, vl, curry, opt_params, params, body, export) ->
    eval_let_fun_field_exp venv pos loc v vl curry opt_params params body export
  | LetObjectExp (_, v, [], s, e, export) ->
    eval_let_object_exp venv pos v s e export
  | LetObjectExp (loc, v, vl, s, e, export) ->
    eval_let_object_field_exp venv pos loc v vl s e export
  | LetThisExp (_, e) ->
    eval_let_this_exp venv pos e
  | ShellExp (loc, e) ->
    eval_shell_exp venv pos loc e
  | IfExp (_, cases) ->
    eval_if_exp venv pos cases
  | SequenceExp (_, e) ->
    eval_sequence_exp venv pos e
  | SectionExp (_, _, e, export) ->
    eval_section_exp venv pos e export
  | OpenExp (loc, s) ->
    eval_open_exp venv pos loc s
  | IncludeExp (loc, s, e) ->
    eval_include_exp venv pos loc s e
  | ApplyExp (loc, f, args, kargs) ->
    eval_apply_exp venv pos loc f args kargs
  | SuperApplyExp (loc, super, v, args, kargs) ->
    eval_super_apply_exp venv pos loc super v args kargs
  | MethodApplyExp (loc, v, vl, args, kargs) ->
    eval_method_apply_exp venv pos loc v vl args kargs
  | ReturnBodyExp (_, e, id) ->
    eval_return_body_exp venv pos e id
  | StringExp (_, s) ->
    eval_string_value_exp venv pos s
  | ReturnExp (loc, s, id) ->
    eval_return_exp venv pos loc s id
  | ReturnSaveExp _ ->
    eval_return_save_exp venv pos
  | ReturnObjectExp (_, names) ->
    eval_return_object_exp venv pos names
  | KeyExp (loc, v) ->
    eval_key_exp venv pos loc v
  | StaticExp (_, node, key, e) ->
    eval_static_exp venv pos node key e

(*
 * Variable definitions.
 *)
and eval_let_var_exp venv pos v flag s =
  let pos = string_pos "eval_var_exp" pos in
  let venv, s = eval_string_export_exp venv pos s in
  let s =
    match flag with
      VarDefNormal ->
      s
    | VarDefAppend ->
      append_arrays venv pos (Omake_env.venv_get_var venv pos v) s
  in
  let venv = Omake_env.venv_add_var venv v s in
  venv, s

and eval_let_var_field_exp venv pos loc v vl flag s =
  let pos = string_pos "eval_var_field_exp" pos in
  let venv, e = eval_string_export_exp venv pos s in
  let path, obj, v = eval_find_field venv pos loc v vl in
  let e =
    match flag with
      VarDefNormal ->
      e
    | VarDefAppend ->
      append_arrays venv pos (Omake_env.venv_find_field venv obj pos v) e
  in
  let venv, obj = Omake_env.venv_add_field venv obj pos v e in
  let venv = Omake_env.hoist_path venv path obj in
  venv, e

(*
 * Key (property) definitions.
 *)
and eval_let_key_exp venv pos v flag s =
  let pos = string_pos "eval_let_key_exp" pos in

  let venv, s = eval_string_export_exp venv pos s in

  (* Get the current property list *)
  let map =
    try Omake_env.venv_find_var_exn venv Omake_var.map_field_var with
      Not_found ->
      raise (Omake_value_type.OmakeException (pos, StringError "current object is not a Map"))
  in
  let map = eval_map venv pos map in
  let v : Omake_value_type.t = ValData v in
  (* Add the new definition *)
  let s =
    match flag with
    | VarDefNormal ->
      s
    | VarDefAppend ->
      append_arrays venv pos (Omake_env.venv_map_find map pos v) s
  in
  let map = Omake_env.venv_map_add map pos v s in
  let venv = Omake_env.venv_add_var venv Omake_var.map_field_var (ValMap map) in
  venv, s

(*
 * Function definitions.
 *)
and eval_let_fun_exp venv pos _ v curry opt_params params body export =
  let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
  let env = Omake_env.venv_get_env venv in
  let e : Omake_value_type.t =
    if curry then
      ValFunCurry (env, [], opt_params, params, body, export, [])
    else
      ValFun (env, opt_params, params, body, export)
  in
  let venv = Omake_env.venv_add_var venv v e in
  venv, e

and eval_let_fun_field_exp venv pos loc v vl curry opt_params params body export =
  let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
  let env = Omake_env.venv_get_env venv in
  let e : Omake_value_type.t =
    if curry then
      ValFunCurry (env, [], opt_params, params, body, export, [])
    else
      ValFun (env, opt_params, params, body, export)
  in
  let path, obj, v = eval_find_field venv pos loc v vl in
  let venv, obj = Omake_env.venv_add_field venv obj pos v e in
  let venv = Omake_env.hoist_path venv path obj in
  venv, e

(*
 * Shell expression.
 *)
and eval_shell_exp venv pos loc e =
  let pos = string_pos "eval_shell_exp" pos in
  let () =
    if !Omake_shell_type.debug_shell then
      Format.eprintf "@[<v 3>eval_shell_exp (pid = %i):@ %a@]@." (**)
        (Unix.getpid()) Omake_ir_print.pp_print_string_exp e
  in
  let v = Omake_env.venv_find_var venv pos loc Omake_var.system_var in
  let venv, s = eval_string_export_exp venv pos e in
  eval_apply_export venv pos loc v [s] []

(*
 * Conditionals.
 * The test should expand to a Boolean of some form.
 *)
and eval_if_cases venv pos cases =
  match cases with
    (s, el, export) :: cases ->
    let s = eval_string_exp venv pos s in
    let b = bool_of_value venv pos s in
    if b then
      eval_sequence_export_exp venv pos el export
    else
      eval_if_cases venv pos cases
  | [] ->
    venv, ValNone

and eval_if_exp venv pos cases =
  let pos = string_pos "eval_if_exp" pos in
  eval_if_cases venv pos cases

(*
 * Sequence.
 *)
and eval_sequence venv pos result el =
  match el with
    e :: el ->
    let venv, result = eval_exp venv result e in
    eval_sequence venv pos result el
  | [] ->
    venv, result

and eval_sequence_export venv pos result el export =
  let venv_new, result = eval_sequence venv pos result el in
  let venv = Omake_env.add_exports venv venv_new pos export in
  venv, result

and eval_sequence_exp venv pos el =
  let pos = string_pos "eval_sequence_exp" pos in
  eval_sequence venv pos ValNone el

and eval_sequence_export_exp venv pos el export =
  let pos = string_pos "eval_sequence_export_exp" pos in
  eval_sequence_export venv pos ValNone el export

and eval_section_exp venv pos el export =
  let pos = string_pos "eval_section_exp" pos in
  eval_sequence_export venv pos ValNone el export

(*
 * Look for a cached object.  If it does not exist,
 * then evaluate the body to create the object.
 * Inline all the fields.
 *)
and eval_static_exp venv pos node key el =
  let pos = string_pos "eval_static_exp" pos in
  let obj =
    try Omake_env.venv_find_static_object venv node key with
      Not_found ->
      (* Evaluate the object, and save it *)
      let _, result = eval_sequence (Omake_env.venv_define_object venv) pos ValNone el in
      let obj = eval_object venv pos result in
      Omake_env.venv_add_static_object venv node key obj;
      obj
  in
  let venv = Omake_env.venv_include_static_object venv obj in
  venv, ValNone

(*
 * Object.
 * The argument string is ignored.
 * Push a new object.
 *)
and eval_let_object_exp venv pos v s el export =
  let pos = string_pos "eval_let_object_exp" pos in
  let parent = eval_string_exp venv pos s in
  let obj = eval_object venv pos parent in
  let venv_obj = Omake_env.venv_define_object venv in
  let venv_obj = Omake_env.venv_include_object venv_obj obj in
  let venv_obj, result = eval_sequence venv_obj pos ValNone el in
  let venv = Omake_env.venv_add_var venv v result in
  let venv = Omake_env.add_exports venv venv_obj pos export in
  venv, result

and eval_let_object_field_exp venv pos loc v vl s el export =
  let pos = string_pos "eval_let_object_field_exp" pos in
  let parent = eval_string_exp venv pos s in
  let obj = eval_object venv pos parent in
  let venv_obj = Omake_env.venv_define_object venv in
  let venv_obj = Omake_env.venv_include_object venv_obj obj in
  let venv_obj, e = eval_sequence venv_obj pos ValNone el in
  let path, obj, v = eval_find_field venv pos loc v vl in
  let venv, obj = Omake_env.venv_add_field venv obj pos v e in
  let venv = Omake_env.hoist_path venv path obj in
  let venv = Omake_env.add_exports venv venv_obj pos export in
  venv, e

(*
 * This.
 * Set the current object to the given object.
 *)
and eval_let_this_exp venv pos s =
  let pos = string_pos "eval_this_exp" pos in
  let venv, obj = eval_string_export_exp venv pos s in
  let obj = eval_object venv pos obj in
  let venv = Omake_env.venv_with_object venv obj in
  venv, ValObject obj

(*
 * Include a file.
 * The environment after the file is evaluated is used in the rest
 * of this file.
 *)
and eval_include_exp venv pos loc s _ =
  let pos = string_pos "eval_include" pos in
  let name =
    match eval_string_exp venv pos s with
      ValNode node ->
      (* Use an absolute name, preventing path lookup *)
      Omake_node.Node.absname node
    | name ->
      string_of_value venv pos name
  in
  let node = find_include_file venv pos loc name in
  let venv = Omake_env.venv_add_file venv node in
  let venv = include_file venv Omake_env.IncludePervasives pos loc node in
  venv, ValNone

(*
 * Open a file.
 * Include it if it is not already included.
 *)
and eval_open_exp venv pos loc nodes =
  let pos = string_pos "eval_open" pos in
  let venv =
    List.fold_left (fun venv node ->
      if Omake_env.venv_is_included_file venv node then
        venv
      else
        let venv = Omake_env.venv_add_file venv node in
        include_file venv Omake_env.IncludePervasives pos loc node) venv nodes
  in
  venv, ValNone

(*
 * Key lookup.
 *)
and eval_key_exp venv pos loc v =
  let pos = string_pos "eval_key_exp" pos in
  let result = eval_key venv pos loc v in
  venv, result

(*
 * Function application.
 *)
and eval_apply_exp venv pos loc f args kargs =
  let pos = string_pos "eval_apply_exp" pos in
  eval_apply_string_export_exp venv venv pos loc (Omake_env.venv_find_var venv pos loc f) args kargs

and eval_super_apply_exp venv pos loc super v args kargs =
  let pos = string_pos "eval_super_apply_exp" pos in
  let v = Omake_env.venv_find_super_field venv pos loc super v in
  eval_apply_string_export_exp venv venv pos loc v args kargs

and eval_method_apply_exp venv pos loc v vl args kargs =
  let pos = string_pos "eval_method_apply_exp" pos in
  let venv_obj, path, v = eval_with_method venv pos loc v vl in
  eval_apply_method_export_exp venv venv_obj pos loc path v args kargs

(*
 * Return a value.  This is just the identity.
 *)
and eval_return_body_exp venv pos e id =
  let _pos = string_pos "eval_return_body_exp" pos in
  try eval_sequence_exp venv pos e with
    Omake_value_type.Return (_, v, id') when id' == id ->
    venv, v

and eval_return_exp venv pos loc s id =
  let pos = string_pos "eval_return_exp" pos in
  let result = eval_string_exp venv pos s in
  raise (Omake_value_type.Return (loc, result, id))

and eval_string_value_exp venv pos s =
  let pos = string_pos "eval_string_value_exp" pos in
  let result = eval_string_exp venv pos s in
  venv, result

and eval_return_save_exp venv pos =
  let _pos = string_pos "eval_return_save_exp" pos in
  venv, ValNone

and eval_return_object_exp venv _ names =
  let result = Omake_env.venv_current_object venv names in
  venv, ValObject result

(*
 * Include a file.
 *)
and eval_include_file venv scope pos loc node =
  let ir = compile_ir venv scope pos loc node in
  let venv_new = Omake_env.venv_add_var venv 
      Omake_var.file_var (ValNode node) in
  let venv_new, result = eval_exp venv_new ValNone ir.ir_exp in
  let venv = Omake_env.add_exports venv venv_new pos ExportAll in
  venv, result

and include_file venv scope pos loc target =
  let pos = string_pos "include_file" pos in
  let venv = Omake_env.venv_add_included_file venv target in
  let venv, _ = eval_include_file venv scope pos loc target in
  venv

(*
 * Parse and evaluate a file as if it were an object.
 *)
and eval_object_file venv pos loc node =
  let parse_obj info node =
    let ir = compile_add_ir_info venv IncludePervasives pos loc node info in
    match ir with
      { ir_classnames = names;
        ir_exp = e;
        _}      ->

      let venv = Omake_env.venv_get_pervasives venv node in
      let venv = Omake_env.venv_define_object venv in
      let venv, _ = eval_exp venv ValNone e in
      Omake_env.venv_current_object venv names in
  compile_object parse_obj venv pos loc node

(************************************************************************
 * Evaluator.
*)
and eval venv e =
  let _, result = eval_exp venv ValNone e in
  result

let eval_open_file = open_ir

let eval_apply = eval_apply_export

(************************************************************************
 * Project compiler.
*)
let compile venv =
  let rootname =
    if Sys.file_exists Omake_state.makeroot_name then
      Omake_state.makeroot_name
    else
      Omake_state.makeroot_short_name
  in
  let node = Omake_env.venv_intern venv PhonyProhibited rootname in
  let venv = Omake_env.venv_add_file venv node in
  let loc = Lm_location.bogus_loc (Omake_node.Node.fullname node) in
  let pos = string_pos "compile" (loc_exp_pos loc) in
  let _ = eval_include_file venv IncludePervasives pos loc node in
  if Lm_debug.debug print_rules then
    Format.eprintf "@[<hv 3>Rules:%a@]@." Omake_env.pp_print_explicit_rules venv

(************************************************************************
 * Dependencies.
*)

let probe_compile_deps = I.create "Omake_eval.compile_deps"


let compile_deps venv node =
  I.instrument probe_compile_deps
  (fun buf ->
  let deps = Omake_ast_lex.parse_deps buf in
  let vars = Omake_env.venv_include_scope venv IncludePervasives in
  let senv_empty = Omake_ir_ast.penv_of_vars (open_ir venv) venv node vars in
  List.map 
    (fun (target, source, loc) ->
      let pos = string_pos "compile_deps" (loc_exp_pos loc) in
      let _, target = Omake_ir_ast.build_string senv_empty target pos in
      let _, source = Omake_ir_ast.build_string senv_empty source pos in
      let target = eval_string_exp venv pos target in
      let source = eval_string_exp venv pos source in
      let targets = strings_of_value venv pos target in
      let sources = strings_of_value venv pos source in
      targets, sources) deps
  )
