

include Omake_pos.Make (struct let name = "Omake_value" end)


(*
 * Get some functions from the evaluator.
 *)
let eval_value        = Omake_eval.eval_value
let eval_single_value = Omake_eval.eval_single_value
let eval_prim_value   = Omake_eval.eval_prim_value

(*
 * These functions fail on arrays.
 *)
let string_of_value   = Omake_eval.string_of_value

(*
 * These functions are safe.
 *)
let values_of_value   = Omake_eval.values_of_value
let strings_of_value  = Omake_eval.strings_of_value
let bool_of_value     = Omake_eval.bool_of_value

(*
 * Get the $value field of the object.
 *)
let eval_object_value _ pos obj =
  let pos = string_pos "eval_object_value" pos in
  try Omake_env.venv_find_field_internal_exn obj Omake_symbol.builtin_sym with
    Not_found ->
    raise (Omake_value_type.OmakeException (pos, StringError "not a primitive object"))

let add_object_value obj x =
  Omake_env.venv_add_field_internal obj Omake_symbol.builtin_sym x

(*
 * Concatenate.
 *)
let concat_array (xs : Omake_value_type.t list) : Omake_value_type.t = 
  match xs with 
  | [ValWhite s]
  | [ValString s] ->
    ValData s
  | [ValSequence _] as vl ->
    ValQuote vl
  | [v] ->
    v
  | vl ->
    ValArray vl

let concat_strings  xs : Omake_value_type.t = 
  match xs with  
  | [s] ->
    ValData s
  | sl ->
    ValArray (List.map (fun s -> Omake_value_type.ValData s) sl)

(************************************************************************
 * Conversions.
*)

(*
 * Numbers.
 *)
let int_of_value venv pos v =
  match eval_prim_value venv pos v with
    ValInt i
  | ValOther (ValExitCode i) ->
    i
  | ValFloat x ->
    int_of_float x
  | v ->
    let s = string_of_value venv pos v in
    try int_of_string s with
      Failure _ ->
      raise (Omake_value_type.OmakeException (pos, StringStringError ("not an integer", s)))

let float_of_value venv pos v =
  match eval_prim_value venv pos v with
    ValInt i
  | ValOther (ValExitCode i) ->
    float_of_int i
  | ValFloat x ->
    x
  | v ->
    let s = string_of_value venv pos v in
    try float_of_string s with
      Failure _ ->
      raise (Omake_value_type.OmakeException (pos, StringStringError ("not a floating-point number", s)))

let number_of_value venv pos v =
  let v = eval_prim_value venv pos v in
  match v with
    ValInt _
  | ValFloat _ ->
    v
  | ValOther (ValExitCode i) ->
    ValInt i
  | _ ->
    let s = string_of_value venv pos v in
    try ValInt (int_of_string s) with
      Failure _ ->
      try ValFloat (float_of_string s) with
        Failure _ ->
        raise (Omake_value_type.OmakeException (pos, StringStringError ("not a number", s)))

(*
 * Variables.
 *)
let var_of_value venv pos v =
  let v = eval_prim_value venv pos v in
  match v with
    ValVar (_, v) ->
    v
  | _ ->
    raise (Omake_value_type.OmakeException (pos, StringValueError ("not a var", v)))

let vars_of_value venv pos v =
  List.map (var_of_value venv pos) (values_of_value venv pos v)

(*
 * Maps.
 *)
let map_of_value venv pos v =
  match eval_prim_value venv pos v with
    ValMap map ->
    map
  | v ->
    raise (Omake_value_type.OmakeException (pos, StringValueError ("not a map", v)))

(*
 * Values that can be used as keys.
 *)
let rec key_of_value venv pos v =
  let pos = string_pos "key_of_value" pos in
  let v = eval_prim_value venv pos v in
  match v with
    ValNone
  | ValDir _
  | ValNode _
  | ValData _
  | ValInt _
  | ValFloat _
  | ValOther (ValExitCode _)
  | ValOther (ValLocation _)
  | ValVar _ ->
    v
  | ValQuote _
  | ValQuoteString _
  | ValWhite _
  | ValString _
  | ValSequence _ ->
    ValData (string_of_value venv pos v)
  | ValArray _ ->
    let values = values_of_value venv pos v in
    let values = List.map (key_of_value venv pos) values in
    ValArray values
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
  | ValOther _
  | ValDelayed _ ->
    raise (Omake_value_type.OmakeException (pos, StringValueError ("bad map key", v)))


(*
 * Files and directories.
 *)
let file_of_value = Omake_eval.file_of_value

let dir_of_value venv pos dir =
  let pos = string_pos "dir_of_value" pos in
  let dir = eval_prim_value venv pos dir in
  match dir with
    ValDir dir ->
    dir
  | ValNode _
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValString _
  | ValSequence _
  | ValArray _
  | ValInt _
  | ValFloat _ ->
    Omake_env.venv_intern_dir venv (string_of_value venv pos dir)
  | ValNone
  | ValWhite _
  | ValVar _
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
  | ValOther _
  | ValDelayed _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "not a directory"))

let node_value_of_value venv pos ?(follow_symlinks=true) v =
  let pos = string_pos "node_value_of_value" pos in
  let arg = eval_prim_value venv pos v in
  match arg with
    ValNode _
  | ValDir _ ->
    arg
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValString _
  | ValSequence _
  | ValArray _
  | ValMaybeApply _
  | ValStringExp _
  | ValBody _
  | ValInt _
  | ValFloat _ ->
    let name = string_of_value venv pos v in
    let node = Omake_env.venv_intern venv PhonyExplicit name in
    let cache = Omake_env.venv_cache venv in
    if Omake_cache.is_dir cache ~follow_symlinks node then
      ValDir (Omake_env.venv_intern_dir venv name) 
    else
      ValNode node
  | ValNone
  | ValWhite _
  | ValVar _
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
  | ValOther _
  | ValDelayed _ ->
    raise (Omake_value_type.OmakeException (pos, StringValueError("not a file", v)))

let dir_value_of_value venv pos v =
  let pos = string_pos "dir_value_of_value" pos in
  let arg = eval_prim_value venv pos v in
  match arg with
    ValNode _
  | ValDir _ ->
    arg
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValString _
  | ValSequence _
  | ValArray _
  | ValMaybeApply _
  | ValStringExp _
  | ValBody _
  | ValInt _
  | ValFloat _ ->
    let name = string_of_value venv pos v in
    ValDir (Omake_env.venv_intern_dir venv name)
  | ValNone
  | ValWhite _
  | ValVar _
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
  | ValOther _
  | ValDelayed _ ->
    raise (Omake_value_type.OmakeException (pos, StringValueError("not a file", v)))

let filename_of_value venv pos v =
  let pos = string_pos "filename_of_value" pos in
  let arg = eval_prim_value venv pos v in
  match arg with
    ValNode node ->
    Omake_node.Node.fullname node
  | ValDir dir ->
    Omake_node.Dir.fullname dir
  | _ ->
    Omake_node.Node.fullname (file_of_value venv pos v)

(*
 * Channels.  The string &<int> represents channels.
 *)
let prim_channel_of_string venv pos s =
  let pos = string_pos "channel_of_string" pos in
  if s <> "" && s.[0] = '&' then
    let id =
      try int_of_string (String.sub s 1 (String.length s - 1)) with
        Failure _ ->
        raise (Omake_value_type.OmakeException (pos, StringStringError ("not a channel string", s)))
    in
    Omake_env.venv_find_channel_by_id venv pos id
  else
    raise (Omake_value_type.OmakeException (pos, StringStringError ("not a channel string", s)))

(* let channel_of_string venv pos s = *)
(*   Omake_env.venv_find_channel venv pos (prim_channel_of_string venv pos s) *)

let rec is_int_string s i len =
  if i = len then
    true
  else
    match s.[i] with
      '0'..'9' ->
      is_int_string s (succ i) len
    | _ ->
      false

let is_channel_string s =
  s <> "" && s.[0] = '&' && is_int_string s 1 (String.length s)

let prim_channel_of_value venv pos v =
  let pos = string_pos "prim_channel_of_value" pos in
  let arg = eval_prim_value venv pos v in
  match arg with
    ValChannel (_, channel) ->
    channel
  | ValNode _
  | ValDir _
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValString _
  | ValSequence _ ->
    prim_channel_of_string venv pos (string_of_value venv pos arg)
  | ValInt _
  | ValFloat _
  | ValMaybeApply _
  | ValVar _
  | ValStringExp _
  | ValBody _
  | ValNone
  | ValWhite _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValArray _
  | ValRules _
  | ValMap _
  | ValObject _
  | ValClass _
  | ValCases _
  | ValOther _
  | ValDelayed _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "not a channel"))

let prim_channel_of_var venv pos loc v =
  prim_channel_of_value venv pos (Omake_env.venv_find_var venv pos loc v)

let channel_of_var venv pos loc v =
  let channel = prim_channel_of_var venv pos loc v in
  Omake_env.venv_find_channel venv pos channel

let channel_of_value venv pos v =
  let pos = string_pos "channel_of_value" pos in
  let channel = prim_channel_of_value venv pos v in
  Omake_env.venv_find_channel venv pos channel

let in_channel_of_any_value venv pos v =
  let pos = string_pos "in_channel_of_any_value" pos in
  let arg = eval_prim_value venv pos v in
  match arg with
    ValChannel (InChannel, p)
  | ValChannel (InOutChannel, p) ->
    p, false
  | ValNode _
  | ValDir _
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValString _
  | ValSequence _
  | ValMaybeApply _
  | ValStringExp _
  | ValBody _
  | ValInt _
  | ValFloat _ ->
    let s = string_of_value venv pos arg in
    if is_channel_string s then
      prim_channel_of_string venv pos s, false
    else
      let node = Omake_env.venv_intern venv PhonyProhibited s in
      let name = Omake_node.Node.fullname node in
      let fd =
        try Lm_unix_util.openfile name [Unix.O_RDONLY] 0 with
          Unix.Unix_error _ as exn ->
          raise (Omake_value_type.UncaughtException (pos, exn))
      in
      let chan = Lm_channel.create name Lm_channel.FileChannel Lm_channel.InChannel false (Some fd) in
      let pc = Omake_env.venv_add_channel venv chan in
      pc, true
  | ValChannel (OutChannel, _)
  | ValNone
  | ValWhite _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValArray _
  | ValRules _
  | ValMap _
  | ValObject _
  | ValClass _
  | ValCases _
  | ValOther _
  | ValVar _
  | ValDelayed _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "not an input channel"))

let out_channel_of_any_value venv pos v =
  let pos = string_pos "out_channel_of_any_value" pos in
  let arg = eval_prim_value venv pos v in
  match arg with
    ValChannel (OutChannel, p)
  | ValChannel (InOutChannel, p) ->
    p, false
  | ValNode _
  | ValDir _
  | ValData _
  | ValQuote _
  | ValString _
  | ValQuoteString _
  | ValSequence _
  | ValMaybeApply _
  | ValStringExp _
  | ValBody _
  | ValInt _
  | ValFloat _ ->
    let s = string_of_value venv pos arg in
    if is_channel_string s then
      prim_channel_of_string venv pos s, false
    else
      let node = Omake_env.venv_intern venv PhonyProhibited s in
      let name = Omake_node.Node.fullname node in
      let fd =
        try Lm_unix_util.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 with
          Unix.Unix_error _ as exn ->
          raise (Omake_value_type.UncaughtException (pos, exn))
      in
      let chan = Lm_channel.create name Lm_channel.FileChannel Lm_channel.OutChannel false (Some fd) in
      let prim = Omake_env.venv_add_channel venv chan in
      prim, true
  | ValChannel (InChannel, _)
  | ValNone
  | ValWhite _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValArray _
  | ValRules _
  | ValMap _
  | ValObject _
  | ValClass _
  | ValCases _
  | ValOther _
  | ValVar _
  | ValDelayed _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "not an output channel"))

(*
 * Check whether the value has any glob characters in it.
 *)
let rec is_glob_value options (v : Omake_value_type.t) =
  match v with
  |  ValString s ->
    Lm_glob.is_glob_string options s
  | ValSequence vl
  | ValArray vl ->
    is_glob_value_list options vl
  | ValQuoteString _
  | ValChannel _
  | ValNode _
  | ValDir _
  | ValData _
  | ValQuote _
  | ValMaybeApply _
  | ValStringExp _
  | ValBody _
  | ValInt _
  | ValFloat _
  | ValNone
  | ValWhite _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValRules _
  | ValMap _
  | ValObject _
  | ValClass _
  | ValCases _
  | ValOther _
  | ValVar _
  | ValDelayed _ ->
    false

and is_glob_value_list options vl =
  List.exists (is_glob_value options) vl

(*
 * Lexing and parsing.
 *)
let current_lexer venv pos =
  let pos = string_pos "current_lexer" pos in
  try
    match Omake_env.venv_find_var_exn venv Omake_var.builtin_field_var with
      ValOther (ValLexer lexer) ->
      lexer
    | v ->
      raise (Omake_value_type.OmakeException (pos, StringValueError ("not a lexer", v)))
  with
    Not_found ->
    Omake_lexer.Lexer.empty

let current_parser venv pos =
  let pos = string_pos "current_parser" pos in
  try
    match Omake_env.venv_find_var_exn venv Omake_var.builtin_field_var with
      ValOther (ValParser parser) ->
      parser
    | v ->
      raise (Omake_value_type.OmakeException (pos, StringValueError ("not a parser", v)))
  with
    Not_found ->
    Omake_parser.Parser.empty

let loc_of_value venv pos v =
  match eval_prim_value venv pos v with
    ValOther (ValLocation loc) ->
    loc
  | _ ->
    raise (Omake_value_type.OmakeException (pos, StringValueError ("not a location", v)))

