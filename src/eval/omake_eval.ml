(*
 * Predefined set of functions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2010 Mojave Group, California Institute of Technology and
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
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_debug
open Lm_printf
open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_exec
open Omake_wild
open Omake_node
open Omake_util
open Omake_state
open Omake_ir_ast
open Omake_symbol
open Omake_command
open Omake_ir_print
open Omake_node_sig
open Omake_exec_type
open Omake_exec_util
open Omake_shell_type
open Omake_cache_type
open Omake_value_type
open Omake_value_print
open Omake_command_type
open Omake_command_digest

module Pos = MakePos (struct let name = "Omake_eval" end);;
open Pos

let debug_eval =
   create_debug (**)
      { debug_name = "debug-eval";
        debug_description = "Debug the evaluator";
        debug_value = false
      }

let print_ast =
   create_debug (**)
      { debug_name = "print-ast";
        debug_description = "Print the AST after parsing";
        debug_value = false
      }

let print_ir =
   create_debug (**)
      { debug_name = "print-ir";
        debug_description = "Print the IR after parsing";
        debug_value = false
      }

let print_rules =
    create_debug (**)
      { debug_name = "print-rules";
        debug_description = "Print the rules after evaluation";
        debug_value = false
      }

let print_files =
   create_debug (**)
      { debug_name = "print-files";
        debug_description = "Print the files as they are read";
        debug_value = false
      }

(*
 * For now, use a bogu location for parameters.
 *)
let param_loc = bogus_loc "Omake_eval.param"

(*
 * Including files.
 *)
type include_flag =
   IncludeFile
 | IncludeSubdir

(************************************************************************
 * Utilities.
 *)
let raise_uncaught_exception pos = function
   Sys.Break
 | OmakeException _
 | OmakeFatal _
 | OmakeFatalErr _
 | UncaughtException _ as exn ->
      raise exn
 | exn ->
      raise (UncaughtException (pos, exn))

(*
 * Add an optional quote.
 *)
let buffer_add_quote buf = function
   Some c -> Buffer.add_char buf c
 | None -> ()

(*
 * The various forms of empty values.
 *)
let rec is_empty_value v =
   match v with
      ValNone
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
         (try is_empty_value (venv_find_field_internal_exn obj builtin_sym) with
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
let rec is_array_value v =
   match v with
      ValArray _ ->
         true
    | ValSequence [v]
    | ValQuote [v] ->
         is_array_value v
    | ValObject obj ->
         (try
             match venv_find_field_internal_exn obj builtin_sym with
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
   FullArity    of value list * value list
 | PartialArity of arity * value list

let rec concat_n_args args1 args2 n =
   if n = 0 then
      FullArity (List.rev args1, args2)
   else
      match args2 with
         arg :: args2 ->
            concat_n_args (arg :: args1) args2 (n - 1)
       | [] ->
            raise (Invalid_argument "concat_n_args")

let arity_apply_args arity args1 args2 =
   let len = List.length args2 in
      match arity with
         ArityRange (min, max) ->
            if len < min then
               let arity = ArityRange (min - len, max - len) in
               let args = List.rev_append args2 args1 in
                  PartialArity (arity, args)
            else if len < max then
               let args = List.rev_append args1 args2 in
                  FullArity (args, [])
            else
               concat_n_args args1 args2 max
       | ArityExact i ->
            if len < i then
               let arity = ArityExact (i - len) in
               let args = List.rev_append args2 args1 in
                  PartialArity (arity, args)
            else
               concat_n_args args1 args2 i
       | ArityNone ->
            FullArity ([], List.rev_append args1 args2)
       | ArityAny ->
            FullArity (List.rev_append args1 args2, [])

(************************************************************************
 * Compiling utilities.
 *)
let postprocess_ir venv ir =
   let () =
      if debug print_ir then
         eprintf "@[<v 3>IR1:@ %a@]@." Omake_ir_print.pp_print_exp ir.ir_exp
   in
   let ir = { ir with ir_exp = Omake_ir_semant.build_prog venv ir.ir_exp } in
   let () =
      if debug print_ir then
         eprintf "@[<v 3>IR2:@ %a@]@." Omake_ir_print.pp_print_exp ir.ir_exp
   in
      ir

(*
 * Parse and evaluate a file.
 *)
let rec parse_ir venv scope node =
   let filename = Node.fullname node in
   let ast = Omake_ast_lex.parse_ast filename in
   let () =
      if debug print_ast then
         eprintf "@[<v 3>AST (initial):@ %a@]@." Omake_ast_print.pp_print_prog ast
   in
   let ast = Omake_exp_lex.compile_prog ast in
   let () =
      if debug print_ast then
         eprintf "@[<v 3>AST %a:@ %a@]@." pp_print_node node Omake_ast_print.pp_print_prog ast
   in
   let vars = venv_include_scope venv scope in
   let senv, ir = Omake_ir_ast.compile_prog (penv_of_vars (open_ir venv) venv node vars) ast in
      postprocess_ir venv ir

(*
 * When constructing a path, the relative filenames
 * should be auto-rehash.
 *
 *    values  : the path
 *    dirname : the subdirectory to search (often ".")
 *)
and path_of_values_select venv pos values dirname =
   let rec collect groups auto_rehash items values =
      match values with
         v :: values ->
            let rehash_flag, dir =
               match v with
                  ValDir dir ->
                     false, dir
                | ValNode node ->
                     let dir = venv_intern_dir venv (string_of_value venv pos v) in
                        false, dir
                | _ ->
                     let s = string_of_value venv pos v in
                     let rehash_flag = not (Lm_filename_util.is_absolute s) in
                     let dir = venv_intern_dir venv s in
                        rehash_flag, dir
            in
            let dir = Dir.chdir dir dirname in
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
   let dir_of_value v =
      let dir =
         match v with
            ValDir dir ->
               dir
          | _ ->
               venv_intern_dir venv (string_of_value venv pos v)
      in
         Dir.chdir dir dirname
   in
      [true, List.map dir_of_value values]

and path_of_values venv pos values dirname =
   let auto_rehash =
      try bool_of_value venv pos (venv_find_var_exn venv auto_rehash_var) with
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
   let cache = venv_cache venv in
      if not (Filename.is_relative filename) || not (Filename.is_implicit filename) then
         let fullname = filename ^ omake_file_suffix in
         let node1 = venv_intern venv PhonyProhibited fullname in
            if Omake_cache.exists cache node1 then
               node1
            else
               let node2 = venv_intern venv PhonyProhibited filename in
                  if Omake_cache.exists cache node2 then
                     node2
                  else
                     let print_error buf =
                        fprintf buf "@[<hv 3>include file not found, neither file exists:@ %a@ %a@]" (**)
                           pp_print_node node1
                           pp_print_node node2
                     in
                        raise (OmakeException (loc_pos loc pos, LazyError print_error))
      else
         let dirname = Filename.dirname filename in
         let basename = Filename.basename filename in
         let fullname = basename ^ omake_file_suffix in
         let path = venv_find_var venv pos loc omakepath_var in
         let full_path = values_of_value venv pos path in
         let path = path_of_values venv pos full_path dirname in
         let cache = venv_cache venv in
         let listing = Omake_cache.ls_path cache path in
            try
               match Omake_cache.listing_find cache listing fullname with
                  DirEntry dir ->
                     raise (OmakeException (loc_pos loc pos, StringDirError ("is a directory", dir)))
                | NodeEntry node ->
                     node
            with
               Not_found ->
                  try
                     match Omake_cache.listing_find cache listing basename with
                        DirEntry dir ->
                           raise (OmakeException (loc_pos loc pos, StringDirError ("is a directory", dir)))
                      | NodeEntry node ->
                           node
                  with
                     Not_found ->
                        let print_error buf =
                           fprintf buf "@[<hv 3>include file %s not found in OMAKEPATH@ (@[<hv3>OMAKEPATH[] =%a@])@]" (**)
                              filename
                              pp_print_value_list full_path
                        in
                           raise (OmakeException (loc_pos loc pos, LazyError print_error))

and open_ir venv filename pos loc =
   let pos = string_pos "open_ir" pos in
   let source = find_include_file venv pos loc filename in
   let ir = compile_ir venv IncludePervasives pos loc source in
      if !print_ir then begin
         eprintf "@[<v 3>Vars: %a" pp_print_node source;
         SymbolTable.iter (fun v info ->
               eprintf "@ %a = %a" pp_print_symbol v pp_print_var_info info) ir.ir_vars;
         eprintf "@]@."
      end;
      source, ir.ir_vars

(*
 * The include file contains the IR for the file.
 * Try to load the old entry.
 * If it fails, compile the file and save the new entry.
 *)
and compile_add_ir_info venv scope pos loc source info =
   let _pos = string_pos "compile_add_ir_info" pos in
      try Static.get_ir info with
         Not_found ->
            let ir = parse_ir venv scope source in
               Static.add_ir info ir;
               ir

and compile_ir_info venv scope pos loc source info =
   let _pos = string_pos "compile_ir_info" pos in
      try Static.find_ir info with
         Not_found ->
            Static.rewrite info (compile_add_ir_info venv scope pos loc source)

and compile_ir venv scope pos loc source =
   let pos = string_pos "compile_ir" pos in
      (*
       * Try to get a cached copy.
       *)
      try venv_find_ir_file_exn venv source with
         Not_found ->
            let ir =
               (*
                * Open the database.
                *)
               try Static.read venv source (compile_ir_info venv scope pos loc source)
               with Not_found ->
                  raise (OmakeException (loc_pos loc pos, StringNodeError ("can't open IR", source)))
            in
               venv_add_ir_file venv source ir;
               ir

(*
 * The object file contains the evaluated file.
 *)
and compile_add_object_info compile venv pos source info =
   let _pos = string_pos "compile_add_object_info_info" pos in
      try Static.get_object info with
         Not_found ->
            let obj = compile info source in
               Static.add_object info obj;
               obj

(*
 * Try to load the old entry.
 * If it fails, compile the file and save the new entry.
 *)
and compile_object_info compile venv pos source info =
   let _pos = string_pos "compile_object_info" pos in
      try Static.find_object info with
         Not_found ->
            Static.rewrite info (compile_add_object_info compile venv pos source)

and compile_object compile venv pos loc source =
   let pos = string_pos "compile_ast" pos in

      (*
       * Try to get a cached copy.
       *)
      try venv_find_object_file_exn venv source with
         Not_found ->
            let obj =
               (*
                * Open the database.
                *)
               try Static.read venv source (compile_object_info compile venv pos source)
               with Not_found ->
                  raise (OmakeException (loc_pos loc pos, StringNodeError ("can't open object", source)))
            in
               venv_add_object_file venv source obj;
               obj

(************************************************************************
 * Value operations.
 *)

(*
 * Get the string representation of a value.
 * It not legal to convert an array to a string.
 *)
and string_of_value venv pos v =
   let pos = string_pos "string_of_value" pos in
   let scratch_buf = Buffer.create 32 in
   let rec collect v =
      match eval_prim_value venv pos v with
         (* Values that expand to nothing *)
         ValNone
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
               fprintf buf "@[<v 3>Array value where string expected:";
               fprintf buf "@ Use the $(string ...) function if you really want to do this";
               fprintf buf "@ @[<v 3>The array has length %d:" (List.length vl);
               ignore (List.fold_left (fun index v ->
                             fprintf buf "@ @[<hv 3>[%d] =@ %a@]" index pp_print_value v;
                             succ index) 0 vl);
               fprintf buf "@]@]@."
            in
               raise (OmakeException (pos, LazyError print_error))
       | ValInt i ->
            Buffer.add_string scratch_buf (string_of_int i)
       | ValFloat x ->
            Buffer.add_string scratch_buf (string_of_float x)
       | ValData s
       | ValWhite s
       | ValString s ->
            Buffer.add_string scratch_buf s
       | ValDir dir2 ->
            Buffer.add_string scratch_buf (venv_dirname venv dir2)
       | ValNode node ->
            Buffer.add_string scratch_buf (venv_nodename venv node)
       | ValStringExp _
       | ValMaybeApply _
       | ValDelayed _ ->
            raise (Invalid_argument "string_of_value")
   in
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
      match eval_value venv pos v with
         (* Values that expand to nothing *)
         ValNone
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
            Buffer.add_string scratch_buf (venv_dirname venv dir2)
       | ValNode node ->
            Buffer.add_string scratch_buf (venv_nodename venv node)
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
   let group tokens  = ValSequence tokens in
   let wrap_string s = ValString s in
   let wrap_data s   = ValData s in
   let wrap_token s  = ValData s in
   let lexer _ _ _   = None in
   let tokens = Lm_string_util.tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group in

   (*
    * Array elements are always separate values.
    * The arrays are flattened.
    *)
   let rec collect_array tokens vl vll =
      match vl, vll with
         v :: vl, _ ->
            (match eval_value venv pos v with
                ValArray el ->
                   collect_array tokens el (vl :: vll)
              | ValSequence [v] ->
                   collect_array tokens (v :: vl) vll
              | v ->
                   collect_array (Lm_string_util.tokens_atomic tokens v) vl vll)
       | [], vl :: vll ->
            collect_array tokens vl vll
       | [], [] ->
            tokens
   in

   (*
    * Collect_string is used when we have seen whitespace
    * in a sequence.  Collect the values into the string buffer,
    * then parse the string into separate tokens.
    *)
   let rec collect tokens vl vll =
      match vl, vll with
         v :: vl, _ ->
            let v = eval_catenable_value venv pos v in
               (match v with
                   ValNone ->
                      collect tokens vl vll

                   (* Strings *)
                 | ValWhite s
                 | ValString s ->
                      collect (Lm_string_util.tokens_string tokens s) vl vll
                 | ValSequence el ->
                      collect tokens el (vl :: vll)

                   (* Other catenable values *)
                 | ValData _
                 | ValInt _
                 | ValFloat _
                 | ValDir _
                 | ValNode _
                 | ValQuote _
                 | ValQuoteString _ ->
                      collect (Lm_string_util.tokens_add tokens v) vl vll

                   (* Atomic values *)
                 | ValArray el ->
                      collect (collect_array (Lm_string_util.tokens_break tokens) el []) vl vll
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
                      collect (Lm_string_util.tokens_atomic tokens v) vl vll
                 | ValStringExp _
                 | ValMaybeApply _
                 | ValDelayed _ ->
                      raise (OmakeException (pos, StringValueError ("illegal application", v))))
       | [], vl :: vll ->
            collect tokens vl vll
       | [], [] ->
            Lm_string_util.tokens_flush tokens
   in
      collect tokens [v] []

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
   let group tokens  = TokGroup tokens in
   let wrap_string s = TokString (ValString s) in
   let wrap_data s   = TokString (ValData s) in
   let wrap_token s  = TokToken s in
   let tokens = Lm_string_util.tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group in

   (*
    * Array elements are always separate values.
    * The arrays are flattened.
    *)
   let rec collect_array (tokens : tok Lm_string_util.tokens) vl vll =
      match vl, vll with
         v :: vl, _ ->
            (match eval_value venv pos v with
                ValArray el ->
                   collect_array tokens el (vl :: vll)
              | ValSequence [v] ->
                   collect_array tokens (v :: vl) vll
              | v ->
                   collect_array (Lm_string_util.tokens_atomic tokens (TokString v)) vl vll)
       | [], vl :: vll ->
            collect_array tokens vl vll
       | [], [] ->
            tokens
   in

   (*
    * Collect_string is used when we have seen whitespace
    * in a sequence.  Collect the values into the string buffer,
    * then parse the string into separate tokens.
    *)
   let rec collect (tokens : tok Lm_string_util.tokens) vl vll =
      match vl, vll with
         v :: vl, _ ->
            let v = eval_catenable_value venv pos v in
               (match v with
                   ValNone ->
                      collect tokens vl vll

                   (* Strings *)
                 | ValWhite s
                 | ValString s ->
                      collect (Lm_string_util.tokens_lex tokens s) vl vll
                 | ValSequence el ->
                      collect tokens el (vl :: vll)

                   (* Other catenable values *)
                 | ValData _
                 | ValInt _
                 | ValFloat _
                 | ValDir _
                 | ValNode _
                 | ValQuote _ ->
                      collect (Lm_string_util.tokens_add tokens (TokString v)) vl vll
                 | ValQuoteString (_, v) ->
                      collect (Lm_string_util.tokens_add tokens (TokString (ValQuote v))) vl vll

                   (* Atomic values *)
                 | ValArray el ->
                      collect (collect_array (Lm_string_util.tokens_break tokens) el []) vl vll
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
                      collect (Lm_string_util.tokens_atomic tokens (TokString v)) vl vll
                 | ValStringExp _
                 | ValMaybeApply _
                 | ValDelayed _ ->
                      raise (OmakeException (pos, StringValueError ("illegal application", v))))
       | [], vl :: vll ->
            collect tokens vl vll
       | [], [] ->
            Lm_string_util.tokens_flush tokens
   in
      collect tokens [v] []

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
                            arg_buffer_add_data tokens s
                         else
                            arg_buffer_add_string tokens s
                      in
                         collect is_quoted tokens vl vll
                 | ValData s ->
                      collect is_quoted (arg_buffer_add_data tokens s) vl vll
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
                      let tokens = arg_buffer_add_data tokens (string_of_value venv pos v) in
                         collect is_quoted tokens vl vll

                   (* Illegal values *)
                 | ValStringExp _
                 | ValMaybeApply _
                 | ValDelayed _ ->
                      raise (OmakeException (pos, StringValueError ("illegal application", v))))
       | [], vl :: vll ->
            collect is_quoted tokens vl vll
       | [], [] ->
            arg_buffer_contents tokens
   in
      collect false arg_buffer_empty vl []

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
            Omake_util.bool_of_string s
       | [ValQuote vl] ->
            Omake_util.bool_of_string (string_of_quote venv pos None vl)
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
            Node.node_of_dir dir
       | ValData _
       | ValString _
       | ValSequence _
       | ValQuote _
       | ValQuoteString _
       | ValInt _
       | ValFloat _ ->
            venv_intern venv PhonyExplicit (string_of_value venv pos file)
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
            raise (OmakeException (pos, StringError "illegal value"))

(*
 * Be lazy about concatenating arrays, to
 * avoid quadratic behavior.
 *)
and append_arrays venv pos a1 a2 =
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
      match venv_find_static_info venv pos key with
         StaticValue obj ->
            obj
       | StaticRule srule ->
            let { srule_env  = venv;
                  srule_deps = deps;
                  srule_vals = values;
                  srule_exp  = e
                } = srule
            in
            let values = List.flatten (List.map (values_of_value venv pos) values) in
            let values = List.map (eval_prim_value venv pos) values in
            let digest = digest_of_exp pos values e in
            let cache = venv_cache venv in
            let obj =
               (* Try to fetch the value from the memo *)
               try Omake_cache.find_value cache key srule.srule_static deps digest with
                  Not_found ->
                     (* Finally, if we don't have a value, evaluate the rule.
                      * Prevent recursive calls *)
                     let () = venv_set_static_info venv key (StaticValue empty_obj) in
                     let venv, v = eval_exp venv ValNone e in
                     let obj = eval_object venv pos v in
                        Omake_cache.add_value cache key srule.srule_static deps digest (MemoSuccess obj);
                        obj
            in
               venv_set_static_info venv key (StaticValue obj);
               obj
   in
      venv_find_field_internal obj pos v

and eval_value_delayed venv pos p =
   match !p with
      ValValue v ->
         eval_value_core venv pos v
    | ValStaticApply (key, v) ->
         let v = eval_value_static venv pos key v in
            p := ValValue v;
            eval_value_core venv pos v

(*
 * Unfold the outermost application to get a real value.
 *)
and eval_value_core venv pos v =
   match v with
      ValMaybeApply (loc, v) ->
         let v =
            try Some (venv_find_var_exn venv v) with
               Not_found ->
                  None
         in
            (match v with
                Some v -> ValArray [eval_value_core venv pos (eval_var venv pos loc v)]
              | None -> ValNone)
    | ValDelayed p ->
         eval_value_delayed venv pos p
    | ValSequence [v] ->
         eval_value_core venv pos v
    | ValStringExp (env, e) ->
         let v = eval_string_exp (venv_with_env venv env) pos e in
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

and eval_prim_value venv pos v =
   let pos = string_pos "eval_prim_value" pos in
   let v = eval_value venv pos v in
      match v with
         ValArray [v] ->
            eval_prim_value venv pos v
       | ValObject obj ->
            (try venv_find_field_internal_exn obj builtin_sym with
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
                match venv_find_field_internal_exn obj builtin_sym with
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
and eval_body_value venv pos v =
   match eval_value venv pos v with
      ValSequence sl ->
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

and eval_body_exp venv pos x v =
   match eval_value venv pos v with
      ValSequence sl ->
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
      ValFun (env, _, [], body, _) ->
         let venv = venv_with_env venv env in
         let _, result = eval_sequence venv pos ValNone body in
            result
    | ValFunCurry (env, args, _, [], body, _, []) ->
         let venv = venv_with_partial_args venv env args in
         let _, result = eval_sequence venv pos ValNone body in
            result
    | ValFunCurry (env, args, _, [], body, export, kargs) ->
         (* XXX: verify that we should pass forward the exports *)
         let venv_new = venv_with_partial_args venv env args in
         let venv_new, v = eval_sequence venv_new pos ValNone body in
         let venv = add_exports venv venv_new pos export in
            eval_apply venv pos loc v [] kargs
    | ValPrim (_, _, ApplyEmpty, f) ->
         snd (venv_apply_prim_fun f venv pos loc [] [])
    | _ ->
         v

(*
 * Evaluate a key.
 *)
and eval_key venv pos loc v =
   try
      let map = eval_map venv pos (venv_find_var_exn venv map_field_var) in
         venv_map_find map pos (ValData v)
   with
      Not_found ->
         raise (OmakeException (loc_pos loc pos, UnboundKey v))

(*
 * Evaluate an application.
 *)
and eval_apply venv pos loc v args kargs =
   let pos = string_pos "eval_apply" pos in
   match eval_value venv pos v with
      ValFun (env, keywords, params, body, _) ->
         let venv = venv_add_args venv pos loc env params args keywords kargs in
         let _, result = eval_sequence_exp venv pos body in
            result
    | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
         let venv_new, args, kargs = venv_add_curry_args venv pos loc env pargs params args keywords kargs1 kargs in
         let venv_new, v = eval_sequence_exp venv_new pos body in
         let venv = add_exports venv venv_new pos export in
            eval_apply venv pos loc v args kargs
    | ValPrim (_, _, _, f) ->
         snd (venv_apply_prim_fun f venv pos loc args kargs)
    | ValPrimCurry (_, _, f, args1, kargs1) ->
         snd (venv_apply_prim_fun f venv pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs))
    | v ->
         if args = [] && kargs = [] then
            v
         else
            let print_error buf =
               fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
               List.iter (fun arg ->
                     fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_value arg) args;
               List.iter (fun (v, arg) ->
                     fprintf buf "@ @[<hv 3>%a = %a@]" pp_print_symbol v pp_print_value arg) kargs;
               fprintf buf "@]"
            in
               raise (OmakeException (pos, LazyError print_error))

(*
 * Evaluate an application with string arguments.
 *)
and eval_apply_string_exp venv venv_obj pos loc v args kargs =
   let pos = string_pos "eval_apply_string_exp" pos in
      match eval_value venv pos v with
         ValFun (env, keywords, params, body, _) ->
            let args = List.map (eval_string_exp venv pos) args in
            let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
            let venv_new = venv_add_args venv_obj pos loc env params args keywords kargs in
            let _, result = eval_sequence_exp venv_new pos body in
               result
       | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
            let args = List.map (eval_string_exp venv pos) args in
            let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
            let venv_new, args, kargs = venv_add_curry_args venv_obj pos loc env pargs params args keywords kargs1 kargs in
            let venv_new, v = eval_sequence_exp venv_new pos body in
            let venv = add_exports venv venv_new pos export in
               eval_apply venv pos loc v args kargs
       | ValPrim (_, be_eager, _, f) ->
            let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
            let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp true venv pos s) kargs in
               snd (venv_apply_prim_fun f venv_obj pos loc args kargs)
       | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
            let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
            let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp true venv pos s) kargs in
               snd (venv_apply_prim_fun f venv_obj pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs))
       | v ->
            if args = [] && kargs = [] then
               v
            else
               let print_error buf =
                  fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
                  List.iter (fun arg ->
                        fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_string_exp arg) args;
                  List.iter (fun (v, arg) ->
                        fprintf buf "@ @[<hv 3>%a = %a@]" pp_print_symbol v pp_print_string_exp arg) kargs;
                  fprintf buf "@]"
               in
                  raise (OmakeException (pos, LazyError print_error))

(*
 * Get a function from a value.
 *)
and eval_fun venv pos v =
   match eval_value venv pos v with
      ValFun (env, keywords, params, body, export) ->
         let f venv pos loc args kargs =
            let venv_new = venv_add_args venv pos loc env params args keywords kargs in
            let venv_new, result = eval_sequence_exp venv_new pos body in
            let venv = add_exports venv venv_new pos export in
               venv, result
         in
            true, f
    | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
         let f venv pos loc args kargs =
            let venv_new, args, kargs = venv_add_curry_args venv pos loc env pargs params args keywords kargs1 kargs in
            let venv_new, v = eval_sequence_exp venv_new pos body in
            let venv = add_exports venv venv_new pos export in
               eval_apply_export venv pos loc v args kargs
         in
            true, f
    | ValPrim (_, be_eager, _, f) ->
         be_eager, venv_apply_prim_fun f
    | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
         let f venv pos loc args2 kargs2 =
            venv_apply_prim_fun f venv pos loc (List.rev_append args1 args2) (List.rev_append kargs1 kargs2)
         in
            be_eager, f
    | ValBody (body, export) ->
         let arity = ArityExact 0 in
         let f venv pos loc args kargs =
            if args <> [] || kargs <> [] then
               raise (OmakeException (loc_pos loc pos, ArityMismatch (arity, List.length args)));
            eval_sequence_export_exp venv pos body export
         in
            true, f
    | _ ->
         raise (OmakeException (pos, StringError "not a function"))

(*
 * Get an object from a variable.
 *)
and eval_map venv pos x =
   match eval_value venv pos x with
      ValMap map ->
         map
    | _ ->
         raise (OmakeException (pos, StringError "not a map"))

and eval_object venv pos x =
   try eval_object_exn venv pos x with
      Not_found ->
         raise (OmakeException (pos, StringError "not an object"))

and eval_object_exn venv pos x =
   let x = eval_value venv pos x in
   match x with
      ValObject env ->
         env
    | ValInt _
    | ValOther (ValExitCode _) ->
         create_object venv x int_object_var
    | ValFloat _ ->
         create_object venv x float_object_var
    | ValData _
    | ValQuote _
    | ValQuoteString _ ->
         create_object venv x string_object_var
    | ValSequence _
    | ValWhite _
    | ValString _
    | ValNone ->
         create_object venv x sequence_object_var
    | ValArray _ ->
         create_object venv x array_object_var
    | ValFun _
    | ValFunCurry _
    | ValPrim _
    | ValPrimCurry _ ->
         create_object venv x fun_object_var
    | ValRules _ ->
         create_object venv x rule_object_var
    | ValNode _ ->
         create_object venv x file_object_var
    | ValDir _ ->
         create_object venv x dir_object_var
    | ValBody _ ->
         create_object venv x body_object_var
    | ValChannel (InChannel, _) ->
         create_object venv x in_channel_object_var
    | ValChannel (OutChannel, _) ->
         create_object venv x out_channel_object_var
    | ValChannel (InOutChannel, _) ->
         create_object venv x in_out_channel_object_var
    | ValOther (ValLexer _) ->
         create_object venv x lexer_object_var
    | ValOther (ValParser _) ->
         create_object venv x parser_object_var
    | ValOther (ValLocation _) ->
         create_object venv x location_object_var
    | ValOther (ValEnv _) ->
         raise (OmakeException (pos, StringError "dereferenced <env>"))
    | ValClass _ ->
         raise (Invalid_argument "internal error: dereferenced $class")
    | ValCases _ ->
         raise (Invalid_argument "internal error: dereferenced cases")
    | ValMap _ ->
         create_map venv x map_object_var
    | ValVar _ ->
         create_object venv x var_object_var
    | ValStringExp _
    | ValMaybeApply _
    | ValDelayed _ ->
         raise (Invalid_argument "find_object")

and create_object venv x v =
   let obj = venv_find_var_exn venv v in
      match obj with
         ValObject env ->
            venv_add_field_internal env builtin_sym x
       | _ ->
            raise Not_found

and create_map venv x v =
   let obj = venv_find_var_exn venv v in
      match obj with
         ValObject env ->
            venv_add_field_internal env map_sym x
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
         let path, v = venv_find_field_path_exn venv path obj pos v in
         let obj = eval_object_exn venv pos v in
            eval_find_field_exn venv path obj pos vl
    | [] ->
         raise (OmakeException (pos, StringError "empty method name"))

and eval_find_field_aux venv envl pos v vl =
   match envl with
      [env] ->
         let env = eval_object_exn venv pos env in
         let path = PathVar v in
            eval_find_field_exn venv path env pos vl
    | env :: envl ->
         let env = eval_object_exn venv pos env in
         let path = PathVar v in
            (try eval_find_field_exn venv path env pos vl with
                Not_found ->
                   eval_find_field_aux venv envl pos v vl)
    | [] ->
         raise Not_found

and eval_find_field venv pos loc v vl =
   let envl = venv_current_objects venv pos v in
      try eval_find_field_aux venv envl pos v vl with
         Not_found ->
            let pos = string_pos "eval_find_field" pos in
               raise (OmakeException (pos, UnboundMethod vl))

(*
 * Method paths.
 *)
and eval_with_method_exn venv path obj pos vl =
   match vl with
      [v] ->
         let v = venv_find_field_exn venv obj pos v in
         let venv = venv_with_object venv obj in
            venv, path, v
    | v :: vl ->
         let path, v = venv_find_field_path_exn venv path obj pos v in
         let obj = eval_object_exn venv pos v in
            eval_with_method_exn venv path obj pos vl
    | [] ->
         raise (OmakeException (pos, StringError "empty method name"))

and eval_with_method_aux venv envl pos v vl =
   match envl with
      [env] ->
         let env = eval_object_exn venv pos env in
         let path = PathVar v in
            eval_with_method_exn venv path env pos vl
    | env :: envl ->
         let env = eval_object_exn venv pos env in
         let path = PathVar v in
            (try eval_with_method_exn venv path env pos vl with
                Not_found ->
                   eval_with_method_aux venv envl pos v vl)
    | [] ->
         raise Not_found

and eval_with_method venv pos loc v vl =
   let envl = venv_current_objects venv pos v in
      try eval_with_method_aux venv envl pos v vl with
         Not_found ->
            let pos = string_pos "eval_with_method" (loc_pos loc pos) in
               raise (OmakeException (pos, UnboundMethod vl))

(*
 * Method paths.
 *)
and eval_find_method_exn venv obj pos vl =
   match vl with
      [v] ->
         let v = venv_find_field_exn venv obj pos v in
         let venv = venv_with_object venv obj in
            venv, v
    | v :: vl ->
         let v = venv_find_field_exn venv obj pos v in
         let obj = eval_object_exn venv pos v in
            eval_find_method_exn venv obj pos vl
    | [] ->
         raise (OmakeException (pos, StringError "empty method name"))

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
   let envl = venv_current_objects venv pos v in
      try eval_find_method_aux venv envl pos vl with
         Not_found ->
            let pos = string_pos "eval_find_method" (loc_pos loc pos) in
               raise (OmakeException (pos, UnboundMethod vl))

(*
 * Check whether a field is defined.
 *)
and eval_defined_field_exn venv env pos vl =
   match vl with
      [v] ->
         venv_defined_field venv env v
    | v :: vl ->
         let v = venv_find_field_exn venv env pos v in
         let obj = eval_object_exn venv pos v in
            eval_defined_field_exn venv obj pos vl
    | [] ->
         raise (OmakeException (pos, StringError "empty method name"))

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

and eval_defined_field venv pos loc v vl =
   let envl = venv_current_objects venv pos v in
      try eval_defined_field_aux venv envl pos vl with
         Not_found ->
            false

(*
 * Simplify a quoted value if possible.
 * Strings are concatenated.
 *)
and simplify_quote_val venv pos c el =
   match el with
      [ValWhite s]
    | [ValString s]
    | [ValData s] ->
         (match c with
             None ->
                ValData s
           | Some c ->
                ValQuoteString (c, [ValData s]))
    | _ ->
         let buf = Buffer.create 32 in
         let flush vl =
            if Buffer.length buf = 0 then
               vl
            else
               let s = Buffer.contents buf in
                  Buffer.clear buf;
                  ValData s :: vl
         in
         let rec collect vl el =
            match el with
               e :: el ->
                  (match eval_value venv pos e with
                      ValWhite s
                    | ValString s
                    | ValData s ->
                         Buffer.add_string buf s;
                         collect vl el
                    | v ->
                         collect (v :: flush vl) el)
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
       | FunString (loc, opt_params, params, body, export) ->
            let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
            let env = venv_get_env venv in
               ValFun (env, opt_params, params, body, export)
       | ApplyString (loc, v, [], []) ->
            eval_var venv pos loc (venv_find_var venv pos loc v)
       | ApplyString (loc, v, args, kargs) ->
            eval_apply_string_exp venv venv pos loc (venv_find_var venv pos loc v) args kargs
       | SuperApplyString (loc, super, v, args, kargs) ->
            let v = venv_find_super_field venv pos loc super v in
               eval_apply_string_exp venv venv pos loc v args kargs
       | MethodApplyString (loc, v, vl, args, kargs) ->
            let venv_obj, v = eval_find_method venv pos loc v vl in
               eval_apply_string_exp venv venv_obj pos loc v args kargs
       | SequenceString (loc, sl) ->
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
            ValObject (venv_this venv)
       | LazyString (loc, s) ->
            ValStringExp (venv_get_env venv, s)
       | LetVarString (_, v, s1, s2) ->
            let x = eval_string_exp venv pos s1 in
            let venv = venv_add_var venv v x in
               eval_string_exp venv pos s2

and eval_keyword_string_exp venv pos (v, s) =
   v, eval_string_exp venv pos s

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
      ValStringExp (venv_get_env venv, s)

(************************************************************************
 * Export versions.
 *
 * These functions with the _export suffix also allow modifications
 * to the environment.
 *)
and eval_var_export venv pos loc v =
   let pos = string_pos "eval_var_export" pos in

   (* Do not use eval_value; we don't want to force evaluation *)
   match v with
      ValFun (env, _, [], body, export) ->
         let venv_new = venv_with_env venv env in
         let venv_new, result = eval_sequence venv_new pos ValNone body in
         let venv = add_exports venv venv_new pos export in
            venv, result
    | ValFunCurry (env, pargs, _, [], body, export, []) ->
         let venv_new = venv_with_partial_args venv env pargs in
         let venv_new, result = eval_sequence venv_new pos ValNone body in
         let venv = add_exports venv venv_new pos export in
            venv, result
    | ValFunCurry (env, pargs, _, [], body, export, kargs) ->
         let venv_new = venv_with_partial_args venv env pargs in
         let venv_new, v = eval_sequence venv_new pos ValNone body in
         let venv = add_exports venv venv_new pos export in
            eval_apply_export venv pos loc v [] kargs
    | ValPrim (_, _, ApplyEmpty, f) ->
         venv_apply_prim_fun f venv pos loc [] []
    | _ ->
         venv, v

(*
 * Evaluate an application.
 *)
and eval_apply_export venv pos loc v args kargs =
   let pos = string_pos "eval_apply_export" pos in
   match eval_value venv pos v with
      ValFun (env, keywords, params, body, export) ->
         let venv_new = venv_add_args venv pos loc env params args keywords kargs in
         let venv_new, result = eval_sequence_exp venv_new pos body in
         let venv = add_exports venv venv_new pos export in
            venv, result
    | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
         let venv_new, args, kargs = venv_add_curry_args venv pos loc env pargs params args keywords kargs1 kargs in
         let venv_new, v = eval_sequence_exp venv_new pos body in
         let venv = add_exports venv venv_new pos export in
            eval_apply_export venv pos loc v args kargs
    | ValPrim (_, _, _, f) ->
         venv_apply_prim_fun f venv pos loc args kargs
    | ValPrimCurry (_, _, f, args1, kargs1) ->
         venv_apply_prim_fun f venv pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs)
    | v ->
         if args = [] && kargs = [] then
            venv, v
         else
            let print_error buf =
               fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
               List.iter (fun arg ->
                     fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_value arg) args;
               List.iter (fun (v, arg) ->
                     fprintf buf "@ @[<hv 3>%a = %a@]" pp_print_symbol v pp_print_value arg) kargs;
               fprintf buf "@]"
            in
               raise (OmakeException (pos, LazyError print_error))

and eval_partial_apply venv pos loc v args kargs =
   match eval_value venv pos v with
      ValFun (env, keywords, params, body, export) ->
         (match venv_add_partial_args venv pos loc env [] params args keywords [] kargs with
             PartialApply (env, pargs, keywords, params, kargs) ->
                venv, ValFunCurry (env, pargs, keywords, params, body, export, kargs)
           | FullApply (venv, args, kargs) ->
                let venv_new, v = eval_sequence_exp venv pos body in
                let venv = add_exports venv venv_new pos export in
                   eval_partial_apply venv pos loc v args kargs)
    | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
         (match venv_add_partial_args venv pos loc env pargs params args keywords kargs1 kargs with
             PartialApply (env, pargs, keywords, params, kargs) ->
                venv, ValFunCurry (env, pargs, keywords, params, body, export, kargs)
           | FullApply (venv, args, kargs) ->
                let venv_new, v = eval_sequence_exp venv pos body in
                let venv = add_exports venv venv_new pos export in
                   eval_partial_apply venv pos loc v args kargs)
    | ValPrim (arity, eager, _, f) ->
         (match arity_apply_args arity [] args with
             FullArity (current_args, rest_args) ->
                (* We assume the primitive takes all the keyword args *)
                let venv, v = venv_apply_prim_fun f venv pos loc current_args kargs in
                   eval_partial_apply venv pos loc v rest_args []
           | PartialArity (arity, args) ->
                venv, ValPrimCurry (arity, eager, f, args, List.rev kargs))
    | ValPrimCurry (arity, eager, f, args1, kargs1) ->
         (match arity_apply_args arity args1 args with
             FullArity (current_args, rest_args) ->
                (* We assume the primitive takes all the keyword args *)
                let venv, v = venv_apply_prim_fun f venv pos loc current_args kargs in
                   eval_partial_apply venv pos loc v rest_args []
           | PartialArity (arity, args) ->
                venv, ValPrimCurry (arity, eager, f, args, List.rev_append kargs kargs1))
    | v ->
         if args = [] && kargs = [] then
            venv, v
         else
            let print_error buf =
               fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
               List.iter (fun arg ->
                     fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_value arg) args;
               List.iter (fun (v, arg) ->
                     fprintf buf "@ @[<hv 3>%a = %a@]" pp_print_symbol v pp_print_value arg) kargs;
               fprintf buf "@]"
            in
               raise (OmakeException (pos, LazyError print_error))

and eval_apply_string_export_exp venv venv_new pos loc v args kargs =
   let pos = string_pos "eval_apply_string_export_exp" pos in
   match eval_value venv pos v with
      ValFun (env, keywords, params, body, export) ->
         let args = List.map (eval_string_exp venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
         let venv_new = venv_add_args venv_new pos loc env params args keywords kargs in
         let venv_new, result = eval_sequence_exp venv_new pos body in
         let venv = add_exports venv venv_new pos export in
            venv, result
    | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
         let args = List.map (eval_string_exp venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
         let venv_new, args, kargs = venv_add_curry_args venv_new pos loc env pargs params args keywords kargs1 kargs in
         let venv_new, v = eval_sequence_exp venv_new pos body in
         let venv = add_exports venv venv_new pos export in
            eval_apply_export venv pos loc v args kargs
    | ValPrim (_, be_eager, _, f) ->
         let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
            venv_apply_prim_fun f venv_new pos loc args kargs
    | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
         let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
            venv_apply_prim_fun f venv_new pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs)
    | v ->
         if args = [] && kargs = [] then
            venv, v
         else
            let print_error buf =
               fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
               List.iter (fun arg ->
                     fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_string_exp arg) args;
               List.iter (fun (v, arg) ->
                     fprintf buf "@ @[<hv 3>%a = %a@]" pp_print_symbol v pp_print_string_exp arg) kargs;
               fprintf buf "@]"
            in
               raise (OmakeException (pos, LazyError print_error))

and eval_apply_method_export_exp venv venv_obj pos loc path v args kargs =
   let pos = string_pos "eval_apply_method_export_exp" pos in
   match eval_value venv pos v with
      ValFun (env, keywords, params, body, export) ->
         let args = List.map (eval_string_exp venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
         let venv_new = venv_add_args venv_obj pos loc env params args keywords kargs in
         let venv_new, result = eval_sequence_exp venv_new pos body in
         let venv = add_path_exports venv venv_obj venv_new pos path export in
            venv, result
    | ValFunCurry (env, pargs, keywords, params, body, export, kargs1) ->
         (* XXX: JYH: this, need to think about *)
         let args = List.map (eval_string_exp venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_string_exp venv pos s) kargs in
         let venv_new, args, kargs = venv_add_curry_args venv_obj pos loc env pargs params args keywords kargs1 kargs in
         let venv_new, v = eval_sequence_exp venv_new pos body in
         let venv = add_path_exports venv venv_obj venv_new pos path export in
            eval_apply_export venv pos loc v args kargs
    | ValPrim (_, be_eager, _, f) ->
         let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
         let venv_new, result = venv_apply_prim_fun f venv_obj pos loc args kargs in
         let venv = hoist_this venv venv_new path in
            venv, result
    | ValPrimCurry (_, be_eager, f, args1, kargs1) ->
         let args = List.map (eval_prim_arg_exp be_eager venv pos) args in
         let kargs = List.map (fun (v, s) -> v, eval_prim_arg_exp be_eager venv pos s) kargs in
         let venv_new, result = venv_apply_prim_fun f venv_obj pos loc (List.rev_append args1 args) (List.rev_append kargs1 kargs) in
         let venv = hoist_this venv venv_new path in
            venv, result
    | v ->
         if args = [] && kargs = [] then
            venv, v
         else
            let print_error buf =
               fprintf buf "@[<v 3>illegal function application:@ @[<hv 3>function:@ %a@]" pp_print_value v;
               List.iter (fun arg ->
                     fprintf buf "@ @[<hv 3>arg = %a@]" pp_print_string_exp arg) args;
               List.iter (fun (v, arg) ->
                     fprintf buf "@ @[<hv 3>%a = %a@]" pp_print_symbol v pp_print_string_exp arg) kargs;
               fprintf buf "@]"
            in
               raise (OmakeException (pos, LazyError print_error))

(*
 * Evaluate a string expression, and allow exports.
 *)
and eval_string_export_exp venv pos s =
   let pos = string_pos "eval_string_export_exp" pos in
      match s with
         NoneString _ ->
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
       | FunString (loc, opt_params, params, body, export) ->
            let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
            let env = venv_get_env venv in
               venv, ValFun (env, opt_params, params, body, export)
       | ApplyString (loc, v, [], []) ->
            eval_var_export venv pos loc (venv_find_var venv pos loc v)
       | ApplyString (loc, v, args, kargs) ->
            eval_apply_string_export_exp venv venv pos loc (venv_find_var venv pos loc v) args kargs
       | SuperApplyString (loc, super, v, args, kargs) ->
            let v = venv_find_super_field venv pos loc super v in
               eval_apply_string_export_exp venv venv pos loc v args kargs
       | MethodApplyString (loc, v, vl, args, kargs) ->
            let venv_obj, path, v = eval_with_method venv pos loc v vl in
               eval_apply_method_export_exp venv venv_obj pos loc path v args kargs
       | SequenceString (loc, sl) ->
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
            venv, ValObject (venv_this venv)
       | LazyString (loc, s) ->
            venv, ValStringExp (venv_get_env venv, s)
       | LetVarString (_, v, s1, s2) ->
            let venv, x = eval_string_export_exp venv pos s1 in
            let venv = venv_add_var venv v x in
               eval_string_export_exp venv pos s2

(************************************************************************
 * Evaluate an expression.
 *)
and eval_exp venv result e =
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
            append_arrays venv pos (venv_get_var venv pos v) s
   in
   let venv = venv_add_var venv v s in
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
            append_arrays venv pos (venv_find_field venv obj pos v) e
   in
   let venv, obj = venv_add_field venv obj pos v e in
   let venv = hoist_path venv path obj in
      venv, e

(*
 * Key (property) definitions.
 *)
and eval_let_key_exp venv pos v flag s =
   let pos = string_pos "eval_let_key_exp" pos in
   let v = ValData v in
   let venv, s = eval_string_export_exp venv pos s in

   (* Get the current property list *)
   let map =
      try venv_find_var_exn venv map_field_var with
         Not_found ->
            raise (OmakeException (pos, StringError "current object is not a Map"))
   in
   let map = eval_map venv pos map in

   (* Add the new definition *)
   let s =
      match flag with
         VarDefNormal ->
            s
       | VarDefAppend ->
            append_arrays venv pos (venv_map_find map pos v) s
   in
   let map = venv_map_add map pos v s in
   let venv = venv_add_var venv map_field_var (ValMap map) in
      venv, s

(*
 * Function definitions.
 *)
and eval_let_fun_exp venv pos loc v curry opt_params params body export =
   let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
   let env = venv_get_env venv in
   let e =
      if curry then
         ValFunCurry (env, [], opt_params, params, body, export, [])
      else
         ValFun (env, opt_params, params, body, export)
   in
   let venv = venv_add_var venv v e in
      venv, e

and eval_let_fun_field_exp venv pos loc v vl curry opt_params params body export =
   let opt_params = eval_keyword_param_value_list_exp venv pos opt_params in
   let env = venv_get_env venv in
   let e =
      if curry then
         ValFunCurry (env, [], opt_params, params, body, export, [])
      else
         ValFun (env, opt_params, params, body, export)
   in
   let path, obj, v = eval_find_field venv pos loc v vl in
   let venv, obj = venv_add_field venv obj pos v e in
   let venv = hoist_path venv path obj in
      venv, e

(*
 * Shell expression.
 *)
and eval_shell_exp venv pos loc e =
   let pos = string_pos "eval_shell_exp" pos in
   let () =
      if !debug_shell then
         eprintf "@[<v 3>eval_shell_exp (pid = %i):@ %a@]@." (**)
            (Unix.getpid()) pp_print_string_exp e
   in
   let v = venv_find_var venv pos loc system_var in
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
   let venv = add_exports venv venv_new pos export in
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
      try venv_find_static_object venv node key with
         Not_found ->
            (* Evaluate the object, and save it *)
            let _, result = eval_sequence (venv_define_object venv) pos ValNone el in
            let obj = eval_object venv pos result in
               venv_add_static_object venv node key obj;
               obj
   in
   let venv = venv_include_static_object venv obj in
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
   let venv_obj = venv_define_object venv in
   let venv_obj = venv_include_object venv_obj obj in
   let venv_obj, result = eval_sequence venv_obj pos ValNone el in
   let venv = venv_add_var venv v result in
   let venv = add_exports venv venv_obj pos export in
      venv, result

and eval_let_object_field_exp venv pos loc v vl s el export =
   let pos = string_pos "eval_let_object_field_exp" pos in
   let parent = eval_string_exp venv pos s in
   let obj = eval_object venv pos parent in
   let venv_obj = venv_define_object venv in
   let venv_obj = venv_include_object venv_obj obj in
   let venv_obj, e = eval_sequence venv_obj pos ValNone el in
   let path, obj, v = eval_find_field venv pos loc v vl in
   let venv, obj = venv_add_field venv obj pos v e in
   let venv = hoist_path venv path obj in
   let venv = add_exports venv venv_obj pos export in
      venv, e

(*
 * This.
 * Set the current object to the given object.
 *)
and eval_let_this_exp venv pos s =
   let pos = string_pos "eval_this_exp" pos in
   let venv, obj = eval_string_export_exp venv pos s in
   let obj = eval_object venv pos obj in
   let venv = venv_with_object venv obj in
      venv, ValObject obj

(*
 * Include a file.
 * The environment after the file is evaluated is used in the rest
 * of this file.
 *)
and eval_include_exp venv pos loc s commands =
   let pos = string_pos "eval_include" pos in
   let name =
      match eval_string_exp venv pos s with
         ValNode node ->
            (* Use an absolute name, preventing path lookup *)
            Node.absname node
       | name ->
            string_of_value venv pos name
   in
   let node = find_include_file venv pos loc name in
   let venv = venv_add_file venv node in
   let venv = include_file venv IncludePervasives pos loc node in
      venv, ValNone

(*
 * Open a file.
 * Include it if it is not already included.
 *)
and eval_open_exp venv pos loc nodes =
   let pos = string_pos "eval_open" pos in
   let venv =
      List.fold_left (fun venv node ->
            if venv_is_included_file venv node then
               venv
            else
               let venv = venv_add_file venv node in
                  include_file venv IncludePervasives pos loc node) venv nodes
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
      eval_apply_string_export_exp venv venv pos loc (venv_find_var venv pos loc f) args kargs

and eval_super_apply_exp venv pos loc super v args kargs =
   let pos = string_pos "eval_super_apply_exp" pos in
   let v = venv_find_super_field venv pos loc super v in
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
         Return (_, v, id') when id' == id ->
            venv, v

and eval_return_exp venv pos loc s id =
   let pos = string_pos "eval_return_exp" pos in
   let result = eval_string_exp venv pos s in
      raise (Return (loc, result, id))

and eval_string_value_exp venv pos s =
   let pos = string_pos "eval_string_value_exp" pos in
   let result = eval_string_exp venv pos s in
      venv, result

and eval_return_save_exp venv pos =
   let _pos = string_pos "eval_return_save_exp" pos in
      venv, ValNone

and eval_return_object_exp venv pos names =
   let result = venv_current_object venv names in
      venv, ValObject result

(*
 * Include a file.
 *)
and eval_include_file venv scope pos loc node =
   let ir = compile_ir venv scope pos loc node in
   let venv_new = venv_add_var venv file_var (ValNode node) in
   let venv_new, result = eval_exp venv_new ValNone ir.ir_exp in
   let venv = add_exports venv venv_new pos ExportAll in
      venv, result

and include_file venv scope pos loc target =
   let pos = string_pos "include_file" pos in
   let venv = venv_add_included_file venv target in
   let venv, _ = eval_include_file venv scope pos loc target in
      venv

(*
 * Parse and evaluate a file as if it were an object.
 *)
and eval_object_file venv pos loc node =
   let parse_obj info node =
      let ir = compile_add_ir_info venv IncludePervasives pos loc node info in
      let { ir_classnames = names;
            ir_exp = e
          } = ir
      in
      let venv = venv_get_pervasives venv node in
      let venv = venv_define_object venv in
      let venv, result = eval_exp venv ValNone e in
         venv_current_object venv names
   in
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
      if Sys.file_exists makeroot_name then
         makeroot_name
      else
         makeroot_short_name
   in
   let node = venv_intern venv PhonyProhibited rootname in
   let venv = venv_add_file venv node in
   let loc = bogus_loc (Node.fullname node) in
   let pos = string_pos "compile" (loc_exp_pos loc) in
   let _ = eval_include_file venv IncludePervasives pos loc node in
      if debug print_rules then
         eprintf "@[<hv 3>Rules:%a@]@." pp_print_explicit_rules venv

(************************************************************************
 * Dependencies.
 *)
let compile_deps venv node buf =
   let deps = Omake_ast_lex.parse_deps buf in
   let vars = venv_include_scope venv IncludePervasives in
   let senv_empty = penv_of_vars (open_ir venv) venv node vars in
      List.map (fun (target, source, loc) ->
            let pos = string_pos "compile_deps" (loc_exp_pos loc) in
            let _, target = build_string senv_empty target pos in
            let _, source = build_string senv_empty source pos in
            let target = eval_string_exp venv pos target in
            let source = eval_string_exp venv pos source in
            let targets = strings_of_value venv pos target in
            let sources = strings_of_value venv pos source in
               targets, sources) deps

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
