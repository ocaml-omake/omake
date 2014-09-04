(*
 * Virtual identifiers for various things in files.
 *)


(*
 * These are all builtin variables.
 *
 * ZZZ: in 0.9.8 they are VarGlobal.
 *)
let loc = Lm_location.bogus_loc "Builtin"

let create_pervasives_var v =
   Omake_ir.VarGlobal (loc, v)

(*
 * Generally useful virtual variables.
 *)
let argv_var                   = create_pervasives_var Omake_symbol.argv_sym
let options_var                = create_pervasives_var Omake_symbol.options_object_sym

let explicit_target_var        = create_pervasives_var Omake_symbol.explicit_target_sym
let cwd_var                    = create_pervasives_var Omake_symbol.cwd_sym
let stdlib_var                 = create_pervasives_var Omake_symbol.stdlib_sym
let stdroot_var                = create_pervasives_var Omake_symbol.stdroot_sym
let ostype_var                 = create_pervasives_var Omake_symbol.ostype_sym
let omakepath_var              = create_pervasives_var Omake_symbol.omakepath_sym
let path_var                   = create_pervasives_var Omake_symbol.path_sym
let auto_rehash_var            = create_pervasives_var Omake_symbol.auto_rehash_sym
(* let printexitvalue_var         = create_pervasives_var Omake_symbol.printexitvalue_sym *)
let system_var                 = create_pervasives_var Omake_symbol.system_sym
let oshell_var                 = create_pervasives_var Omake_symbol.oshell_sym
let cdpath_var                 = create_pervasives_var Omake_symbol.cdpath_sym
let history_file_var           = create_pervasives_var Omake_symbol.history_file_sym
let history_length_var         = create_pervasives_var Omake_symbol.history_length_sym
let targets_var                = create_pervasives_var Omake_symbol.targets_sym
let build_summary_var          = create_pervasives_var Omake_symbol.build_summary_sym

let prompt_var                 = create_pervasives_var Omake_symbol.prompt_sym
let ignoreeof_var              = create_pervasives_var Omake_symbol.ignoreeof_sym
let exit_on_uncaught_exception_var = create_pervasives_var Omake_symbol.exit_on_uncaught_exception_sym
let abort_on_command_error_var = create_pervasives_var Omake_symbol.abort_on_command_error_sym
let create_subdirs_var         = create_pervasives_var Omake_symbol.create_subdirs_sym
let allow_empty_subdirs_var    = create_pervasives_var Omake_symbol.allow_empty_subdirs_sym
let glob_options_var           = create_pervasives_var Omake_symbol.glob_options_sym
let glob_ignore_var            = create_pervasives_var Omake_symbol.glob_ignore_sym
let glob_allow_var             = create_pervasives_var Omake_symbol.glob_allow_sym
let scanner_mode_var           = create_pervasives_var Omake_symbol.scanner_mode_sym

let stdin_var                  = create_pervasives_var Omake_symbol.stdin_sym
let stdout_var                 = create_pervasives_var Omake_symbol.stdout_sym
let stderr_var                 = create_pervasives_var Omake_symbol.stderr_sym

let star_var                   = create_pervasives_var Omake_symbol.star_sym
let at_var                     = create_pervasives_var Omake_symbol.at_sym
let gt_var                     = create_pervasives_var Omake_symbol.gt_sym
let plus_var                   = create_pervasives_var Omake_symbol.plus_sym
let hat_var                    = create_pervasives_var Omake_symbol.hat_sym
let lt_var                     = create_pervasives_var Omake_symbol.lt_sym
let amp_var                    = create_pervasives_var Omake_symbol.amp_sym
let braces_var                 = create_pervasives_var Omake_symbol.braces_sym
let fs_var                     = create_pervasives_var Omake_symbol.fs_sym
let rs_var                     = create_pervasives_var Omake_symbol.rs_sym
let filename_var               = create_pervasives_var Omake_symbol.filename_sym
let fnr_var                    = create_pervasives_var Omake_symbol.fnr_sym

let parse_loc_var              = create_pervasives_var Omake_symbol.parse_loc_sym
let zero_var                   = create_pervasives_var Omake_symbol.zero_sym
let nf_var                     = create_pervasives_var Omake_symbol.nf_sym

let object_var                 = create_pervasives_var Omake_symbol.object_sym
let int_object_var             = create_pervasives_var Omake_symbol.int_object_sym
let float_object_var           = create_pervasives_var Omake_symbol.float_object_sym
let string_object_var          = create_pervasives_var Omake_symbol.string_object_sym
let sequence_object_var        = create_pervasives_var Omake_symbol.sequence_object_sym
let array_object_var           = create_pervasives_var Omake_symbol.array_object_sym
let fun_object_var             = create_pervasives_var Omake_symbol.fun_object_sym
let rule_object_var            = create_pervasives_var Omake_symbol.rule_object_sym
let file_object_var            = create_pervasives_var Omake_symbol.file_object_sym
let dir_object_var             = create_pervasives_var Omake_symbol.dir_object_sym
let body_object_var            = create_pervasives_var Omake_symbol.body_object_sym
let in_channel_object_var      = create_pervasives_var Omake_symbol.in_channel_object_sym
let out_channel_object_var     = create_pervasives_var Omake_symbol.out_channel_object_sym
let in_out_channel_object_var  = create_pervasives_var Omake_symbol.in_out_channel_object_sym
let lexer_object_var           = create_pervasives_var Omake_symbol.lexer_object_sym
let parser_object_var          = create_pervasives_var Omake_symbol.parser_object_sym
let location_object_var        = create_pervasives_var Omake_symbol.location_object_sym
let map_object_var             = create_pervasives_var Omake_symbol.map_object_sym
let shell_object_var           = create_pervasives_var Omake_symbol.shell_object_sym
let target_object_var          = create_pervasives_var Omake_symbol.target_object_sym
let stat_object_var            = create_pervasives_var Omake_symbol.stat_object_sym
let passwd_object_var          = create_pervasives_var Omake_symbol.passwd_object_sym
let group_object_var           = create_pervasives_var Omake_symbol.group_object_sym
let pipe_object_var            = create_pervasives_var Omake_symbol.pipe_object_sym
let select_object_var          = create_pervasives_var Omake_symbol.pipe_object_sym
let runtime_exception_var      = create_pervasives_var Omake_symbol.runtime_exception_sym
let var_object_var             = create_pervasives_var Omake_symbol.var_object_sym
let tm_object_var              = create_pervasives_var Omake_symbol.tm_object_sym

let extends_var                = create_pervasives_var Omake_symbol.extends_sym
let omakeflags_var             = create_pervasives_var Omake_symbol.omakeflags_sym
let omakeargv_var              = create_pervasives_var Omake_symbol.omakeargv_sym

let printexitvalue_var         = create_pervasives_var Omake_symbol.printexitvalue_sym

let loc_field_var              = Omake_ir.VarThis (loc, Omake_symbol.loc_sym)
let builtin_field_var          = Omake_ir.VarThis (loc, Omake_symbol.builtin_sym)
let map_field_var              = Omake_ir.VarThis (loc, Omake_symbol.map_sym)
let current_prec_field_var     = Omake_ir.VarThis (loc, Omake_symbol.current_prec_sym)
let lexer_field_var            = Omake_ir.VarThis (loc, Omake_symbol.lexer_sym)

let file_var                   = Omake_ir.VarPrivate (loc, Omake_symbol.file_sym)
let file_id_var                = Omake_ir.VarPrivate (loc, Omake_symbol.file_id_sym)
let wild_var                   = Omake_ir.VarPrivate (loc, Omake_symbol.wild_sym)

(*
 * Special handling for small numeric vars.
 *)
let create_numeric_var =
   let numeric_vars = ref [||] in
   let resize i =
      let size = (i + 1) * 2 in
      let table = Array.create size wild_var in
         for j = 0 to pred size do
            table.(j) <- create_pervasives_var (Lm_symbol.add (string_of_int j))
         done;
         numeric_vars := table;
         table
   in
   let get i =
      let table = !numeric_vars in
         if i < Array.length table then
            table.(i)
         else
            (resize i).(i)
   in
      get

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
