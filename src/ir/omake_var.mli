(*
 * Variables.
 *)



(*
 * Generally useful variables in pervasives.
 *)
val explicit_target_var        : Omake_ir.var_info
val wild_var                   : Omake_ir.var_info
val cwd_var                    : Omake_ir.var_info
val stdlib_var                 : Omake_ir.var_info
val stdroot_var                : Omake_ir.var_info
val ostype_var                 : Omake_ir.var_info
val omakepath_var              : Omake_ir.var_info
val path_var                   : Omake_ir.var_info
val auto_rehash_var            : Omake_ir.var_info
(* val printexitvalue_var         : Omake_ir.var_info *)
val system_var                 : Omake_ir.var_info
val oshell_var                 : Omake_ir.var_info
val cdpath_var                 : Omake_ir.var_info
val history_file_var           : Omake_ir.var_info
val history_length_var         : Omake_ir.var_info
val targets_var                : Omake_ir.var_info
val build_summary_var          : Omake_ir.var_info

val prompt_var                 : Omake_ir.var_info
val ignoreeof_var              : Omake_ir.var_info
val exit_on_uncaught_exception_var : Omake_ir.var_info
val abort_on_command_error_var : Omake_ir.var_info
val create_subdirs_var         : Omake_ir.var_info
val allow_empty_subdirs_var    : Omake_ir.var_info
val glob_options_var           : Omake_ir.var_info
val glob_ignore_var            : Omake_ir.var_info
val glob_allow_var             : Omake_ir.var_info
val scanner_mode_var           : Omake_ir.var_info

val stdin_var                  : Omake_ir.var_info
val stdout_var                 : Omake_ir.var_info
val stderr_var                 : Omake_ir.var_info

val argv_var                   : Omake_ir.var_info
val options_var                : Omake_ir.var_info

val star_var                   : Omake_ir.var_info
val at_var                     : Omake_ir.var_info
val gt_var                     : Omake_ir.var_info
val plus_var                   : Omake_ir.var_info
val hat_var                    : Omake_ir.var_info
val lt_var                     : Omake_ir.var_info
val amp_var                    : Omake_ir.var_info
val braces_var                 : Omake_ir.var_info

val parse_loc_var              : Omake_ir.var_info
val zero_var                   : Omake_ir.var_info
val nf_var                     : Omake_ir.var_info
val fs_var                     : Omake_ir.var_info
val rs_var                     : Omake_ir.var_info
val filename_var               : Omake_ir.var_info
val fnr_var                    : Omake_ir.var_info

val object_var                 : Omake_ir.var_info
val int_object_var             : Omake_ir.var_info
val float_object_var           : Omake_ir.var_info
val string_object_var          : Omake_ir.var_info
val sequence_object_var        : Omake_ir.var_info
val array_object_var           : Omake_ir.var_info
val fun_object_var             : Omake_ir.var_info
val rule_object_var            : Omake_ir.var_info
val file_object_var            : Omake_ir.var_info
val dir_object_var             : Omake_ir.var_info
val body_object_var            : Omake_ir.var_info
val in_channel_object_var      : Omake_ir.var_info
val out_channel_object_var     : Omake_ir.var_info
val in_out_channel_object_var  : Omake_ir.var_info
val lexer_object_var           : Omake_ir.var_info
val parser_object_var          : Omake_ir.var_info
val location_object_var        : Omake_ir.var_info
val map_object_var             : Omake_ir.var_info
val shell_object_var           : Omake_ir.var_info
val target_object_var          : Omake_ir.var_info
val stat_object_var            : Omake_ir.var_info
val passwd_object_var          : Omake_ir.var_info
val group_object_var           : Omake_ir.var_info
val pipe_object_var            : Omake_ir.var_info
val select_object_var          : Omake_ir.var_info
val runtime_exception_var      : Omake_ir.var_info
val var_object_var             : Omake_ir.var_info
val tm_object_var              : Omake_ir.var_info

val printexitvalue_var         : Omake_ir.var_info

val extends_var                : Omake_ir.var_info
val omakeflags_var             : Omake_ir.var_info
val omakeargv_var              : Omake_ir.var_info

(*
 * Internal fields.
 *)
val loc_field_var              : Omake_ir.var_info
val builtin_field_var          : Omake_ir.var_info
val map_field_var              : Omake_ir.var_info
val current_prec_field_var     : Omake_ir.var_info
val lexer_field_var            : Omake_ir.var_info
val file_var                   : Omake_ir.var_info
val file_id_var                : Omake_ir.var_info

(*
 * $0, $1, $2...
 *)
val create_numeric_var         : int -> Omake_ir.var_info
