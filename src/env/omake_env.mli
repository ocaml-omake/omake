(*
 * Debugging.
 *)
val debug_scanner  : bool ref
val debug_implicit : bool ref

(*
 * Type of environments.
 *)
type venv

(*
 * Full and partial applications.
 *)
type partial_apply =
   FullApply    of venv * Omake_value_type.value list * Omake_value_type.keyword_value list
 | PartialApply of Omake_value_type.env * Omake_value_type.param_value list * Omake_value_type.keyword_param_value list * Omake_ir.param list * Omake_value_type.keyword_value list

(*
 * Command lists are used for rule bodies.
 * They have their environment, a list of sources,
 * and the actual body.  The body is polymorphic
 * for various kinds of commands.
 *)
type command_info =
   { command_env       : venv;
     command_sources   : Omake_node.Node.t list;
     command_values    : Omake_value_type.value list;
     command_body      : Omake_value_type.command list
   }

(*
 * A rule description.
 *)
and erule =
   { rule_loc         : Lm_location.loc;
     rule_env         : venv;
     rule_target      : Omake_node.Node.t;
     rule_effects     : Omake_node.NodeSet.t;
     rule_locks       : Omake_node.NodeSet.t;
     rule_sources     : Omake_node.NodeSet.t;
     rule_scanners    : Omake_node.NodeSet.t;
     rule_match       : string option;
     rule_multiple    : Omake_value_type.rule_multiple;
     rule_commands    : command_info list
   }

(*
 * A listing of all the explicit rules.
 *
 *    explicit_targets     : the collapsed rules for each explicit target
 *    explicit_deps        : the table of explicit rules that are just dependencies
 *    explicit_rules       : the table of all individual explicit rules
 *    explicit_directories : the environment for each directory in the project
 *)
and erule_info =
   { explicit_targets         : erule Omake_node.NodeTable.t;
     explicit_deps            : (Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_node.NodeSet.t) Omake_node.NodeTable.t;   (* locks, sources, scanners *)
     explicit_rules           : erule Omake_node.NodeMTable.t;
     explicit_directories     : venv Omake_node.DirTable.t
   }

type srule =
   { srule_loc      : Lm_location.loc;
     srule_static   : bool;
     srule_env      : venv;
     srule_key      : Omake_value_type.value;
     srule_deps     : Omake_node.NodeSet.t;
     srule_vals     : Omake_value_type.value list;
     srule_exp      : Omake_ir.exp
   }

type static_info =
   StaticRule of srule
 | StaticValue of Omake_value_type.obj

(*
 * Command lines.
 *)
and arg_command_inst = (Omake_ir.exp, arg_pipe, Omake_value_type.value) Omake_command_type.poly_command_inst
and arg_command_line = (venv, Omake_ir.exp, arg_pipe, Omake_value_type.value) Omake_command_type.poly_command_line

and string_command_inst = (Omake_ir.exp, string_pipe, Omake_value_type.value) Omake_command_type.poly_command_inst
and string_command_line = (venv, Omake_ir.exp, string_pipe, Omake_value_type.value) Omake_command_type.poly_command_line

and apply        = venv -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> (Lm_symbol.t * string) list -> Omake_value_type.value list -> int * venv * Omake_value_type.value

and value_cmd    = (unit, Omake_value_type.value list, Omake_value_type.value list) Omake_shell_type.poly_cmd
and value_apply  = (Omake_value_type.value list, Omake_value_type.value list, apply) Omake_shell_type.poly_apply
and value_group  = (unit, Omake_value_type.value list, Omake_value_type.value list, Omake_value_type.value list, apply) Omake_shell_type.poly_group
and value_pipe   = (unit, Omake_value_type.value list, Omake_value_type.value list, Omake_value_type.value list, apply) Omake_shell_type.poly_pipe

and arg_cmd      = (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_command_type.arg) Omake_shell_type.poly_cmd
and arg_apply    = (Omake_value_type.value, Omake_command_type.arg, apply) Omake_shell_type.poly_apply
and arg_group    = 
    (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_value_type.value, Omake_command_type.arg, apply) Omake_shell_type.poly_group

and arg_pipe     = (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_value_type.value, Omake_command_type.arg, apply) Omake_shell_type.poly_pipe

and string_cmd   = (Omake_shell_type.simple_exe, string, string) Omake_shell_type.poly_cmd
and string_apply = (Omake_value_type.value, string, apply) Omake_shell_type.poly_apply
and string_group = (Omake_shell_type.simple_exe, string, Omake_value_type.value, string, apply) Omake_shell_type.poly_group
and string_pipe  = (Omake_shell_type.simple_exe, string, Omake_value_type.value, string, apply) Omake_shell_type.poly_pipe

(*
 * Command line parsing.
 *)
type lexer = string -> int -> int -> int option

type tok =
   TokString of Omake_value_type.value
 | TokToken  of string
 | TokGroup  of tok list

(*
 * Type of execution servers.
 *)
type pid =
   InternalPid of int
 | ExternalPid of int
 | ResultPid of int * venv * Omake_value_type.value

type exec = (arg_command_line, pid, Omake_value_type.value) Omake_exec.Exec.t

(*
 * Ordering info is abstract.
 *)
type ordering_info

(*
 * Inclusion scope is usually Pervasives,
 * but it may include everything in scope.
 *)
type include_scope =
   IncludePervasives
 | IncludeAll

(*
 * Check if command list does not contain anything to execute.
 *)
val commands_are_trivial : command_info list -> bool

(*
 * Convert a target to a raw string.
 *)
val string_of_target : venv -> Omake_value_type.target -> string

(*
 * This takes the starting directory.
 *)
val create          : Omake_options.t -> string -> exec -> Omake_cache.t -> venv

(*
 * Pervasives management.
 *)
val venv_set_pervasives : venv -> unit
val venv_get_pervasives : venv -> Omake_node.Node.t -> venv

(*
 * Variables in scope.
 *)
val venv_include_scope : venv -> include_scope -> Omake_ir.senv

(*
 * Fork, so that a thread can work on a private copy in peace.
 *)
val venv_fork       : venv -> venv
val venv_unfork     : venv -> venv -> venv

(*
 * Global values.
 *)
val venv_exec       : venv -> exec
val venv_cache      : venv -> Omake_cache.t
val venv_add_cache  : venv -> Omake_cache.t -> venv

(*
 * Add values to environment.
 *)
val venv_chdir            : venv -> Lm_location.loc -> string -> venv
val venv_chdir_dir        : venv -> Lm_location.loc -> Omake_node.Dir.t -> venv
val venv_chdir_tmp        : venv -> Omake_node.Dir.t -> venv
val venv_add_dir          : venv -> unit
val venv_directories      : venv -> venv Omake_node.DirTable.t
val venv_add_explicit_dir : venv -> Omake_node.Dir.t -> unit
val venv_remove_explicit_dir : venv -> Omake_node.Dir.t -> unit
val venv_add_file         : venv -> Omake_node.Node.t -> venv
val venv_intern           : venv -> Omake_node_sig.phony_ok -> string -> Omake_node.Node.t
val venv_intern_cd        : venv -> Omake_node_sig.phony_ok -> Omake_node.Dir.t -> string -> Omake_node.Node.t
val venv_intern_dir       : venv -> string -> Omake_node.Dir.t
val venv_intern_target    : venv -> Omake_node_sig.phony_ok -> Omake_value_type.target -> Omake_node.Node.t
val venv_dirname          : venv -> Omake_node.Dir.t -> string
val venv_nodename         : venv -> Omake_node.Node.t -> string

val venv_mount       : venv -> Omake_node_sig.mount_option list -> Omake_node.Dir.t -> Omake_node.Dir.t -> venv

val venv_add_var     : venv -> Omake_ir.var_info -> Omake_value_type.value -> venv
val venv_add_phony   : venv -> Lm_location.loc -> Omake_value_type.target list -> venv

val venv_add_args      : venv -> Omake_value_type.pos -> Lm_location.loc -> Omake_value_type.env -> Omake_ir.param list -> Omake_value_type.value list -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> venv
val venv_with_args     : venv -> Omake_value_type.pos -> Lm_location.loc -> Omake_ir.param list -> Omake_value_type.value list -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> venv

val venv_add_curry_args : venv ->Omake_value_type.pos -> Lm_location.loc
    -> Omake_value_type.env -> Omake_value_type.param_value list -> Omake_ir.param list -> Omake_value_type.value list
    -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> Omake_value_type.keyword_value list
    -> venv * Omake_value_type.value list * Omake_value_type.keyword_value list
val venv_add_partial_args : venv -> Omake_value_type.pos -> Lm_location.loc
    -> Omake_value_type.env -> Omake_value_type.param_value list -> Omake_ir.param list -> Omake_value_type.value list
    -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> Omake_value_type.keyword_value list
    -> partial_apply
val venv_with_partial_args : venv -> Omake_value_type.env -> Omake_value_type.param_value list -> venv

val venv_add_wild_match  : venv -> Omake_value_type.value -> venv
val venv_add_match_values : venv -> Omake_value_type.value list -> venv
val venv_add_match_args  : venv -> string list -> venv
val venv_add_match       : venv -> string -> string list -> venv
val venv_explicit_target : venv -> Omake_node.Node.t -> venv
val venv_explicit_find   : venv -> Omake_value_type.pos -> Omake_node.Node.t -> erule

val venv_add_rule : venv -> Omake_value_type.pos -> Lm_location.loc ->
   Omake_value_type.rule_multiple ->                     (* multiple, scanner, etc *)
   Omake_value_type.target list ->                       (* targets *)
   Omake_value_type.target list ->                       (* patterns *)
   Omake_value_type.target Omake_value_type.source list ->                (* effects *)
   Omake_value_type.target Omake_value_type.source list ->                (* sources *)
   Omake_value_type.target Omake_value_type.source list ->                (* scanners *)
   Omake_value_type.value list ->                        (* additional values the Omake_value_type.target depends on *)
   Omake_value_type.command list ->                      (* commands *)
   venv * Omake_node.Node.t list

val venv_add_memo_rule : venv -> Omake_value_type.pos -> Lm_location.loc ->
   bool ->                              (* multiple *)
   bool ->                              (* static flag *)
   Omake_value_type.value ->                             (* key *)
   Omake_ir.var_info list ->                     (* variables to be defined *)
   Omake_value_type.target Omake_value_type.source list ->                (* sources *)
   Omake_value_type.value list ->                        (* additional values the Omake_value_type.target depends on *)
   Omake_ir.exp ->                               (* commands *)
   venv

val venv_set_static_info  : venv -> Omake_value_type.value -> static_info -> unit
val venv_find_static_info : venv -> Omake_value_type.pos -> Omake_value_type.value -> static_info

(*
 * System environment.
 *)
val venv_environment : venv -> string Lm_symbol.SymbolTable.t
val venv_setenv : venv -> Omake_ir.var -> string -> venv
val venv_getenv : venv -> Omake_ir.var -> string
val venv_unsetenv : venv -> Omake_ir.var -> venv
val venv_defined_env : venv -> Omake_ir.var -> bool

(*
 * Handle options.
 *)
val venv_options          : venv -> Omake_options.t
val venv_with_options     : venv -> Omake_options.t -> venv
val venv_set_options      : venv -> Lm_location.loc -> Omake_value_type.pos -> string list -> venv

(*
 * Values represented with handles.
 *)
val venv_add_environment      : venv -> Omake_value_type.handle_env
val venv_find_environment     : venv -> Omake_value_type.pos -> Omake_value_type.handle_env -> venv

(*
 * Find values.
 *)
val venv_dir                  : venv -> Omake_node.Dir.t
val venv_defined              : venv -> Omake_ir.var_info -> bool
(* val venv_defined_field        : venv -> Omake_value_type.obj -> Omake_ir.var -> bool *)

val venv_get_var              : venv -> Omake_value_type.pos -> Omake_ir.var_info -> Omake_value_type.value

val venv_find_var             : venv -> Omake_value_type.pos -> Lm_location.loc -> Omake_ir.var_info -> Omake_value_type.value
val venv_find_var_exn         : venv -> Omake_ir.var_info -> Omake_value_type.value
val venv_find_object_or_empty : venv -> Omake_ir.var_info -> Omake_value_type.obj

(*
 * Static environments.
 *)
val venv_empty_env       : Omake_value_type.env
val venv_get_env         : venv -> Omake_value_type.env
val venv_with_env        : venv -> Omake_value_type.env -> venv

(*
 * Static values.
 *)
val venv_find_static_object    : venv -> Omake_node.Node.t -> Lm_symbol.t -> Omake_value_type.obj
val venv_add_static_object     : venv -> Omake_node.Node.t -> Lm_symbol.t -> Omake_value_type.obj -> unit
val venv_include_static_object : venv -> Omake_value_type.obj -> venv
val venv_save_static_values    : venv -> unit

(*
 * Primitive functions.
 *)
type prim_fun_data = venv -> Omake_value_type.pos -> Lm_location.loc -> Omake_value_type.value list -> Omake_value_type.keyword_value list -> venv * Omake_value_type.value

val venv_add_prim_fun    : venv -> Omake_ir.var -> prim_fun_data -> Omake_value_type.prim_fun
val venv_apply_prim_fun  : Omake_value_type.prim_fun -> prim_fun_data

(*
 * Channels.
 *)
val venv_stdin            : Omake_value_type.prim_channel
val venv_stdout           : Omake_value_type.prim_channel
val venv_stderr           : Omake_value_type.prim_channel

val venv_add_channel      : venv -> Lm_channel.t -> Omake_value_type.prim_channel
val venv_close_channel    : venv -> Omake_value_type.pos -> Omake_value_type.prim_channel -> unit
val venv_find_channel     : venv -> Omake_value_type.pos -> Omake_value_type.prim_channel -> Lm_channel.t
val venv_find_channel_by_channel  : venv -> Omake_value_type.pos -> Lm_channel.t -> Omake_value_type.prim_channel
val venv_find_channel_by_id       : venv -> Omake_value_type.pos -> int -> Omake_value_type.prim_channel
val venv_add_formatter_channel    : venv -> Format.formatter -> Omake_value_type.prim_channel

(*
 * Objects.
 *)
val venv_empty_object    : Omake_value_type.obj
val venv_this            : venv -> Omake_value_type.obj
val venv_current_object  : venv -> Lm_symbol.t list -> Omake_value_type.obj
val venv_define_object   : venv -> venv
val venv_with_object     : venv -> Omake_value_type.obj -> venv
val venv_include_object  : venv -> Omake_value_type.obj -> venv
val venv_flatten_object  : venv -> Omake_value_type.obj -> venv
val venv_find_super_field : venv -> Omake_value_type.pos -> Lm_location.loc -> Lm_symbol.t -> Lm_symbol.t -> Omake_value_type.value

(* ZZZ: this doesn't exist in 0.9.9 *)
val venv_current_objects : venv -> Omake_value_type.pos -> Omake_ir.var_info -> Omake_value_type.value list

val venv_add_class       : Omake_value_type.obj -> Lm_symbol.t -> Omake_value_type.obj
val venv_instanceof      : Omake_value_type.obj -> Lm_symbol.t -> bool

val venv_find_field_path_exn  : venv -> Omake_value_type.path -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.path * Omake_value_type.value
val venv_find_field_path      : venv -> Omake_value_type.path -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.path * Omake_value_type.value
val venv_find_field_exn       : venv -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.value
val venv_find_field           : venv -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.value
val venv_add_field            : venv -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.value -> venv * Omake_value_type.obj
val venv_defined_field        : venv -> Omake_value_type.obj -> Omake_ir.var -> bool
val venv_object_length        : Omake_value_type.obj -> int

(* Internal hacks when we don't care about checking *)
val venv_add_field_internal       : Omake_value_type.obj -> Omake_ir.var -> Omake_value_type.value -> Omake_value_type.obj
val venv_find_field_internal      : Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.value
val venv_find_field_internal_exn  : Omake_value_type.obj -> Omake_ir.var -> Omake_value_type.value
val venv_defined_field_internal   : Omake_value_type.obj -> Omake_ir.var -> bool
val venv_object_fold_internal     : ('a -> Omake_ir.var -> Omake_value_type.value -> 'a) -> 'a -> Omake_value_type.obj -> 'a

val venv_add_included_file    : venv -> Omake_node.Node.t -> venv
val venv_is_included_file     : venv -> Omake_node.Node.t -> bool
val venv_find_ir_file_exn     : venv -> Omake_node.Node.t -> Omake_ir.ir
val venv_add_ir_file          : venv -> Omake_node.Node.t -> Omake_ir.ir -> unit
val venv_find_object_file_exn : venv -> Omake_node.Node.t -> Omake_value_type.obj
val venv_add_object_file      : venv -> Omake_node.Node.t -> Omake_value_type.obj -> unit

(*
 * Maps.
 *)
val venv_map_empty       : Omake_value_type.map
val venv_map_add         : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value -> Omake_value_type.map
val venv_map_remove      : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.map
val venv_map_find        : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.value -> Omake_value_type.value
val venv_map_mem         : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.value -> bool
val venv_map_iter        : (Omake_value_type.value -> Omake_value_type.value -> unit) -> Omake_value_type.map -> unit
val venv_map_map         : (Omake_value_type.value -> Omake_value_type.value -> Omake_value_type.value) -> Omake_value_type.map -> Omake_value_type.map
val venv_map_fold        : ('a -> Omake_value_type.value -> Omake_value_type.value -> 'a) -> 'a -> Omake_value_type.map -> 'a
val venv_map_length      : Omake_value_type.map -> int

(*
 * Get a list of all the files that were read.
 *)
val venv_files : venv -> Omake_node.NodeSet.t

(*
 * Get the explicit rules.
 *)
val venv_explicit_exists : venv -> Omake_node.Node.t -> bool
val venv_explicit_rules  : venv -> erule_info

(*
 * Find all the implicit rules and dependencies.
 *    (static_deps, lock_deps, scanner_deps, value_deps)
 *)
val venv_find_implicit_deps  : venv -> Omake_node.Node.t -> Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_value_type.value list
val venv_find_implicit_rules : venv -> Omake_node.Node.t -> erule list

(*
 * Ordering.
 *)
val venv_add_orders        : venv -> Lm_location.loc -> Omake_value_type.target list -> venv
val venv_is_order          : venv -> string -> bool
val venv_add_ordering_rule : venv -> Omake_value_type.pos -> Lm_location.loc -> Omake_ir.var -> Omake_value_type.target -> Omake_value_type.target list -> venv
val venv_get_ordering_info : venv -> Omake_ir.var -> ordering_info
val venv_get_ordering_deps : venv -> ordering_info -> Omake_node.NodeSet.t -> Omake_node.NodeSet.t

(*
 * Update the environment with a result.
 *)
val add_exports      : venv -> venv -> Omake_value_type.pos -> Omake_ir.export -> venv
val add_path_exports : venv -> venv -> venv -> Omake_value_type.pos -> Omake_value_type.path -> Omake_ir.export -> venv
val hoist_path       : venv -> Omake_value_type.path -> Omake_value_type.obj -> venv
val hoist_this       : venv -> venv -> Omake_value_type.path -> venv

(*
 * Cached buildable flags.
 *)
val venv_find_target_is_buildable_exn : venv -> Omake_node.Node.t -> bool
val venv_find_target_is_buildable_proper_exn : venv -> Omake_node.Node.t -> bool
val venv_add_target_is_buildable : venv -> Omake_node.Node.t -> bool -> unit
val venv_add_target_is_buildable_proper : venv -> Omake_node.Node.t -> bool -> unit

(*
 * Printing.
 *)
val pp_print_tok : tok Lm_printf.t 

val pp_print_string_pipe : string_pipe Lm_printf.t 
val pp_print_string_command_inst : string_command_inst Lm_printf.t 
val pp_print_string_command_line : string_command_line Lm_printf.t 
val pp_print_string_command_lines : string_command_line list Lm_printf.t 

val pp_print_arg_pipe : arg_pipe Lm_printf.t 
val pp_print_arg_command_inst : arg_command_inst Lm_printf.t
val pp_print_arg_command_line : arg_command_line Lm_printf.t 
val pp_print_arg_command_lines : arg_command_line list Lm_printf.t

(************************************************************************
 * For squashing (producing digests).
 *)
val squash_prim_fun : Omake_value_type.prim_fun -> Omake_ir.var
val squash_object : Omake_value_type.obj -> Omake_value_type.value Lm_symbol.SymbolTable.t

(*
 * General exception includes debugging info.
 *)
exception Break             of Lm_location.loc * venv

(*
 * For debugging.
 *)
val pp_print_explicit_rules : venv Lm_printf.t 
val pp_print_rule           : erule Lm_printf.t 

(*
 * Static values.
 *)
val debug_db : bool ref

(*
 * Static loading.
 *)
module type StaticSig =
sig
   type in_handle
   type out_handle

   (*
    * Open a file.  The Omake_node.Node.t is the name of the _source_ file,
    * not the .omc file.  We'll figure out where the .omc file
    * goes on our own.  Raises Not_found if the source file
    * can't be found.
    * The implementation will make sure all the locking/unlocking is done properly.
    *)
   val read        : venv -> Omake_node.Node.t -> (in_handle -> 'a) -> 'a
   val rewrite     : in_handle -> (out_handle -> 'a) -> 'a

   (*
    * Fetch the two kinds of entries.
    *)
   val find_ir     : in_handle -> Omake_ir.ir
   val find_object : in_handle -> Omake_value_type.obj

   val get_ir      : out_handle -> Omake_ir.ir
   val get_object  : out_handle -> Omake_value_type.obj

   (*
    * Add the two kinds of entries.
    *)
   val add_ir      : out_handle -> Omake_ir.ir -> unit
   val add_object  : out_handle -> Omake_value_type.obj -> unit
end

module Static : StaticSig;;
