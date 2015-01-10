(*
 * Debugging.
 *)
val debug_scanner  : bool ref
val debug_implicit : bool ref

(*
 * Type of environments.
 *)
type t

(*
 * Full and partial applications.
 *)
type partial_apply =
  | FullApply    of t * Omake_value_type.t list * Omake_value_type.keyword_value list
  | PartialApply of 
     Omake_value_type.env * Omake_value_type.param_value list * 
     Omake_value_type.keyword_param_value list * 
     Omake_ir.param list * Omake_value_type.keyword_value list

(*
 * Command lists are used for rule bodies.
 * They have their environment, a list of sources,
 * and the actual body.  The body is polymorphic
 * for various kinds of commands.
 *)
type command_info =
   { command_env       : t;
     command_sources   : Omake_node.Node.t list;
     command_values    : Omake_value_type.t list;
     command_body      : Omake_value_type.command list
   }

(*
 * A rule description.
 *)
and erule =
   { rule_loc         : Lm_location.t;
     rule_env         : t;
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
     explicit_deps            : 
       (Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_node.NodeSet.t) Omake_node.NodeTable.t;
     (* locks, sources, scanners *)
     explicit_rules           : erule Omake_node.NodeMTable.t;
     explicit_directories     : t Omake_node.DirTable.t
   }

type srule =
   { srule_loc      : Lm_location.t;
     srule_static   : bool;
     srule_env      : t;
     srule_key      : Omake_value_type.t;
     srule_deps     : Omake_node.NodeSet.t;
     srule_vals     : Omake_value_type.t list;
     srule_exp      : Omake_ir.exp
   }

type static_info =
   StaticRule of srule
 | StaticValue of Omake_value_type.obj

(*
 * Command lines.
 *)
and arg_command_inst = 
  (Omake_ir.exp, arg_pipe, Omake_value_type.t) Omake_command_type.poly_command_inst
and arg_command_line = 
  (t, Omake_ir.exp, arg_pipe, Omake_value_type.t) Omake_command_type.poly_command_line

and string_command_inst = (Omake_ir.exp, string_pipe, Omake_value_type.t) Omake_command_type.poly_command_inst
and string_command_line = (t, Omake_ir.exp, string_pipe, Omake_value_type.t) Omake_command_type.poly_command_line

and apply        = t -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> (Lm_symbol.t * string) list -> Omake_value_type.t list -> int * t * Omake_value_type.t

and value_cmd    = (unit, Omake_value_type.t list, Omake_value_type.t list) Omake_shell_type.poly_cmd
and value_apply  = (Omake_value_type.t list, Omake_value_type.t list, apply) Omake_shell_type.poly_apply
and value_group  = (unit, Omake_value_type.t list, Omake_value_type.t list, Omake_value_type.t list, apply) Omake_shell_type.poly_group
and value_pipe   = (unit, Omake_value_type.t list, Omake_value_type.t list, Omake_value_type.t list, apply) Omake_shell_type.poly_pipe

and arg_cmd      = (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_command_type.arg) Omake_shell_type.poly_cmd
and arg_apply    = (Omake_value_type.t, Omake_command_type.arg, apply) Omake_shell_type.poly_apply
and arg_group    = 
    (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_value_type.t, Omake_command_type.arg, apply) Omake_shell_type.poly_group

and arg_pipe     = (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_value_type.t, Omake_command_type.arg, apply) Omake_shell_type.poly_pipe

and string_cmd   = (Omake_shell_type.simple_exe, string, string) Omake_shell_type.poly_cmd
and string_apply = (Omake_value_type.t, string, apply) Omake_shell_type.poly_apply
and string_group = (Omake_shell_type.simple_exe, string, Omake_value_type.t, string, apply) Omake_shell_type.poly_group
and string_pipe  = 
  (Omake_shell_type.simple_exe, string, Omake_value_type.t, string, apply) Omake_shell_type.poly_pipe

(*
 * Command line parsing.
 *)
type lexer = string -> int -> int -> int option

type tok =
   TokString of Omake_value_type.t
 | TokToken  of string
 | TokGroup  of tok list

(*
 * Type of execution servers.
 *)
type pid =
   InternalPid of int
 | ExternalPid of int
 | ResultPid of int * t * Omake_value_type.t

type exec = (arg_command_line, pid, Omake_value_type.t) Omake_exec.Exec.t

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


(* Target directories, compressed to a small int *)
type target_dir

(*
 * Check if command list does not contain anything to execute.
 *)
val commands_are_trivial : command_info list -> bool

(*
 * Convert a target to a raw string.
 *)
val string_of_target : t -> Omake_value_type.target -> string

(*
 * This takes the starting directory.
 *)
val create          : Omake_options.t -> string -> exec -> Omake_cache.t -> t

(*
 * Pervasives management.
 *)
val venv_set_pervasives : t -> unit
val venv_get_pervasives : t -> Omake_node.Node.t -> t

(*
 * Variables in scope.
 *)
val venv_include_scope : t -> include_scope -> Omake_ir.senv

(*
 * Fork, so that a thread can work on a private copy in peace.
 *)
val venv_fork       : t -> t
val venv_unfork     : t -> t -> t

(*
 * Global values.
 *)
val venv_exec       : t -> exec
val venv_cache      : t -> Omake_cache.t
val venv_add_cache  : t -> Omake_cache.t -> t

(*
 * Add values to environment.
 *)
val venv_chdir            : t -> Lm_location.t -> string -> t
val venv_chdir_dir        : t -> Lm_location.t -> Omake_node.Dir.t -> t
val venv_chdir_tmp        : t -> Omake_node.Dir.t -> t
val venv_add_dir          : t -> unit
val venv_directories      : t -> t Omake_node.DirTable.t
val venv_add_explicit_dir : t -> Omake_node.Dir.t -> unit
val venv_remove_explicit_dir : t -> Omake_node.Dir.t -> unit
val venv_add_file         : t -> Omake_node.Node.t -> t
val venv_intern           : t -> Omake_node_sig.phony_ok -> string -> Omake_node.Node.t
val venv_intern_cd        : t -> Omake_node_sig.phony_ok -> Omake_node.Dir.t -> string -> Omake_node.Node.t
val venv_intern_dir       : t -> string -> Omake_node.Dir.t
val venv_intern_target    : t -> Omake_node_sig.phony_ok -> Omake_value_type.target -> Omake_node.Node.t
val venv_dirname          : t -> Omake_node.Dir.t -> string
val venv_nodename         : t -> Omake_node.Node.t -> string

val venv_mount       : t -> Omake_node_sig.mount_option list -> Omake_node.Dir.t -> Omake_node.Dir.t -> t

val venv_add_var     : t -> Omake_ir.var_info -> Omake_value_type.t -> t
val venv_add_phony   : t -> Lm_location.t -> Omake_value_type.target list -> t

val venv_add_args      : t -> Omake_value_type.pos -> Lm_location.t -> Omake_value_type.env -> Omake_ir.param list -> Omake_value_type.t list -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> t
val venv_with_args     : t -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.param list -> Omake_value_type.t list -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> t

val venv_add_curry_args : t ->Omake_value_type.pos -> Lm_location.t
    -> Omake_value_type.env -> Omake_value_type.param_value list -> Omake_ir.param list -> Omake_value_type.t list
    -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> Omake_value_type.keyword_value list
    -> t * Omake_value_type.t list * Omake_value_type.keyword_value list
val venv_add_partial_args : t -> Omake_value_type.pos -> Lm_location.t
    -> Omake_value_type.env -> Omake_value_type.param_value list -> Omake_ir.param list -> Omake_value_type.t list
    -> Omake_value_type.keyword_param_value list -> Omake_value_type.keyword_value list -> Omake_value_type.keyword_value list
    -> partial_apply
val venv_with_partial_args : t -> Omake_value_type.env -> Omake_value_type.param_value list -> t

val venv_add_wild_match  : t -> Omake_value_type.t -> t
val venv_add_match_values : t -> Omake_value_type.t list -> t
val venv_add_match_args  : t -> string list -> t
val venv_add_match       : t -> string -> string list -> t
val venv_explicit_target : t -> Omake_node.Node.t -> t
val venv_explicit_find   : t -> Omake_value_type.pos -> Omake_node.Node.t -> erule

val venv_add_rule : t -> Omake_value_type.pos -> Lm_location.t ->
   Omake_value_type.rule_multiple ->                     (* multiple, scanner, etc *)
   Omake_value_type.target list ->                       (* targets *)
   Omake_value_type.target list ->                       (* patterns *)
   Omake_value_type.target Omake_value_type.source list ->                (* effects *)
   Omake_value_type.target Omake_value_type.source list ->                (* sources *)
   Omake_value_type.target Omake_value_type.source list ->                (* scanners *)
   Omake_value_type.t list ->                        (* additional values the Omake_value_type.target depends on *)
   Omake_value_type.command list ->                      (* commands *)
   t * Omake_node.Node.t list

val venv_add_memo_rule : t -> Omake_value_type.pos -> Lm_location.t ->
   bool ->                              (* multiple *)
   bool ->                              (* static flag *)
   Omake_value_type.t ->                             (* key *)
   Omake_ir.var_info list ->                     (* variables to be defined *)
   Omake_value_type.target Omake_value_type.source list ->                (* sources *)
   Omake_value_type.t list ->                        (* additional values the Omake_value_type.target depends on *)
   Omake_ir.exp ->                               (* commands *)
   t

val venv_set_static_info  : t -> Omake_value_type.t -> static_info -> unit
val venv_find_static_info : t -> Omake_value_type.pos -> Omake_value_type.t -> static_info

(*
 * System environment.
 *)
val venv_environment : t -> string Lm_symbol.SymbolTable.t
val venv_setenv : t -> Omake_ir.var -> string -> t
val venv_getenv : t -> Omake_ir.var -> string
val venv_unsetenv : t -> Omake_ir.var -> t
val venv_defined_env : t -> Omake_ir.var -> bool

(*
 * Handle options.
 *)
val venv_options          : t -> Omake_options.t
val venv_with_options     : t -> Omake_options.t -> t
val venv_set_options      : t -> Lm_location.t -> Omake_value_type.pos -> string list -> t

(*
 * Values represented with handles.
 *)
val venv_add_environment      : t -> Omake_value_type.handle_env
val venv_find_environment     : t -> Omake_value_type.pos -> Omake_value_type.handle_env -> t

(*
 * Find values.
 *)
val venv_dir                  : t -> Omake_node.Dir.t
val venv_defined              : t -> Omake_ir.var_info -> bool
(* val venv_defined_field        : t -> Omake_value_type.obj -> Omake_ir.var -> bool *)

val venv_get_var              : t -> Omake_value_type.pos -> Omake_ir.var_info -> Omake_value_type.t

val venv_find_var             : t -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var_info -> Omake_value_type.t
val venv_find_var_exn         : t -> Omake_ir.var_info -> Omake_value_type.t
val venv_find_object_or_empty : t -> Omake_ir.var_info -> Omake_value_type.obj

(*
 * Static environments.
 *)
val venv_empty_env       : Omake_value_type.env
val venv_get_env         : t -> Omake_value_type.env
val venv_with_env        : t -> Omake_value_type.env -> t

(*
 * Static values.
 *)
val venv_find_static_object    : t -> Omake_node.Node.t -> Lm_symbol.t -> Omake_value_type.obj
val venv_add_static_object     : t -> Omake_node.Node.t -> Lm_symbol.t -> Omake_value_type.obj -> unit
val venv_include_static_object : t -> Omake_value_type.obj -> t
val venv_save_static_values    : t -> unit

(*
 * Primitive functions.
 *)
type prim_fun_data = t -> Omake_value_type.pos -> Lm_location.t -> Omake_value_type.t list -> Omake_value_type.keyword_value list -> t * Omake_value_type.t

val venv_add_prim_fun    : t -> Omake_ir.var -> prim_fun_data -> Omake_value_type.prim_fun
val venv_apply_prim_fun  : Omake_value_type.prim_fun -> prim_fun_data

(*
 * Channels.
 *)
val venv_stdin            : Omake_value_type.prim_channel
val venv_stdout           : Omake_value_type.prim_channel
val venv_stderr           : Omake_value_type.prim_channel

val venv_add_channel      : t -> Lm_channel.t -> Omake_value_type.prim_channel
val venv_close_channel    : t -> Omake_value_type.pos -> Omake_value_type.prim_channel -> unit
val venv_find_channel     : t -> Omake_value_type.pos -> Omake_value_type.prim_channel -> Lm_channel.t
val venv_find_channel_by_channel  : t -> Omake_value_type.pos -> Lm_channel.t -> Omake_value_type.prim_channel
val venv_find_channel_by_id       : t -> Omake_value_type.pos -> int -> Omake_value_type.prim_channel
val venv_add_formatter_channel    : t -> Format.formatter -> Omake_value_type.prim_channel

(*
 * Objects.
 *)
val venv_empty_object    : Omake_value_type.obj
val venv_this            : t -> Omake_value_type.obj
val venv_current_object  : t -> Lm_symbol.t list -> Omake_value_type.obj
val venv_define_object   : t -> t
val venv_with_object     : t -> Omake_value_type.obj -> t
val venv_include_object  : t -> Omake_value_type.obj -> t
val venv_flatten_object  : t -> Omake_value_type.obj -> t
val venv_find_super_field : t -> Omake_value_type.pos -> Lm_location.t -> Lm_symbol.t -> Lm_symbol.t -> Omake_value_type.t

(* ZZZ: this doesn't exist in 0.9.9 *)
val venv_current_objects : t -> Omake_value_type.pos -> Omake_ir.var_info -> Omake_value_type.t list

val venv_add_class       : Omake_value_type.obj -> Lm_symbol.t -> Omake_value_type.obj
val venv_instanceof      : Omake_value_type.obj -> Lm_symbol.t -> bool

val venv_find_field_path_exn  : t -> Omake_value_type.path -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.path * Omake_value_type.t
val venv_find_field_path      : t -> Omake_value_type.path -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.path * Omake_value_type.t
val venv_find_field_exn       : t -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.t
val venv_find_field           : t -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.t
val venv_add_field            : t -> Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.t -> t * Omake_value_type.obj
val venv_defined_field        : t -> Omake_value_type.obj -> Omake_ir.var -> bool
val venv_object_length        : Omake_value_type.obj -> int

(* Internal hacks when we don't care about checking *)
val venv_add_field_internal       : Omake_value_type.obj -> Omake_ir.var -> Omake_value_type.t -> Omake_value_type.obj
val venv_find_field_internal      : Omake_value_type.obj -> Omake_value_type.pos -> Omake_ir.var -> Omake_value_type.t
val venv_find_field_internal_exn  : Omake_value_type.obj -> Omake_ir.var -> Omake_value_type.t
val venv_defined_field_internal   : Omake_value_type.obj -> Omake_ir.var -> bool
val venv_object_fold_internal     : ('a -> Omake_ir.var -> Omake_value_type.t -> 'a) -> 'a -> Omake_value_type.obj -> 'a

val venv_add_included_file    : t -> Omake_node.Node.t -> t
val venv_is_included_file     : t -> Omake_node.Node.t -> bool
val venv_find_ir_file_exn     : t -> Omake_node.Node.t -> Omake_ir.t
val venv_add_ir_file          : t -> Omake_node.Node.t -> Omake_ir.t -> unit
val venv_find_object_file_exn : t -> Omake_node.Node.t -> Omake_value_type.obj
val venv_add_object_file      : t -> Omake_node.Node.t -> Omake_value_type.obj -> unit

(*
 * Maps.
 *)
val venv_map_empty       : Omake_value_type.map
val venv_map_add         : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t -> Omake_value_type.map
val venv_map_remove      : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.map
val venv_map_find        : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.t -> Omake_value_type.t
val venv_map_mem         : Omake_value_type.map -> Omake_value_type.pos -> Omake_value_type.t -> bool
val venv_map_iter        : (Omake_value_type.t -> Omake_value_type.t -> unit) -> Omake_value_type.map -> unit
val venv_map_map         : (Omake_value_type.t -> Omake_value_type.t -> Omake_value_type.t) -> Omake_value_type.map -> Omake_value_type.map
val venv_map_fold        : ('a -> Omake_value_type.t -> Omake_value_type.t -> 'a) -> 'a -> Omake_value_type.map -> 'a
val venv_map_length      : Omake_value_type.map -> int

(*
 * Get a list of all the files that were read.
 *)
val venv_files : t -> Omake_node.NodeSet.t

(*
 * Get the explicit rules.
 *)
val venv_explicit_exists : t -> Omake_node.Node.t -> bool
val venv_explicit_rules  : t -> erule_info

(*
 * Find all the implicit rules and dependencies.
 *    (static_deps, lock_deps, scanner_deps, value_deps)
 *)
val venv_find_implicit_deps  : t -> Omake_node.Node.t -> Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_value_type.t list
val venv_find_implicit_rules : t -> Omake_node.Node.t -> erule list

(*
 * Ordering.
 *)
val venv_add_orders        : t -> Lm_location.t -> Omake_value_type.target list -> t
val venv_is_order          : t -> string -> bool
val venv_add_ordering_rule : t -> Omake_value_type.pos -> Lm_location.t -> Omake_ir.var -> Omake_value_type.target -> Omake_value_type.target list -> t
val venv_get_ordering_info : t -> Omake_ir.var -> ordering_info
val venv_get_ordering_deps : t -> ordering_info -> Omake_node.NodeSet.t -> Omake_node.NodeSet.t

(*
 * Update the environment with a result.
 *)
val add_exports      : t -> t -> Omake_value_type.pos -> Omake_ir.export -> t
val add_path_exports : t -> t -> t -> Omake_value_type.pos -> Omake_value_type.path -> Omake_ir.export -> t
val hoist_path       : t -> Omake_value_type.path -> Omake_value_type.obj -> t
val hoist_this       : t -> t -> Omake_value_type.path -> t

(*
 * Cached buildable flags.
 *)
val venv_lookup_target_dir : t -> Omake_node.Dir.t -> target_dir
val venv_find_target_is_buildable_exn : t -> target_dir -> string -> Omake_node_sig.node_kind -> bool
val venv_find_target_is_buildable_proper_exn : t -> target_dir -> string -> Omake_node_sig.node_kind -> bool
val venv_add_target_is_buildable : t -> target_dir -> string -> Omake_node_sig.node_kind -> bool -> unit
val venv_add_target_is_buildable_proper : t -> target_dir -> string -> Omake_node_sig.node_kind -> bool -> unit

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
val squash_object : Omake_value_type.obj -> Omake_value_type.t Lm_symbol.SymbolTable.t

(*
 * General exception includes debugging info.
 *)
exception Break             of Lm_location.t * t

(*
 * For debugging.
 *)
val pp_print_explicit_rules : t Lm_printf.t 
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
   val read        : t -> Omake_node.Node.t -> (in_handle -> 'a) -> 'a
   val rewrite     : in_handle -> (out_handle -> 'a) -> 'a

   (*
    * Fetch the two kinds of entries.
    *)
   val find_ir     : in_handle -> Omake_ir.t
   val find_object : in_handle -> Omake_value_type.obj

   val get_ir      : out_handle -> Omake_ir.t
   val get_object  : out_handle -> Omake_value_type.obj

   (*
    * Add the two kinds of entries.
    *)
   val add_ir      : out_handle -> Omake_ir.t -> unit
   val add_object  : out_handle -> Omake_value_type.obj -> unit
end

module Static : StaticSig;;
