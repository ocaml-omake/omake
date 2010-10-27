(*
 * Environment for evaluating OMakefiles.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology, and
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
open Lm_printf

open Lm_string_util
open Lm_string_set
open Lm_location
open Lm_symbol

open Omake_ir
open Omake_pos
open Omake_var
open Omake_node
open Omake_exec
open Omake_cache
open Omake_lexer
open Omake_parser
open Omake_options
open Omake_node_sig
open Omake_exec_type
open Omake_shell_type
open Omake_value_type
open Omake_command_type
open Omake_ir_free_vars
open Omake_handle_table

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
   FullApply    of venv * value list * keyword_value list
 | PartialApply of env * param_value list * keyword_param_value list * param list * keyword_value list

(*
 * Command lists are used for rule bodies.
 * They have their environment, a list of sources,
 * and the actual body.  The body is polymorphic
 * for various kinds of commands.
 *)
type command_info =
   { command_env       : venv;
     command_sources   : Node.t list;
     command_values    : value list;
     command_body      : command list
   }

(*
 * A rule description.
 *)
and erule =
   { rule_loc         : loc;
     rule_env         : venv;
     rule_target      : Node.t;
     rule_effects     : NodeSet.t;
     rule_locks       : NodeSet.t;
     rule_sources     : NodeSet.t;
     rule_scanners    : NodeSet.t;
     rule_match       : string option;
     rule_multiple    : rule_multiple;
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
   { explicit_targets         : erule NodeTable.t;
     explicit_deps            : (NodeSet.t * NodeSet.t * NodeSet.t) NodeTable.t;   (* locks, sources, scanners *)
     explicit_rules           : erule NodeMTable.t;
     explicit_directories     : venv DirTable.t
   }

type srule =
   { srule_loc      : loc;
     srule_static   : bool;
     srule_env      : venv;
     srule_key      : value;
     srule_deps     : NodeSet.t;
     srule_vals     : value list;
     srule_exp      : exp
   }

type static_info =
   StaticRule of srule
 | StaticValue of obj

(*
 * Command lines.
 *)
and arg_command_inst = (exp, arg_pipe, value) poly_command_inst
and arg_command_line = (venv, exp, arg_pipe, value) poly_command_line

and string_command_inst = (exp, string_pipe, value) poly_command_inst
and string_command_line = (venv, exp, string_pipe, value) poly_command_line

and apply        = venv -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> (symbol * string) list -> value list -> int * venv * value

and value_cmd    = (unit, value list, value list) poly_cmd
and value_apply  = (value list, value list, apply) poly_apply
and value_group  = (unit, value list, value list, value list, apply) poly_group
and value_pipe   = (unit, value list, value list, value list, apply) poly_pipe

and arg_cmd      = (arg cmd_exe, arg, arg) poly_cmd
and arg_apply    = (value, arg, apply) poly_apply
and arg_group    = (arg cmd_exe, arg, value, arg, apply) poly_group
and arg_pipe     = (arg cmd_exe, arg, value, arg, apply) poly_pipe

and string_cmd   = (simple_exe, string, string) poly_cmd
and string_apply = (value, string, apply) poly_apply
and string_group = (simple_exe, string, value, string, apply) poly_group
and string_pipe  = (simple_exe, string, value, string, apply) poly_pipe

(*
 * Command line parsing.
 *)
type lexer = string -> int -> int -> int option

type tok =
   TokString of value
 | TokToken  of string
 | TokGroup  of tok list

(*
 * Type of execution servers.
 *)
type pid =
   InternalPid of int
 | ExternalPid of int
 | ResultPid of int * venv * value

type exec = (arg_command_line, pid, value) Exec.t

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
val string_of_target : venv -> target -> string

(*
 * This takes the starting directory.
 *)
val create          : omake_options -> string -> exec -> Omake_cache.t -> venv

(*
 * Pervasives management.
 *)
val venv_set_pervasives : venv -> unit
val venv_get_pervasives : venv -> Node.t -> venv

(*
 * Variables in scope.
 *)
val venv_include_scope : venv -> include_scope -> senv

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
val venv_chdir            : venv -> loc -> string -> venv
val venv_chdir_dir        : venv -> loc -> Dir.t -> venv
val venv_chdir_tmp        : venv -> Dir.t -> venv
val venv_add_dir          : venv -> unit
val venv_directories      : venv -> venv DirTable.t
val venv_add_explicit_dir : venv -> Dir.t -> unit
val venv_remove_explicit_dir : venv -> Dir.t -> unit
val venv_add_file         : venv -> Node.t -> venv
val venv_intern           : venv -> phony_ok -> string -> Node.t
val venv_intern_cd        : venv -> phony_ok -> Dir.t -> string -> Node.t
val venv_intern_dir       : venv -> string -> Dir.t
val venv_intern_target    : venv -> phony_ok -> target -> Node.t
val venv_dirname          : venv -> Dir.t -> string
val venv_nodename         : venv -> Node.t -> string

val venv_mount       : venv -> mount_option list -> Dir.t -> Dir.t -> venv

val venv_add_var     : venv -> var_info -> value -> venv
val venv_add_phony   : venv -> loc -> target list -> venv

val venv_add_args      : venv -> pos -> loc -> env -> param list -> value list -> keyword_param_value list -> keyword_value list -> venv
val venv_with_args     : venv -> pos -> loc -> param list -> value list -> keyword_param_value list -> keyword_value list -> venv

val venv_add_curry_args : venv -> pos -> loc
    -> env -> param_value list -> param list -> value list
    -> keyword_param_value list -> keyword_value list -> keyword_value list
    -> venv * value list * keyword_value list
val venv_add_partial_args : venv -> pos -> loc
    -> env -> param_value list -> param list -> value list
    -> keyword_param_value list -> keyword_value list -> keyword_value list
    -> partial_apply
val venv_with_partial_args : venv -> env -> param_value list -> venv

val venv_add_wild_match  : venv -> value -> venv
val venv_add_match_values : venv -> value list -> venv
val venv_add_match_args  : venv -> string list -> venv
val venv_add_match       : venv -> string -> string list -> venv
val venv_explicit_target : venv -> Node.t -> venv
val venv_explicit_find   : venv -> pos -> Node.t -> erule

val venv_add_rule : venv -> pos -> loc ->
   rule_multiple ->                     (* multiple, scanner, etc *)
   target list ->                       (* targets *)
   target list ->                       (* patterns *)
   target source list ->                (* effects *)
   target source list ->                (* sources *)
   target source list ->                (* scanners *)
   value list ->                        (* additional values the target depends on *)
   command list ->                      (* commands *)
   venv * Node.t list

val venv_add_memo_rule : venv -> pos -> loc ->
   bool ->                              (* multiple *)
   bool ->                              (* static flag *)
   value ->                             (* key *)
   var_info list ->                     (* variables to be defined *)
   target source list ->                (* sources *)
   value list ->                        (* additional values the target depends on *)
   exp ->                               (* commands *)
   venv

val venv_set_static_info  : venv -> value -> static_info -> unit
val venv_find_static_info : venv -> pos -> value -> static_info

(*
 * System environment.
 *)
val venv_environment : venv -> string SymbolTable.t
val venv_setenv : venv -> var -> string -> venv
val venv_getenv : venv -> var -> string
val venv_unsetenv : venv -> var -> venv
val venv_defined_env : venv -> var -> bool

(*
 * Handle options.
 *)
val venv_options          : venv -> omake_options
val venv_with_options     : venv -> omake_options -> venv
val venv_set_options      : venv -> loc -> pos -> string list -> venv

(*
 * Values represented with handles.
 *)
val venv_add_environment      : venv -> handle_env
val venv_find_environment     : venv -> pos -> handle_env -> venv

(*
 * Find values.
 *)
val venv_dir                  : venv -> Dir.t
val venv_defined              : venv -> var_info -> bool
val venv_defined_field        : venv -> obj -> var -> bool

val venv_get_var              : venv -> pos -> var_info -> value

val venv_find_var             : venv -> pos -> loc -> var_info -> value
val venv_find_var_exn         : venv -> var_info -> value
val venv_find_object_or_empty : venv -> var_info -> obj

(*
 * Static environments.
 *)
val venv_empty_env       : env
val venv_get_env         : venv -> env
val venv_with_env        : venv -> env -> venv

(*
 * Static values.
 *)
val venv_find_static_object    : venv -> Node.t -> symbol -> obj
val venv_add_static_object     : venv -> Node.t -> symbol -> obj -> unit
val venv_include_static_object : venv -> obj -> venv
val venv_save_static_values    : venv -> unit

(*
 * Primitive functions.
 *)
type prim_fun_data = venv -> pos -> loc -> value list -> keyword_value list -> venv * value

val venv_add_prim_fun    : venv -> var -> prim_fun_data -> prim_fun
val venv_apply_prim_fun  : prim_fun -> prim_fun_data

(*
 * Channels.
 *)
val venv_stdin            : prim_channel
val venv_stdout           : prim_channel
val venv_stderr           : prim_channel

val venv_add_channel      : venv -> Lm_channel.t -> prim_channel
val venv_close_channel    : venv -> pos -> prim_channel -> unit
val venv_find_channel     : venv -> pos -> prim_channel -> Lm_channel.t
val venv_find_channel_by_channel  : venv -> pos -> Lm_channel.t -> prim_channel
val venv_find_channel_by_id       : venv -> pos -> int -> prim_channel
val venv_add_formatter_channel    : venv -> Format.formatter -> prim_channel

(*
 * Objects.
 *)
val venv_empty_object    : obj
val venv_this            : venv -> obj
val venv_current_object  : venv -> symbol list -> obj
val venv_define_object   : venv -> venv
val venv_with_object     : venv -> obj -> venv
val venv_include_object  : venv -> obj -> venv
val venv_flatten_object  : venv -> obj -> venv
val venv_find_super_field : venv -> pos -> loc -> symbol -> symbol -> value

(* ZZZ: this doesn't exist in 0.9.9 *)
val venv_current_objects : venv -> pos -> var_info -> value list

val venv_add_class       : obj -> symbol -> obj
val venv_instanceof      : obj -> symbol -> bool

val venv_find_field_path_exn  : venv -> path -> obj -> pos -> var -> path * value
val venv_find_field_path      : venv -> path -> obj -> pos -> var -> path * value
val venv_find_field_exn       : venv -> obj -> pos -> var -> value
val venv_find_field           : venv -> obj -> pos -> var -> value
val venv_add_field            : venv -> obj -> pos -> var -> value -> venv * obj
val venv_defined_field        : venv -> obj -> var -> bool
val venv_object_length        : obj -> int

(* Internal hacks when we don't care about checking *)
val venv_add_field_internal       : obj -> var -> value -> obj
val venv_find_field_internal      : obj -> pos -> var -> value
val venv_find_field_internal_exn  : obj -> var -> value
val venv_defined_field_internal   : obj -> var -> bool
val venv_object_fold_internal     : ('a -> var -> value -> 'a) -> 'a -> obj -> 'a

val venv_add_included_file    : venv -> Node.t -> venv
val venv_is_included_file     : venv -> Node.t -> bool
val venv_find_ir_file_exn     : venv -> Node.t -> ir
val venv_add_ir_file          : venv -> Node.t -> ir -> unit
val venv_find_object_file_exn : venv -> Node.t -> obj
val venv_add_object_file      : venv -> Node.t -> obj -> unit

(*
 * Maps.
 *)
val venv_map_empty       : map
val venv_map_add         : map -> pos -> value -> value -> map
val venv_map_remove      : map -> pos -> value -> map
val venv_map_find        : map -> pos -> value -> value
val venv_map_mem         : map -> pos -> value -> bool
val venv_map_iter        : (value -> value -> unit) -> map -> unit
val venv_map_map         : (value -> value -> value) -> map -> map
val venv_map_fold        : ('a -> value -> value -> 'a) -> 'a -> map -> 'a
val venv_map_length      : map -> int

(*
 * Get a list of all the files that were read.
 *)
val venv_files : venv -> NodeSet.t

(*
 * Get the explicit rules.
 *)
val venv_explicit_exists : venv -> Node.t -> bool
val venv_explicit_rules  : venv -> erule_info

(*
 * Find all the implicit rules and dependencies.
 *    (static_deps, lock_deps, scanner_deps, value_deps)
 *)
val venv_find_implicit_deps  : venv -> Node.t -> NodeSet.t * NodeSet.t * NodeSet.t * value list
val venv_find_implicit_rules : venv -> Node.t -> erule list

(*
 * Ordering.
 *)
val venv_add_orders        : venv -> loc -> target list -> venv
val venv_is_order          : venv -> string -> bool
val venv_add_ordering_rule : venv -> pos -> loc -> var -> target -> target list -> venv
val venv_get_ordering_info : venv -> var -> ordering_info
val venv_get_ordering_deps : venv -> ordering_info -> NodeSet.t -> NodeSet.t

(*
 * Update the environment with a result.
 *)
val add_exports      : venv -> venv -> pos -> export -> venv
val add_path_exports : venv -> venv -> venv -> pos -> path -> export -> venv
val hoist_path       : venv -> path -> obj -> venv
val hoist_this       : venv -> venv -> path -> venv

(*
 * Cached buildable flags.
 *)
val venv_find_target_is_buildable_exn : venv -> Node.t -> bool
val venv_find_target_is_buildable_proper_exn : venv -> Node.t -> bool
val venv_add_target_is_buildable : venv -> Node.t -> bool -> unit
val venv_add_target_is_buildable_proper : venv -> Node.t -> bool -> unit

(*
 * Printing.
 *)
val pp_print_tok : formatter -> tok -> unit

val pp_print_string_pipe : formatter -> string_pipe -> unit
val pp_print_string_command_inst : formatter -> string_command_inst -> unit
val pp_print_string_command_line : formatter -> string_command_line -> unit
val pp_print_string_command_lines : formatter -> string_command_line list -> unit

val pp_print_arg_pipe : formatter -> arg_pipe -> unit
val pp_print_arg_command_inst : formatter -> arg_command_inst -> unit
val pp_print_arg_command_line : formatter -> arg_command_line -> unit
val pp_print_arg_command_lines : formatter -> arg_command_line list -> unit

(************************************************************************
 * For squashing (producing digests).
 *)
val squash_prim_fun : prim_fun -> var
val squash_object : obj -> value SymbolTable.t

(*
 * General exception includes debugging info.
 *)
exception Break             of loc * venv

(*
 * For debugging.
 *)
val pp_print_explicit_rules : formatter -> venv -> unit
val pp_print_rule           : formatter -> erule -> unit

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
    * Open a file.  The Node.t is the name of the _source_ file,
    * not the .omc file.  We'll figure out where the .omc file
    * goes on our own.  Raises Not_found if the source file
    * can't be found.
    * The implementation will make sure all the locking/unlocking is done properly.
    *)
   val read        : venv -> Node.t -> (in_handle -> 'a) -> 'a
   val rewrite     : in_handle -> (out_handle -> 'a) -> 'a

   (*
    * Fetch the two kinds of entries.
    *)
   val find_ir     : in_handle -> ir
   val find_object : in_handle -> obj

   val get_ir      : out_handle -> ir
   val get_object  : out_handle -> obj

   (*
    * Add the two kinds of entries.
    *)
   val add_ir      : out_handle -> ir -> unit
   val add_object  : out_handle -> obj -> unit
end

module Static : StaticSig;;

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
