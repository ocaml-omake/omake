(*
 * The environment for evaluating programs.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech and HRL Laboratories, LLC
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
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf

open Lm_debug
open Lm_symbol
open Lm_int_set
open Lm_location
open Lm_string_set
open Lm_string_util

open Omake_ir
open Omake_pos
open Omake_util
open Omake_wild
open Omake_node
open Omake_exec
open Omake_state
open Omake_symbol
open Omake_ir_print
open Omake_node_sig
open Omake_print_util
open Omake_shell_type
open Omake_command_type
open Omake_ir_free_vars
open Omake_handle_table
open Omake_value_print
open Omake_value_type
open Omake_options
open Omake_var

module Pos = MakePos (struct let name = "Omake_env" end)
open Pos;;

(*
 * Debugging.
 *)
let debug_scanner =
   create_debug (**)
      { debug_name = "scanner";
        debug_description = "Display debugging information for scanner selection";
        debug_value = false
      }

(*
 * Debugging.
 *)
let debug_implicit =
   create_debug (**)
      { debug_name = "implicit";
        debug_description = "Display debugging information for implicit rule selection";
        debug_value = false
      }

(*
 * Debug file database (.omc files).
 *)
let debug_db = Lm_db.debug_db

(*
 * Command lists have source arguments.
 *)
type command_info =
   { command_env     : venv;
     command_sources : Node.t list;
     command_values  : value list;
     command_body    : command list
   }

(*
 * An implicit rule with a body.
 *
 * In an implicit rule, we compile the targets/sources
 * to wild patterns.
 *)
and irule =
   { irule_loc        : loc;
     irule_multiple   : rule_multiple;
     irule_targets    : StringSet.t option;
     irule_patterns   : wild_in_patt list;
     irule_locks      : source_core source list;
     irule_sources    : source_core source list;
     irule_scanners   : source_core source list;
     irule_values     : value list;
     irule_body       : command list
   }

(*
 * An implicit dependency.  There is no body, but
 * it may have value dependencies.
 *)
and inrule =
   { inrule_loc        : loc;
     inrule_multiple   : rule_multiple;
     inrule_patterns   : wild_in_patt list;
     inrule_locks      : source_core source list;
     inrule_sources    : source_core source list;
     inrule_scanners   : source_core source list;
     inrule_values     : value list
   }

(*
 * Explicit rules.
 *)
and erule =
   { rule_loc          : loc;
     rule_env          : venv;
     rule_target       : Node.t;
     rule_effects      : NodeSet.t;
     rule_locks        : NodeSet.t;
     rule_sources      : NodeSet.t;
     rule_scanners     : NodeSet.t;
     rule_match        : string option;
     rule_multiple     : rule_multiple;
     rule_commands     : command_info list
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

(*
 * An ordering rule.
 * For now, this just defines an extra dependency
 * of the form:  patt1 -> patt2
 * This means that if a file depends on patt1,
 * then it also depends on patt2.
 *)
and orule =
   { orule_loc      : loc;
     orule_name     : symbol;
     orule_pattern  : wild_in_patt;
     orule_sources  : source_core list
   }

and ordering_info = orule list

(*
 * A static rule.
 *)
and srule =
   { srule_loc      : loc;
     srule_static   : bool;
     srule_env      : venv;
     srule_key      : value;
     srule_deps     : NodeSet.t;
     srule_vals     : value list;
     srule_exp      : exp
   }

and static_info =
   StaticRule of srule
 | StaticValue of obj

(*
 * The environment contains three scopes:
 *    1. The dynamic scope
 *    2. The current object
 *    3. The static scope
 * Lookup occurs in that order, unless the variables
 * have been defined otherwise.
 *
 * Each function has its own static scope.
 * The dynamic scope comes from the caller.
 *)
and venv =
   { venv_dynamic        : env;
     venv_this           : obj;
     venv_static         : env;
     venv_inner          : venv_inner
   }

and venv_inner =
   { venv_environ        : string SymbolTable.t;
     venv_dir            : Dir.t;
     venv_phony          : NodeSet.t;
     venv_implicit_deps  : inrule list;
     venv_implicit_rules : irule list;
     venv_options        : omake_options;
     venv_globals        : venv_globals;
     venv_mount          : Mount.t;
     venv_included_files : NodeSet.t
   }

and venv_globals =
   { venv_global_index                       : int;

     (* Execution service *)
     venv_exec                               : exec;

     (* File cache *)
     venv_cache                              : Omake_cache.t;

     (* Mounting functions *)
     venv_mount_info                         : mount_info;

     (* Values from handles *)
     venv_environments                       : venv HandleTable.t;

     (* The set of files we have ever read *)
     mutable venv_files                      : NodeSet.t;

     (* Save the environment for each directory in the project *)
     mutable venv_directories                : venv DirTable.t;
     mutable venv_excluded_directories       : DirSet.t;

     (* All the phony targets we have ever generated *)
     mutable venv_phonies                    : PreNodeSet.t;

     (* Explicit rules are global *)
     mutable venv_explicit_rules             : erule list;
     mutable venv_explicit_targets           : erule NodeTable.t;
     mutable venv_explicit_new               : erule list;

     (* Ordering rules *)
     mutable venv_ordering_rules             : orule list;
     mutable venv_orders                     : StringSet.t;

     (* Static rules *)
     mutable venv_memo_rules                 : static_info ValueTable.t;

     (* Cached values for files *)
     mutable venv_ir_files                   : ir NodeTable.t;
     mutable venv_object_files               : obj NodeTable.t;

     (* Cached values for static sections *)
     mutable venv_static_values              : obj SymbolTable.t NodeTable.t;
     mutable venv_modified_values            : obj SymbolTable.t NodeTable.t;

     (* Cached values for the target_is_buildable function *)
     mutable venv_target_is_buildable        : bool NodeTable.t;
     mutable venv_target_is_buildable_proper : bool NodeTable.t;

     (* The state right after Pervasives is evaluated *)
     mutable venv_pervasives_vars            : senv;
     mutable venv_pervasives_obj             : obj
   }

(*
 * Type of execution servers.
 *)
and pid =
   InternalPid of int
 | ExternalPid of int
 | ResultPid of int * venv * value

and exec = (arg_command_line, pid, value) Exec.t

(*
 * Execution service.
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
 * Error during translation.
 *)
exception Break             of loc * venv

(*
 * Now the stuff that is really global, not saved in venv.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end;;

module IntTable = Lm_map.LmMake (IntCompare);;

type prim_fun_data = venv -> pos -> loc -> value list -> keyword_value list -> venv * value

type venv_runtime =
   { venv_channels               : Lm_channel.t IntHandleTable.t;
     mutable venv_primitives     : prim_fun_data SymbolTable.t
   }

let venv_runtime =
   { venv_channels       = IntHandleTable.create ();
     venv_primitives     = SymbolTable.empty
   }

(*
 * Command line parsing.
 *)
type lexer = string -> int -> int -> int option

type tok =
   TokString of value
 | TokToken  of string
 | TokGroup  of tok list

(*
 * Inclusion scope is usually Pervasives,
 * but it may include everything in scope.
 *)
type include_scope =
   IncludePervasives
 | IncludeAll

(*
 * Full and partial applications.
 *)
type partial_apply =
   FullApply    of venv * value list * keyword_value list
 | PartialApply of env * param_value list * keyword_param_value list * param list * keyword_value list

(************************************************************************
 * Access to the globals.
 * This actually performs some computation in 0.9.9
 *)
let venv_globals venv =
   venv.venv_inner.venv_globals

(*
 * Map functions.
 *)
let check_map_key = ValueCompare.check

let venv_map_empty = ValueTable.empty

let venv_map_add map pos v1 v2 =
   ValueTable.add map (check_map_key pos v1) v2

let venv_map_remove map pos v1 =
   ValueTable.remove map (check_map_key pos v1)

let venv_map_find map pos v =
   try ValueTable.find map (check_map_key pos v) with
      Not_found ->
         raise (OmakeException (pos, UnboundValue v))

let venv_map_mem map pos v =
   ValueTable.mem map (check_map_key pos v)

let venv_map_iter   = ValueTable.iter
let venv_map_map    = ValueTable.mapi
let venv_map_fold   = ValueTable.fold
let venv_map_length = ValueTable.cardinal

(************************************************************************
 * Printing.
 *)
let rec pp_print_command buf command =
   match command with
      CommandSection (arg, fv, e) ->
         fprintf buf "@[<hv 3>section %a@ %a@]" pp_print_value arg pp_print_exp_list e
    | CommandValue (_, _, v) ->
         pp_print_string_exp buf v

and pp_print_commands buf commands =
   List.iter (fun command -> fprintf buf "@ %a" pp_print_command command) commands

and pp_print_command_info buf info =
   let { command_env     = venv;
         command_sources = sources;
         command_body    = commands
       } = info
   in
      fprintf buf "@[<hv 0>@[<hv 3>{@ command_dir = %a;@ @[<b 3>command_sources =%a@]@ @[<b 3>command_body =%a@]@]@ }@]" (**)
         pp_print_dir venv.venv_inner.venv_dir
         pp_print_node_list sources
         pp_print_commands commands

and pp_print_command_info_list buf infos =
   List.iter (fun info -> fprintf buf "@ %a" pp_print_command_info info) infos

and pp_print_rule buf erule =
   let { rule_target      = target;
         rule_effects     = effects;
         rule_locks       = locks;
         rule_sources     = sources;
         rule_scanners    = scanners;
         rule_commands    = commands
       } = erule
   in
      fprintf buf "@[<hv 0>@[<hv 3>rule {";
      fprintf buf "@ target = %a" pp_print_node target;
      fprintf buf "@ @[<b 3>effects =%a@]" pp_print_node_set effects;
      fprintf buf "@ @[<b 3>locks =%a@]" pp_print_node_set locks;
      fprintf buf "@ @[<b 3>sources =%a@]" pp_print_node_set sources;
      fprintf buf "@ @[<b 3>scanners =%a@]" pp_print_node_set scanners;
      fprintf buf "@ @[<hv 3>commands =%a@]" pp_print_command_info_list commands;
      fprintf buf "@]@ }@]"

let pp_print_explicit_rules buf venv =
   fprintf buf "@[<hv 3>Explicit rules:";
   List.iter (fun erule -> fprintf buf "@ %a" pp_print_rule erule) venv.venv_inner.venv_globals.venv_explicit_rules;
   fprintf buf "@]"

(************************************************************************
 * Pipeline printing.
 *)

(*
 * Token printing.
 *)
let rec pp_print_tok buf tok =
   match tok with
      TokString v ->
         pp_print_value buf v
    | TokToken s ->
         fprintf buf "$%s" s
    | TokGroup toks ->
         fprintf buf "(%a)" pp_print_tok_list toks

and pp_print_tok_list buf toks =
   match toks with
      [tok] ->
         pp_print_tok buf tok
    | tok :: toks ->
         pp_print_tok buf tok;
         pp_print_char buf ' ';
         pp_print_tok_list buf toks
    | [] ->
         ()

let pp_print_simple_exe buf exe =
   match exe with
      ExeString s ->
         pp_print_string buf s
    | ExeQuote s ->
         fprintf buf "\\%s" s
    | ExeNode node ->
         pp_print_node buf node

(*
 * Pipes based on strings.
 *)
module PrintString =
struct
   type arg_command = string
   type arg_apply   = value
   type arg_other   = string
   type exe         = simple_exe

   let pp_print_arg_command = pp_arg_data_string
   let pp_print_arg_apply   = pp_print_value
   let pp_print_arg_other   = pp_arg_data_string
   let pp_print_exe         = pp_print_simple_exe
end;;

module PrintStringPipe = MakePrintPipe (PrintString);;

module PrintStringv =
struct
   type argv = string_pipe

   let pp_print_argv = PrintStringPipe.pp_print_pipe
end;;

module PrintStringCommand = MakePrintCommand (PrintStringv);;

let pp_print_string_pipe = PrintStringPipe.pp_print_pipe
let pp_print_string_command_inst = PrintStringCommand.pp_print_command_inst
let pp_print_string_command_line = PrintStringCommand.pp_print_command_line
let pp_print_string_command_lines = PrintStringCommand.pp_print_command_lines

(*
 * Pipes based on arguments.
 *)
module PrintArg =
struct
   type arg_command = Omake_command_type.arg
   type arg_apply   = value
   type arg_other   = arg_command
   type exe         = arg_command cmd_exe

   let pp_print_arg_command = Omake_command_type.pp_print_arg
   let pp_print_arg_apply   = pp_print_simple_value
   let pp_print_arg_other   = pp_print_arg_command
   let pp_print_exe buf exe =
      match exe with
         CmdArg arg ->
            pp_print_arg_command buf arg
       | CmdNode node ->
            pp_print_node buf node
end;;

module PrintArgPipe = MakePrintPipe (PrintArg);;

module PrintArgv =
struct
   type argv = arg_pipe

   let pp_print_argv = PrintArgPipe.pp_print_pipe
end;;

module PrintArgCommand = MakePrintCommand (PrintArgv);;

let pp_print_arg_pipe = PrintArgPipe.pp_print_pipe
let pp_print_arg_command_inst = PrintArgCommand.pp_print_command_inst
let pp_print_arg_command_line = PrintArgCommand.pp_print_command_line
let pp_print_arg_command_lines = PrintArgCommand.pp_print_command_lines

(************************************************************************
 * Utilities.
 *)

(*
 * Don't make command info if there are no commands.
 *)
let make_command_info venv sources values body =
   match values, body with
      [], [] ->
         []
    | _ ->
         [{ command_env     = venv;
            command_sources = sources;
            command_values  = values;
            command_body    = body
          }]

(*
 * Check if the commands are trivial.
 *)
let commands_are_trivial commands =
   List.for_all (fun command -> command.command_body = []) commands

(*
 * Multiple flags.
 *)
let is_multiple_rule = function
   RuleMultiple
 | RuleScannerMultiple ->
      true
 | RuleSingle
 | RuleScannerSingle ->
      false

let is_scanner_rule = function
   RuleScannerSingle
 | RuleScannerMultiple ->
      true
 | RuleSingle
 | RuleMultiple ->
      false

let rule_kind = function
   RuleScannerSingle
 | RuleScannerMultiple ->
      RuleScanner
 | RuleSingle
 | RuleMultiple ->
      RuleNormal

(************************************************************************
 * Handles.
 *)
let venv_add_environment venv =
   HandleTable.add venv.venv_inner.venv_globals.venv_environments venv

let venv_find_environment venv pos hand =
   try HandleTable.find venv.venv_inner.venv_globals.venv_environments hand with
      Not_found ->
         let pos = string_pos "venv_find_environment" pos in
            raise (OmakeException (pos, StringError "unbound environment"))

(************************************************************************
 * Channels.
 *)

(*
 * Add a channel slot.
 *)
let venv_add_index_channel index data =
   let channels = venv_runtime.venv_channels in
   let channel = IntHandleTable.create_handle channels index in
      Lm_channel.set_id data index;
      IntHandleTable.add channels channel data;
      channel

let venv_add_channel venv data =
   let channels = venv_runtime.venv_channels in
   let channel = IntHandleTable.new_handle channels in
   let index = IntHandleTable.int_of_handle channel in
      Lm_channel.set_id data index;
      IntHandleTable.add channels channel data;
      channel

let add_channel file kind mode binary fd =
   Lm_channel.create file kind mode binary (Some fd)

let venv_stdin  = venv_add_index_channel 0 (add_channel "<stdin>"  Lm_channel.PipeChannel Lm_channel.InChannel  false Unix.stdin)
let venv_stdout = venv_add_index_channel 1 (add_channel "<stdout>" Lm_channel.PipeChannel Lm_channel.OutChannel false Unix.stdout)
let venv_stderr = venv_add_index_channel 2 (add_channel "<stderr>" Lm_channel.PipeChannel Lm_channel.OutChannel false Unix.stderr)

(*
 * A formatting channel.
 *)
let venv_add_formatter_channel venv fmt =
   let channels = venv_runtime.venv_channels in
   let fd = Lm_channel.create "formatter" Lm_channel.FileChannel Lm_channel.OutChannel true None in
   let channel = IntHandleTable.new_handle channels in
   let index = IntHandleTable.int_of_handle channel in
   let reader s off len =
      raise (Unix.Unix_error (Unix.EINVAL, "formatter-channel", ""))
   in
   let writer s off len =
      Format.pp_print_string fmt (String.sub s off len);
      len
   in
      Lm_channel.set_id fd index;
      Lm_channel.set_io_functions fd reader writer;
      IntHandleTable.add channels channel fd;
      channel

(*
 * Get the channel.
 *)
let venv_channel_data channel =
   (* Standard channels are always available *)
   if IntHandleTable.int_of_handle channel <= 2 then
      IntHandleTable.find_any venv_runtime.venv_channels channel
   else
      IntHandleTable.find venv_runtime.venv_channels channel

(*
 * When a channel is closed, close the buffers too.
 *)
let venv_close_channel venv pos channel =
   try
      let fd = venv_channel_data channel in
         Lm_channel.close fd;
         IntHandleTable.remove venv_runtime.venv_channels channel
   with
      Not_found ->
         (* Fail silently *)
         ()

(*
 * Get the channel.
 *)
let venv_find_channel venv pos channel =
   let pos = string_pos "venv_find_in_channel" pos in
      try venv_channel_data channel with
         Not_found ->
            raise (OmakeException (pos, StringError "channel is closed"))

(*
 * Finding by identifiers.
 *)
let venv_find_channel_by_channel venv pos fd =
   let index, _, _, _ = Lm_channel.info fd in
      try IntHandleTable.find_value venv_runtime.venv_channels index fd with
         Not_found ->
            raise (OmakeException (pos, StringError "channel is closed"))

let venv_find_channel_by_id venv pos index =
   try IntHandleTable.find_any_handle venv_runtime.venv_channels index with
      Not_found ->
         raise (OmakeException (pos, StringError "channel is closed"))

(************************************************************************
 * Primitive values.
 *)

(*
 * Allocate a function primitive.
 *)
let venv_add_prim_fun venv name data =
   venv_runtime.venv_primitives <- SymbolTable.add venv_runtime.venv_primitives name data;
   name

(*
 * Look up the primitive value if we haven't seen it already.
 *)
let venv_apply_prim_fun name venv pos loc args =
   let f =
      try SymbolTable.find venv_runtime.venv_primitives name with
         Not_found ->
            raise (OmakeException (loc_pos loc pos, UnboundVar name))
   in
      f venv pos loc args

(************************************************************************
 * Target cache.
 *
 * To keep this up-to-date, entries are added for explicit targets,
 * and the cache is flushed whenever an implicit rule is added.
 *)
let venv_find_target_is_buildable_exn venv target =
   NodeTable.find venv.venv_inner.venv_globals.venv_target_is_buildable target

let venv_find_target_is_buildable_proper_exn venv target =
   NodeTable.find venv.venv_inner.venv_globals.venv_target_is_buildable_proper target

let venv_add_target_is_buildable venv target flag =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_target_is_buildable <- NodeTable.add globals.venv_target_is_buildable target flag

let venv_add_target_is_buildable_proper venv target flag =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_target_is_buildable_proper <- NodeTable.add globals.venv_target_is_buildable_proper target flag

let venv_add_explicit_targets venv rules =
   let globals = venv.venv_inner.venv_globals in
   let { venv_target_is_buildable = cache;
         venv_target_is_buildable_proper = cache_proper
       } = globals
   in
   let cache =
      List.fold_left (fun cache erule ->
            NodeTable.add cache erule.rule_target true) cache rules
   in
   let cache_proper =
      List.fold_left (fun cache erule ->
            NodeTable.add cache erule.rule_target true) cache_proper rules
   in
      globals.venv_target_is_buildable <- cache;
      globals.venv_target_is_buildable_proper <- cache_proper

let venv_flush_target_cache venv =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_target_is_buildable <- NodeTable.empty;
      globals.venv_target_is_buildable_proper <- NodeTable.empty

(*
 * Save explicit rules.
 *)
let venv_save_explicit_rules venv loc rules =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_explicit_new <- List.rev_append rules globals.venv_explicit_new;
      venv_add_explicit_targets venv rules

(*
 * Add an explicit dependency.
 *)
let venv_add_explicit_dep venv loc target source =
   let erule =
      { rule_loc        = loc;
        rule_env        = venv;
        rule_target     = target;
        rule_effects    = NodeSet.singleton target;
        rule_sources    = NodeSet.singleton source;
        rule_locks      = NodeSet.empty;
        rule_scanners   = NodeSet.empty;
        rule_match      = None;
        rule_multiple   = RuleSingle;
        rule_commands   = []
      }
   in
      ignore (venv_save_explicit_rules venv loc [erule])

(*
 * Phony names.
 *)
let venv_add_phony venv loc names =
   if names = [] then
      venv
   else
      let inner = venv.venv_inner in
      let { venv_dir = dir;
            venv_phony = phony
          } = inner
      in
      let globals = venv_globals venv in
      let phonies = globals.venv_phonies in
      let phony, phonies =
         List.fold_left (fun (phony, phonies) name ->
               let name =
                  match name with
                     TargetNode _ ->
                        raise (OmakeException (loc_exp_pos loc, StringError ".PHONY arguments should be names"))
                   | TargetString s ->
                        s
               in
               let gnode = Node.create_phony_global name in
               let dnode = Node.create_phony_dir dir name in
               let phony = NodeSet.add phony dnode in
               let phonies = PreNodeSet.add phonies (Node.dest gnode) in
               let phonies = PreNodeSet.add phonies (Node.dest dnode) in
                  venv_add_explicit_dep venv loc gnode dnode;
                  phony, phonies) (phony, phonies) names
      in
      let inner = { inner with venv_phony = phony } in
      let venv = { venv with venv_inner = inner } in
         globals.venv_phonies <- phonies;
         venv

(************************************************************************
 * Static values.
 *)

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

(*
 * For static values, we access the db a bit more directly
 *)
module type InternalStaticSig =
sig
   include StaticSig
   val write       : venv -> Node.t -> (out_handle -> 'a) -> 'a

   val find_values : in_handle -> obj SymbolTable.t
   val add_values  : out_handle -> obj SymbolTable.t -> unit
end

module Static : InternalStaticSig =
struct
   (************************************************************************
    * Types.
    *)

   (*
    * A .omc file.
    *)
   type t =
      { db_file         : Unix.file_descr option;
        db_name         : Node.t;
        db_digest       : string;
        db_env          : venv;
        db_flush_ir     : bool;
        db_flush_static : bool
      }
   type in_handle = t
   type out_handle = t

   (*
    * Tags for the various kinds of entries.
    *)
   let ir_tag      = 0, Lm_db.HostIndependent
   let object_tag  = 1, Lm_db.HostDependent
   let values_tag  = 2, Lm_db.HostDependent

   (************************************************************************
    * Operations.
    *)

   (*
    * Open a file.  The Node.t is the name of the _source_ file,
    * not the .omc file.  We'll figure out where the .omc file
    * goes on our own.
    *)
   let create_mode mode venv source =
      (* Get the source digest *)
      let cache = venv.venv_inner.venv_globals.venv_cache in
      let digest =
         match Omake_cache.stat cache source with
            Some digest ->
               digest
          | None ->
               raise Not_found
      in

      (*
       * Open the result file.  The lock_cache_file function
       * will try to use the target directory first, and
       * fall back to ~/.omake/cache if that is not writable.
       *)
      let source_name = Node.absname source in
      let dir = Filename.dirname source_name in
      let name = Filename.basename source_name in
      let name =
         if Filename.check_suffix name ".om" then
            Filename.chop_suffix name ".om"
         else
            name
      in
      let name = name ^ ".omc" in
      let target_fd =
         try
            let target_name, target_fd = Omake_state.get_cache_file dir name in
               if !debug_db then
                  eprintf "@[<v 3>Omake_db.create:@ %a --> %s@]@." pp_print_node source target_name;
               Unix.set_close_on_exec target_fd;
               Omake_state.lock_file target_fd mode;
               Some target_fd
         with
            Unix.Unix_error _
          | Failure _ ->
               eprintf "@[<v 3>OMake warning: could not create and/or lock a cache file for@ %s@]@." source_name;
               None
      in
         { db_file         = target_fd;
           db_name         = source;
           db_digest       = digest;
           db_env          = venv;
           db_flush_ir     = opt_flush_include venv.venv_inner.venv_options;
           db_flush_static = opt_flush_static venv.venv_inner.venv_options;
         }

   (*
    * Restart with a write lock.
    *)
   let rewrite info f =
      match info.db_file with
         Some fd ->
            ignore (Unix.lseek fd 0 Unix.SEEK_SET: int);
            Omake_state.lock_file fd Unix.F_ULOCK;
            Omake_state.lock_file fd Unix.F_LOCK;
            let finish () =
               ignore (Unix.lseek fd 0 Unix.SEEK_SET: int);
               Omake_state.lock_file fd Unix.F_ULOCK;
               Omake_state.lock_file fd Unix.F_RLOCK
            in
               begin try
                  let result = f info in
                     finish ();
                     result
                  with exn ->
                     finish ();
                     raise exn
               end
       | None ->
            f info

   (*
    * Close the file.
    *)
   let close info =
      match info with
         { db_file = Some fd; db_name = name } ->
            if !debug_db then
               eprintf "Omake_db.close: %a@." pp_print_node name;
            Unix.close fd
       | { db_file = None } ->
            ()

   let perform mode venv source f =
      let info = create_mode mode venv source in
         try
            let result = f info in
               close info;
               result
         with exn ->
            close info;
            raise exn

   let read venv source f = perform Unix.F_RLOCK venv source f
   let write venv source f = perform Unix.F_LOCK venv source f

   (*
    * Add the three kinds of entries.
    *)
   let add_ir info ir =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = venv } ->
            if !debug_db then
               eprintf "Omake_db.add_ir: %a@." pp_print_node name;
            Lm_db.add fd (Node.absname name) ir_tag Omake_magic.ir_magic digest ir
       | { db_file = None } ->
            ()

   let add_object info obj =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = venv } ->
            if !debug_db then
               eprintf "Omake_db.add_object: %a@." pp_print_node name;
            Lm_db.add fd (Node.absname name) object_tag Omake_magic.obj_magic digest obj
       | { db_file = None } ->
            ()

   let add_values info obj =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = venv } ->
            if !debug_db then
               eprintf "Omake_db.add_values: %a@." pp_print_node name;
            Lm_db.add fd (Node.absname name) values_tag Omake_magic.obj_magic digest obj
       | { db_file = None } ->
            ()

   (*
    * Fetch the three kinds of entries.
    *)
   let find_ir = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false } ->
         if !debug_db then
            eprintf "Omake_db.find_ir: finding: %a@." pp_print_node name;
         let ir = Lm_db.find fd (Node.absname name) ir_tag Omake_magic.ir_magic digest in
            if !debug_db then
               eprintf "Omake_db.find_ir: found: %a@." pp_print_node name;
            ir
    | { db_file = None }
    | { db_flush_ir = true } ->
         raise Not_found

   let find_object = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false; db_flush_static = false } ->
         if !debug_db then
            eprintf "Omake_db.find_object: finding: %a@." pp_print_node name;
         let obj = Lm_db.find fd (Node.absname name) object_tag Omake_magic.obj_magic digest in
            if !debug_db then
               eprintf "Omake_db.find_object: found: %a@." pp_print_node name;
            obj
    | { db_file = None }
    | { db_flush_ir = true }
    | { db_flush_static = true } ->
         raise Not_found

   let find_values = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false; db_flush_static = false } ->
         if !debug_db then
            eprintf "Omake_db.find_values: finding: %a@." pp_print_node name;
         let obj = Lm_db.find fd (Node.absname name) values_tag Omake_magic.obj_magic digest in
            if !debug_db then
               eprintf "Omake_db.find_values: found: %a@." pp_print_node name;
            obj
    | { db_file = None }
    | { db_flush_ir = true }
    | { db_flush_static = true } ->
         raise Not_found

   let get_ir     = find_ir
   let get_object = find_object
end;;

(*
 * Cached object files.
 *)
let venv_find_ir_file_exn venv node =
   NodeTable.find venv.venv_inner.venv_globals.venv_ir_files node

let venv_add_ir_file venv node obj =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_ir_files <- NodeTable.add globals.venv_ir_files node obj

let venv_find_object_file_exn venv node =
   NodeTable.find venv.venv_inner.venv_globals.venv_object_files node

let venv_add_object_file venv node obj =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_object_files <- NodeTable.add globals.venv_object_files node obj

(************************************************************************
 * Variables.
 *)

(*
 * Default empty object.
 *)
let venv_empty_object = SymbolTable.empty

(*
 * For variables, try to look them up as 0-arity functions first.
 *)
let venv_find_var_private_exn venv v =
   SymbolTable.find venv.venv_static v

let venv_find_var_dynamic_exn venv v =
   SymbolTable.find venv.venv_dynamic v

let venv_find_var_protected_exn venv v =
   try SymbolTable.find venv.venv_this v with
      Not_found ->
         try SymbolTable.find venv.venv_dynamic v with
            Not_found ->
               try SymbolTable.find venv.venv_static v with
                  Not_found ->
                     ValString (SymbolTable.find venv.venv_inner.venv_environ v)

let venv_find_var_global_exn venv v =
   try SymbolTable.find venv.venv_dynamic v with
      Not_found ->
         try SymbolTable.find venv.venv_this v with
            Not_found ->
               try SymbolTable.find venv.venv_static v with
                  Not_found ->
                     ValString (SymbolTable.find venv.venv_inner.venv_environ v)

let venv_find_var_exn venv v =
   match v with
      VarPrivate (_, v) ->
         venv_find_var_private_exn venv v
    | VarThis (_, v) ->
         venv_find_var_protected_exn venv v
    | VarVirtual (_, v) ->
         venv_find_var_dynamic_exn venv v
    | VarGlobal (_, v) ->
         venv_find_var_global_exn venv v

let venv_get_var venv pos v =
   try venv_find_var_exn venv v with
      Not_found ->
         let pos = string_pos "venv_get_var" pos in
            raise (OmakeException (pos, UnboundVarInfo v))

let venv_find_var venv pos loc v =
   try venv_find_var_exn venv v with
      Not_found ->
         let pos = string_pos "venv_find_var" (loc_pos loc pos) in
            raise (OmakeException (loc_pos loc pos, UnboundVarInfo v))

let venv_find_object_or_empty venv v =
   try
      match venv_find_var_exn venv v with
         ValObject obj ->
            obj
       | _ ->
            venv_empty_object
   with
      Not_found ->
         venv_empty_object

let venv_defined venv v =
   let { venv_this = this;
         venv_static = static;
         venv_dynamic = dynamic
       } = venv
   in
      match v with
         VarPrivate (_, v) ->
            SymbolTable.mem static v
       | VarVirtual (_, v) ->
            SymbolTable.mem dynamic v
       | VarThis (_, v)
       | VarGlobal (_, v) ->
            SymbolTable.mem this v || SymbolTable.mem dynamic v || SymbolTable.mem static v

(*
 * Adding to variable environment.
 * Add to the current object and the static scope.
 *)
let venv_add_var venv v s =
   let { venv_this = this;
         venv_static = static;
         venv_dynamic = dynamic
       } = venv
   in
      match v with
         VarPrivate (_, v) ->
            { venv with venv_static  = SymbolTable.add static v s }
       | VarVirtual (_, v) ->
            { venv with venv_dynamic = SymbolTable.add dynamic v s }
       | VarThis (_, v) ->
            { venv with venv_this    = SymbolTable.add this v s;
                        venv_static  = SymbolTable.add static v s
            }
       | VarGlobal (_, v) ->
            { venv with venv_dynamic = SymbolTable.add dynamic v s;
                        venv_static  = SymbolTable.add static v s
            }

(*
 * Add the arguments given an environment.
 *)
let rec venv_add_keyword_args pos venv keywords kargs =
   match keywords, kargs with
      (v1, v_info, opt_arg) :: keywords_tl, (v2, arg) :: kargs_tl ->
         let i = Lm_symbol.compare v1 v2 in
            if i = 0 then
               venv_add_keyword_args pos (venv_add_var venv v_info arg) keywords_tl kargs_tl
            else if i < 0 then
               match opt_arg with
                  Some arg ->
                     venv_add_keyword_args pos (venv_add_var venv v_info arg) keywords_tl kargs
                | None ->
                     raise (OmakeException (pos, StringVarError ("keyword argument is required", v1)))
            else
               raise (OmakeException (pos, StringVarError ("no such keyword", v2)))
    | (v1, _, None) :: _, [] ->
         raise (OmakeException (pos, StringVarError ("keyword argument is required", v1)))
    | (v1, v_info, Some arg) :: keywords_tl, [] ->
         venv_add_keyword_args pos (venv_add_var venv v_info arg) keywords_tl kargs
    | [], [] ->
         venv
    | [], (v2, _) :: _ ->
         raise (OmakeException (pos, StringVarError ("no such keyword", v2)))

let venv_add_args venv pos loc static params args keywords kargs =
   let venv = { venv with venv_static = static } in
   let venv = venv_add_keyword_args pos venv keywords kargs in
   let len1 = List.length params in
   let len2 = List.length args in
   let () =
      if len1 <> len2 then
         raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact len1, len2)))
   in
      List.fold_left2 venv_add_var venv params args

(*
 * Add the arguments to the given static environment.
 *)
let venv_with_args venv pos loc params args keywords kargs =
   venv_add_args venv pos loc venv.venv_static params args keywords kargs

(*
 * Curried-applications.
 *
 * XXX: this needs to be checked, and performance improved too.
 *
 * Here is the idea:
 *
 * - Given a normal arg
 *      + add the value to the env
 *      + if params = [] then call the function
 * - Given a keyword arg
 *      + if the keyword is valid here, add it to the env, subtract from keywords
 *      + if not valid here, add to pending kargs
 *)
let rec collect_merge_kargs pos rev_kargs kargs1 kargs2 =
   match kargs1, kargs2 with
      ((v1, _) as karg1) :: kargs1_tl, ((v2, _) as karg2) :: kargs2_tl ->
         let i = Lm_symbol.compare v1 v2 in
            if i = 0 then
               raise (OmakeException (pos, StringVarError ("duplicate keyword", v1)))
            else if i < 0 then
               collect_merge_kargs pos (karg1 :: rev_kargs) kargs1_tl kargs2
            else
               collect_merge_kargs pos (karg2 :: rev_kargs) kargs1 kargs2_tl
    | [], kargs
    | kargs, [] ->
         List.rev_append rev_kargs kargs

let merge_kargs pos kargs1 kargs2 =
   match kargs1, kargs2 with
      [], kargs
    | kargs, [] ->
         kargs
    | _ ->
         collect_merge_kargs pos [] kargs1 kargs2

let add_partial_args venv args =
   List.fold_left (fun venv (v, arg) ->
         venv_add_var venv v arg) venv args

let rec apply_curry_args pos venv skipped_kargs params args =
   match params, args with
      [], _ ->
         venv, args, List.rev skipped_kargs
    | _, [] ->
         raise (OmakeException (pos, ArityMismatch (ArityExact (List.length params), 0)))
    | v :: params, arg :: args ->
         apply_curry_args pos (venv_add_var venv v arg) skipped_kargs params args

let rec venv_add_curry_args pos venv params args keywords skipped_kargs kargs =
   match keywords, kargs with
      (v1, v_info, opt_arg) :: keywords_tl, ((v2, arg) as karg) :: kargs_tl ->
         let i = Lm_symbol.compare v1 v2 in
            if i = 0 then
               venv_add_curry_args pos (venv_add_var venv v_info arg) params args keywords_tl skipped_kargs kargs_tl
            else if i < 0 then
               match opt_arg with
                  Some arg ->
                     venv_add_curry_args pos (venv_add_var venv v_info arg) params args keywords_tl skipped_kargs kargs
                | None ->
                     raise (OmakeException (pos, StringVarError ("keyword argument is required", v1)));
            else
               venv_add_curry_args pos venv params args keywords (karg :: skipped_kargs) kargs_tl
    | (v1, _, None) :: _, [] ->
         raise (OmakeException (pos, StringVarError ("keyword argument is required", v1)))
    | (_, v_info, Some arg) :: keywords_tl, [] ->
         venv_add_curry_args pos (venv_add_var venv v_info arg) params args keywords_tl skipped_kargs kargs
    | [], karg :: kargs_tl ->
         venv_add_curry_args pos venv params args keywords (karg :: skipped_kargs) kargs_tl
    | [], [] ->
         apply_curry_args pos venv skipped_kargs params args

let venv_add_curry_args venv pos loc static pargs params args keywords kargs1 kargs2 =
   let venv = { venv with venv_static = static } in
   let venv = add_partial_args venv pargs in
      venv_add_curry_args pos venv params args keywords [] (merge_kargs pos kargs1 kargs2)

(*
 * Also provide a form for partial applications.
 *)
let rec add_partial_keywords pos venv = function
   (v, _, None) :: _ ->
      raise (OmakeException (pos, StringVarError ("keyword argument is required", v)))
 | (_, v_info, Some arg) :: keywords_tl ->
      add_partial_keywords pos (venv_add_var venv v_info arg) keywords_tl
 | [] ->
      venv

let rec apply_partial_args venv pos loc static env skipped_keywords keywords skipped_kargs params args =
   match params, args with
      [], _ ->
         let venv = { venv with venv_static = static } in
         let venv = add_partial_args venv env in
         let venv = add_partial_keywords pos venv skipped_keywords in
         let venv = add_partial_keywords pos venv keywords in
            FullApply (venv, args, List.rev skipped_kargs)
    | _, [] ->
         PartialApply (static, env, List.rev_append skipped_keywords keywords, params, List.rev skipped_kargs)
    | v :: params, arg :: args ->
         apply_partial_args venv pos loc static ((v, arg) :: env) skipped_keywords keywords skipped_kargs params args

let rec venv_add_partial_args venv pos loc static env params args skipped_keywords keywords skipped_kargs kargs =
   match keywords, kargs with
      ((v1, v_info, opt_arg) as key) :: keywords_tl, ((v2, arg) as karg) :: kargs_tl ->
         let i = Lm_symbol.compare v1 v2 in
            if i = 0 then
               venv_add_partial_args venv pos loc static ((v_info, arg) :: env) params args skipped_keywords keywords_tl skipped_kargs kargs_tl
            else if i < 0 then
               venv_add_partial_args venv pos loc static env params args (key :: skipped_keywords) keywords_tl skipped_kargs kargs
            else
               venv_add_partial_args venv pos loc static env params args skipped_keywords keywords (karg :: skipped_kargs) kargs_tl
    | key :: keywords_tl, [] ->
         venv_add_partial_args venv pos loc static env params args (key :: skipped_keywords) keywords_tl skipped_kargs kargs
    | [], karg :: kargs_tl ->
         venv_add_partial_args venv pos loc static env params args skipped_keywords keywords (karg :: skipped_kargs) kargs_tl
    | [], [] ->
         apply_partial_args venv pos loc static env skipped_keywords keywords skipped_kargs params args

let venv_add_partial_args venv pos loc static pargs params args keywords kargs1 kargs2 =
   venv_add_partial_args venv pos loc static pargs params args [] keywords [] (merge_kargs pos kargs1 kargs2)

let venv_with_partial_args venv env args =
   let venv = { venv with venv_static = env } in
      add_partial_args venv args

(*
 * The system environment.
 *)
let venv_environment venv =
   venv.venv_inner.venv_environ

let venv_getenv venv v =
   SymbolTable.find venv.venv_inner.venv_environ v

let venv_setenv venv v x =
   { venv with venv_inner = { venv.venv_inner with venv_environ = SymbolTable.add venv.venv_inner.venv_environ v x } }

let venv_unsetenv venv v =
   { venv with venv_inner = { venv.venv_inner with venv_environ = SymbolTable.remove venv.venv_inner.venv_environ v } }

let venv_defined_env venv v =
   SymbolTable.mem venv.venv_inner.venv_environ v

(*
 * Options.
 *)
let venv_options venv =
   venv.venv_inner.venv_options

let venv_with_options venv options =
   { venv with venv_inner = { venv.venv_inner with venv_options = options } }

let venv_set_options_aux venv loc pos argv =
   let argv = Array.of_list argv in
   let add_unknown options s =
      raise (OmakeException (loc_pos loc pos, StringStringError ("unknown option", s)))
   in
   let options_spec =
      Lm_arg.StrictOptions, (**)
         ["Make options", options_spec;
          "Output options", output_spec]
   in
   let options =
      try Lm_arg.fold_argv argv options_spec venv.venv_inner.venv_options add_unknown "Generic system builder" with
          Lm_arg.BogusArg s ->
            raise (OmakeException (loc_pos loc pos, StringError s))
   in
      venv_with_options venv options

let venv_set_options venv loc pos argv =
   venv_set_options_aux venv loc pos ("omake" :: argv)

(************************************************************************
 * Manipulating static objects.
 *)

(*
 * Static values.  Load the values from the file
 * if necessary.  Raises Not_found if the object has not already
 * been loaded.
 *)
let venv_find_static_object venv node v =
   let globals = venv.venv_inner.venv_globals in
   let static = globals.venv_static_values in
   let table =
      try NodeTable.find static node with
         Not_found ->
            (* Load it from the file *)
            let table = Static.read venv node Static.find_values in
               globals.venv_static_values <- NodeTable.add static node table;
               table
   in
      SymbolTable.find table v

(*
 * Define a static var.
 * Save the object on the modified list so it will get
 * written back to the file.
 *)
let venv_add_static_object venv node key obj =
   let globals = venv.venv_inner.venv_globals in
   let { venv_static_values = static;
         venv_modified_values = modified
       } = globals
   in
   let table =
      try NodeTable.find static node with
         Not_found ->
            SymbolTable.empty
   in
   let table = SymbolTable.add table key obj in
      globals.venv_static_values <- NodeTable.add static node table;
      globals.venv_modified_values <- NodeTable.add modified node table

(*
 * Inline the static variables into the current environment.
 *)
let venv_include_static_object venv obj =
   let { venv_dynamic = dynamic } = venv in
   let dynamic = SymbolTable.fold SymbolTable.add dynamic obj in
      { venv with venv_dynamic = dynamic }

(*
 * Save the modified values.
 *)
let venv_save_static_values venv =
   let globals = venv.venv_inner.venv_globals in
      NodeTable.iter (fun node table ->
            try Static.write venv node (fun fd -> Static.add_values fd table)
            with Not_found ->
               ()) globals.venv_modified_values;
      globals.venv_modified_values <- NodeTable.empty

(************************************************************************
 * Methods and objects.
 *)

(*
 * Create a path when fetching fields, so that we
 * can hoist the exports from a method call.
 *)
let raise_field_error mode pos loc v =
   let print_error buf =
      fprintf buf "@[<v 3>Accessing %s field: %a@ The variable was defined at the following location@ %a@]" (**)
         mode
         pp_print_symbol v
         pp_print_location loc
   in
      raise (OmakeException (pos, LazyError print_error))

let rec squash_path_info path info =
   match path with
      PathVar _ ->
         PathVar info
    | PathField (path, _, _) ->
         squash_path_info path info

(*
 * When finding a value, also construct the path to
 * the value.
 *)
let venv_find_field_path_exn venv path obj pos v =
   PathField (path, obj, v), SymbolTable.find obj v

let venv_find_field_path venv path obj pos v =
   try venv_find_field_path_exn venv path obj pos v with
      Not_found ->
         let pos = string_pos "venv_find_field_path" pos in
            raise (OmakeException (pos, UnboundFieldVar (obj, v)))

(*
 * Simple finding.
 *)
let venv_find_field_exn venv obj pos v =
   SymbolTable.find obj v

let venv_find_field venv obj pos v =
   try venv_find_field_exn venv obj pos v with
      Not_found ->
         let pos = string_pos "venv_find_field" pos in
            raise (OmakeException (pos, UnboundFieldVar (obj, v)))

(*
 * Super fields come from the class.
 *)
let venv_find_super_field venv pos loc v1 v2 =
   let table = venv_get_class venv.venv_this in
      try
         let obj = SymbolTable.find table v1 in
            venv_find_field_exn venv obj pos v2
      with
         Not_found ->
            let pos = string_pos "venv_find_super_field" (loc_pos loc pos) in
               raise (OmakeException (pos, StringVarError ("unbound super var", v2)))

(*
 * Add a field.
 *)
let venv_add_field venv obj pos v e =
   venv, SymbolTable.add obj v e

(*
 * Hacked versions bypass translation.
 *)
let venv_add_field_internal = SymbolTable.add
let venv_defined_field_internal = SymbolTable.mem
let venv_find_field_internal_exn = SymbolTable.find
let venv_find_field_internal obj pos v =
   try SymbolTable.find obj v with
      Not_found ->
         let pos = string_pos "venv_find_field_internal" pos in
            raise (OmakeException (pos, UnboundFieldVar (obj, v)))

let venv_object_fold_internal = SymbolTable.fold

let venv_object_length = SymbolTable.cardinal

(*
 * Test whether a field is defined.
 *)
let venv_defined_field_exn venv obj v =
   SymbolTable.mem obj v

let venv_defined_field venv obj v =
   try venv_defined_field_exn venv obj v with
      Not_found ->
         false

(*
 * Add a class to an object.
 *)
let venv_add_class obj v =
   let table = venv_get_class obj in
   let table = SymbolTable.add table v obj in
      SymbolTable.add obj class_sym (ValClass table)

(*
 * Execute a method in an object.
 * If we are currently in the outermost object,
 * push the dynamic scope.
 *)
let venv_with_object venv this =
   { venv with venv_this = this }

(*
 * Define a new object.
 *)
let venv_define_object venv =
   venv_with_object venv SymbolTable.empty

(*
 * Add the class to the current object.
 *)
let venv_instanceof obj s =
   SymbolTable.mem (venv_get_class obj) s

(*
 * Include the fields in the given class.
 * Be careful to merge classnames.
 *)
let venv_include_object_aux obj1 obj2 =
   let table1 = venv_get_class obj1 in
   let table2 = venv_get_class obj2 in
   let table = SymbolTable.fold SymbolTable.add table1 table2 in
   let obj1 = SymbolTable.fold SymbolTable.add obj1 obj2 in
      SymbolTable.add obj1 class_sym (ValClass table)

let venv_include_object venv obj2 =
   let obj = venv_include_object_aux venv.venv_this obj2 in
      { venv with venv_this = obj }

let venv_flatten_object venv obj2 =
   let obj = venv_include_object_aux venv.venv_dynamic obj2 in
      { venv with venv_dynamic = obj }

(*
 * Function scoping.
 *)
let venv_empty_env =
   SymbolTable.empty

let venv_get_env venv =
   venv.venv_static

let venv_with_env venv env =
   { venv with venv_static = env }

(*
 * The current object is always in the venv_this field.
 *)
let venv_this venv =
   venv.venv_this

let venv_current_object venv classnames =
   let obj = venv.venv_this in
      if classnames = [] then
         obj
      else
         let table = venv_get_class obj in
         let table = List.fold_left (fun table v -> SymbolTable.add table v obj) table classnames in
            SymbolTable.add obj class_sym (ValClass table)

(*
 * ZZZ: this will go away in 0.9.9.
 *)
let rec filter_objects venv pos v objl = function
      obj :: rev_objl ->
         let objl =
            try venv_find_field_exn venv obj pos v :: objl with
               Not_found ->
                  objl
         in
            filter_objects venv pos v objl rev_objl
    | [] ->
         objl

let venv_current_objects venv pos v =
   let { venv_this = this;
         venv_dynamic = dynamic;
         venv_static = static
       } = venv
   in
   let v, objl =
      match v with
         VarPrivate (_, v) ->
            v, [static]
       | VarThis (_, v) ->
            v, [static; dynamic; this]
       | VarVirtual (_, v) ->
            v, [dynamic]
       | VarGlobal (_, v) ->
            v, [static; this; dynamic]
   in
      filter_objects venv pos v [] objl

(************************************************************************
 * Environment.
 *)

(*
 * Convert a filename to a node.
 *)
let venv_intern venv phony_flag name =
   let { venv_mount   = mount;
         venv_dir     = dir
       } = venv.venv_inner
   in
   let globals = venv_globals venv in
   let { venv_phonies = phonies;
         venv_mount_info = mount_info
       } = globals
   in
      create_node_or_phony phonies mount_info mount phony_flag dir name

let venv_intern_target venv phony_flag target =
   match target with
      TargetNode node -> node
    | TargetString name -> venv_intern venv phony_flag name

let venv_intern_cd venv phony_flag dir name =
   let mount = venv.venv_inner.venv_mount in
   let globals = venv_globals venv in
   let { venv_phonies = phonies;
         venv_mount_info = mount_info
       } = globals
   in
      create_node_or_phony phonies mount_info mount phony_flag dir name

let venv_intern_rule_target venv multiple name =
   let node =
      match name with
         TargetNode node ->
            node
       | TargetString name ->
            venv_intern venv PhonyOK name
   in
      match multiple with
         RuleScannerSingle
       | RuleScannerMultiple ->
            Node.create_escape NodeScanner node
       | RuleSingle
       | RuleMultiple ->
            node

let venv_intern_dir venv name =
   Dir.chdir venv.venv_inner.venv_dir name

let venv_intern_list venv names =
   List.map (venv_intern venv) names

let node_set_of_list nodes =
   List.fold_left NodeSet.add NodeSet.empty nodes

let node_set_add_names venv phony_flag nodes names =
   List.fold_left (fun nodes name ->
         NodeSet.add nodes (venv_intern venv phony_flag name)) nodes names

let node_set_of_names venv phony_flag names =
   node_set_add_names venv phony_flag NodeSet.empty names

(*
 * Convert back to a string.
 *)
let venv_dirname venv dir =
   if opt_absname venv.venv_inner.venv_options then
      Dir.absname dir
   else
      Dir.name venv.venv_inner.venv_dir dir

let venv_nodename venv dir =
   if opt_absname venv.venv_inner.venv_options then
      Node.absname dir
   else
      Node.name venv.venv_inner.venv_dir dir

(*
 * Add a mount point.
 *)
let venv_mount venv options src dst =
   let inner = venv.venv_inner in
   let mount = Mount.mount inner.venv_mount options src dst in
   let inner = { inner with venv_mount = mount } in
      { venv with venv_inner = inner }

(*
 * A target is wild if it is a string with a wild char.
 *)
let target_is_wild target =
   match target with
      TargetNode _ ->
         false
    | TargetString s ->
         is_wild s

let string_of_target venv target =
   match target with
      TargetString s ->
         s
    | TargetNode node ->
         venv_nodename venv node

(*
 * Compile a wild pattern.
 * It is an error if it isn't wild.
 *)
let compile_wild_pattern venv pos loc target =
   match target with
      TargetString s when is_wild s ->
         if Lm_string_util.contains_any s Lm_filename_util.separators then
            raise (OmakeException (loc_pos loc pos, StringStringError ("filename pattern is a path", s)));
         wild_compile_in s
    | _ ->
         raise (OmakeException (loc_pos loc pos, StringTargetError ("patterns must be wildcards", target)))

(*
 * Compile a source.
 *)
let compile_source_core venv s =
   match s with
      TargetNode node ->
         SourceNode node
    | TargetString s ->
         if is_wild s then
            SourceWild (wild_compile_out s)
         else
            SourceNode (venv_intern venv PhonyOK s)

let compile_source venv (kind, s) =
   kind, compile_source_core venv s

let compile_implicit3_target pos loc = function
   TargetString s ->
      if Lm_string_util.contains_any s Lm_filename_util.separators then
         raise (OmakeException (loc_pos loc pos, StringStringError ("target of a 3-place rule is a path", s)));
      s
 | target ->
      raise (OmakeException (loc_pos loc pos, StringTargetError ("target of a 3-place rule is not a simple string", target)))

(*
 * Perform a wild substitution on a source.
 *)
let subst_source_core venv dir subst source =
   match source with
      SourceWild wild ->
         let s = wild_subst subst wild in
            venv_intern_cd venv PhonyOK dir s
    | SourceNode node ->
         node

let subst_source venv dir subst (kind, source) =
   Node.create_escape kind (subst_source_core venv dir subst source)

(*
 * No wildcard matching.
 *)
let intern_source venv (kind, source) =
   let source =
      match source with
         TargetNode node ->
            node
       | TargetString name ->
            venv_intern venv PhonyOK name
   in
      Node.create_escape kind source

(************************************************************************
 * Rules
 *)

(*
 * Symbols for directories.
 *)
let wild_sym            = Lm_symbol.add wild_string
let explicit_target_sym = Lm_symbol.add "<EXPLICIT_TARGET>"

(*
 * Don't save explicit rules.
 *)
let venv_explicit_target venv target =
   venv_add_var venv explicit_target_var (ValNode target)

(*
 * Save explicit rules.
 *)
let venv_save_explicit_rules venv loc erules =
   (* Filter out the rules with a different target *)
   let erules =
      try
         match venv_find_var_dynamic_exn venv explicit_target_sym with
            ValNode target ->
               let rules =
                  List.fold_left (fun rules erule ->
                        if Node.equal erule.rule_target target then
                           erule :: rules
                        else
                           rules) [] erules
               in
               let rules = List.rev rules in
               let () =
                  if rules = [] then
                     let print_error buf =
                        fprintf buf "@[<b 3>Computed rule for `%a' produced no useful rules:" pp_print_node target;
                        List.iter (fun erule ->
                              fprintf buf "@ `%a'" pp_print_node erule.rule_target) erules;
                        fprintf buf "@]"
                     in
                        raise (OmakeException (loc_exp_pos loc, LazyError print_error))
               in
                  rules
          | _ ->
               erules
      with
         Not_found ->
            erules
   in
      venv_save_explicit_rules venv loc erules

(*
 * Add the wild target.
 *)
let venv_add_wild_match venv v =
   venv_add_var venv wild_var v

let command_add_wild venv wild command =
   match command with
      CommandSection _ ->
         command
    | CommandValue(loc, env, s) ->
         let env = venv_get_env (venv_add_wild_match (venv_with_env venv env) wild) in
            CommandValue(loc, env, s)

(*
 * This is the standard way to add results of a pattern match.
 *)
let venv_add_match_values venv args =
   let venv, _ =
      List.fold_left (fun (venv, i) arg ->
            let v = create_numeric_var i in
            let venv = venv_add_var venv v arg in
               venv, succ i) (venv, 1) args
   in
      venv

let venv_add_match_args venv args =
   let venv, _ =
      List.fold_left (fun (venv, i) arg ->
            let v = create_numeric_var i in
            let venv = venv_add_var venv v (ValData arg) in
               venv, succ i) (venv, 1) args
   in
      venv

let venv_add_match venv line args =
   let args = List.map (fun s -> ValData s) args in
   let venv, _ =
      List.fold_left (fun (venv, i) arg ->
            let v = create_numeric_var i in
            let venv = venv_add_var venv v arg in
               venv, succ i) (venv, 1) args
   in
   let venv = venv_add_var venv zero_var (ValData line) in
   let venv = venv_add_var venv star_var (ValArray args) in
   let venv = venv_add_var venv nf_var   (ValInt (List.length args)) in
      venv

(*
 * Create an environment.
 *)
let create_environ () =
   let env = Unix.environment () in
   let len = Array.length env in
   let rec collect table i =
      if i = len then
         table
      else
         let s = env.(i) in
         let j = String.index s '=' in
         let name = String.sub s 0 j in
         let name =
            if Sys.os_type = "Win32" then
               String.uppercase name
            else
               name
         in
         let v = Lm_symbol.add name in
         let x = String.sub s (j + 1) (String.length s - j - 1) in
         let table = SymbolTable.add table v x in
            collect table (succ i)
   in
      collect SymbolTable.empty 0

let create options dir exec cache =
   let cwd = Dir.cwd () in
   let env = create_environ () in
   let mount_info =
      { mount_file_exists = Omake_cache.exists cache;
        mount_file_reset  = (fun node -> ignore (Omake_cache.force_stat cache node));
        mount_is_dir      = Omake_cache.is_dir cache;
        mount_digest      = Omake_cache.stat cache;
        mount_stat        = Omake_cache.stat_unix cache
      }
   in
   let globals =
      { venv_global_index               = 0;
        venv_exec                       = exec;
        venv_cache                      = cache;
        venv_mount_info                 = mount_info;
        venv_environments               = HandleTable.create ();
        venv_files                      = NodeSet.empty;
        venv_directories                = DirTable.empty;
        venv_excluded_directories       = DirSet.empty;
        venv_phonies                    = PreNodeSet.empty;
        venv_explicit_rules             = [];
        venv_explicit_new               = [];
        venv_explicit_targets           = NodeTable.empty;
        venv_ordering_rules             = [];
        venv_orders                     = StringSet.empty;
        venv_memo_rules                 = ValueTable.empty;
        venv_pervasives_obj             = SymbolTable.empty;
        venv_pervasives_vars            = SymbolTable.empty;
        venv_ir_files                   = NodeTable.empty;
        venv_object_files               = NodeTable.empty;
        venv_static_values              = NodeTable.empty;
        venv_modified_values            = NodeTable.empty;
        venv_target_is_buildable        = NodeTable.empty;
        venv_target_is_buildable_proper = NodeTable.empty
      }
   in
   let inner =
      { venv_dir            = cwd;
        venv_environ        = env;
        venv_phony          = NodeSet.empty;
        venv_implicit_deps  = [];
        venv_implicit_rules = [];
        venv_globals        = globals;
        venv_options        = options;
        venv_mount          = Mount.empty;
        venv_included_files = NodeSet.empty
      }
   in
   let venv =
      { venv_this           = SymbolTable.empty;
        venv_dynamic        = SymbolTable.empty;
        venv_static         = SymbolTable.empty;
        venv_inner          = inner
      }
   in
   let venv = venv_add_phony venv (Lm_location.bogus_loc makeroot_name) [TargetString ".PHONY"] in
   let venv = venv_add_var venv cwd_var (ValDir cwd) in
   let venv = venv_add_var venv stdlib_var (ValDir Dir.lib) in
   let venv = venv_add_var venv stdroot_var (ValNode (venv_intern_cd venv PhonyProhibited Dir.lib "OMakeroot")) in
   let venv = venv_add_var venv ostype_var (ValString Sys.os_type) in
   let venv = venv_add_wild_match venv (ValData wild_string) in
   let omakepath =
      try
         let path = Lm_string_util.split pathsep (SymbolTable.find env omakepath_sym) in
            List.map (fun s -> ValString s) path
      with
         Not_found ->
            [ValString "."; ValDir Dir.lib]
   in
   let omakepath = ValArray omakepath in
   let venv = venv_add_var venv omakepath_var omakepath in
   let path =
      try
         let path = Lm_string_util.split pathsep (SymbolTable.find env path_sym) in
            ValArray (List.map (fun s -> ValData s) path)
      with
         Not_found ->
            eprintf "*** omake: PATH environment variable is not set!@.";
            ValArray []
   in
   let venv = venv_add_var venv path_var path in
      venv

(*
 * Create a fresh environment from the pervasives.
 * This is used for compiling objects.
 *)
let venv_set_pervasives venv =
   let globals = venv.venv_inner.venv_globals in
   let obj = venv.venv_dynamic in
   let loc = bogus_loc "Pervasives" in
   let vars =
      SymbolTable.fold (fun vars v _ ->
            SymbolTable.add vars v (VarVirtual (loc, v))) SymbolTable.empty obj
   in
      globals.venv_pervasives_obj <- venv.venv_dynamic;
      globals.venv_pervasives_vars <- vars

let venv_get_pervasives venv node =
   let { venv_inner = inner } = venv in
   let { venv_environ = env;
         venv_options = options;
         venv_globals = globals
       } = inner
   in
   let { venv_exec            = exec;
         venv_cache           = cache;
         venv_pervasives_obj  = obj;
         venv_pervasives_vars = vars
       } = globals
   in
   let inner =
      { venv_dir            = Node.dir node;
        venv_environ        = env;
        venv_phony          = NodeSet.empty;
        venv_implicit_deps  = [];
        venv_implicit_rules = [];
        venv_globals        = globals;
        venv_options        = options;
        venv_mount          = Mount.empty;
        venv_included_files = NodeSet.empty
      }
   in
      { venv_this           = SymbolTable.empty;
        venv_dynamic        = obj;
        venv_static         = SymbolTable.empty;
        venv_inner          = inner
      }

(*
 * Fork the environment, so that changes really have no effect on the old one.
 * This is primarly used when a thread wants a private copy of the environment.
 *)
let venv_fork venv =
   let inner = venv.venv_inner in
   let globals = inner.venv_globals in
   let globals = { globals with venv_global_index = succ globals.venv_global_index; venv_exec = globals.venv_exec } in
   let inner = { inner with venv_globals = globals } in
      { venv with venv_inner = inner }

let copy_var src_dynamic dst_dynamic v =
   try
      SymbolTable.add dst_dynamic v (SymbolTable.find src_dynamic v)
   with
      Not_found ->
         SymbolTable.remove dst_dynamic v

let copy_vars dst_dynamic src_dynamic vars =
   List.fold_left (copy_var src_dynamic) dst_dynamic vars

let copy_var_list =
   [stdin_sym; stdout_sym; stderr_sym]

let venv_unfork venv_dst venv_src =
   let { venv_dynamic = dst_dynamic;
         venv_inner = dst_inner
       } = venv_dst
   in
   let { venv_dynamic = src_dynamic;
         venv_inner = src_inner
       } = venv_src
   in
   let inner = { dst_inner with venv_globals = src_inner.venv_globals } in
   let dst_dynamic = copy_vars dst_dynamic src_dynamic copy_var_list in
      { venv_dst with venv_dynamic = dst_dynamic;
                      venv_inner = inner
      }

(*
 * Get the scope of all variables.
 *)
let venv_include_scope venv mode =
   match mode with
      IncludePervasives ->
         venv.venv_inner.venv_globals.venv_pervasives_vars
    | IncludeAll ->
         let loc = bogus_loc "venv_include_scope" in
         let { venv_this = this;
               venv_dynamic = dynamic
             } = venv
         in
         let vars = SymbolTable.mapi (fun v _ -> VarThis (loc, v)) this in
         let vars = SymbolTable.fold (fun vars v _ -> SymbolTable.add vars v (VarGlobal (loc, v))) vars dynamic in
            vars

(*
 * Add an included file.
 *)
let venv_is_included_file venv node =
   NodeSet.mem venv.venv_inner.venv_included_files node

let venv_add_included_file venv node =
   let inner = venv.venv_inner in
   let inner = { inner with venv_included_files = NodeSet.add inner.venv_included_files node } in
      { venv with venv_inner = inner }

(*
 * Global state.
 *)
let venv_exec venv =
   venv.venv_inner.venv_globals.venv_exec

let venv_cache venv =
   venv.venv_inner.venv_globals.venv_cache

let venv_add_cache venv cache =
   let inner = venv.venv_inner in
   let globals = inner.venv_globals in
   let globals = { globals with venv_cache = cache } in
   let inner = { inner with venv_globals = globals } in
      { venv with venv_inner = inner }

(*
 * Change directories.  Update the CWD var, and all a default
 * rule for all the phonies.
 *)
let venv_chdir_tmp venv dir =
   { venv with venv_inner = { venv.venv_inner with venv_dir = dir } }

let venv_chdir_dir venv loc dir =
   let inner = venv.venv_inner in
   let { venv_dir = cwd;
         venv_phony = phony
       } = inner
   in
      if Dir.equal dir cwd then
         venv
      else
         let venv = venv_add_var venv cwd_var (ValDir dir) in
         let venv = venv_chdir_tmp venv dir in
         let globals = venv_globals venv in
         let phonies = globals.venv_phonies in
         let phony, phonies =
            NodeSet.fold (fun (phony, phonies) node ->
                  let node' = Node.create_phony_chdir node dir in
                  let phony = NodeSet.add phony node' in
                  let phonies = PreNodeSet.add phonies (Node.dest node') in
                     venv_add_explicit_dep venv loc node node';
                     phony, phonies) (NodeSet.empty, phonies) phony
         in
         let inner =
            { inner with venv_dir = dir;
                         venv_phony = phony
            }
         in
         let venv = { venv with venv_inner = inner } in
            globals.venv_phonies <- phonies;
            venv

let venv_chdir venv loc dir =
   let dir = Dir.chdir venv.venv_inner.venv_dir dir in
      venv_chdir_dir venv loc dir

(*
 * The public version does not mess whith the phonies.
 *)
let venv_chdir_tmp venv dir =
   let cwd = venv.venv_inner.venv_dir in
      if Dir.equal dir cwd then
         venv
      else
         let venv = venv_add_var venv cwd_var (ValDir dir) in
            venv_chdir_tmp venv dir

(*
 * Get the dir.
 *)
let venv_dir venv =
   venv.venv_inner.venv_dir

(*
 * When an OMakefile in a dir is read, save the venv
 * to be used for targets that do not have nay explicit target rule.
 *)
let venv_add_dir venv =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_directories <- DirTable.add globals.venv_directories venv.venv_inner.venv_dir venv

let venv_directories venv =
   let globals = venv.venv_inner.venv_globals in
      DirSet.fold DirTable.remove globals.venv_directories globals.venv_excluded_directories

let venv_add_explicit_dir venv dir =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_directories <- DirTable.add globals.venv_directories dir venv;
      globals.venv_excluded_directories <- DirSet.remove globals.venv_excluded_directories dir

let venv_remove_explicit_dir venv dir =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_excluded_directories <- DirSet.add globals.venv_excluded_directories dir

let venv_find_target_dir_opt venv target =
   let target_dir = Node.dir target in
      if Dir.equal venv.venv_inner.venv_dir target_dir then
         Some venv
      else
         try Some (DirTable.find venv.venv_inner.venv_globals.venv_directories target_dir) with
            Not_found ->
               None

(*
 * When a file is read, remember it as a configuration file.
 *)
let venv_add_file venv node =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_files <- NodeSet.add globals.venv_files node;
      venv

(*
 * Get all the configuration files.
 *)
let venv_files venv =
   venv.venv_inner.venv_globals.venv_files

(*
 * Add a null rule.
 *)
let venv_add_implicit_deps venv pos loc multiple patterns locks sources scanners values =
   let pos = string_pos "venv_add_implicit_deps" pos in
   let patterns = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
   let nrule =
      { inrule_loc        = loc;
        inrule_multiple   = multiple;
        inrule_patterns   = patterns;
        inrule_locks      = locks;
        inrule_sources    = sources;
        inrule_scanners   = scanners;
        inrule_values     = values
      }
   in
   let venv = { venv with venv_inner = { venv.venv_inner with venv_implicit_deps = nrule :: venv.venv_inner.venv_implicit_deps } } in
      venv_flush_target_cache venv;
      venv, []

(*
 * Add an implicit rule.
 *)
let venv_add_implicit_rule venv loc multiple targets patterns locks sources scanners values body =
   let irule =
      { irule_loc        = loc;
        irule_multiple   = multiple;
        irule_targets    = targets;
        irule_patterns   = patterns;
        irule_locks      = locks;
        irule_sources    = sources;
        irule_scanners   = scanners;
        irule_values     = values;
        irule_body       = body
      }
   in
   let venv = { venv with venv_inner = { venv.venv_inner with venv_implicit_rules = irule :: venv.venv_inner.venv_implicit_rules } } in
      venv_flush_target_cache venv;
      venv, []

(*
 * Add an 2-place implicit rule.
 *)
let venv_add_implicit2_rule venv pos loc multiple patterns locks sources scanners values body =
   let pos = string_pos "venv_add_implicit2_rule" pos in
   let patterns = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
      if debug debug_implicit then
         eprintf "@[<hv 3>venv_add_implicit2_rule:@ @[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
            pp_print_wild_list patterns
            pp_print_source_list sources;
      venv_add_implicit_rule venv loc multiple None patterns locks sources scanners values body

(*
 * Add an explicit rule.
 *)
let venv_add_explicit_rules venv pos loc multiple targets locks sources scanners values body =
   let _pos = string_pos "venv_add_explicit_rules" pos in
   let target_args = List.map (venv_intern_rule_target venv multiple) targets in
   let lock_args = List.map (intern_source venv) locks in
   let source_args = List.map (intern_source venv) sources in
   let scanner_args = List.map (intern_source venv) scanners in
   let effects = node_set_of_list target_args in
   let locks = node_set_of_list lock_args in
   let sources = node_set_of_list source_args in
   let scanners = node_set_of_list scanner_args in
   let commands = make_command_info venv source_args values body in
   let add_target target =
      { rule_loc        = loc;
        rule_env        = venv;
        rule_target     = target;
        rule_effects    = effects;
        rule_locks      = locks;
        rule_sources    = sources;
        rule_scanners   = scanners;
        rule_match      = None;
        rule_multiple   = multiple;
        rule_commands   = commands
      }
   in
   let rules = List.map add_target target_args in
   let names = List.map (fun erule -> erule.rule_target) rules in
      venv_save_explicit_rules venv loc rules;
      venv, names

(*
 * Add a 3-place rule (automatically implicit).
 *)
let venv_add_implicit3_rule venv pos loc multiple targets locks patterns sources scanners values body =
   let pos = string_pos "venv_add_implicit3_rule" pos in
   let patterns = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
   let targets = List.map (compile_implicit3_target pos loc) targets in
   let rec check_target target = function
      pattern :: patterns ->
         begin match wild_match pattern target with
            Some _ -> ()
          | None -> check_target target patterns
         end
    | [] ->
         raise (OmakeException (loc_pos loc pos, StringStringError ("bad match", target)))
   in
   let () = List.iter (fun target -> check_target target patterns) targets in
      if debug debug_implicit then
         eprintf "@[<hv 3>venv_add_implicit3_rule:@ @[<b 3>targets =%a@] @[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
            pp_print_string_list targets
            pp_print_wild_list patterns
            pp_print_source_list sources;
      venv_add_implicit_rule venv loc multiple (Some (StringSet.of_list targets)) patterns locks sources scanners values body

let rec is_implicit loc = function
   [] -> false
 | [target] -> target_is_wild target
 | target :: targets ->
      let imp1 = target_is_wild target in
      let imp2 = is_implicit loc targets in
         if imp1 <> imp2 then
            raise (OmakeException (loc_exp_pos loc, (**)
               StringError "Rule contains an illegal mixture of implicit (pattern) targets and explicit ones"))
         else
            imp1

(*
 * Figure out what to do based on all the parts.
 * A 2-place rule is implicit if the targets do not contain a %. 3-place rules are always implicit.
 *)
let venv_add_rule venv pos loc multiple targets patterns locks sources scanners values commands =
   let pos = string_pos "venv_add_rule" pos in
      try match targets, patterns, commands with
         [], [], _ ->
            raise (OmakeException (loc_exp_pos loc, StringError "invalid null rule"))
       | _, [], [] ->
            if is_implicit loc targets then
               venv_add_implicit_deps venv pos loc multiple targets locks sources scanners values
            else
               venv_add_explicit_rules venv pos loc multiple targets locks sources scanners values commands
       | _, [], _ ->
            if is_implicit loc targets then
               venv_add_implicit2_rule venv pos loc multiple targets locks sources scanners values commands
            else
               venv_add_explicit_rules venv pos loc multiple targets locks sources scanners values commands
       | _ ->
            if not (is_implicit loc patterns) then
               raise (OmakeException (loc_exp_pos loc, StringError "3-place rule does not contain patterns"))
            else
               venv_add_implicit3_rule venv pos loc multiple targets locks patterns sources scanners values commands
      with
         Failure err ->
            raise (OmakeException (loc_exp_pos loc, StringError err))

(*
 * Flush the explicit list.
 *)
let venv_explicit_flush venv =
   let globals = venv.venv_inner.venv_globals in
   let { venv_explicit_rules           = erules;
         venv_explicit_targets         = targets;
         venv_explicit_new             = enew
       } = globals
   in
      if enew <> [] then
         let targets, erules =
            List.fold_left (fun (targets, erules) erule ->
                  let erules = erule :: erules in
                  let targets = NodeTable.add targets erule.rule_target erule in
                     targets, erules) (targets, erules) (List.rev enew)
         in
            globals.venv_explicit_new <- [];
            globals.venv_explicit_rules <- erules;
            globals.venv_explicit_targets <- targets

(*
 * Check if an explicit rule exists.
 *)
let venv_explicit_find venv pos target =
   venv_explicit_flush venv;
   try NodeTable.find venv.venv_inner.venv_globals.venv_explicit_targets target with
      Not_found ->
         raise (OmakeException (pos, StringNodeError ("explicit target not found", target)))

let venv_explicit_exists venv target =
   venv_explicit_flush venv;
   NodeTable.mem venv.venv_inner.venv_globals.venv_explicit_targets target

let multiple_add_error errors target loc1 loc2 =
   let table = !errors in
   let table =
      if NodeMTable.mem table target then
         table
      else
         NodeMTable.add table target loc1
   in
      errors := NodeMTable.add table target loc2

let multiple_print_error errors buf =
   fprintf buf "@[<v 3>Multiple ways to build the following targets";
   NodeMTable.iter_all (fun target locs ->
      let locs = List.sort Lm_location.compare locs in
         fprintf buf "@ @[<v 3>%a:" pp_print_node target;
         List.iter (fun loc -> fprintf buf "@ %a" pp_print_location loc) locs;
         fprintf buf "@]") errors;
   fprintf buf "@]"

let raise_multiple_error errors =
   let _, loc = NodeMTable.choose errors in
      raise (OmakeException (loc_exp_pos loc, LazyError (multiple_print_error errors)))

(*
 * Get the explicit rules.  Build a table indexed by target.
 *)
let venv_explicit_rules venv =
   let errors = ref NodeMTable.empty in
   let add_target table target erule =
      NodeTable.filter_add table target (fun entry ->
            match entry with
               Some erule' ->
                  (*
                   * For .PHONY targets, multiple is ignored.
                   * Otherwise, multiple must be the same for both targets.
                   *)
                  let multiple = is_multiple_rule erule.rule_multiple in
                  let multiple' = is_multiple_rule erule'.rule_multiple in
                     if Node.is_phony target
                        || (multiple && multiple')
                        || ((not multiple && not multiple')
                            && (commands_are_trivial erule.rule_commands || commands_are_trivial erule'.rule_commands))
                     then
                        { erule with rule_commands = erule'.rule_commands @ erule.rule_commands }
                     else begin
                        multiple_add_error errors target erule'.rule_loc erule.rule_loc;
                        erule'
                     end
             | None ->
                  erule)
   in
      if not (NodeMTable.is_empty !errors) then
         raise_multiple_error !errors
      else
         let add_deps table target locks sources scanners =
            NodeTable.filter_add table target (function
               Some (lock_deps, source_deps, scanner_deps) ->
                  NodeSet.union lock_deps locks, NodeSet.union source_deps sources, NodeSet.union scanner_deps scanners
             | None ->
                  locks, sources, scanners)
         in
         let info =
            { explicit_targets      = NodeTable.empty;
              explicit_deps         = NodeTable.empty;
              explicit_rules        = NodeMTable.empty;
              explicit_directories  = venv_directories venv
            }
         in
            venv_explicit_flush venv;
            List.fold_left (fun info erule ->
                  let { rule_target   = target;
                        rule_locks    = locks;
                        rule_sources  = sources;
                        rule_scanners = scanners;
                      } = erule
                  in
                  let target_table   = add_target info.explicit_targets target erule in
                  let dep_table      = add_deps info.explicit_deps target locks sources scanners in
                     { info with explicit_targets  = target_table;
                                 explicit_deps     = dep_table;
                                 explicit_rules    = NodeMTable.add info.explicit_rules target erule
                     }) info (List.rev venv.venv_inner.venv_globals.venv_explicit_rules)

(*
 * Find all the explicit dependencies listed through null
 * rules.
 *)
let venv_find_implicit_deps_inner venv target =
   let target_dir  = Node.dir target in
   let target_name = Node.tail target in
   let is_scanner =
      match Node.kind target with
         NodeScanner -> RuleScanner
       | _ -> RuleNormal
   in
      List.fold_left (fun (lock_deps, source_deps, scanner_deps, value_deps) nrule ->
            let { inrule_multiple = multiple;
                  inrule_patterns = patterns;
                  inrule_locks    = locks;
                  inrule_sources  = sources;
                  inrule_scanners = scanners;
                  inrule_values   = values
                } = nrule
            in
               if rule_kind multiple = is_scanner then
                  let rec search patterns =
                     match patterns with
                        pattern :: patterns ->
                           (match wild_match pattern target_name with
                               Some subst ->
                                  let lock_deps =
                                     List.fold_left (fun lock_deps source ->
                                           let source = subst_source venv target_dir subst source in
                                              NodeSet.add lock_deps source) lock_deps locks
                                  in
                                  let source_deps =
                                     List.fold_left (fun names source ->
                                           let source = subst_source venv target_dir subst source in
                                              NodeSet.add names source) source_deps sources
                                  in
                                  let scanner_deps =
                                     List.fold_left (fun scanner_deps source ->
                                           let source = subst_source venv target_dir subst source in
                                              NodeSet.add scanner_deps source) scanner_deps scanners
                                  in
                                  let value_deps = values @ value_deps in
                                     lock_deps, source_deps, scanner_deps, value_deps
                             | None ->
                                  search patterns)
                      | [] ->
                           lock_deps, source_deps, scanner_deps, value_deps
                  in
                     search patterns
               else
                  lock_deps, source_deps, scanner_deps, value_deps) (**)
         (NodeSet.empty, NodeSet.empty, NodeSet.empty, []) venv.venv_inner.venv_implicit_deps

let venv_find_implicit_deps venv target =
   match venv_find_target_dir_opt venv target with
      Some venv ->
         venv_find_implicit_deps_inner venv target
    | None ->
         NodeSet.empty, NodeSet.empty, NodeSet.empty, []

(*
 * Find the commands from implicit rules.
 *)
let venv_find_implicit_rules_inner venv target =
   let target_dir  = Node.dir target in
   let target_name = Node.tail target in
   let is_scanner =
      match Node.kind target with
         NodeScanner -> RuleScanner
       | _ -> RuleNormal
   in
   let _ =
      if debug debug_implicit then
         eprintf "Finding implicit rules for %s@." target_name
   in
   let rec patt_search = function
         pattern :: patterns ->
            begin match wild_match pattern target_name with
               None -> patt_search patterns
             | (Some _) as subst -> subst
            end
       | [] ->
            None
   in
   let rec collect matched = function
      irule :: irules ->
         let multiple = irule.irule_multiple in
            if rule_kind multiple = is_scanner then
               let subst =
                  if debug debug_implicit then begin
                     eprintf "@[<hv 3>venv_find_implicit_rules: considering implicit rule for@ target = %s:@ " target_name;
                     begin match irule.irule_targets with
                        Some targets ->
                           eprintf "@[<b 3>3-place targets =%a@]@ " pp_print_string_list (StringSet.elements targets)
                      | None ->
                           ()
                     end;
                     eprintf "@[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
                        pp_print_wild_list irule.irule_patterns
                        pp_print_source_list irule.irule_sources
                  end;
                  let matches =
                     match irule.irule_targets with
                        None -> true
                      | Some targets -> StringSet.mem targets target_name
                  in
                     if matches then
                        patt_search irule.irule_patterns
                     else
                        None
               in
               let matched =
                  match subst with
                     Some subst ->
                        let source_args = List.map (subst_source venv target_dir subst) irule.irule_sources in
                        let sources = node_set_of_list source_args in
                        let lock_args = List.map (subst_source venv target_dir subst) irule.irule_locks in
                        let locks = node_set_of_list lock_args in
                        let scanner_args = List.map (subst_source venv target_dir subst) irule.irule_scanners in
                        let scanners = node_set_of_list scanner_args in
                        let core = wild_core subst in
                        let core_val = ValData core in
                        let venv = venv_add_wild_match venv core_val in
                        let commands = List.map (command_add_wild venv core_val) irule.irule_body in
                        let commands = make_command_info venv source_args irule.irule_values commands in
                        let effects =
                           List.fold_left (fun effects pattern ->
                                 let effect = wild_subst_in subst pattern in
                                 let effect = venv_intern_rule_target venv multiple (TargetString effect) in
                                    NodeSet.add effects effect) NodeSet.empty irule.irule_patterns
                        in
                        let erule =
                           { rule_loc         = irule.irule_loc;
                             rule_env         = venv;
                             rule_target      = target;
                             rule_match       = Some core;
                             rule_effects     = effects;
                             rule_locks       = locks;
                             rule_sources     = sources;
                             rule_scanners    = scanners;
                             rule_multiple    = multiple;
                             rule_commands    = commands
                           }
                        in
                           if debug debug_implicit then
                              eprintf "@[<hv 3>Added implicit rule for %s:%a@]@." (**)
                                 target_name pp_print_command_info_list commands;
                           erule :: matched
                   | None ->
                        matched
               in
                  collect matched irules
            else
               collect matched irules
    | [] ->
         List.rev matched
   in
      collect [] venv.venv_inner.venv_implicit_rules

let venv_find_implicit_rules venv target =
   match venv_find_target_dir_opt venv target with
      Some venv ->
         venv_find_implicit_rules_inner venv target
    | None ->
         []

(************************************************************************
 * Ordering rules.
 *)

(*
 * Add an order.
 *)
let venv_add_orders venv loc targets =
   let globals = venv.venv_inner.venv_globals in
   let orders =
      List.fold_left (fun orders target ->
            let name =
               match target with
                  TargetNode _ ->
                     raise (OmakeException (loc_exp_pos loc, StringTargetError (".ORDER should be a name", target)))
                | TargetString s ->
                     s
            in
               StringSet.add orders name) globals.venv_orders targets
   in
      globals.venv_orders <- orders;
      venv

(*
 * Check for order.
 *)
let venv_is_order venv name =
   StringSet.mem venv.venv_inner.venv_globals.venv_orders name

(*
 * Add an ordering rule.
 *)
let venv_add_ordering_rule venv pos loc name pattern sources =
   let pos = string_pos "venv_add_ordering_deps" pos in
   let pattern = compile_wild_pattern venv pos loc pattern in
   let sources = List.map (compile_source_core venv) sources in
   let orule =
      { orule_loc = loc;
        orule_name = name;
        orule_pattern = pattern;
        orule_sources = sources
      }
   in
   let globals = venv.venv_inner.venv_globals in
      globals.venv_ordering_rules <- orule :: globals.venv_ordering_rules;
      venv

(*
 * Get the ordering dependencies for a name.
 *)
let venv_get_ordering_info venv name =
   List.fold_left (fun orules orule ->
         if Lm_symbol.eq orule.orule_name name then
            orule :: orules
         else
            orules) [] venv.venv_inner.venv_globals.venv_ordering_rules

(*
 * Get extra dependencies.
 *)
let venv_get_ordering_deps venv orules deps =
   let step deps =
      NodeSet.fold (fun deps dep ->
            let target_dir = Node.dir dep in
            let target_str = Node.tail dep in
               List.fold_left (fun deps orule ->
                     let { orule_pattern = pattern;
                           orule_sources = sources
                         } = orule
                     in
                        match wild_match pattern target_str with
                           Some subst ->
                              List.fold_left (fun deps source ->
                                    let source = subst_source_core venv target_dir subst source in
                                       NodeSet.add deps source) deps sources
                         | None ->
                              deps) deps orules) deps deps
   in
   let rec fixpoint deps =
      let deps' = step deps in
         if NodeSet.cardinal deps' = NodeSet.cardinal deps then
            deps
         else
            fixpoint deps'
   in
      fixpoint deps

(************************************************************************
 * Static rules.
 *)

(*
 * Each of the commands evaluates to an object.
 *)
let venv_add_memo_rule venv pos loc multiple is_static key vars sources values body =
   let source_args = List.map (intern_source venv) sources in
   let sources = node_set_of_list source_args in
   let srule =
      { srule_loc  = loc;
        srule_static = is_static;
        srule_env  = venv;
        srule_key  = key;
        srule_deps = sources;
        srule_vals = values;
        srule_exp  = body
      }
   in
   let globals = venv_globals venv in
   let venv =
      List.fold_left (fun venv info ->
            let _, v = var_of_var_info info in
               venv_add_var venv info (ValDelayed (ref (ValStaticApply (key, v))))) venv vars
   in
      globals.venv_memo_rules <- ValueTable.add globals.venv_memo_rules key (StaticRule srule);
      venv

(*
 * Force the evaluation.
 *)
let venv_set_static_info venv key v =
   let globals = venv_globals venv in
      globals.venv_memo_rules <- ValueTable.add globals.venv_memo_rules key v

let venv_find_static_info venv pos key =
   try ValueTable.find venv.venv_inner.venv_globals.venv_memo_rules key with
      Not_found ->
         raise (OmakeException (pos, StringValueError ("Static section not defined", key)))

(************************************************************************
 * Return values.
 *)

(*
 * Export an item from one environment to another.
 *)
let copy_var pos dst src v =
   try SymbolTable.add dst v (SymbolTable.find src v) with
      Not_found ->
         raise (OmakeException (pos, UnboundVar v))

let export_item pos venv_dst venv_src = function
   ExportVar (VarPrivate (_, v)) ->
      { venv_dst with venv_static = copy_var pos venv_dst.venv_static venv_src.venv_static v }
 | ExportVar (VarThis (_, v)) ->
      { venv_dst with venv_this = copy_var pos venv_dst.venv_this venv_src.venv_this v }
 | ExportVar (VarVirtual (_, v)) ->
      { venv_dst with venv_dynamic = copy_var pos venv_dst.venv_dynamic venv_src.venv_dynamic v }
 | ExportVar (VarGlobal (_, v)) ->
      (*
       * For now, we don't know which scope to use, so we
       * copy them all.
       *)
      let { venv_dynamic = dynamic_src;
            venv_static  = static_src;
            venv_this    = this_src
          } = venv_src
      in
      let { venv_dynamic = dynamic_dst;
            venv_static  = static_dst;
            venv_this    = this_dst
          } = venv_dst
      in
      let dynamic, found =
         try SymbolTable.add dynamic_dst v (SymbolTable.find dynamic_src v), true with
            Not_found ->
               dynamic_dst, false
      in
      let static, found =
         try SymbolTable.add static_dst v (SymbolTable.find static_src v), true with
            Not_found ->
               static_dst, found
      in
      let this, found =
         try SymbolTable.add this_dst v (SymbolTable.find this_src v), true with
            Not_found ->
               this_dst, found
      in
         if not found then
            raise (OmakeException (pos, UnboundVar v));
         { venv_dst with venv_dynamic = dynamic;
                         venv_static = static;
                         venv_this = this
         }
 | ExportRules ->
      (*
       * Export the implicit rules.
       *)
      let inner_src = venv_src.venv_inner in
      let inner_dst =
         { venv_dst.venv_inner with
           venv_implicit_deps = inner_src.venv_implicit_deps;
           venv_implicit_rules = inner_src.venv_implicit_rules;
         }
      in
         { venv_dst with venv_inner = inner_dst }
 | ExportPhonies ->
      (*
       * Export the phony vars.
       *)
      let inner_dst = { venv_dst.venv_inner with venv_phony = venv_src.venv_inner.venv_phony } in
         { venv_dst with venv_inner = inner_dst }

let export_list pos venv_dst venv_src vars =
   List.fold_left (fun venv_dst v ->
         export_item pos venv_dst venv_src v) venv_dst vars

(*
 * Exported environment does not include static values.
 *
 * We want to preserve pointer equality on venv2 to avoid giving unnecessary
 * "these files are targeted separately, but appear as effects of a single rule"
 * warnings.
 *)
let venv_export_venv venv1 venv2 =
   if venv1.venv_static == venv2.venv_static then
      venv2
   else
      { venv2 with venv_static = venv1.venv_static }

(*
 * Add the exported result to the current environment.
 *)
let add_exports venv_dst venv_src pos = function
   ExportNone ->
      venv_dst
 | ExportAll ->
      venv_export_venv venv_dst venv_src
 | ExportList vars ->
      export_list pos venv_dst venv_src vars

(*
 * venv_orig - environment before the function call.
 * venv_dst - environment after "entering" the object namespace, before the function call
 * venv_src - environment after the function call
 *
 *    # venv_orig is here
 *    A.B.C.f(1)
 *    # venv_dst is venv_orig with this = A.B.C
 *    # venv_src is venv when A.B.C.f returns
 *
 * 1. export from venv_src into venv_dst
 * 2. take venv_orig.venv_this
 * 3. update along the path A.B.C
 *)
let rec hoist_path venv path obj =
   match path with
      PathVar v ->
         venv_add_var venv v (ValObject obj)
    | PathField (path, parent_obj, v) ->
         let obj = SymbolTable.add parent_obj v (ValObject obj) in
            hoist_path venv path obj

let hoist_this venv_orig venv_obj path =
   let venv = { venv_obj with venv_this = venv_orig.venv_this } in
      hoist_path venv path venv_obj.venv_this

let add_path_exports venv_orig venv_dst venv_src pos path = function
   ExportNone ->
      venv_orig
 | ExportAll ->
      hoist_this venv_orig (venv_export_venv venv_dst venv_src) path
 | ExportList vars ->
      hoist_this venv_orig (export_list pos venv_dst venv_src vars) path

(************************************************************************
 * Squashing.
 *)
let squash_prim_fun f =
   f

let squash_object obj =
   obj

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
