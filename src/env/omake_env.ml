
module TargetElem = struct
  type t = int * string * Omake_node_sig.node_kind
  let compare (h1,s1,k1) (h2,s2,k2) =
    if h1=h2 then
      let p1 = String.compare s1 s2 in
      if p1 = 0 then
        compare k1 k2
      else
        p1
    else
      h1-h2
  let intern ((s1,k1) as key) =
    let h1 = Hashtbl.hash key in
    (h1,s1,k1)
end

module TargetMap = Lm_map.Make(TargetElem)

(*
 * Command lists have source arguments.
 *)
type command_info =
  { command_env     : t;
    command_sources : Omake_node.Node.t list;
    command_values  : Omake_value_type.t list;
    command_body    : Omake_value_type.command list
  }

(*
 * An implicit rule with a body.
 *
 * In an implicit rule, we compile the targets/sources
 * to wild patterns.
 *)
and irule =
  { irule_loc        : Lm_location.t;
    irule_multiple   : Omake_value_type.rule_multiple;
    irule_targets    : Lm_string_set.StringSet.t option;
    irule_patterns   : Lm_wild.in_patt list;
    irule_locks      : Omake_value_type.source_core Omake_value_type.source list;
    irule_sources    : Omake_value_type.source_core Omake_value_type.source list;
    irule_scanners   : Omake_value_type.source_core Omake_value_type.source list;
    irule_values     : Omake_value_type.t list;
    irule_body       : Omake_value_type.command list
  }

(*
 * An implicit dependency.  There is no body, but
 * it may have value dependencies.
 *)
and inrule =
  { inrule_loc        : Lm_location.t;
    inrule_multiple   : Omake_value_type.rule_multiple;
    inrule_patterns   : Lm_wild.in_patt list;
    inrule_locks      : Omake_value_type.source_core Omake_value_type.source list;
    inrule_sources    : Omake_value_type.source_core Omake_value_type.source list;
    inrule_scanners   : Omake_value_type.source_core Omake_value_type.source list;
    inrule_values     : Omake_value_type.t list
  }

(*
 * Explicit rules.
 *)
and erule =
  { rule_loc          : Lm_location.t;
    rule_env          : t;
    rule_target       : Omake_node.Node.t;
    rule_effects      : Omake_node.NodeSet.t;
    rule_locks        : Omake_node.NodeSet.t;
    rule_sources      : Omake_node.NodeSet.t;
    rule_scanners     : Omake_node.NodeSet.t;
    rule_match        : string option;
    rule_multiple     : Omake_value_type.rule_multiple;
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
  { explicit_targets         : erule Omake_node.NodeTable.t;
    explicit_deps            : (Omake_node.NodeSet.t * Omake_node.NodeSet.t * Omake_node.NodeSet.t) Omake_node.NodeTable.t;   (* locks, sources, scanners *)
    explicit_rules           : erule Omake_node.NodeMTable.t;
    explicit_directories     : t Omake_node.DirTable.t
  }

(*
 * An ordering rule.
 * For now, this just defines an extra dependency
 * of the form:  patt1 -> patt2
 * This means that if a file depends on patt1,
 * then it also depends on patt2.
 *)
and orule =
  { orule_loc      : Lm_location.t;
    orule_name     : Lm_symbol.t;
    orule_pattern  : Lm_wild.in_patt;
    orule_sources  : Omake_value_type.source_core list
  }

and ordering_info = orule list

and srule =
  { srule_loc      : Lm_location.t;
    srule_static   : bool;
    srule_env      : t;
    srule_key      : Omake_value_type.t;
    srule_deps     : Omake_node.NodeSet.t;
    srule_vals     : Omake_value_type.t list;
    srule_exp      : Omake_ir.exp
  }

and static_info =
    StaticRule of srule
  | StaticValue of Omake_value_type.obj

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
and t =
  { venv_dynamic        : Omake_value_type.env;
    venv_this           : Omake_value_type.obj;
    venv_static         : Omake_value_type.env;
    venv_inner          : venv_inner
  }

and venv_inner =
  { venv_environ        : string Lm_symbol.SymbolTable.t;
    venv_dir            : Omake_node.Dir.t;
    venv_phony          : Omake_node.NodeSet.t;
    venv_implicit_deps  : inrule list;
    venv_implicit_rules : irule list;
    venv_options        : Omake_options.t;
    venv_globals        : venv_globals;
    venv_mount          : Omake_node.Mount.t;
    venv_included_files : Omake_node.NodeSet.t
  }

and venv_globals =
  { mutable venv_parent                     : (venv_globals * int) option;
      (* after a venv_fork this is the pointer to the source; it is set back
         to None when any of the versions is changed. The int is the version
         of the parent.
       *)

    mutable venv_version                    : int;
      (* increased after a change *)

    mutable venv_mutex                      : Lm_thread.Mutex.t;

    (* At present, the venv_parent/venv_version mechanism is only used to
       accelerate target_is_buildable{_proper}. If a forked venv is still
       identical to the original, this cache can be better updated in the
       parent (back propagation).

       TODO: another candidate for back propagation is the file cache.
     *)

    (* Execution service *)
    venv_exec                               : exec;

    (* File cache *)
    venv_cache                              : Omake_cache.t;

    (* Mounting functions *)
    venv_mount_info                         : Omake_node.mount_info;

    (* Values from handles *)
    venv_environments                       : t Lm_handle_table.t;

    (* The set of files we have ever read *)
    mutable venv_files                      : Omake_node.NodeSet.t;

    (* Save the environment for each directory in the project *)
    mutable venv_directories                : t Omake_node.DirTable.t;
    mutable venv_excluded_directories       : Omake_node.DirSet.t;

    (* All the phony targets we have ever generated *)
    mutable venv_phonies                    : Omake_node.PreNodeSet.t;

    (* Explicit rules are global *)
    mutable venv_explicit_rules             : erule list;
    mutable venv_explicit_targets           : erule Omake_node.NodeTable.t;
    mutable venv_explicit_new               : erule list;

    (* Ordering rules *)
    mutable venv_ordering_rules             : orule list;
    mutable venv_orders                     : Lm_string_set.StringSet.t;

    (* Static rules *)
    mutable venv_memo_rules                 : static_info Omake_value_util.ValueTable.t;

    (* Cached values for files *)
    mutable venv_ir_files                   : Omake_ir.t Omake_node.NodeTable.t;
    mutable venv_object_files               : Omake_value_type.obj Omake_node.NodeTable.t;

    (* Cached values for static sections *)
    mutable venv_static_values              : Omake_value_type.obj Lm_symbol.SymbolTable.t Omake_node.NodeTable.t;
    mutable venv_modified_values            : Omake_value_type.obj Lm_symbol.SymbolTable.t Omake_node.NodeTable.t;

    (* Cached values for the target_is_buildable function *)
    (* This uses now a compression: we map directories to small integers
       target_dir. This mapping is implemented by venv_target_dirs.
       For every (candidate) target file we map the file name to two bitsets
       (buildable,non_buildable) enumerating the directories where the file
       can be built or not be built.
     *)
    mutable venv_target_dirs                : target_dir Omake_node.DirTable.t;
    mutable venv_target_next_dir            : target_dir;
    mutable venv_target_is_buildable        : (Lm_bitset.t * Lm_bitset.t) TargetMap.t;
    mutable venv_target_is_buildable_proper : (Lm_bitset.t * Lm_bitset.t) TargetMap.t;

    (* The state right after Pervasives is evaluated *)
    mutable venv_pervasives_vars            : Omake_ir.senv;
    mutable venv_pervasives_obj             : Omake_value_type.obj
  }

and target_dir = int

(*
 * Type of execution servers.
 *)
and pid =
    InternalPid of int
  | ExternalPid of int
  | ResultPid of int * t * Omake_value_type.t

and exec = (arg_command_line, pid, Omake_value_type.t) Omake_exec.Exec.t

(*
 * Execution service.
 *)
and arg_command_inst = (Omake_ir.exp, arg_pipe, Omake_value_type.t) Omake_command_type.poly_command_inst
and arg_command_line = (t, Omake_ir.exp, arg_pipe, Omake_value_type.t) Omake_command_type.poly_command_line

and string_command_inst = (Omake_ir.exp, string_pipe, Omake_value_type.t) Omake_command_type.poly_command_inst
and string_command_line = (t, Omake_ir.exp, string_pipe, Omake_value_type.t) Omake_command_type.poly_command_line

and apply        = t -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> (Lm_symbol.t * string) list -> Omake_value_type.t list -> int * t * Omake_value_type.t

and value_cmd    = (unit, Omake_value_type.t list, Omake_value_type.t list) Omake_shell_type.poly_cmd
and value_apply  = (Omake_value_type.t list, Omake_value_type.t list, apply) Omake_shell_type.poly_apply
and value_group  = (unit, Omake_value_type.t list, Omake_value_type.t list, Omake_value_type.t list, apply) Omake_shell_type.poly_group
and value_pipe   = (unit, Omake_value_type.t list, Omake_value_type.t list, Omake_value_type.t list, apply) Omake_shell_type.poly_pipe

and arg_cmd      = (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_command_type.arg) Omake_shell_type.poly_cmd
and arg_apply    = (Omake_value_type.t, Omake_command_type.arg, apply) Omake_shell_type.poly_apply
and arg_group    = (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_value_type.t, Omake_command_type.arg, apply) Omake_shell_type.poly_group
and arg_pipe     = (Omake_command_type.arg Omake_shell_type.cmd_exe, Omake_command_type.arg, Omake_value_type.t, Omake_command_type.arg, apply) Omake_shell_type.poly_pipe

and string_cmd   = (Omake_shell_type.simple_exe, string, string) Omake_shell_type.poly_cmd
and string_apply = (Omake_value_type.t, string, apply) Omake_shell_type.poly_apply
and string_group = (Omake_shell_type.simple_exe, string, Omake_value_type.t, string, apply) Omake_shell_type.poly_group
and string_pipe  = (Omake_shell_type.simple_exe, string, Omake_value_type.t, string, apply) Omake_shell_type.poly_pipe

(*
 * Error during translation.
 *)
exception Break             of Lm_location.t * t

type prim_fun_data = t -> Omake_value_type.pos -> Lm_location.t ->
  Omake_value_type.t list -> Omake_value_type.keyword_value list -> t * Omake_value_type.t

type venv_runtime =
   { venv_channels               : Lm_channel.t Lm_int_handle_table.t;
     mutable venv_primitives     : prim_fun_data Lm_symbol.SymbolTable.t
   }

(*
 * Command line parsing.
 *)
type lexer = string -> int -> int -> int option

type tok =
   TokString of Omake_value_type.t
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
   FullApply    of t * Omake_value_type.t list * Omake_value_type.keyword_value list
 | PartialApply of Omake_value_type.env * Omake_value_type.param_value list * Omake_value_type.keyword_param_value list * Omake_ir.param list * Omake_value_type.keyword_value list


let venv_runtime :  venv_runtime =
   { venv_channels       = Lm_int_handle_table.create ();
     venv_primitives     = Lm_symbol.SymbolTable.empty
   }


(*
 * Now the stuff that is really global, not saved in venv.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end;;

module IntTable = Lm_map.LmMake (IntCompare);;


(************************************************************************
 * Access to the globals.
 * This actually performs some computation in 0.9.9
 *)
let venv_globals venv =
   venv.venv_inner.venv_globals

let venv_get_mount_listing venv =
  Omake_node.Mount.mount_listing venv.venv_inner.venv_mount

let venv_protect globals f =
  Lm_thread.Mutex.lock globals.venv_mutex;
  try
    let r = f() in
    Lm_thread.Mutex.unlock globals.venv_mutex;
    r
  with
    | exn ->
        Lm_thread.Mutex.unlock globals.venv_mutex;
        raise exn


let venv_synch venv f =
  let globals = venv_globals venv in
  venv_protect
    globals
    (fun () ->
       match globals.venv_parent with
         | Some(pglobals, pversion) ->
             venv_protect
               pglobals 
               (fun () ->
                  if pversion = pglobals.venv_version then
                    f globals (Some pglobals)
                  else (
                    globals.venv_parent <- None;
                    f globals None
                  )
               )
         | None ->
             f globals None
    )


let venv_incr_version venv f =
  (* At present, this function needs to be called when any change is done that
     may affect target_is_buildable(_proper), i.e. the addition of implicit,
     explicit or phony rules.
   *)
  let g = venv_globals venv in
  venv_protect
    g
    (fun () ->
       g.venv_version <- g.venv_version + 1;
       g.venv_parent <- None;
       f()
    )

(*
 * Map functions.
 *)
let check_map_key = Omake_value_util.ValueCompare.check

let venv_map_empty = Omake_value_util.ValueTable.empty

let venv_map_add map pos v1 v2 =
   Omake_value_util.ValueTable.add map (check_map_key pos v1) v2

let venv_map_remove map pos v1 =
   Omake_value_util.ValueTable.remove map (check_map_key pos v1)

let venv_map_find map pos v =
   try Omake_value_util.ValueTable.find map (check_map_key pos v) with
      Not_found ->
         raise (Omake_value_type.OmakeException (pos, UnboundValue v))

let venv_map_mem map pos v =
   Omake_value_util.ValueTable.mem map (check_map_key pos v)

let venv_map_iter   = Omake_value_util.ValueTable.iter
let venv_map_map    = Omake_value_util.ValueTable.mapi
let venv_map_fold   = Omake_value_util.ValueTable.fold
let venv_map_length = Omake_value_util.ValueTable.cardinal

(************************************************************************
 * Printing.
 *)
let rec pp_print_command buf command =
  match command with
    Omake_value_type.CommandSection (arg, _fv, e) ->
    Format.fprintf buf "@[<hv 3>section %a@ %a@]" Omake_value_print.pp_print_value arg Omake_ir_print.pp_print_exp_list e
  | CommandValue (_, _, v) ->
    Omake_ir_print.pp_print_string_exp buf v

and pp_print_commands buf commands =
  List.iter (fun command -> Format.fprintf buf "@ %a" pp_print_command command) commands

and pp_print_command_info buf info =
  let { command_env     = venv;
        command_sources = sources;
        command_body    = commands;
        _
      } = info
  in
  Format.fprintf buf "@[<hv 0>@[<hv 3>{@ command_dir = %a;@ @[<b 3>command_sources =%a@]@ @[<b 3>command_body =%a@]@]@ }@]" (**)
    Omake_node.pp_print_dir venv.venv_inner.venv_dir
    Omake_node.pp_print_node_list sources
    pp_print_commands commands

and pp_print_command_info_list buf infos =
  List.iter (fun info -> Format.fprintf buf "@ %a" pp_print_command_info info) infos

and pp_print_rule buf erule =
  let { rule_target      = target;
        rule_effects     = effects;
        rule_locks       = locks;
        rule_sources     = sources;
        rule_scanners    = scanners;
        rule_commands    = commands;
        _
      } = erule
  in
  Format.fprintf buf "@[<hv 0>@[<hv 3>rule {";
  Format.fprintf buf "@ target = %a" Omake_node.pp_print_node target;
  Format.fprintf buf "@ @[<b 3>effects =%a@]" Omake_node.pp_print_node_set effects;
  Format.fprintf buf "@ @[<b 3>locks =%a@]" Omake_node.pp_print_node_set locks;
  Format.fprintf buf "@ @[<b 3>sources =%a@]" Omake_node.pp_print_node_set sources;
  Format.fprintf buf "@ @[<b 3>scanners =%a@]" Omake_node.pp_print_node_set scanners;
  Format.fprintf buf "@ @[<hv 3>commands =%a@]" pp_print_command_info_list commands;
  Format.fprintf buf "@]@ }@]"

let pp_print_explicit_rules buf venv =
   Format.fprintf buf "@[<hv 3>Explicit rules:";
   List.iter (fun erule -> Format.fprintf buf "@ %a" pp_print_rule erule) venv.venv_inner.venv_globals.venv_explicit_rules;
   Format.fprintf buf "@]"

(************************************************************************
 * Pipeline printing.
 *)

(*
 * Token printing.
 *)
let rec pp_print_tok buf tok =
   match tok with
      TokString v ->
         Omake_value_print.pp_print_value buf v
    | TokToken s ->
         Format.fprintf buf "$%s" s
    | TokGroup toks ->
         Format.fprintf buf "(%a)" pp_print_tok_list toks

and pp_print_tok_list buf toks =
   match toks with
      [tok] ->
         pp_print_tok buf tok
    | tok :: toks ->
         pp_print_tok buf tok;
         Lm_printf.pp_print_char buf ' ';
         pp_print_tok_list buf toks
    | [] ->
         ()

let pp_print_simple_exe buf exe =
   match exe with
      Omake_shell_type.ExeString s ->
         Lm_printf.pp_print_string buf s
    | ExeQuote s ->
         Format.fprintf buf "\\%s" s
    | ExeNode node ->
         Omake_node.pp_print_node buf node

(*
 * Pipes based on strings.
 *)
module PrintString =
struct
   type arg_command = string
   type arg_apply   = Omake_value_type.t
   type arg_other   = string
   type exe         = Omake_shell_type.simple_exe

   let pp_print_arg_command = Omake_command_type.pp_arg_data_string
   let pp_print_arg_apply   = Omake_value_print.pp_print_value
   let pp_print_arg_other   = Omake_command_type.pp_arg_data_string
   let pp_print_exe         = pp_print_simple_exe
end;;

module PrintStringPipe = Omake_shell_type.MakePrintPipe (PrintString);;

module PrintStringv =
struct
   type argv = string_pipe

   let pp_print_argv = PrintStringPipe.pp_print_pipe
end;;

module PrintStringCommand = Omake_command_type.MakePrintCommand (PrintStringv);;

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
   type arg_apply   = Omake_value_type.t
   type arg_other   = arg_command
   type exe         = arg_command Omake_shell_type.cmd_exe

   let pp_print_arg_command = Omake_command_type.pp_print_arg
   let pp_print_arg_apply   = Omake_value_print.pp_print_simple_value
   let pp_print_arg_other   = pp_print_arg_command
   let pp_print_exe buf exe =
      match exe with
         Omake_shell_type.CmdArg arg ->
            pp_print_arg_command buf arg
       | CmdNode node ->
            Omake_node.pp_print_node buf node
end;;

module PrintArgPipe = Omake_shell_type.MakePrintPipe (PrintArg);;

module PrintArgv =
struct
   type argv = arg_pipe

   let pp_print_argv = PrintArgPipe.pp_print_pipe
end;;

module PrintArgCommand = Omake_command_type.MakePrintCommand (PrintArgv);;

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
   Omake_value_type.RuleMultiple
 | RuleScannerMultiple ->
      true
 | RuleSingle
 | RuleScannerSingle ->
      false

(* let is_scanner_rule = function *)
(*    Omake_value_type.RuleScannerSingle *)
(*  | RuleScannerMultiple -> *)
(*       true *)
(*  | RuleSingle *)
(*  | RuleMultiple -> *)
(*       false *)

let rule_kind = function
   Omake_value_type.RuleScannerSingle
 | RuleScannerMultiple ->
      Omake_value_type.RuleScanner
 | RuleSingle
 | RuleMultiple ->
      RuleNormal

(************************************************************************
 * Handles.
 *)
let venv_add_environment venv =
   Lm_handle_table.add venv.venv_inner.venv_globals.venv_environments venv
module Pos= Omake_pos.Make (struct let name = "Omake_env" end)
let venv_find_environment venv pos hand =
  try Lm_handle_table.find venv.venv_inner.venv_globals.venv_environments hand with
    Not_found ->
    let pos = Pos.string_pos "venv_find_environment" pos in
    raise (Omake_value_type.OmakeException (pos, StringError "unbound environment"))

(************************************************************************
 * Channels.
*)

(*
 * Add a channel slot.
 *)
let venv_add_index_channel index data =
   let channels = venv_runtime.venv_channels in
   let channel = Lm_int_handle_table.create_handle channels index in
      Lm_channel.set_id data index;
      Lm_int_handle_table.add channels channel data;
      channel

let venv_add_channel _venv data =
   let channels = venv_runtime.venv_channels in
   let channel = Lm_int_handle_table.new_handle channels in
   let index = Lm_int_handle_table.int_of_handle channel in
      Lm_channel.set_id data index;
      Lm_int_handle_table.add channels channel data;
      channel

let add_channel file kind mode binary fd =
   Lm_channel.create file kind mode binary (Some fd)

let venv_stdin  = venv_add_index_channel 0 (add_channel "<stdin>"  Lm_channel.PipeChannel Lm_channel.InChannel  false Unix.stdin)
let venv_stdout = venv_add_index_channel 1 (add_channel "<stdout>" Lm_channel.PipeChannel Lm_channel.OutChannel false Unix.stdout)
let venv_stderr = venv_add_index_channel 2 (add_channel "<stderr>" Lm_channel.PipeChannel Lm_channel.OutChannel false Unix.stderr)

(*
 * A formatting channel.
 *)
let venv_add_formatter_channel _venv fmt =
   let channels = venv_runtime.venv_channels in
   let fd = Lm_channel.create "formatter" Lm_channel.FileChannel Lm_channel.OutChannel true None in
   let channel = Lm_int_handle_table.new_handle channels in
   let index = Lm_int_handle_table.int_of_handle channel in
   let reader _s _off _len =
      raise (Unix.Unix_error (Unix.EINVAL, "formatter-channel", ""))
   in
   let writer s off len =
      Format.pp_print_string fmt (Bytes.to_string (Bytes.sub s off len));
      len
   in
      Lm_channel.set_id fd index;
      Lm_channel.set_io_functions fd reader writer;
      Lm_int_handle_table.add channels channel fd;
      channel

(*
 * Get the channel.
 *)
let venv_channel_data channel =
   (* Standard channels are always available *)
   if Lm_int_handle_table.int_of_handle channel <= 2 then
      Lm_int_handle_table.find_any venv_runtime.venv_channels channel
   else
      Lm_int_handle_table.find venv_runtime.venv_channels channel

(*
 * When a channel is closed, close the buffers too.
 *)
let venv_close_channel _venv _pos channel =
   try
      let fd = venv_channel_data channel in
         Lm_channel.close fd;
         Lm_int_handle_table.remove venv_runtime.venv_channels channel
   with
      Not_found ->
         (* Fail silently *)
         ()

(*
 * Get the channel.
 *)
let venv_find_channel _venv pos channel =
   let pos = Pos.string_pos "venv_find_in_channel" pos in
      try venv_channel_data channel with
         Not_found ->
            raise (Omake_value_type.OmakeException (pos, StringError "channel is closed"))

(*
 * Finding by identifiers.
 *)
let venv_find_channel_by_channel _venv pos fd =
   let index, _, _, _ = Lm_channel.info fd in
      try Lm_int_handle_table.find_value venv_runtime.venv_channels index fd with
         Not_found ->
            raise (Omake_value_type.OmakeException (pos, StringError "channel is closed"))

let venv_find_channel_by_id _venv pos index =
   try Lm_int_handle_table.find_any_handle venv_runtime.venv_channels index with
      Not_found ->
         raise (Omake_value_type.OmakeException (pos, StringError "channel is closed"))

(************************************************************************
 * Primitive values.
 *)

(*
 * Allocate a function primitive.
 *)
let venv_add_prim_fun _venv name data =
   venv_runtime.venv_primitives <- Lm_symbol.SymbolTable.add venv_runtime.venv_primitives name data;
   name
let debug_scanner =
   Lm_debug.create_debug (**)
      { debug_name = "scanner";
        debug_description = "Display debugging information for scanner selection";
        debug_value = false
      }

let debug_implicit =
   Lm_debug.create_debug (**)
      { debug_name = "implicit";
        debug_description = "Display debugging information for implicit rule selection";
        debug_value = false
      }

(*
 * Debug file database (.omc files).
 *)
let debug_db = Lm_db.debug_db

(*
 * Look up the primitive value if we haven't seen it already.
 *)
let venv_apply_prim_fun name venv pos loc args =
   let f =
      try Lm_symbol.SymbolTable.find venv_runtime.venv_primitives name with
         Not_found ->
            raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, UnboundVar name))
   in
      f venv pos loc args

(************************************************************************
 * Target cache.
 *
 * To keep this up-to-date, entries are added for explicit targets,
 * and the cache is flushed whenever an implicit rule is added.
 *)

let lookup_target_dir_in g dir =
  try
    Omake_node.DirTable.find g.venv_target_dirs dir
  with
    | Not_found ->
        let tdir = g.venv_target_next_dir in
        g.venv_target_next_dir <- tdir+1;
        let tab =
          Omake_node.DirTable.add g.venv_target_dirs dir tdir in
        g.venv_target_dirs <- tab;
        tdir


let venv_lookup_target_dir venv dir =
   venv_synch
     venv
     (fun globals pglobals_opt ->
        match pglobals_opt with
          | Some pglobals ->
              let tdir = lookup_target_dir_in pglobals dir in
              globals.venv_target_next_dir <- pglobals.venv_target_next_dir;
              globals.venv_target_dirs <- pglobals.venv_target_dirs;
              tdir
          | None ->
              lookup_target_dir_in globals dir
     )


let squeeze_phony =
  (* This is OK because whenever we add a phony target we flush the cache *)
  function
  | Omake_node_sig.NodePhony ->
      Omake_node_sig.NodeNormal
  | other ->
      other


let venv_find_target_is_buildable_exn venv target_dir file node_kind = 
  let node_kind = squeeze_phony node_kind in
  let g = venv_globals venv in
  let ikey = TargetElem.intern (file,node_kind) in
  let (bset,nonbset) =
    TargetMap.find ikey g.venv_target_is_buildable in
  Lm_bitset.is_set bset target_dir || (
    if not(Lm_bitset.is_set nonbset target_dir) then raise Not_found;
    false
  )

let venv_find_target_is_buildable_multi venv file node_kind =
  let node_kind = squeeze_phony node_kind in
  let g = venv_globals venv in
  let ikey = TargetElem.intern (file,node_kind) in
  let (bset,nonbset) =
    try
      TargetMap.find ikey g.venv_target_is_buildable
    with
      | Not_found ->
          (Lm_bitset.create(), Lm_bitset.create()) in
  (fun target_dir ->
     Lm_bitset.is_set bset target_dir || (
       if not(Lm_bitset.is_set nonbset target_dir) then raise Not_found;
       false
     )
  )


let venv_find_target_is_buildable_proper_exn venv target_dir file node_kind =
  let node_kind = squeeze_phony node_kind in
  let g = venv_globals venv in
  let ikey = TargetElem.intern (file,node_kind) in
  let (bset,nonbset) =
    TargetMap.find ikey g.venv_target_is_buildable_proper in
  Lm_bitset.is_set bset target_dir || (
    if not(Lm_bitset.is_set nonbset target_dir) then raise Not_found;
    false
  )

let add_target_to m target_dir file node_kind flag =
  let node_kind = squeeze_phony node_kind in
  let ikey = TargetElem.intern (file,node_kind) in
  let (bset,nonbset) =
    try TargetMap.find ikey m
    with Not_found -> (Lm_bitset.create(), Lm_bitset.create()) in
  let (bset',nonbset') =
    if flag then
      (Lm_bitset.set bset target_dir, nonbset)
    else
      (bset, Lm_bitset.set nonbset target_dir) in
  TargetMap.add ikey (bset',nonbset') m


let venv_add_target_is_buildable venv target_dir file node_kind flag =
   let add g =
     let tab =
       add_target_to
         g.venv_target_is_buildable target_dir file node_kind flag in
     g.venv_target_is_buildable <- tab in
   venv_synch
     venv
     (fun globals pglobals_opt ->
        match pglobals_opt with
          | Some pglobals ->
              add pglobals;
              globals.venv_target_is_buildable <-
                pglobals.venv_target_is_buildable
          | None ->
              add globals
     )

let venv_add_target_is_buildable_multi venv file node_kind tdirs_pos tdirs_neg =
  let node_kind = squeeze_phony node_kind in
  let add g =
    let ikey = TargetElem.intern (file,node_kind) in
    let (bset,nonbset) =
      try TargetMap.find ikey g.venv_target_is_buildable
      with Not_found -> (Lm_bitset.create(), Lm_bitset.create()) in
    let bset' = Lm_bitset.set_multiple bset tdirs_pos in
    let nonbset' = Lm_bitset.set_multiple nonbset tdirs_neg in
    let tab = TargetMap.add ikey (bset',nonbset') g.venv_target_is_buildable in
    g.venv_target_is_buildable <- tab in
  venv_synch
    venv
    (fun globals pglobals_opt ->
       match pglobals_opt with
         | Some pglobals ->
             add pglobals;
             globals.venv_target_is_buildable <-
               pglobals.venv_target_is_buildable
         | None ->
             add globals
    )


let venv_add_target_is_buildable_proper venv target_dir file node_kind flag =
   let add g =
     let tab =
       add_target_to
         g.venv_target_is_buildable_proper target_dir file node_kind flag in
     g.venv_target_is_buildable_proper <- tab in
   venv_synch
     venv
     (fun globals pglobals_opt ->
        match pglobals_opt with
          | Some pglobals ->
              add pglobals;
              globals.venv_target_is_buildable_proper <-
                pglobals.venv_target_is_buildable_proper
          | None ->
              add globals
     )

let venv_add_explicit_targets venv rules =
   venv_incr_version
     venv
     (fun () ->
        let globals = venv.venv_inner.venv_globals in
        let { venv_target_is_buildable = cache;
              venv_target_is_buildable_proper = cache_proper;
              _
            } = globals
        in
        let add cache erule =
          let dir = Omake_node.Node.dir erule.rule_target in
          let tdir = lookup_target_dir_in globals dir in
          let file = Omake_node.Node.tail erule.rule_target in
          let nkind = Omake_node.Node.kind erule.rule_target in
          add_target_to cache tdir file nkind true in
        let cache = List.fold_left add cache rules in
        let cache_proper = List.fold_left add cache_proper rules in
        globals.venv_target_is_buildable <- cache;
        globals.venv_target_is_buildable_proper <- cache_proper
     )

let venv_flush_target_cache venv =
   venv_incr_version
     venv
     (fun () ->
        let globals = venv.venv_inner.venv_globals in
        globals.venv_target_is_buildable <- TargetMap.empty;
        globals.venv_target_is_buildable_proper <- TargetMap.empty
     )

(*
 * Save explicit rules.
 *)
let venv_save_explicit_rules venv _loc rules =
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
        rule_effects    = Omake_node.NodeSet.singleton target;
        rule_sources    = Omake_node.NodeSet.singleton source;
        rule_locks      = Omake_node.NodeSet.empty;
        rule_scanners   = Omake_node.NodeSet.empty;
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
          venv_phony = phony;
          _
        } = inner
    in
    let globals = venv_globals venv in
    let phonies = globals.venv_phonies in
    let phony, phonies =
      List.fold_left (fun (phony, phonies) name ->
        let name =
          match name with
            Omake_value_type.TargetNode _ ->
            raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, StringError ".PHONY arguments should be names"))
          | TargetString s ->
            s
        in
        let gnode = Omake_node.Node.create_phony_global name in
        let dnode = Omake_node.Node.create_phony_dir dir name in
        let phony = Omake_node.NodeSet.add phony dnode in
        let phonies = Omake_node.PreNodeSet.add phonies (Omake_node.Node.dest gnode) in
        let phonies = Omake_node.PreNodeSet.add phonies (Omake_node.Node.dest dnode) in
        venv_add_explicit_dep venv loc gnode dnode;
        phony, phonies) (phony, phonies) names
    in
    let inner = { inner with venv_phony = phony } in
    let venv = { venv with venv_inner = inner } in
    venv_incr_version venv (fun () -> ());
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

(*
 * For static values, we access the db a bit more directly
 *)
module type InternalStaticSig =
sig
   include StaticSig
   val write       : t -> Omake_node.Node.t -> (out_handle -> 'a) -> 'a

   val find_values : in_handle -> Omake_value_type.obj Lm_symbol.SymbolTable.t
   val add_values  : out_handle -> Omake_value_type.obj Lm_symbol.SymbolTable.t -> unit
end

module Static : InternalStaticSig =
struct
  
   (*
    * A .omc file.
    *)
   type handle =
      { db_file         : Unix.file_descr option;
        db_name         : Omake_node.Node.t;
        db_digest       : string;
        db_env          : t;
        db_flush_ir     : bool;
        db_flush_static : bool
      }
   type in_handle = handle
   type out_handle = handle

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
    * Open a file.  The Omake_node.Node.t is the name of the _source_ file,
    * not the .omc file.  We'll figure out where the .omc file
    * goes on our own.
    *)
   let create_mode mode venv source =
      (* Get the source digest *)
      let cache = venv.venv_inner.venv_globals.venv_cache in
      let digest =
         match Omake_cache.stat cache source with
            Some (_,digest) ->
               digest
          | None ->
               raise Not_found
      in

      (*
       * Open the result file.  The lock_cache_file function
       * will try to use the target directory first, and
       * fall back to ~/.omake/cache if that is not writable.
       *)
      let source_name = Omake_node.Node.absname source in
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
                  Lm_printf.eprintf "@[<v 3>Omake_db.create:@ %a --> %s@]@." Omake_node.pp_print_node source target_name;
               Unix.set_close_on_exec target_fd;
               Omake_state.lock_file target_fd mode;
               Some target_fd
         with
            Unix.Unix_error _
          | Failure _ ->
               Lm_printf.eprintf "@[<v 3>OMake warning: could not create and/or lock a cache file for@ %s@]@." source_name;
               None
      in
         { db_file         = target_fd;
           db_name         = source;
           db_digest       = digest;
           db_env          = venv;
           db_flush_ir     = Omake_options.opt_flush_include venv.venv_inner.venv_options;
           db_flush_static = Omake_options.opt_flush_static venv.venv_inner.venv_options;
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
         { db_file = Some fd; db_name = name ; _} ->
            if !debug_db then
               Lm_printf.eprintf "Omake_db.close: %a@." Omake_node.pp_print_node name;
            Unix.close fd
       | { db_file = None ; _} ->
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
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = _venv ; _} ->
            if !debug_db then
               Lm_printf.eprintf "Omake_db.add_ir: %a@." Omake_node.pp_print_node name;
            Lm_db.add fd (Omake_node.Node.absname name) ir_tag Omake_magic.ir_magic digest ir
       | { db_file = None ; _} ->
            ()

   let add_object info obj =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = _venv ; _} ->
            if !debug_db then
               Lm_printf.eprintf "Omake_db.add_object: %a@." Omake_node.pp_print_node name;
            Lm_db.add fd (Omake_node.Node.absname name) object_tag Omake_magic.obj_magic digest obj
       | { db_file = None ; _} ->
            ()

   let add_values info obj =
      match info with
         { db_file = Some fd; db_name = name; db_digest = digest; db_env = _venv ; _} ->
            if !debug_db then
               Lm_printf.eprintf "Omake_db.add_values: %a@." Omake_node.pp_print_node name;
            Lm_db.add fd (Omake_node.Node.absname name) values_tag Omake_magic.obj_magic digest obj
       | { db_file = None ; _} ->
            ()

   (*
    * Fetch the three kinds of entries.
    *)
   let find_ir = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false ; _} ->
         if !debug_db then
            Lm_printf.eprintf "Omake_db.find_ir: finding: %a@." Omake_node.pp_print_node name;
         let ir = Lm_db.find fd (Omake_node.Node.absname name) ir_tag Omake_magic.ir_magic digest in
            if !debug_db then
               Lm_printf.eprintf "Omake_db.find_ir: found: %a@." Omake_node.pp_print_node name;
            ir
    | { db_file = None ; _}
    | { db_flush_ir = true ; _} ->
         raise Not_found

   let find_object = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false; db_flush_static = false ; _} ->
         if !debug_db then
            Lm_printf.eprintf "Omake_db.find_object: finding: %a@." Omake_node.pp_print_node name;
         let obj = Lm_db.find fd (Omake_node.Node.absname name) object_tag Omake_magic.obj_magic digest in
            if !debug_db then
               Lm_printf.eprintf "Omake_db.find_object: found: %a@." Omake_node.pp_print_node name;
            obj
    | { db_file = None ; _}
    | { db_flush_ir = true ;_}
    | { db_flush_static = true ; _} ->
         raise Not_found

   let find_values = function
      { db_file = Some fd; db_name = name; db_digest = digest; db_flush_ir = false; db_flush_static = false ; _} ->
         if !debug_db then
            Lm_printf.eprintf "Omake_db.find_values: finding: %a@." Omake_node.pp_print_node name;
         let obj = Lm_db.find fd (Omake_node.Node.absname name) values_tag Omake_magic.obj_magic digest in
            if !debug_db then
               Lm_printf.eprintf "Omake_db.find_values: found: %a@." Omake_node.pp_print_node name;
            obj
    | { db_file = None ; _}
    | { db_flush_ir = true ; _}
    | { db_flush_static = true ; _} ->
         raise Not_found

   let get_ir     = find_ir
   let get_object = find_object
end;;

(*
 * Cached object files.
 *)
let venv_find_ir_file_exn venv node =
   Omake_node.NodeTable.find venv.venv_inner.venv_globals.venv_ir_files node

let venv_add_ir_file venv node obj =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_ir_files <- Omake_node.NodeTable.add globals.venv_ir_files node obj

let venv_find_object_file_exn venv node =
   Omake_node.NodeTable.find venv.venv_inner.venv_globals.venv_object_files node

let venv_add_object_file venv node obj =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_object_files <- Omake_node.NodeTable.add globals.venv_object_files node obj

(************************************************************************
 * Variables.
 *)

(*
 * Default empty object.
 *)
let venv_empty_object = Lm_symbol.SymbolTable.empty

(*
 * For variables, try to look them up as 0-arity functions first.
 *)
let venv_find_var_private_exn venv v =
   Lm_symbol.SymbolTable.find venv.venv_static v

let venv_find_var_dynamic_exn venv v =
   Lm_symbol.SymbolTable.find venv.venv_dynamic v

let venv_find_var_protected_exn venv v =
   try Lm_symbol.SymbolTable.find venv.venv_this v with
      Not_found ->
         try Lm_symbol.SymbolTable.find venv.venv_dynamic v with
            Not_found ->
               try Lm_symbol.SymbolTable.find venv.venv_static v with
                  Not_found ->
                     ValString (Lm_symbol.SymbolTable.find venv.venv_inner.venv_environ v)

let venv_find_var_global_exn venv v =
   try Lm_symbol.SymbolTable.find venv.venv_dynamic v with
      Not_found ->
         try Lm_symbol.SymbolTable.find venv.venv_this v with
            Not_found ->
               try Lm_symbol.SymbolTable.find venv.venv_static v with
                  Not_found ->
                     ValString (Lm_symbol.SymbolTable.find venv.venv_inner.venv_environ v)

let venv_find_var_exn venv v =
   match v with
      Omake_ir.VarPrivate (_, v) ->
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
         let pos = Pos.string_pos "venv_get_var" pos in
            raise (Omake_value_type.OmakeException (pos, UnboundVarInfo v))

let venv_find_var venv pos loc v =
   try venv_find_var_exn venv v with
      Not_found ->
         let pos = Pos.string_pos "venv_find_var" (Pos.loc_pos loc pos) in
            raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, UnboundVarInfo v))

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
         venv_dynamic = dynamic;
         _
       } = venv
   in
      match v with
         Omake_ir.VarPrivate (_, v) ->
            Lm_symbol.SymbolTable.mem static v
       | VarVirtual (_, v) ->
            Lm_symbol.SymbolTable.mem dynamic v
       | VarThis (_, v)
       | VarGlobal (_, v) ->
            Lm_symbol.SymbolTable.mem this v || Lm_symbol.SymbolTable.mem dynamic v || Lm_symbol.SymbolTable.mem static v

(*
 * Adding to variable environment.
 * Add to the current object and the static scope.
 *)
let venv_add_var venv v s =
   let { venv_this = this;
         venv_static = static;
         venv_dynamic = dynamic;
         _
       } = venv
   in
      match v with
         Omake_ir.VarPrivate (_, v) ->
            { venv with venv_static  = Lm_symbol.SymbolTable.add static v s }
       | VarVirtual (_, v) ->
            { venv with venv_dynamic = Lm_symbol.SymbolTable.add dynamic v s }
       | VarThis (_, v) ->
            { venv with venv_this    = Lm_symbol.SymbolTable.add this v s;
                        venv_static  = Lm_symbol.SymbolTable.add static v s
            }
       | VarGlobal (_, v) ->
            { venv with venv_dynamic = Lm_symbol.SymbolTable.add dynamic v s;
                        venv_static  = Lm_symbol.SymbolTable.add static v s
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
                     raise (Omake_value_type.OmakeException (pos, StringVarError ("keyword argument is required", v1)))
            else
               raise (Omake_value_type.OmakeException (pos, StringVarError ("no such keyword", v2)))
    | (v1, _, None) :: _, [] ->
         raise (Omake_value_type.OmakeException (pos, StringVarError ("keyword argument is required", v1)))
    | (_, v_info, Some arg) :: keywords_tl, [] ->
         venv_add_keyword_args pos (venv_add_var venv v_info arg) keywords_tl kargs
    | [], [] ->
         venv
    | [], (v2, _) :: _ ->
         raise (Omake_value_type.OmakeException (pos, StringVarError ("no such keyword", v2)))

let venv_add_args venv pos loc static params args keywords kargs =
   let venv = { venv with venv_static = static } in
   let venv = venv_add_keyword_args pos venv keywords kargs in
   let len1 = List.length params in
   let len2 = List.length args in
   let () =
      if len1 <> len2 then
         raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, ArityMismatch (ArityExact len1, len2)))
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
               raise (Omake_value_type.OmakeException (pos, StringVarError ("duplicate keyword", v1)))
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
         raise (Omake_value_type.OmakeException (pos, ArityMismatch (ArityExact (List.length params), 0)))
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
                     raise (Omake_value_type.OmakeException (pos, StringVarError ("keyword argument is required", v1)));
            else
               venv_add_curry_args pos venv params args keywords (karg :: skipped_kargs) kargs_tl
    | (v1, _, None) :: _, [] ->
         raise (Omake_value_type.OmakeException (pos, StringVarError ("keyword argument is required", v1)))
    | (_, v_info, Some arg) :: keywords_tl, [] ->
         venv_add_curry_args pos (venv_add_var venv v_info arg) params args keywords_tl skipped_kargs kargs
    | [], karg :: kargs_tl ->
         venv_add_curry_args pos venv params args keywords (karg :: skipped_kargs) kargs_tl
    | [], [] ->
         apply_curry_args pos venv skipped_kargs params args

let venv_add_curry_args venv pos _loc static pargs params args keywords kargs1 kargs2 =
   let venv = { venv with venv_static = static } in
   let venv = add_partial_args venv pargs in
      venv_add_curry_args pos venv params args keywords [] (merge_kargs pos kargs1 kargs2)

(*
 * Also provide a form for partial applications.
 *)
let rec add_partial_keywords pos venv = function
   (v, _, None) :: _ ->
      raise (Omake_value_type.OmakeException (pos, StringVarError ("keyword argument is required", v)))
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
      ((v1, v_info, _) as key) :: keywords_tl, ((v2, arg) as karg) :: kargs_tl ->
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
   Lm_symbol.SymbolTable.find venv.venv_inner.venv_environ v

let venv_setenv venv v x =
   { venv with venv_inner = { venv.venv_inner with venv_environ = Lm_symbol.SymbolTable.add venv.venv_inner.venv_environ v x } }

let venv_unsetenv venv v =
   { venv with venv_inner = { venv.venv_inner with venv_environ = Lm_symbol.SymbolTable.remove venv.venv_inner.venv_environ v } }

let venv_defined_env venv v =
   Lm_symbol.SymbolTable.mem venv.venv_inner.venv_environ v

let venv_options (venv : t) : Omake_options.t =
   venv.venv_inner.venv_options

let venv_with_options venv (options : Omake_options.t)  : t =
   { venv with venv_inner = { venv.venv_inner with venv_options = options } }

let venv_set_options_aux venv loc pos argv =
   let argv = Array.of_list argv in
   let add_unknown _options s =
      raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringStringError ("unknown option", s)))
   in
   let options_spec =
      Lm_arg.StrictOptions, (**)
         ["Make options", Omake_options.options_spec;
          "Output options", Omake_options.output_spec]
   in
   let options =
      try Lm_arg.fold_argv argv options_spec venv.venv_inner.venv_options add_unknown
          "Generic system builder" with
        Lm_arg.BogusArg s ->
            raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringError s))
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
      try Omake_node.NodeTable.find static node with
         Not_found ->
            (* Load it from the file *)
            let table = Static.read venv node Static.find_values in
               globals.venv_static_values <- Omake_node.NodeTable.add static node table;
               table
   in
      Lm_symbol.SymbolTable.find table v

(*
 * Define a static var.
 * Save the object on the modified list so it will get
 * written back to the file.
 *)
let venv_add_static_object venv node key obj =
   let globals = venv.venv_inner.venv_globals in
   let { venv_static_values = static;
         venv_modified_values = modified;
         _
       } = globals
   in
   let table =
      try Omake_node.NodeTable.find static node with
         Not_found ->
            Lm_symbol.SymbolTable.empty
   in
   let table = Lm_symbol.SymbolTable.add table key obj in
      globals.venv_static_values <- Omake_node.NodeTable.add static node table;
      globals.venv_modified_values <- Omake_node.NodeTable.add modified node table

(*
 * Inline the static variables into the current environment.
 *)
let venv_include_static_object venv obj =
   let { venv_dynamic = dynamic ; _} = venv in
   let dynamic = Lm_symbol.SymbolTable.fold Lm_symbol.SymbolTable.add dynamic obj in
      { venv with venv_dynamic = dynamic }

(*
 * Save the modified values.
 *)
let venv_save_static_values venv =
   let globals = venv.venv_inner.venv_globals in
      Omake_node.NodeTable.iter (fun node table ->
            try Static.write venv node (fun fd -> Static.add_values fd table)
            with Not_found ->
               ()) globals.venv_modified_values;
      globals.venv_modified_values <- Omake_node.NodeTable.empty

(************************************************************************
 * Methods and objects.
 *)

(*
 * Create a path when fetching fields, so that we
 * can hoist the exports from a method call.
 *)
(* let raise_field_error mode pos loc v = *)
(*    let print_error buf = *)
(*       Format.fprintf buf "@[<v 3>Accessing %s field: %a@ The variable was defined at the following location@ %a@]" (\**\) *)
(*          mode *)
(*          Lm_symbol.pp_print_symbol v *)
(*          Lm_location.pp_print_location loc *)
(*    in *)
(*       raise (Omake_value_type.OmakeException (pos, LazyError print_error)) *)

(* let rec squash_path_info path info = *)
(*   match path with *)
(*   |Omake_value_type.PathVar _ -> *)
(*     Omake_value_type.PathVar info *)
(*   | PathField (path, _, _) -> *)
(*     squash_path_info path info *)

(*
 * When finding a value, also construct the path to
 * the value.
 *)
let venv_find_field_path_exn _venv path obj _pos v =
   Omake_value_type.PathField (path, obj, v), Lm_symbol.SymbolTable.find obj v

let venv_find_field_path venv path obj pos v =
   try venv_find_field_path_exn venv path obj pos v with
      Not_found ->
         let pos = Pos.string_pos "venv_find_field_path" pos in
            raise (Omake_value_type.OmakeException (pos, UnboundFieldVar (obj, v)))

(*
 * Simple finding.
 *)
let venv_find_field_exn _venv obj _pos v =
   Lm_symbol.SymbolTable.find obj v

let venv_find_field venv obj pos v =
   try venv_find_field_exn venv obj pos v with
      Not_found ->
         let pos = Pos.string_pos "venv_find_field" pos in
            raise (Omake_value_type.OmakeException (pos, UnboundFieldVar (obj, v)))

(*
 * Super fields come from the class.
 *)
let venv_find_super_field venv pos loc v1 v2 =
   let table = Omake_value_util.venv_get_class venv.venv_this in
      try
         let obj = Lm_symbol.SymbolTable.find table v1 in
            venv_find_field_exn venv obj pos v2
      with
         Not_found ->
            let pos = Pos.string_pos "venv_find_super_field" (Pos.loc_pos loc pos) in
               raise (Omake_value_type.OmakeException (pos, StringVarError ("unbound super var", v2)))

(*
 * Add a field.
 *)
let venv_add_field venv obj _pos v e =
   venv, Lm_symbol.SymbolTable.add obj v e

(*
 * Hacked versions bypass translation.
 *)
let venv_add_field_internal = Lm_symbol.SymbolTable.add
let venv_defined_field_internal = Lm_symbol.SymbolTable.mem
let venv_find_field_internal_exn = Lm_symbol.SymbolTable.find
let venv_find_field_internal obj pos v =
   try Lm_symbol.SymbolTable.find obj v with
      Not_found ->
         let pos = Pos.string_pos "venv_find_field_internal" pos in
            raise (Omake_value_type.OmakeException (pos, UnboundFieldVar (obj, v)))

let venv_object_fold_internal = Lm_symbol.SymbolTable.fold

let venv_object_length = Lm_symbol.SymbolTable.cardinal

(*
 * Test whether a field is defined.
 *)
let venv_defined_field_exn _venv obj v =
   Lm_symbol.SymbolTable.mem obj v

let venv_defined_field venv obj v =
   try venv_defined_field_exn venv obj v with
      Not_found ->
         false

(*
 * Add a class to an object.
 *)
let venv_add_class obj v =
   let table = Omake_value_util.venv_get_class obj in
   let table = Lm_symbol.SymbolTable.add table v obj in
      Lm_symbol.SymbolTable.add obj Omake_value_util.class_sym (ValClass table)

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
   venv_with_object venv Lm_symbol.SymbolTable.empty

(*
 * Add the class to the current object.
 *)
let venv_instanceof obj s =
   Lm_symbol.SymbolTable.mem (Omake_value_util.venv_get_class obj) s

(*
 * Include the fields in the given class.
 * Be careful to merge classnames.
 *)
let venv_include_object_aux obj1 obj2 =
   let table1 = Omake_value_util.venv_get_class obj1 in
   let table2 = Omake_value_util.venv_get_class obj2 in
   let table = Lm_symbol.SymbolTable.fold Lm_symbol.SymbolTable.add table1 table2 in
   let obj1 = Lm_symbol.SymbolTable.fold Lm_symbol.SymbolTable.add obj1 obj2 in
      Lm_symbol.SymbolTable.add obj1 Omake_value_util.class_sym (ValClass table)

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
   Lm_symbol.SymbolTable.empty

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
    let table = Omake_value_util.venv_get_class obj in
    let table = List.fold_left (fun table v -> Lm_symbol.SymbolTable.add table v obj) table classnames in
    Lm_symbol.SymbolTable.add obj Omake_value_util.class_sym (ValClass table)

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
         venv_static = static;
         _
       } = venv
   in
   let v, objl =
      match v with
         Omake_ir.VarPrivate (_, v) ->
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
         venv_dir     = dir;
         _
       } = venv.venv_inner
   in
   let globals = venv_globals venv in
   let { venv_phonies = phonies;
         venv_mount_info = mount_info;
         _
       } = globals
   in
      Omake_node.create_node_or_phony phonies mount_info mount phony_flag dir name

let venv_intern_target venv phony_flag target =
   match target with
      Omake_value_type.TargetNode node -> node
    | TargetString name -> venv_intern venv phony_flag name

let venv_intern_cd_1 venv phony_flag dir pname =
   let mount = venv.venv_inner.venv_mount in
   let globals = venv_globals venv in
   let { venv_phonies = phonies;
         venv_mount_info = mount_info;
         _
       } = globals
   in
      Omake_node.create_node_or_phony_1 phonies mount_info mount phony_flag dir pname

let venv_intern_cd venv phony_flag dir name =
  venv_intern_cd_1 venv phony_flag dir (Omake_node.parse_phony_name name)


let venv_intern_cd_node_kind venv phony_flag dir pname =
   let globals = venv_globals venv in
   let { venv_phonies = phonies;
         _
       } = globals
   in
   if Omake_node.node_will_be_phony phonies phony_flag dir pname then
     Omake_node_sig.NodePhony
   else
     Omake_node_sig.NodeNormal




let venv_intern_rule_target venv multiple name =
  let node =
    match name with
      Omake_value_type.TargetNode node ->
      node
    | TargetString name ->
      venv_intern venv PhonyOK name
  in
  match multiple with
  | Omake_value_type.RuleScannerSingle
  | RuleScannerMultiple ->
    Omake_node.Node.create_escape NodeScanner node
  | RuleSingle
  | RuleMultiple ->
    node

let venv_intern_dir venv name =
   Omake_node.Dir.chdir venv.venv_inner.venv_dir name

(* let venv_intern_list venv names = *)
(*    List.map (venv_intern venv) names *)

let node_set_of_list nodes =
   List.fold_left Omake_node.NodeSet.add Omake_node.NodeSet.empty nodes

(* let node_set_add_names venv phony_flag nodes names = *)
(*    List.fold_left (fun nodes name -> *)
(*          Omake_node.NodeSet.add nodes (venv_intern venv phony_flag name)) nodes names *)

(* let node_set_of_names venv phony_flag names = *)
(*    node_set_add_names venv phony_flag Omake_node.NodeSet.empty names *)

(*
 * Convert back to a string.
 *)
let venv_dirname venv dir =
   if Omake_options.opt_absname venv.venv_inner.venv_options then
      Omake_node.Dir.absname dir
   else
      Omake_node.Dir.name venv.venv_inner.venv_dir dir

let venv_nodename venv dir =
   if Omake_options.opt_absname venv.venv_inner.venv_options then
      Omake_node.Node.absname dir
   else
      Omake_node.Node.name venv.venv_inner.venv_dir dir

(*
 * Add a mount point.
 *)
let venv_mount venv options src dst =
   let inner = venv.venv_inner in
   let mount = Omake_node.Mount.mount inner.venv_mount options src dst in
   let inner = { inner with venv_mount = mount } in
      { venv with venv_inner = inner }

(*
 * A target is wild if it is a string with a wild char.
 *)
let target_is_wild target =
   match target with
      Omake_value_type.TargetNode _ ->
         false
    | TargetString s ->
         Lm_wild.is_wild s

let string_of_target venv target =
  match target with
  |Omake_value_type.TargetString s ->
    s
  | Omake_value_type.TargetNode node ->
    venv_nodename venv node

(*
 * Compile a wild pattern.
 * It is an error if it isn't wild.
 *)
let compile_wild_pattern _venv pos loc target =
  match target with
  | Omake_value_type.TargetString s when Lm_wild.is_wild s ->
    if Lm_string_util.contains_any s Lm_filename_util.separators then
      raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringStringError ("filename pattern is a path", s)));
    Lm_wild.compile_in s
  | _ ->
    raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringTargetError ("patterns must be wildcards", target)))

(*
 * Compile a source.
 *)
let compile_source_core venv s =
  match s with
  | Omake_value_type.TargetNode node ->
    Omake_value_type.SourceNode node
  | TargetString s ->
    if Lm_wild.is_wild s then
      SourceWild (Lm_wild.compile_out s)
    else
      SourceNode (venv_intern venv PhonyOK s)

let compile_source venv (kind, s) =
   kind, compile_source_core venv s

let compile_implicit3_target pos loc = function
  |Omake_value_type.TargetString s ->
    if Lm_string_util.contains_any s Lm_filename_util.separators then
      raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringStringError ("target of a 3-place rule is a path", s)));
    s
  | target ->
    raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringTargetError ("target of a 3-place rule is not a simple string", target)))

(*
 * Perform a wild substitution on a source.
 *)
let subst_source_core venv dir subst source =
  match source with
  | Omake_value_type.SourceWild wild ->
    let s = Lm_wild.subst subst wild in
    venv_intern_cd venv PhonyOK dir s
  | SourceNode node ->
    node

let subst_source venv dir subst (kind, source) =
   Omake_node.Node.create_escape kind (subst_source_core venv dir subst source)

(*
 * No wildcard matching.
 *)
let intern_source venv (kind, source) =
  let source =
    match source with
    | Omake_value_type.TargetNode node ->
      node
    | TargetString name ->
      venv_intern venv PhonyOK name
  in
  Omake_node.Node.create_escape kind source

(************************************************************************
 * Rules
*)

(*
 * Symbols for directories.
 *)
(* let wild_sym            = Lm_symbol.add Lm_wild.wild_string *)
let explicit_target_sym = Lm_symbol.add "<EXPLICIT_TARGET>"

(*
 * Don't save explicit rules.
 *)
let venv_explicit_target venv target =
   venv_add_var venv Omake_var.explicit_target_var (ValNode target)

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
                        if Omake_node.Node.equal erule.rule_target target then
                           erule :: rules
                        else
                           rules) [] erules
               in
               let rules = List.rev rules in
               let () =
                  if rules = [] then
                     let print_error buf =
                        Format.fprintf buf "@[<b 3>Computed rule for `%a' produced no useful rules:" Omake_node.pp_print_node target;
                        List.iter (fun erule ->
                              Format.fprintf buf "@ `%a'" Omake_node.pp_print_node erule.rule_target) erules;
                        Format.fprintf buf "@]"
                     in
                        raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, LazyError print_error))
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
   venv_add_var venv Omake_var.wild_var v

let command_add_wild venv wild command =
   match command with
      Omake_value_type.CommandSection _ ->
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
            let v = Omake_var.create_numeric_var i in
            let venv = venv_add_var venv v arg in
               venv, succ i) (venv, 1) args
   in
      venv

let venv_add_match_args venv args =
   let venv, _ =
      List.fold_left (fun (venv, i) arg ->
            let v = Omake_var.create_numeric_var i in
            let venv = venv_add_var venv v (ValData arg) in
               venv, succ i) (venv, 1) args
   in
      venv

let venv_add_match venv line args =
   let args = List.map (fun s -> Omake_value_type.ValData s) args in
   let venv, _ =
      List.fold_left (fun (venv, i) arg ->
            let v = Omake_var.create_numeric_var i in
            let venv = venv_add_var venv v arg in
               venv, succ i) (venv, 1) args
   in
   let venv = venv_add_var venv Omake_var.zero_var (Omake_value_type.ValData line) in
   let venv = venv_add_var venv Omake_var.star_var (ValArray args) in
   let venv = venv_add_var venv Omake_var.nf_var   (ValInt (List.length args)) in
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
               String.uppercase_ascii name
            else
               name
         in
         let v = Lm_symbol.add name in
         let x = String.sub s (j + 1) (String.length s - j - 1) in
         let table = Lm_symbol.SymbolTable.add table v x in
            collect table (succ i)
   in
      collect Lm_symbol.SymbolTable.empty 0

let create options _dir exec cache =
  let cwd = Omake_node.Dir.cwd () in
  let env = create_environ () in
  let mount_info =
    { Omake_node_sig.mount_file_exists = Omake_cache.exists cache;
      mount_file_reset  = (fun node -> ignore (Omake_cache.force_stat cache node));
      mount_is_dir      = Omake_cache.is_dir cache;
      mount_digest      = Omake_cache.stat cache;
      mount_stat        = Omake_cache.stat_unix cache
    }
  in
  let globals =
    { venv_parent                     = None;
      venv_version                    = 0;
      venv_mutex                      = Lm_thread.Mutex.create "venv_globals";
      venv_exec                       = exec;
      venv_cache                      = cache;
      venv_mount_info                 = mount_info;
      venv_environments               = Lm_handle_table.create ();
      venv_files                      = Omake_node.NodeSet.empty;
      venv_directories                = Omake_node.DirTable.empty;
      venv_excluded_directories       = Omake_node.DirSet.empty;
      venv_phonies                    = Omake_node.PreNodeSet.empty;
      venv_explicit_rules             = [];
      venv_explicit_new               = [];
      venv_explicit_targets           = Omake_node.NodeTable.empty;
      venv_ordering_rules             = [];
      venv_orders                     = Lm_string_set.StringSet.empty;
      venv_memo_rules                 = Omake_value_util.ValueTable.empty;
      venv_pervasives_obj             = Lm_symbol.SymbolTable.empty;
      venv_pervasives_vars            = Lm_symbol.SymbolTable.empty;
      venv_ir_files                   = Omake_node.NodeTable.empty;
      venv_object_files               = Omake_node.NodeTable.empty;
      venv_static_values              = Omake_node.NodeTable.empty;
      venv_modified_values            = Omake_node.NodeTable.empty;
      venv_target_dirs                = Omake_node.DirTable.empty;
      venv_target_next_dir            = 0;
      venv_target_is_buildable        = TargetMap.empty;
      venv_target_is_buildable_proper = TargetMap.empty
    }
  in
  let inner =
    { venv_dir            = cwd;
      venv_environ        = env;
      venv_phony          = Omake_node.NodeSet.empty;
      venv_implicit_deps  = [];
      venv_implicit_rules = [];
      venv_globals        = globals;
      venv_options        = options;
      venv_mount          = Omake_node.Mount.empty;
      venv_included_files = Omake_node.NodeSet.empty
    }
  in
  let venv =
    { venv_this           = Lm_symbol.SymbolTable.empty;
      venv_dynamic        = Lm_symbol.SymbolTable.empty;
      venv_static         = Lm_symbol.SymbolTable.empty;
      venv_inner          = inner
    }
  in
  let venv = venv_add_phony venv (Lm_location.bogus_loc Omake_state.makeroot_name) [TargetString ".PHONY"] in
  let venv = venv_add_var venv Omake_var.cwd_var (ValDir cwd) in
  let venv = venv_add_var venv Omake_var.stdlib_var (ValDir Omake_node.Dir.lib) in
  let venv = venv_add_var venv Omake_var.stdroot_var (ValNode (venv_intern_cd venv PhonyProhibited Omake_node.Dir.lib "OMakeroot")) in
  let venv = venv_add_var venv Omake_var.ostype_var (ValString Sys.os_type) in
  let venv = venv_add_wild_match venv (ValData Lm_wild.wild_string) in
  let omakepath =
    try
      let path = Lm_string_util.split Lm_filename_util.pathsep (Lm_symbol.SymbolTable.find env Omake_symbol.omakepath_sym) in
      List.map (fun s -> Omake_value_type.ValString s) path
    with
      Not_found ->
      [ValString "."; ValDir Omake_node.Dir.lib]
  in
  let omakepath = Omake_value_type.ValArray omakepath in
  let venv = venv_add_var venv Omake_var.omakepath_var omakepath in
  let path =
    try
      let path = Lm_string_util.split Lm_filename_util.pathsep (Lm_symbol.SymbolTable.find env Omake_symbol.path_sym) in
      Omake_value_type.ValArray (List.map (fun s -> Omake_value_type.ValData s) path)
    with
      Not_found ->
      Lm_printf.eprintf "*** omake: PATH environment variable is not set!@.";
      ValArray []
  in
  let venv = venv_add_var venv Omake_var.path_var path in
  venv

(*
 * Create a fresh environment from the pervasives.
 * This is used for compiling objects.
 *)
let venv_set_pervasives venv =
   let globals = venv.venv_inner.venv_globals in
   let obj = venv.venv_dynamic in
   let loc = Lm_location.bogus_loc "Pervasives" in
   let vars =
      Lm_symbol.SymbolTable.fold (fun vars v _ ->
            Lm_symbol.SymbolTable.add vars v (Omake_ir.VarVirtual (loc, v))) Lm_symbol.SymbolTable.empty obj
   in
      globals.venv_pervasives_obj <- venv.venv_dynamic;
      globals.venv_pervasives_vars <- vars

let venv_get_pervasives venv node =
   let { venv_inner = inner ; _} = venv in
   let { venv_environ = env;
         venv_options = options;
         venv_globals = globals;
         _
       } = inner
   in
   let { 
         venv_pervasives_obj  = obj;
         _
       } = globals
   in
   let inner =
      { venv_dir            = Omake_node.Node.dir node;
        venv_environ        = env;
        venv_phony          = Omake_node.NodeSet.empty;
        venv_implicit_deps  = [];
        venv_implicit_rules = [];
        venv_globals        = globals;
        venv_options        = options;
        venv_mount          = Omake_node.Mount.empty;
        venv_included_files = Omake_node.NodeSet.empty
      }
   in
      { venv_this           = Lm_symbol.SymbolTable.empty;
        venv_dynamic        = obj;
        venv_static         = Lm_symbol.SymbolTable.empty;
        venv_inner          = inner
      }

(*
 * Fork the environment, so that changes really have no effect on the old one.
 * This is primarly used when a thread wants a private copy of the environment.
 *)
let venv_fork venv =
   let inner = venv.venv_inner in
   let globals = inner.venv_globals in
   let globals = { globals with 
                   venv_parent = Some(globals, globals.venv_version);
                   venv_mutex = Lm_thread.Mutex.create "venv_globals";
                   venv_version = 0;
                 } in
   let inner = { inner with venv_globals = globals } in
      { venv with venv_inner = inner }

let copy_var src_dynamic dst_dynamic v =
   try
      Lm_symbol.SymbolTable.add dst_dynamic v (Lm_symbol.SymbolTable.find src_dynamic v)
   with
      Not_found ->
         Lm_symbol.SymbolTable.remove dst_dynamic v

let copy_vars dst_dynamic src_dynamic vars =
   List.fold_left (copy_var src_dynamic) dst_dynamic vars

let copy_var_list =
   Omake_symbol.[stdin_sym; stdout_sym; stderr_sym]

let venv_unfork venv_dst venv_src =
   let { venv_dynamic = dst_dynamic;
         venv_inner = dst_inner;
         _
       } = venv_dst
   in
   let { venv_dynamic = src_dynamic;
         venv_inner = src_inner;
         _
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
    let loc = Lm_location.bogus_loc "venv_include_scope" in
    let { venv_this = this;
          venv_dynamic = dynamic;
          _
        } = venv
    in
    let vars = Lm_symbol.SymbolTable.mapi (fun v _ -> Omake_ir.VarThis (loc, v)) this in
    let vars = Lm_symbol.SymbolTable.fold 
        (fun vars v _ -> Lm_symbol.SymbolTable.add vars v (Omake_ir.VarGlobal (loc, v))) vars dynamic in
    vars

(*
 * Add an included file.
 *)
let venv_is_included_file venv node =
   Omake_node.NodeSet.mem venv.venv_inner.venv_included_files node

let venv_add_included_file venv node =
   let inner = venv.venv_inner in
   let inner = { inner with venv_included_files = Omake_node.NodeSet.add inner.venv_included_files node } in
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
         venv_phony = phony;
         _
       } = inner
   in
      if Omake_node.Dir.equal dir cwd then
         venv
      else
         let venv = venv_add_var venv Omake_var.cwd_var (ValDir dir) in
         let venv = venv_chdir_tmp venv dir in
         let globals = venv_globals venv in
         let phonies = globals.venv_phonies in
         let phony, phonies =
            Omake_node.NodeSet.fold (fun (phony, phonies) node ->
                  let node' = Omake_node.Node.create_phony_chdir node dir in
                  let phony = Omake_node.NodeSet.add phony node' in
                  let phonies = Omake_node.PreNodeSet.add phonies (Omake_node.Node.dest node') in
                     venv_add_explicit_dep venv loc node node';
                     phony, phonies) (Omake_node.NodeSet.empty, phonies) phony
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
   let dir = Omake_node.Dir.chdir venv.venv_inner.venv_dir dir in
      venv_chdir_dir venv loc dir

(*
 * The public version does not mess whith the phonies.
 *)
let venv_chdir_tmp venv dir =
  let cwd = venv.venv_inner.venv_dir in
  if Omake_node.Dir.equal dir cwd then
    venv
  else
    let venv = venv_add_var venv Omake_var.cwd_var (ValDir dir) in
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
      globals.venv_directories <- Omake_node.DirTable.add globals.venv_directories venv.venv_inner.venv_dir venv

let venv_directories venv =
   let globals = venv.venv_inner.venv_globals in
      Omake_node.DirSet.fold Omake_node.DirTable.remove globals.venv_directories globals.venv_excluded_directories

let venv_add_explicit_dir venv dir =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_directories <- Omake_node.DirTable.add globals.venv_directories dir venv;
      globals.venv_excluded_directories <- Omake_node.DirSet.remove globals.venv_excluded_directories dir

let venv_remove_explicit_dir venv dir =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_excluded_directories <- Omake_node.DirSet.add globals.venv_excluded_directories dir

let venv_find_target_dir_opt venv target =
   let target_dir = Omake_node.Node.dir target in
      if Omake_node.Dir.equal venv.venv_inner.venv_dir target_dir then
         Some venv
      else
         try Some (Omake_node.DirTable.find venv.venv_inner.venv_globals.venv_directories target_dir) with
            Not_found ->
               None

(*
 * When a file is read, remember it as a configuration file.
 *)
let venv_add_file venv node =
   let globals = venv.venv_inner.venv_globals in
      globals.venv_files <- Omake_node.NodeSet.add globals.venv_files node;
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
   let pos = Pos.string_pos "venv_add_implicit_deps" pos in
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
   let pos = Pos.string_pos "venv_add_implicit2_rule" pos in
   let patterns = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
      if Lm_debug.debug debug_implicit then
         Lm_printf.eprintf "@[<hv 3>venv_add_implicit2_rule:@ @[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
            Omake_value_print.pp_print_wild_list patterns
            Omake_value_print.pp_print_source_list sources;
      venv_add_implicit_rule venv loc multiple None patterns locks sources scanners values body

(*
 * Add an explicit rule.
 *)
let venv_add_explicit_rules venv pos loc multiple targets locks sources scanners values body =
   let _pos = Pos.string_pos "venv_add_explicit_rules" pos in
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
   let pos = Pos.string_pos "venv_add_implicit3_rule" pos in
   let patterns = List.map (compile_wild_pattern venv pos loc) patterns in
   let locks = List.map (compile_source venv) locks in
   let sources = List.map (compile_source venv) sources in
   let scanners = List.map (compile_source venv) scanners in
   let targets = List.map (compile_implicit3_target pos loc) targets in
   let rec check_target target = function
      pattern :: patterns ->
         begin match Lm_wild.wild_match pattern target with
            Some _ -> ()
          | None -> check_target target patterns
         end
    | [] ->
         raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringStringError ("bad match", target)))
   in
   let () = List.iter (fun target -> check_target target patterns) targets in
      if Lm_debug.debug debug_implicit then
         Lm_printf.eprintf "@[<hv 3>venv_add_implicit3_rule:@ @[<b 3>targets =%a@] @[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
            Omake_node.pp_print_string_list targets
            Omake_value_print.pp_print_wild_list patterns
            Omake_value_print.pp_print_source_list sources;
      venv_add_implicit_rule venv loc multiple (Some (Lm_string_set.StringSet.of_list targets)) patterns locks sources scanners values body

let rec is_implicit loc = function
   [] -> false
 | [target] -> target_is_wild target
 | target :: targets ->
      let imp1 = target_is_wild target in
      let imp2 = is_implicit loc targets in
         if imp1 <> imp2 then
            raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, (**)
               StringError "Rule contains an illegal mixture of implicit (pattern) targets and explicit ones"))
         else
            imp1

(*
 * Figure out what to do based on all the parts.
 * A 2-place rule is implicit if the targets do not contain a %. 3-place rules are always implicit.
 *)
let venv_add_rule venv pos loc multiple targets patterns locks sources scanners values commands =
   let pos = Pos.string_pos "venv_add_rule" pos in
      try match targets, patterns, commands with
         [], [], _ ->
            raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, StringError "invalid null rule"))
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
               raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, StringError "3-place rule does not contain patterns"))
            else
               venv_add_implicit3_rule venv pos loc multiple targets locks patterns sources scanners values commands
      with
         Failure err ->
            raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, StringError err))

(*
 * Flush the explicit list.
 *)
let venv_explicit_flush venv =
   let globals = venv.venv_inner.venv_globals in
   let { venv_explicit_rules           = erules;
         venv_explicit_targets         = targets;
         venv_explicit_new             = enew;
         _
       } = globals
   in
      if enew <> [] then
         let targets, erules =
            List.fold_left (fun (targets, erules) erule ->
                  let erules = erule :: erules in
                  let targets = Omake_node.NodeTable.add targets erule.rule_target erule in
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
   try Omake_node.NodeTable.find venv.venv_inner.venv_globals.venv_explicit_targets target with
      Not_found ->
         raise (Omake_value_type.OmakeException (pos, StringNodeError ("explicit target not found", target)))

let venv_explicit_exists venv target =
   venv_explicit_flush venv;
   Omake_node.NodeTable.mem venv.venv_inner.venv_globals.venv_explicit_targets target

let multiple_add_error errors target loc1 loc2 =
   let table = !errors in
   let table =
      if Omake_node.NodeMTable.mem table target then
         table
      else
         Omake_node.NodeMTable.add table target loc1
   in
      errors := Omake_node.NodeMTable.add table target loc2

let multiple_print_error errors buf =
   Format.fprintf buf "@[<v 3>Multiple ways to build the following targets";
   Omake_node.NodeMTable.iter_all (fun target locs ->
      let locs = List.sort Lm_location.compare locs in
         Format.fprintf buf "@ @[<v 3>%a:" Omake_node.pp_print_node target;
         List.iter (fun loc -> Format.fprintf buf "@ %a" Lm_location.pp_print_location loc) locs;
         Format.fprintf buf "@]") errors;
   Format.fprintf buf "@]"

let raise_multiple_error errors =
   let _, loc = Omake_node.NodeMTable.choose errors in
      raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, LazyError (multiple_print_error errors)))

(*
 * Get the explicit rules.  Build a table indexed by target.
 *)
let venv_explicit_rules venv =
   let errors = ref Omake_node.NodeMTable.empty in
   let add_target table target erule =
      Omake_node.NodeTable.filter_add table target (fun entry ->
            match entry with
               Some erule' ->
                  (*
                   * For .PHONY targets, multiple is ignored.
                   * Otherwise, multiple must be the same for both targets.
                   *)
                  let multiple = is_multiple_rule erule.rule_multiple in
                  let multiple' = is_multiple_rule erule'.rule_multiple in
                     if Omake_node.Node.is_phony target
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
      if not (Omake_node.NodeMTable.is_empty !errors) then
         raise_multiple_error !errors
      else
         let add_deps table target locks sources scanners =
            Omake_node.NodeTable.filter_add table target (function
               Some (lock_deps, source_deps, scanner_deps) ->
                  Omake_node.NodeSet.union lock_deps locks, Omake_node.NodeSet.union source_deps sources, Omake_node.NodeSet.union scanner_deps scanners
             | None ->
                  locks, sources, scanners)
         in
         let info =
            { explicit_targets      = Omake_node.NodeTable.empty;
              explicit_deps         = Omake_node.NodeTable.empty;
              explicit_rules        = Omake_node.NodeMTable.empty;
              explicit_directories  = venv_directories venv
            }
         in
            venv_explicit_flush venv;
            List.fold_left (fun info erule ->
                  let { rule_target   = target;
                        rule_locks    = locks;
                        rule_sources  = sources;
                        rule_scanners = scanners;
                        _
                      } = erule
                  in
                  let target_table   = add_target info.explicit_targets target erule in
                  let dep_table      = add_deps info.explicit_deps target locks sources scanners in
                     { info with explicit_targets  = target_table;
                                 explicit_deps     = dep_table;
                                 explicit_rules    = Omake_node.NodeMTable.add info.explicit_rules target erule
                     }) info (List.rev venv.venv_inner.venv_globals.venv_explicit_rules)

(*
 * Find all the explicit dependencies listed through null
 * rules.
 *)
let venv_find_implicit_deps_inner venv target =
  let target_dir  = Omake_node.Node.dir target in
  let target_name = Omake_node.Node.tail target in
  let is_scanner =
    match Omake_node.Node.kind target with
      NodeScanner -> Omake_value_type.RuleScanner
    | _ -> RuleNormal
  in
  List.fold_left (fun (lock_deps, source_deps, scanner_deps, value_deps) nrule ->
    let { inrule_multiple = multiple;
          inrule_patterns = patterns;
          inrule_locks    = locks;
          inrule_sources  = sources;
          inrule_scanners = scanners;
          inrule_values   = values;
          _
        } = nrule
    in
    if rule_kind multiple = is_scanner then
      let rec search patterns =
        match patterns with
          pattern :: patterns ->
          (match Lm_wild.wild_match pattern target_name with
            Some subst ->
            let lock_deps =
              List.fold_left (fun lock_deps source ->
                let source = subst_source venv target_dir subst source in
                Omake_node.NodeSet.add lock_deps source) lock_deps locks
            in
            let source_deps =
              List.fold_left (fun names source ->
                let source = subst_source venv target_dir subst source in
                Omake_node.NodeSet.add names source) source_deps sources
            in
            let scanner_deps =
              List.fold_left (fun scanner_deps source ->
                let source = subst_source venv target_dir subst source in
                Omake_node.NodeSet.add scanner_deps source) scanner_deps scanners
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
    (Omake_node.NodeSet.empty, Omake_node.NodeSet.empty, Omake_node.NodeSet.empty, []) venv.venv_inner.venv_implicit_deps

let venv_find_implicit_deps venv target =
   match venv_find_target_dir_opt venv target with
      Some venv ->
         venv_find_implicit_deps_inner venv target
    | None ->
         Omake_node.NodeSet.empty, Omake_node.NodeSet.empty, Omake_node.NodeSet.empty, []

(*
 * Find the commands from implicit rules.
 *)
let venv_find_implicit_rules_inner venv target =
  let target_dir  = Omake_node.Node.dir target in
  let target_name = Omake_node.Node.tail target in
  let is_scanner =
    match Omake_node.Node.kind target with
      NodeScanner -> Omake_value_type.RuleScanner
    | _ -> RuleNormal
  in
  let _ =
    if Lm_debug.debug debug_implicit then
      Lm_printf.eprintf "Finding implicit rules for %s@." target_name
  in
  let rec patt_search = function
      pattern :: patterns ->
      begin match Lm_wild.wild_match pattern target_name with
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
          if Lm_debug.debug debug_implicit then begin
            Lm_printf.eprintf "@[<hv 3>venv_find_implicit_rules: considering implicit rule for@ target = %s:@ " target_name;
            begin match irule.irule_targets with
              Some targets ->
              Lm_printf.eprintf "@[<b 3>3-place targets =%a@]@ " Omake_node.pp_print_string_list (Lm_string_set.StringSet.elements targets)
            | None ->
              ()
            end;
            Lm_printf.eprintf "@[<b 3>patterns =%a@]@ @[<b 3>sources =%a@]@]@." (**)
              Omake_value_print.pp_print_wild_list irule.irule_patterns
              Omake_value_print.pp_print_source_list irule.irule_sources
          end;
          let matches =
            match irule.irule_targets with
              None -> true
            | Some targets -> Lm_string_set.StringSet.mem targets target_name
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
            let core = Lm_wild.core subst in
            let core_val = Omake_value_type.ValData core in
            let venv = venv_add_wild_match venv core_val in
            let commands = List.map (command_add_wild venv core_val) irule.irule_body in
            let commands = make_command_info venv source_args irule.irule_values commands in
            let effects =
              List.fold_left (fun effects pattern ->
                let effect = Lm_wild.subst_in subst pattern in
                let effect = venv_intern_rule_target venv multiple (TargetString effect) in
                Omake_node.NodeSet.add effects effect) Omake_node.NodeSet.empty irule.irule_patterns
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
            if Lm_debug.debug debug_implicit then
              Lm_printf.eprintf "@[<hv 3>Added implicit rule for %s:%a@]@." (**)
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
        | Omake_value_type.TargetNode _ ->
          raise (Omake_value_type.OmakeException 
                   (Pos.loc_exp_pos loc, StringTargetError (".ORDER should be a name", target)))
        | TargetString s ->
          s
      in
      Lm_string_set.StringSet.add orders name) globals.venv_orders targets
  in
  globals.venv_orders <- orders;
  venv

(*
 * Check for order.
 *)
let venv_is_order venv name =
   Lm_string_set.StringSet.mem venv.venv_inner.venv_globals.venv_orders name

(*
 * Add an ordering rule.
 *)
let venv_add_ordering_rule venv pos loc name pattern sources =
   let pos = Pos.string_pos "venv_add_ordering_deps" pos in
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
    Omake_node.NodeSet.fold (fun deps dep ->
      let target_dir = Omake_node.Node.dir dep in
      let target_str = Omake_node.Node.tail dep in
      List.fold_left (fun deps orule ->
        let { orule_pattern = pattern;
              orule_sources = sources;
              _
            } = orule
        in
        match Lm_wild.wild_match pattern target_str with
          Some subst ->
          List.fold_left (fun deps source ->
              let source = subst_source_core venv target_dir subst source in
              Omake_node.NodeSet.add deps source) deps sources
        | None ->
          deps) deps orules) deps deps
  in
  let rec fixpoint deps =
    let deps' = step deps in
    if Omake_node.NodeSet.cardinal deps' = Omake_node.NodeSet.cardinal deps then
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
let venv_add_memo_rule venv _pos loc _multiple is_static key vars sources values body =
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
      let _, v = Omake_ir_util.var_of_var_info info in
      venv_add_var venv info (ValDelayed (ref (Omake_value_type.ValStaticApply (key, v))))) venv vars
  in
  globals.venv_memo_rules <- Omake_value_util.ValueTable.add globals.venv_memo_rules key (StaticRule srule);
  venv

(*
 * Force the evaluation.
 *)
let venv_set_static_info venv key v =
   let globals = venv_globals venv in
      globals.venv_memo_rules <- Omake_value_util.ValueTable.add globals.venv_memo_rules key v

let venv_find_static_info venv pos key =
   try Omake_value_util.ValueTable.find venv.venv_inner.venv_globals.venv_memo_rules key with
      Not_found ->
         raise (Omake_value_type.OmakeException (pos, StringValueError ("Static section not defined", key)))

(************************************************************************
 * Return values.
 *)

(*
 * Export an item from one environment to another.
 *)
let copy_var pos dst src v =
   try Lm_symbol.SymbolTable.add dst v (Lm_symbol.SymbolTable.find src v) with
      Not_found ->
         raise (Omake_value_type.OmakeException (pos, UnboundVar v))

let export_item pos venv_dst venv_src = function
  | Omake_ir.ExportVar (VarPrivate (_, v)) ->
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
          venv_this    = this_src;
          _
        } = venv_src
    in
    let { venv_dynamic = dynamic_dst;
          venv_static  = static_dst;
          venv_this    = this_dst;
          _
        } = venv_dst
    in
    let dynamic, found =
      try Lm_symbol.SymbolTable.add dynamic_dst v (Lm_symbol.SymbolTable.find dynamic_src v), true with
        Not_found ->
        dynamic_dst, false
    in
    let static, found =
      try Lm_symbol.SymbolTable.add static_dst v (Lm_symbol.SymbolTable.find static_src v), true with
        Not_found ->
        static_dst, found
    in
    let this, found =
      try Lm_symbol.SymbolTable.add this_dst v (Lm_symbol.SymbolTable.find this_src v), true with
        Not_found ->
        this_dst, found
    in
    if not found then
      raise (Omake_value_type.OmakeException (pos, UnboundVar v));
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
  |Omake_ir.ExportNone ->
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
  | Omake_value_type.PathVar v ->
    venv_add_var venv v (ValObject obj)
  | PathField (path, parent_obj, v) ->
    let obj = Lm_symbol.SymbolTable.add parent_obj v (ValObject obj) in
    hoist_path venv path obj

let hoist_this venv_orig venv_obj path =
   let venv = { venv_obj with venv_this = venv_orig.venv_this } in
      hoist_path venv path venv_obj.venv_this

let add_path_exports venv_orig venv_dst venv_src pos path ( x : Omake_ir.export) =
  match x with
  | ExportNone ->
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
