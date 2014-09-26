
module Pos =  Omake_pos.Make (struct let name = "Omake_build_util" end)

 (*
 * Check whether a node is a leaf node.
*)
let is_leaf_command 
    ({ command_scanner_deps = scanner_deps;
       command_static_deps  = static_deps;
       command_build_deps   = build_deps;
       command_lines        = lines;
       _
     } : Omake_build_type.command) =

  Omake_node.NodeSet.is_empty scanner_deps
  && Omake_node.NodeSet.is_empty static_deps
  && Omake_node.NodeSet.is_empty build_deps
  && (lines = CommandNone)

let is_leaf_node (env : Omake_build_type.t)  node =
  try is_leaf_command (Omake_node.NodeTable.find env.env_commands node) with
    Not_found -> false

(*
 * Maintaining the environment.
*)
let saved_env = ref None

let set_env env =
  saved_env := Some env

let get_env pos loc =
  match !saved_env with
  | Some env -> env
  | None ->
    raise (Omake_value_type.OmakeException 
             (Pos.loc_pos loc pos, StringError "this function can be called only in rule bodies"))

let is_build_phase () =
  !saved_env <> None


 (*
 * Command-line definitions.
*)
let command_defs = ref []

let add_command_def v s =
  command_defs := (v, s) :: !command_defs

let command_defs_are_nonempty () =
  !command_defs <> []

(************************************************************************
 * Run-time variables.
 *
 * Strip the leading qualifiers.
 * This is a big hack, repeating Omake_ir_ast.
 * We may want to move this into there.
*)
let parse_path _ venv pos loc s =
  let vl = List.map Lm_symbol.add (Lm_string_util.split "." s) in
  match Omake_ir_ast.parse_declaration venv pos loc vl with
  | NameEmpty _ ->
    raise (Omake_value_type.OmakeException (pos, StringError "empty name"))

  | NameMethod (_, v, _ :: _) ->
    raise (Omake_value_type.OmakeException (pos, StringVarError ("name has too many components", v)))

  | NameMethod (info, v, vl) ->
    let info : Omake_ir.var_info =
      match info.name_scope with
      | Some VarScopePrivate ->
        VarPrivate (loc, v)
      | Some VarScopeThis ->
        VarThis (loc, v)
      | Some VarScopeVirtual
      | None ->
        VarVirtual (loc, v)
      | Some VarScopeGlobal ->
        VarGlobal (loc, v)
    in
    info, vl

let parse_sym =
  parse_path (fun loc v -> Omake_ir.VarThis (loc, v))

let parse_def venv pos loc s =
  let v, vl =
    parse_path (fun loc v ->
        Omake_ir.VarVirtual (loc, v)) venv pos loc s
  in
  if vl <> [] then
    raise (Omake_value_type.OmakeException (pos, StringError "name has too many components"));
  v


let venv_add_command_defs venv =
  let loc = Lm_location.bogus_loc "<command-line>" in
  let pos = Pos.string_pos "venv_add_command_defs" (Pos.loc_exp_pos loc) in
  List.fold_left (fun venv (v, s) ->
      let v = parse_def venv pos loc v in
      Omake_env.venv_add_var venv v (Omake_value_type.ValString s)) venv !command_defs

(*
 * Extend an object with another.
 * The argument may be a file or an object.
*)
  let object_of_file venv pos loc s : Omake_value_type.obj =
  let pos  = Pos.string_pos "extends" pos in
  let node = Omake_eval.find_include_file venv pos loc s in
    try Omake_env.venv_find_object_file_exn venv node with
    Not_found ->
    let obj = Omake_eval.eval_object_file venv pos loc node in
    Omake_env.venv_add_object_file venv node obj;
obj

(*
 * This is a totally different sorting algorithm than that used in
 * revision 1.2.
 *
 * Here is the new assumption: only direct dependencies matter.
 * That is, the transitive closure is not needed for nodes outside
 * the set being sorted.
 *
 * This version uses a simple DFS to order the nodes.
 *
 * The numbers in IntNodeCompare are the sequence number of the node
 * in the input list. They are used to make the output order as similar
 * to the input one as possible (http://bugzilla.metaprl.org/show_bug.cgi?id=376)
 *)
module IntNodeCompare = struct
   type t = int * Omake_node.Node.t

   let compare (i1, n1) (i2, n2) =
      match i1 - i2 with
         0 ->
            Omake_node.Node.compare n1 n2
       | i -> i
end

module IntNodeSet = Lm_set.LmMake (IntNodeCompare)
module IntNodeTable = Lm_map.LmMake (IntNodeCompare)

(*
 * Get the dependencies for this set of names.
 *)
let command_deps venv orules domain deps =
   let deps =  Omake_env.venv_get_ordering_deps venv orules deps in
      Omake_node.NodeSet.fold (fun deps dep ->
            if Omake_node.NodeTable.mem domain dep then
               IntNodeSet.add deps (Omake_node.NodeTable.find domain dep, dep)
            else
               deps) IntNodeSet.empty deps

(*
 * Build the subgraph, including only those nodes that we actually
 * care about.
 *)
let build_subgraph (env : Omake_build_type.t) venv pos orules domain =
   Omake_node.NodeTable.fold (fun graph node i ->
         try
            let command = Omake_node.NodeTable.find env.env_commands node in
            let deps = command_deps venv orules domain command.command_build_deps in
            let node = i, node in
               IntNodeTable.add graph node (IntNodeSet.remove deps node)
         with
            Not_found ->
               raise (Omake_value_type.OmakeException (pos, StringNodeError ("file is not found", node)))) IntNodeTable.empty domain

let print_cycle wl (_, node) buf =
   let rec print = function
      [] -> 
         raise (Invalid_argument "Omake_build_util: internal_error")
    | ((_, node'), _) :: wl ->
         if not (Omake_node.Node.equal node node') then
            print wl;
         Format.fprintf buf "%a@ > " Omake_node.pp_print_node node';
   in
      Format.fprintf buf "@[<hv 3>Sort failed: found a cycle:@ ";
      print wl;
      Format.fprintf buf "%a@]" Omake_node.pp_print_node node

(*
 * Produce a sort in DFS order.
 *
 * graph - the dependencies of the nodes not touched yet
 * marked - the nodes currently in the work list. "Touching" a marked node again means we found a loop.
 * items - the list constructed so far
 * in_list - the set of nodes in the items list
 * last argument - the "backtrace" (work list).
 *)
let rec dfs_sort_aux pos graph marked items = function
   ((node, deps) :: bt) as all_bt ->
      if IntNodeSet.is_empty deps then
         (* Pop the work list *)
         dfs_sort_aux pos graph (IntNodeSet.remove marked node) (snd node :: items) bt
      else
         let node' = IntNodeSet.choose deps in
            if IntNodeSet.mem marked node' then
               raise (Omake_value_type.OmakeException (pos, LazyError (print_cycle all_bt node')))
            else
               let bt = (node, IntNodeSet.remove deps node') :: bt in
                  if IntNodeTable.mem graph node' then
                     let deps = IntNodeTable.find graph node' in
                     let graph = IntNodeTable.remove graph node' in
                     let marked = IntNodeSet.add marked node' in
                        dfs_sort_aux pos graph marked items ((node', deps) :: bt)
                  else
                     (* node' is already in the items list *)
                     dfs_sort_aux pos graph marked items bt
 | [] ->
      if IntNodeTable.is_empty graph then
         (* We are done! *)
         List.rev items
      else
         (* Pick a starting point and start adding it to the output *)
         let node, deps = IntNodeTable.choose graph in
         let graph = IntNodeTable.remove graph node in
         let marked = IntNodeSet.singleton node in
            dfs_sort_aux pos graph marked items [node, deps]

let dfs_sort pos graph _ =
   if IntNodeTable.is_empty graph then
      []
   else
      dfs_sort_aux pos graph IntNodeSet.empty [] []

(*
 * Check that a list of nodes is in sorted order.
 *)
let check_sort pos graph domain =
  Omake_node.NodeTable.iter (fun node index ->
      let deps = IntNodeTable.find graph (index, node) in
      IntNodeSet.iter (fun (index', dep) ->
          if index' > index then
            let print_problem buf =
              Format.fprintf buf "@[<hv 3>Nodes are out of order:@ Node %a@ Depends on %a@]" (**)
                Omake_node.pp_print_node node
                Omake_node.pp_print_node dep
            in
            raise (Omake_value_type.OmakeException (pos, LazyError print_problem))) deps) domain

(*
 * The main sorting function.
 *)
let sort_aux sorter env venv pos name nodes =
   let pos = Pos.string_pos "sort" pos in

   (* Get extra ordering info *)
   let oinfo =  Omake_env.venv_get_ordering_info venv name in

   (* Produce a table of the listing order *)
   let domain, _ =
      List.fold_left (fun (domain, i) node ->
            let domain = Omake_node.NodeTable.add domain node i in
            let i = succ i in
               domain, i) (Omake_node.NodeTable.empty, 0) nodes
   in

   (* Build the graph *)
   let graph = build_subgraph env venv pos oinfo domain in
      sorter pos graph domain

(*
 * Top-level functions.
 *)
let check_sort = sort_aux check_sort
let sort = sort_aux dfs_sort







(*
 * Get the list pointer for a node class.
 *)
let command_tag : Omake_build_type.command_state -> Omake_build_type.command_tag = function
  | CommandIdle            -> CommandIdleTag
  | CommandInitial         -> CommandInitialTag
  | CommandScanBlocked     -> CommandScanBlockedTag
  | CommandScannedPending  -> CommandScannedPendingTag
  | CommandScanned         -> CommandScannedTag
  | CommandBlocked         -> CommandBlockedTag
  | CommandReady           -> CommandReadyTag
  | CommandPending         -> CommandPendingTag
  | CommandRunning _       -> CommandRunningTag
  | CommandSucceeded _     -> CommandSucceededTag
  | CommandFailed _        -> CommandFailedTag

let get_worklist_command (wl : Omake_build_type.env_wl)
    (x : Omake_build_type.command_tag) = 
  match x with 
  | CommandIdleTag           -> wl.env_idle_wl
  | CommandInitialTag        -> wl.env_initial_wl
  | CommandScanBlockedTag    -> wl.env_scan_blocked_wl
  | CommandScannedPendingTag -> wl.env_scanned_pending_wl
  | CommandScannedTag        -> wl.env_scanned_wl
  | CommandBlockedTag        -> wl.env_blocked_wl
  | CommandReadyTag          -> wl.env_ready_wl
  | CommandPendingTag        -> wl.env_pending_wl
  | CommandRunningTag        -> wl.env_running_wl
  | CommandSucceededTag      -> wl.env_succeeded_wl
  | CommandFailedTag         -> wl.env_failed_wl

let command_worklist (env : Omake_build_type.t) state =
  get_worklist_command env.env_current_wl state

(*
 * Worklist creation.
 *)
let create_wl () : Omake_build_type.env_wl=
  { env_idle_wl            = ref None;
    env_initial_wl         = ref None;
    env_scan_blocked_wl    = ref None;
    env_scanned_pending_wl = ref None;
    env_scanned_wl         = ref None;
    env_blocked_wl         = ref None;
    env_ready_wl           = ref None;
    env_pending_wl         = ref None;
    env_running_wl         = ref None;
    env_succeeded_wl       = ref None;
    env_failed_wl          = ref None
  }

(*
 * Check if a command succeeded.
 *)
let command_succeeded (command : Omake_build_type.command) =
  match command with
  | { command_state = CommandSucceeded _; _ } ->
    true
  | _ ->
    false
(*
 * Test for empty.
 *)
let command_list_is_empty env state =
  let l = command_worklist env state in
  match !l with
  | Some _ -> false
  | None -> true

(*
 * Get the head of a list.
 *)
let command_list_head env state =
  let l = command_worklist env state in
  match !l with
    Some command -> command
  | None -> raise (Invalid_argument "command_list_head")


(*
 * Iterate through the node list.
 *)
let command_iter env state f =
  let rec iter (command_opt : Omake_build_type.command option) =
    match command_opt with
    | Some command ->
      let next = !(command.command_succ) in
      f command;
      iter next
    | None ->
      ()
  in
  iter (!(command_worklist env state))


(*
 * Fold through the command list.
 *)
let command_fold env state f x =
  let rec fold x (command : Omake_build_type.command option) =
    match command with
    | Some command ->
      let next = !(command.command_succ) in
      let x = f x command in
      fold x next
    | None ->
      x
  in
  let x = fold x (!(command_worklist env state)) in
  if env.env_main_wl == env.env_current_wl then
    x
  else
    fold x (!(get_worklist_command env.env_main_wl state))

(*
 * Existential test.
 *)
let command_exists env state f x =
  let rec exists (command : Omake_build_type.command option) =
    match command with
    | Some command ->
      let next = !(command.command_succ) in
      f command || exists next
    | None ->
      x
  in
  exists (!(command_worklist env state))

(*
 * Find a particular command.
 *)
let command_find env state f =
  let rec find (command : Omake_build_type.command option) =
    match command with
    | Some command ->
      if f command then
        command
      else
        find (!(command.command_succ))
    | None ->
      raise Not_found
  in
  find (!(command_worklist env state))

(*
 * Test for empty.
 *)
let command_list_is_empty env state =
  let l = command_worklist env state in
  match !l with
  | Some _ -> false
  | None -> true

(*
 * Get the head of a list.
 *)
let command_list_head env state =
  let l = command_worklist env state in
  match !l with
    Some command -> command
  | None -> raise (Invalid_argument "command_list_head")


let pp_print_command_state buf (state : Omake_build_type.command_state)  =
  match state with
  | CommandIdle                      -> Lm_printf.pp_print_string buf "idle"
  | CommandInitial                   -> Lm_printf.pp_print_string buf "initial"
  | CommandScanBlocked               -> Lm_printf.pp_print_string buf "scan-blocked"
  | CommandScannedPending            -> Lm_printf.pp_print_string buf "scanned-pending"
  | CommandScanned                   -> Lm_printf.pp_print_string buf "scanned"
  | CommandBlocked                   -> Lm_printf.pp_print_string buf "blocked"
  | CommandPending                   -> Lm_printf.pp_print_string buf "pending"
  | CommandReady                     -> Lm_printf.pp_print_string buf "ready"
  | CommandRunning (pid, None)       -> Format.fprintf buf "running(%a)" Omake_exec_id.pp_print_pid pid
  | CommandRunning (pid, Some name)  -> Format.fprintf buf "scanning(%a, %s)" Omake_exec_id.pp_print_pid pid name
  | CommandSucceeded _               -> Lm_printf.pp_print_string buf "succeeded"
  | CommandFailed code               -> Format.fprintf buf "failed(%d)" code

(* let pp_print_command_opt buf (command_opt : Omake_build_type.command option) = *)
(*   match command_opt with *)
(*   | Some { command_target = target; *)
(*            command_state = state; _} ->  *)
(*     Format.fprintf buf "%a[%a]" Omake_node.pp_print_node target pp_print_command_state state *)
(*   | None -> *)
(*     Lm_printf.pp_print_string buf "<none>" *)

let pp_print_command buf (command : Omake_build_type.command) =
  match command with 
    { command_target       = target;
      command_effects      = effects;
      command_locks        = locks;
      command_state        = state;
      command_scanner_deps = scanner_deps;
      command_build_deps   = build_deps;
      command_blocked      = blocked;
      _
    } -> 
    Format.fprintf buf "@[<v 3>%a[%a],@ @[<b 3>effects =%a@]@ @[<b 3>locks =%a@]@ @[<b 3>scanner deps =%a@]@ @[<b 3>build deps =%a@]@ @[<b 3>blocked =%a@]@]" (**)
      Omake_node.pp_print_node target
      pp_print_command_state state
      Omake_node.pp_print_node_set effects
      Omake_node.pp_print_node_set locks
      Omake_node.pp_print_node_set scanner_deps
      Omake_node.pp_print_node_set build_deps
      Omake_node.pp_print_node_list blocked

let pp_print_node_states (env : Omake_build_type.t) buf nodes =
  Omake_node.NodeSet.iter (fun target ->
    try
      let command = Omake_node.NodeTable.find env.env_commands target(* find_command env target *) in
      Format.fprintf buf "@ %a(%a)" (**)
        Omake_node.pp_print_node target
        pp_print_command_state command.command_state
    with
      Not_found ->
      Omake_node.pp_print_node buf target) nodes


let print_stats (env : Omake_build_type.t) message start_time =
  match env with { 
    env_venv = venv;
    env_cache = cache;
    env_scan_count = scan_count;
    env_scan_exec_count = scan_exec_count;
    env_rule_count = rule_count;
    env_rule_exec_count = rule_exec_count;
    _
  } -> 
    let stat_count, digest_count = Omake_cache.stats cache in
    let total_time = Unix.gettimeofday () -. start_time in
    let options = Omake_env.venv_options venv in
    Omake_exec_print.print_leaving_current_directory options;
    if Omake_options.opt_print_status options then begin
      if message <> "done" then begin
        let total = Omake_node.NodeTable.cardinal env.env_commands - env.env_optional_count in
        Lm_printf.printf "*** omake: %i/%i targets are up to date@." env.env_succeeded_count total
      end;
      Lm_printf.printf "*** omake: %s (%a, %d/%d scans, %d/%d rules, %d/%d digests)@." (**)
        message Lm_unix_util.pp_time total_time
        scan_exec_count scan_count
        rule_exec_count rule_count
        digest_count stat_count
    end
(*
 * All of the commands in the Blocked queue are deadlocked.
 *)

let print_deadlock_exn env buf state =
  (* Inconsistency *)
  let failwith_inconsistency (command : Omake_build_type.command) =
    match command with { command_target       = target;
                         command_state        = state;
                         command_effects      = effects;
                         command_scanner_deps = scanner_deps;
                         command_static_deps  = static_deps;
                         command_build_deps   = build_deps;
                         command_loc          = loc;
                         _
                       } -> 
      Format.fprintf buf "@[<v 3>*** omake: inconsistent state %a@ state = %a@ @[<b 3>effects =%a@]@ @[<b 3>build deps =%a@]@ @[<b 3>scanner deps =%a@]@ @[<b 3>static deps = %a@]@." (**)
        Omake_node.pp_print_node target
        pp_print_command_state state
        (pp_print_node_states env) effects
        (pp_print_node_states env) build_deps
        (pp_print_node_states env) scanner_deps
        (pp_print_node_states env) static_deps;
      raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, StringNodeError ("failed on target", target)))
  in

  (* Deadlock *)
  let failwith_deadlock loc target marked =
    let rec print_marked marked =
      match marked with
        mark :: marked ->
        Format.fprintf buf "*** omake: is a dependency of %a@." Omake_node.pp_print_node mark;
        if not (Omake_node.Node.equal mark target) then
          print_marked marked
      | [] ->
        Format.fprintf buf "*** omake: not deadlocked!@."
    in
    Format.fprintf buf "*** omake: deadlock on %a@." Omake_node.pp_print_node target;
    print_marked marked;
    raise (Omake_value_type.OmakeException (Pos.loc_exp_pos loc, StringNodeError ("failed on target", target)))
  in

  (*
   * Find the deadlock.
   *)
  let rec print marked (command : Omake_build_type.command) =
    match command with { command_target = target;
                         command_loc = loc;
                         _
                       } -> 

      (*
     * Find the first dependency that has not been built.
     *)
      let rec search deps' =
        match deps' with
          dep :: deps ->
          let command =
            try
              let command = Omake_node.NodeTable.find env.env_commands dep (* find_command env dep *) in
              if command_succeeded command then
                None
              else
                Some command
            with
              Not_found ->
              Format.fprintf buf "*** omake: Do not know how to build \"%a\" required for \"%a\"@." Omake_node.pp_print_node dep Omake_node.pp_print_node target;
              raise (Failure "blocked")
          in
          (match command with
            Some dep ->
            dep
          | None ->
            search deps)
        | [] ->
          (* All deps have succeeded; this is an inconsistent state *)
          failwith_inconsistency command
      in
      (* Detect deadlock *)
      if List.exists (fun node -> Omake_node.Node.equal node target) marked then
        failwith_deadlock loc target marked;

      (* Otherwise, search for first unsatisfied dependency *)
      let deps =
        Omake_node.NodeSet.union (**)
          (Omake_node.NodeSet.union command.command_build_deps command.command_scanner_deps)
          command.command_static_deps
      in
      print (target :: marked) (search (Omake_node.NodeSet.to_list deps))
  in
  print [] (command_list_head env state)


let print_deadlock env buf state =
  try print_deadlock_exn env buf state with
    Omake_value_type.OmakeException _
  | Failure _ as exn ->
    Format.fprintf buf "%a@." Omake_exn_print.pp_print_exn exn

(*
 * Print the failed commands.
 *)
let print_failed_targets (env : Omake_build_type.t) buf =
  if Omake_options.opt_print_status (Omake_env.venv_options env.env_venv) then begin
    Format.fprintf buf "*** omake: targets were not rebuilt because of errors:";
    (* We use table to get an alphabetical order here - see http://bugzilla.metaprl.org/show_bug.cgi?id=621 *)
    let table = ref Lm_string_set.LexStringMTable.empty in
    let add_command (command : Omake_build_type.command) =
      table := Lm_string_set.LexStringMTable.add !table (Omake_node.Node.absname command.command_target) command
    in
    let () = command_iter env CommandFailedTag add_command in
    Lm_string_set.LexStringMTable.iter (fun _ (command : Omake_build_type.command) ->
      Format.fprintf buf "@\n   @[<v 3>@[<v 3>%a" Omake_node.pp_print_node command.command_target;
      Omake_node.NodeSet.iter (fun dep ->
        if Omake_node.Node.is_real dep && is_leaf_node env dep then
          Format.fprintf buf "@ depends on: %a" Omake_node.pp_print_node dep) command.command_static_deps;
      Format.fprintf buf "@]";
      Omake_build_tee.format_tee_with_nl buf command;
      Format.fprintf buf "@]") !table;
    Format.fprintf buf "@."
  end

let print_failed env buf state =
  if not (command_list_is_empty env CommandFailedTag) then
    print_failed_targets env buf
  else
    print_deadlock env buf state

let eprint_env env = 
  begin 
    Format.eprintf "@[<hv 3>Initial:";
    command_iter env CommandInitialTag (fun command ->
      Format.eprintf "@ %a" pp_print_command command);
    Format.eprintf "@]@.";
    Format.eprintf "@[<hv 3>ScanBlocked:";
    command_iter env CommandScanBlockedTag (fun command ->
      Format.eprintf "@ %a" pp_print_command command);
    Format.eprintf "@]@.";
    Format.eprintf "@[<hv 3>Blocked:";
    command_iter env CommandBlockedTag (fun command ->
      Format.eprintf "@ %a" pp_print_command command);
    Format.eprintf "@]@.";
    Format.eprintf "@[<hv 3>Ready:";
    command_iter env CommandReadyTag (fun command ->
      Format.eprintf "@ %a" pp_print_command command);
    Format.eprintf "@]@.";
    Format.eprintf "@[<hv 3>Running:";
    command_iter env CommandRunningTag (fun command ->
      Format.eprintf "@ %a" pp_print_command command);
    Format.eprintf "@]@.";
    Format.eprintf "@[<hv 3>Succeeded:";
    command_iter env CommandSucceededTag (fun command ->
      Format.eprintf "@ %a" pp_print_command command);
    Format.eprintf "@]@.";
    Format.eprintf "@[<hv 3>Failed:";
    command_iter env CommandFailedTag (fun command ->
      Format.eprintf "@ %a" pp_print_command command);
    Format.eprintf "@]@.";
  end

(** [TODO]
  Take a lock to prevent multiple builds from competing.
 *)
let copy_to_stderr fd =
  let inx = Unix.in_channel_of_descr fd in
  let rec loop () =
    let line = input_line inx in
    Format.eprintf "%s@." line;
    loop ()
  in
  try loop () with
    End_of_file ->
    ()

let wait_for_lock, unlock_db =
  let name = Omake_state.db_name ^ ".lock" in
  let save_fd = ref None in
  let unlock_db () =
    match !save_fd with
    | None -> ()
    | Some fd ->
      let () =
        (* XXX: JYH: this is bad style.
         * Under what circumstances will this fail?
         * BTW, don't use wildcard exception patterns please:/ *)
        try Omake_shell_sys.close_fd fd with
          Unix.Unix_error _ ->
          () in
      save_fd := None
  in
  let wait_for_lock () =
    unlock_db ();
    let fd =
      try Lm_unix_util.openfile name [O_RDWR; O_CREAT] 0o666 with
        Unix.Unix_error _ ->
        raise (Failure ("project lock file is not writable: " ^ name))
    in
    let () =
      (*
       * XXX: TODO: We use lockf, but it is not NFS-safe if filesystem is mounted w/o locking.
       * .omakedb locking is only convenience, not safety, so it's not a huge problem.
       * But may be we should implement a "sloppy" locking as well - see
       * also the mailing list discussions:
       *    - http://lists.metaprl.org/pipermail/omake/2005-November/thread.html#744
       *    - http://lists.metaprl.org/pipermail/omake-devel/2005-November/thread.html#122
       *)
      try
        (* Try for a lock first, and report it if the file is locked *)
        try Lm_unix_util.lockf fd Unix.F_TLOCK 0 with
          Unix.Unix_error (Unix.EAGAIN, _, _) ->
          Format.eprintf "*** omake: the project is currently locked.@.";
          (try copy_to_stderr fd with _ -> ());

          (* Unfortunately, we have to poll, since OCaml doesn't allow ^C during the lock request *)
          let rec poll col =
            let col =
              if col >= 40 then begin
                if col = 40 then Format.eprintf "@.";
                Format.eprintf "*** omake: waiting for project lock: .@?";
                0
              end
              else begin
                Format.eprintf ".@?";
                succ col
              end
            in
            Unix.sleep 1;
            try Lm_unix_util.lockf fd Unix.F_TLOCK 0 with
              Unix.Unix_error (Unix.EAGAIN, _, _) ->
              poll col
          in
          poll 1000
      with
        (*
         * XXX: When lockf is not supported, we just print a warning and keep going.
         *      .omakedb locking is only convenience, not safety, so it's not a huge problem.
         *)
        Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOLCK) as err, _, _) ->
        Format.eprintf "*** omake WARNING: Can not lock the project database file .omakedb:\
                        \t%s. Will proceed anyway.\
                        \tWARNING: Be aware that simultaneously running more than one instance\
                        \t\tof OMake on the same project is not recommended.  It may\
                        \t\tresult in some OMake instances failing to record their\
                        \t\tprogress in the database@."
          (Unix.error_message err)
      | Unix.Unix_error (err, _, _) ->
        raise (Failure ("Failed to lock the file " ^ name ^ ": " ^ (Unix.error_message err)))
      | Failure err ->
        raise (Failure ("Failed to lock the file " ^ name ^ ": " ^ err))
    in
    Omake_shell_sys.set_close_on_exec fd;
    save_fd := Some fd;
    (* Print the message to the lock file  *)
    try
      ignore (Unix.lseek fd 0 Unix.SEEK_SET);
      Lm_unix_util.ftruncate fd;
      let outx = Unix.out_channel_of_descr fd in
      Printf.fprintf outx "*** omake: the project was last locked by %s:%d.\n" (Unix.gethostname ()) (Unix.getpid ());
      Pervasives.flush outx
    with
      Unix.Unix_error _
    | Sys_error _
    | Failure _ ->
      ()
  in
  wait_for_lock, unlock_db

(*
 * Catch the dependency printer.
 *)



let close (env : Omake_build_type.t) =
  Omake_node.NodeTable.iter 
    (fun _ command -> Omake_build_tee.unlink_tee command) env.env_commands;
  Omake_exec.Exec.close env.env_exec;
  Lm_unix_util.try_unlink_file env.env_summary



(*
let env_options (env : Omake_build_type.t) =
  Omake_env.venv_options env.env_venv


let pid = Unix.getpid () (* this is the PID of the main thread *)

(*
 * Save the cache and environment to a file.
 *)
let save_aux (env : Omake_build_type.t) =
  (* Only the "master" thread should be saving the DB *)
  if (pid <> Unix.getpid ()) then begin
    Format.eprintf "@[<hv3>*** OMake Internal ERROR:@ Slave thread %i trying to save db opened by the master thread %i@]@." (Unix.getpid ()) pid;
    raise (Invalid_argument "Internal error: Slave thread trying to save the OMake DB")
  end;

  (* Save the static values *)
  let () = Omake_env.venv_save_static_values env.env_venv in

  (* Save the .omakedb *)
  let cache = env.env_cache in

  (* We want the name to be fairly unique in case locking had failed us. *)
  let db_tmp = Lm_printf.sprintf ".#%s.%s.%i" Omake_state.db_name (Unix.gethostname ()) pid in

  (* Marshal the state to the output file *)
  let outx = Pervasives.open_out_bin db_tmp in
  let includes =
    Omake_node.NodeTable.fold (fun includes node _ ->
      Omake_node.NodeSet.add includes node) Omake_node.NodeSet.empty env.env_includes
  in
  let targets = Omake_node.NodeSet.singleton Omake_cache.env_target in
  try
    Omake_cache.add cache Omake_cache.env_fun
      Omake_cache.env_target targets includes None (MemoSuccess Omake_node.NodeTable.empty);
    Omake_cache.to_channel outx cache;
    close_out outx;
    Unix.rename db_tmp Omake_state.db_name
  with
    Unix.Unix_error (errno, name, arg) ->
    Format.eprintf "*** omake: failure during saving: %s: %s(%s)@." (Unix.error_message errno) name arg;
    close_out outx;
    unlink_file db_tmp
  | Sys_error _
  | Failure _ as exn ->
    Format.eprintf "*** omake: failure during saving: %s@." (Printexc.to_string exn);
    close_out outx;
    unlink_file db_tmp

(*
 * Save to the .omakedb.
 *)
let save env =
  if not (env_options env).dry_run then
    try save_aux env with
      Sys_error _ as exn ->
      Format.eprintf "*** omake: failure during saving: %s@." (Printexc.to_string exn)
*)
