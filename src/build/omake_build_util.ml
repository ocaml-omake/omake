
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

let is_leaf_node (env : Omake_build_type.env)  node =
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
let build_subgraph (env : Omake_build_type.env) venv pos orules domain =
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
