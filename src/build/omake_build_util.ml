
include Omake_pos.Make (struct let name = "Omake_build_util" end)


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
   let pos = string_pos "sort" pos in

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
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
