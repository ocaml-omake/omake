(*  Utilities on targets. *)

include Omake_pos.Make (struct let name = "Omake_target" end)

(*  Target exists or is phony. *)
let target_exists_or_is_phony cache target =
   Omake_cache.exists cache target || Omake_node.Node.is_phony target

(*  Target is part of an explicit rule. *)
let target_is_explicit _ venv target =
   Omake_env.venv_explicit_exists venv target

(*  Target exists, is phony, or there is an explicit rule to build it. *)
let target_exists_or_is_phony_or_is_explicit cache venv target =
   if Lm_debug.debug Omake_env.debug_implicit then
      Format.eprintf "target_exists_or_is_phony_or_is_explicit: %a: %b, %b@." (**)
         Omake_node.pp_print_node target
         (target_exists_or_is_phony cache target)
         (Omake_env.venv_explicit_exists venv target);
   target_exists_or_is_phony cache target || Omake_env.venv_explicit_exists venv target

let icnt_limit = 3

(*
 * A target is buildable if it exists, or
 * if there is an implicit rule whose dependencies
 * are all buildable.
 *)
let rec target_is_buildable_bound bound bound_l icnt cache venv pos target =
   let target = Omake_node.Node.unsquash target in
   (* Check for loops *)
   let bound =
     if icnt >= icnt_limit then
       List.fold_left Omake_node.NodeSet.add bound bound_l
     else
       bound in
   if icnt >= icnt_limit && Omake_node.NodeSet.mem bound target then
     raise (Omake_value_type.OmakeException(pos, StringNodeError("Cyclic implicit dependencies detected", target)));
   (target_exists_or_is_phony_or_is_explicit cache venv target
    || venv_find_buildable_implicit_rule_bound 
         bound (target::bound_l) (icnt+1)
         cache venv pos target <> None)

(* Find an applicable implicit rule with buildable sources *)
and venv_find_buildable_implicit_rule_bound bound bound_l icnt cache venv pos target =
   let irules = Omake_env.venv_find_implicit_rules venv target in
      if Lm_debug.debug Omake_env.debug_implicit then
         Format.eprintf "venv_find_buildable_implicit_rule %a %a: %d commands to consider@." (**)
            Omake_node.pp_print_dir (Omake_env.venv_dir venv)
            Omake_node.pp_print_node target
            (List.length irules);
      search_irules bound bound_l icnt cache venv pos target irules

and search_irules bound bound_l icnt cache venv pos target irules =
   match irules with
      irule :: irules ->
         let sources = irule.rule_sources in
            if Lm_debug.debug Omake_env.debug_implicit then
               Format.eprintf "@[<b 3>venv_find_buildable_implicit_rule: considering implicit rule %a:%a@]@." (**)
                  Omake_node.pp_print_node target
                  Omake_node.pp_print_node_set sources;
            if Omake_node.NodeSet.for_all
                (target_is_buildable_bound bound bound_l icnt cache venv (loc_pos irule.rule_loc pos)) sources then
               let irule' = Omake_rule.expand_rule irule in
                  if irule == irule' || 
                     Omake_node.NodeSet.for_all
                       (target_is_buildable_bound bound bound_l icnt cache venv pos)
                       (Omake_node.NodeSet.diff irule'.rule_sources sources) then begin
                     if Lm_debug.debug Omake_env.debug_implicit then
                        Format.eprintf "@[<b 3>venv_find_buildable_implicit_rule: accepted implicit rule %a:%a@]@." (**)
                           Omake_node.pp_print_node target
                           Omake_node.pp_print_node_set irule'.rule_sources;
                     Some irule'
                  end
                  else
                     search_irules bound bound_l icnt cache venv pos target irules
            else
               search_irules bound bound_l icnt cache venv pos target irules
    | [] ->
         None

(*
 * Outer wrappers.
 *)
(* let check_build_phase pos = *)
(*    if not (Omake_builtin_util.is_build_phase ()) then *)
(*       raise (Omake_value_type.OmakeException (pos, StringError "this command can only be executed in a rule body")) *)

(* XXX: JYH: temporarily disable it *)
let check_build_phase _pos =
   ()

let venv_find_buildable_implicit_rule cache venv pos target =
   check_build_phase pos;
   venv_find_buildable_implicit_rule_bound 
    Omake_node.NodeSet.empty [] 0 cache venv pos target

let target_is_buildable cache venv pos target =
   check_build_phase pos;
   let target = Omake_node.Node.unsquash target in
   let target_dir = Omake_node.Node.dir target in
   let target_file = Omake_node.Node.tail target in
   let node_kind = Omake_node.Node.kind target in
   let tdir = Omake_env.venv_lookup_target_dir venv target_dir in
   try
     Omake_env.venv_find_target_is_buildable_exn
       venv tdir target_file node_kind
   with
     | Not_found ->
         let flag =
           target_is_buildable_bound 
             Omake_node.NodeSet.empty [] 0 cache venv pos target in
         Omake_env.venv_add_target_is_buildable
           venv tdir target_file node_kind flag;
         flag
     
let target_is_buildable_in_path_1 cache venv pos path names =
  (* all [names] are seen as equivalent, and are searched simultaneously.
     e.g. use this for searching for the capitalized and uncapitalized
     versions of an ocaml file name
   *)
  (* NB. The target cache ignores now the phony-ness of targets, so it is
     ok to always look up for NodeNormal
   *)
  if names = [] then
    invalid_arg "Omake_target.target_is_buildable_in_path";
  check_build_phase pos;
  let names = Array.of_list names in
  let pnames = Array.map Omake_node.parse_phony_name names in
  let encache_neg = Array.map (fun _ -> ref []) names in
  let lookup_in_target_cache =
    Array.map
      (fun name ->
         (* NB. This call can be evaluated, returning another function *)
         Omake_env.venv_find_target_is_buildable_multi
           venv name Omake_node_sig.NodeNormal
      ) 
      names in

  let rec search path =
    match path with
      | (dir,tdir) :: path' ->

          let rec check_name i =
            if i < Array.length names then
              let pname = pnames.(i) in
              try
                let found =
                  lookup_in_target_cache.(i) tdir in
                if found then (
                  let target =
                    Omake_env.venv_intern_cd_1 venv PhonyOK dir pname in
                  Some(dir, tdir, i, target)
                ) else
                  (* This is the fast path of the algorithm *)
                  check_name (i+1)
              with
                | Not_found ->
                    let target =
                      Omake_env.venv_intern_cd_1 venv PhonyOK dir pname in
                    let ok =
                      target_is_buildable_bound 
                        Omake_node.NodeSet.empty [] 0 cache venv pos target in
                    if ok then
                      Some(dir, tdir, i, target)
                    else (
                      encache_neg.(i) := tdir :: !(encache_neg.(i));
                      check_name (i+1)
                    )
            else
              search path' in
          check_name 0
      | _ ->
          None in

  let result = search path in
  match result with
    | Some(_dir, tdir, i, target) ->
        Array.iteri
          (fun j encache ->
             let pos_set = if i=j then [tdir] else [] in
             let neg_set = !encache in
             let node_kind = Omake_node_sig.NodeNormal in
             Omake_env.venv_add_target_is_buildable_multi
               venv names.(j) node_kind pos_set neg_set;
          )
          encache_neg;
        Some target
    | None ->
        None


let target_is_buildable_in_path cache venv pos path names =
  let path' = 
    List.map
      (fun dir -> dir, Omake_env.venv_lookup_target_dir venv dir)
      path in
  target_is_buildable_in_path_1 cache venv pos path' names


let target_is_buildable_proper cache venv pos target =
   let target = Omake_node.Node.unsquash target in
   let target_dir = Omake_node.Node.dir target in
   let target_file = Omake_node.Node.tail target in
   let node_kind = Omake_node.Node.kind target in
   let tdir = Omake_env.venv_lookup_target_dir venv target_dir in
      check_build_phase pos;
      try Omake_env.venv_find_target_is_buildable_proper_exn 
            venv tdir target_file node_kind
      with
         Not_found ->
            let flag =
               if target_is_explicit cache venv target then
                  true
               else
                  venv_find_buildable_implicit_rule cache venv pos target <> None
            in
               Omake_env.venv_add_target_is_buildable_proper
                 venv tdir target_file node_kind flag;
               flag

