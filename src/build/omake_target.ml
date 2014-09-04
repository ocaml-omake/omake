(*  Utilities on targets. *)

include Omake_pos.MakePos (struct let name = "Omake_target" end)

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

(*
 * A target is buildable if it exists, or
 * if there is an implicit rule whose dependencies
 * are all buildable.
 *)
let rec target_is_buildable_bound bound cache venv pos target =
   let target = Omake_node.Node.unsquash target in
      try Omake_env.venv_find_target_is_buildable_exn venv target with
         Not_found ->
            (* Check for loops *)
            if Omake_node.NodeSet.mem bound target then
               raise (Omake_value_type.OmakeException(pos, StringNodeError("Cyclic implicit dependencies detected", target)));
            let flag =
               (target_exists_or_is_phony_or_is_explicit cache venv target
                   || venv_find_buildable_implicit_rule_bound 
                     (Omake_node.NodeSet.add bound target) cache venv pos target <> None)
            in
               Omake_env.venv_add_target_is_buildable venv target flag;
               flag

(* Find an applicable implicit rule with buildable sources *)
and venv_find_buildable_implicit_rule_bound bound cache venv pos target =
   let irules = Omake_env.venv_find_implicit_rules venv target in
      if Lm_debug.debug Omake_env.debug_implicit then
         Format.eprintf "venv_find_buildable_implicit_rule %a %a: %d commands to consider@." (**)
            Omake_node.pp_print_dir (Omake_env.venv_dir venv)
            Omake_node.pp_print_node target
            (List.length irules);
      search_irules bound cache venv pos target irules

and search_irules bound cache venv pos target irules =
   match irules with
      irule :: irules ->
         let sources = irule.rule_sources in
            if Lm_debug.debug Omake_env.debug_implicit then
               Format.eprintf "@[<b 3>venv_find_buildable_implicit_rule: considering implicit rule %a:%a@]@." (**)
                  Omake_node.pp_print_node target
                  Omake_node.pp_print_node_set sources;
            if Omake_node.NodeSet.for_all
                (target_is_buildable_bound bound cache venv (loc_pos irule.rule_loc pos)) sources then
               let irule' = Omake_rule.expand_rule irule in
                  if irule == irule' || 
                     Omake_node.NodeSet.for_all
                       (target_is_buildable_bound bound cache venv pos)
                       (Omake_node.NodeSet.diff irule'.rule_sources sources) then begin
                     if Lm_debug.debug Omake_env.debug_implicit then
                        Format.eprintf "@[<b 3>venv_find_buildable_implicit_rule: accepted implicit rule %a:%a@]@." (**)
                           Omake_node.pp_print_node target
                           Omake_node.pp_print_node_set irule'.rule_sources;
                     Some irule'
                  end
                  else
                     search_irules bound cache venv pos target irules
            else
               search_irules bound cache venv pos target irules
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
    Omake_node.NodeSet.empty cache venv pos target

let target_is_buildable cache venv pos target =
   check_build_phase pos;
   target_is_buildable_bound 
    Omake_node.NodeSet.empty cache venv pos target

let target_is_buildable_proper cache venv pos target =
   let target = Omake_node.Node.unsquash target in
      check_build_phase pos;
      try Omake_env.venv_find_target_is_buildable_proper_exn venv target with
         Not_found ->
            let flag =
               if target_is_explicit cache venv target then
                  true
               else
                  venv_find_buildable_implicit_rule cache venv pos target <> None
            in
               Omake_env.venv_add_target_is_buildable_proper venv target flag;
               flag

