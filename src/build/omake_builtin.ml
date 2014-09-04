
include Omake_pos.MakePos (struct let name = "Omake_builtin" end)

(* let object_sym = Lm_symbol.add "Object" *)

(*
 * Add a command line variable definition.
 *)
let add_command_def = Omake_builtin_util.add_command_def
let command_defs_are_nonempty = Omake_builtin_util.command_defs_are_nonempty
let venv_add_command_defs = Omake_builtin_util.venv_add_command_defs


(*
 * Register some builtin info.
 *)
let builtin_info = ref Omake_builtin_type.builtin_empty

let register_builtin info =
  let { Omake_builtin_type.builtin_vars       = builtin_vars1;
        builtin_funs       = builtin_funs1;
        builtin_kfuns      = builtin_kfuns1;
        builtin_objects    = builtin_objects1;
        pervasives_objects = pervasives_objects1;
        phony_targets      = phony_targets1;
        builtin_rules      = builtin_rules1
      } = !builtin_info
  in
  let { Omake_builtin_type.builtin_vars       = builtin_vars2;
        builtin_funs       = builtin_funs2;
        builtin_kfuns      = builtin_kfuns2;
        builtin_objects    = builtin_objects2;
        pervasives_objects = pervasives_objects2;
        phony_targets      = phony_targets2;
        builtin_rules      = builtin_rules2
      } = info
  in
  let info =
    { Omake_builtin_type.builtin_vars       = builtin_vars1 @ builtin_vars2;
      builtin_funs       = builtin_funs1 @ builtin_funs2;
      builtin_kfuns      = builtin_kfuns1 @ builtin_kfuns2;
      builtin_objects    = builtin_objects1 @ builtin_objects2;
      pervasives_objects = pervasives_objects1 @ pervasives_objects2;
      phony_targets      = phony_targets1 @ phony_targets2;
      builtin_rules      = builtin_rules1 @ builtin_rules2
    }
  in
  builtin_info := info

let get_registered_builtins () =
  !builtin_info

(*
 * Check that there are no keyword arguments.
 *)
let wrap_normal_prim_fun f venv pos loc args kargs =
  match kargs with
    [] ->
      venv, f venv pos loc args
  | (v, _) :: _ ->
      raise (Omake_value_type.OmakeException (loc_pos loc pos, StringVarError ("no such parameter", v)))

(*
 * Add all the functions to the environment.
 *)
let venv_add_builtins venv =
  let loc = Lm_location.bogus_loc "<builtins>" in
  let pos = string_pos "venv_add_builtins" (loc_exp_pos loc) in
  match get_registered_builtins () with 
    { builtin_vars ;
      builtin_funs ;       
      builtin_kfuns ;     
      builtin_objects ;     
      pervasives_objects ;  
      phony_targets ;      
      builtin_rules      
    } -> 
  (* Add only to the protected (current object) environment *)
  let venv = Omake_env.venv_add_phony venv loc (List.map (fun s -> Omake_value_type.TargetString s) phony_targets) in
  let venv =
    List.fold_left (fun venv (special, s, f, arity) ->
      let name = Lm_symbol.add s in
      let (v : Omake_ir.var_info) = VarGlobal (loc, name) in
      let p = Omake_env.venv_add_prim_fun venv name (wrap_normal_prim_fun f) in
      let no_args   =
        match (arity : Omake_ir.arity) with
        | ArityExact 0 -> Omake_ir.ApplyEmpty
        | _ -> ApplyNonEmpty
      in
      Omake_env.venv_add_var venv v (ValPrim (arity, special,no_args, p))) venv builtin_funs
  in
  let venv =
    List.fold_left (fun venv (special, s, f, arity) ->
      let name = Lm_symbol.add s in
      let (v : Omake_ir.var_info) = VarGlobal (loc, name) in
      let p = Omake_env.venv_add_prim_fun venv name f in
      let (no_args : Omake_ir.apply_empty_strategy)  =
        match (arity : Omake_ir.arity) with
        | ArityExact 0 -> ApplyEmpty
        | _ -> ApplyNonEmpty
      in
      Omake_env.venv_add_var venv v (ValPrim (arity, special,no_args,  p))) venv builtin_kfuns
  in
  let venv =
    List.fold_left (fun venv (multiple, targets, sources) ->
      let targets = List.map (fun name -> Omake_value_type.TargetString name) targets in
      let sources = List.map (fun source -> Omake_node_sig.NodeNormal, Omake_value_type.TargetString source) sources in
      let multiple : Omake_value_type.rule_multiple =
        if multiple then
          RuleMultiple
        else
          RuleSingle
      in
      let venv, _ = Omake_env.venv_add_rule venv pos loc multiple targets [] [] sources [] [] [] in
      venv) venv builtin_rules
  in

  (* Add the Object object *)
  let obj = Omake_env.venv_empty_object in

  (* Add values to each of the primitive objects *)
  let venv =
    List.fold_left (fun venv (s, v, x) ->
      let obj = Omake_env.venv_add_field_internal obj v x in
      Omake_env.venv_add_var venv (VarGlobal (loc, Lm_symbol.add s)) 
        (ValObject obj)) venv builtin_objects
  in
  let venv =
    List.fold_left (fun venv s ->
      Omake_env.venv_add_var venv (VarGlobal (loc, Lm_symbol.add s))
        (ValObject obj)) venv pervasives_objects
  in

  (* Add the variables last *)
  let venv =
    List.fold_left (fun venv (s, v) ->
      Omake_env.venv_add_var venv (VarGlobal (loc, Lm_symbol.add s))
        (v venv)) venv builtin_vars
  in
  venv

(*
 * Add the Pervasives module.
 *)
let venv_add_pervasives venv =
  let loc = Lm_location.bogus_loc "Omake_builtin" in
  let pos = string_pos "venv_add_pervasives" (loc_exp_pos loc) in
  let () = Omake_env.venv_set_pervasives venv in
  let obj = Omake_builtin_util.object_of_file venv pos loc "Pervasives" in
  let venv = Omake_env.venv_flatten_object venv obj in
  Omake_env.venv_set_pervasives venv;
  venv

(*
 * Load a file.
 *)
let venv_include_rc_file venv name =
  if Sys.file_exists name then
    let node = Omake_env.venv_intern venv PhonyProhibited name in
    try
      let loc = Lm_location.bogus_loc (Filename.basename name) in
      let pos = string_pos "create_venv" (loc_exp_pos loc) in
      Omake_eval.include_file venv IncludePervasives pos loc node
    with
      exn ->
        Format.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
        venv
  else
    venv

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
