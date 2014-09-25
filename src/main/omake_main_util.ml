module Pos =   Omake_pos.Make (struct let name = "Omake_main_util" end);;

let main_remote cwd options targets  =
  (* Set up the environment *)
  Unix.chdir cwd;
  Omake_node.Dir.reset_cwd ();
  let cwd   = Omake_node.Dir.cwd () in
  let exec  = Omake_exec.Exec.create cwd options in
  let cache = Omake_cache.create () in
  let venv  = Omake_env.create options "." exec cache in
  let venv  = Omake_builtin.venv_add_command_defs venv in
  let venv  = Omake_env.venv_add_var venv Omake_var.targets_var 
      (ValString (String.concat " " targets)) in
  let venv  = Omake_builtin.venv_add_builtins venv in

  (* Shell evaluator *)
  let pos = Pos.string_exp_pos "main_remote" in
  let shell = Omake_rule.eval_shell venv pos in
  Omake_exec_remote.main shell options

let print_hash_stats () =
  Format.eprintf "@[<v 3>Hash statistics:@ %t@]@." Lm_hash.pp_print_hash_stats

let rec search start cwd len  i =
  if i < start then
    raise (Omake_value_type.OmakeFatal 
             ("can not find " ^ Omake_state.makeroot_name ^ " or " ^ Omake_state.makeroot_short_name))
  else if cwd.[i] = '/' || cwd.[i] = '\\' then
    (* Maybe file is in this directory *)
    let dir = String.sub cwd 0 i in
    if Sys.file_exists (Filename.concat dir Omake_state.makeroot_name)
    || Sys.file_exists (Filename.concat dir Omake_state.makeroot_short_name)
    then
      let rest = String.sub cwd (i + 1) (len - i - 1) in
      dir, rest
    else
      search start cwd len (i - 1)
  else
    search start cwd len (i - 1)

(*
 * Find the outermost OMakeroot.
 *)
let chroot () =
  let cwd =
    try
      Unix.getcwd ()
    with
      Unix.Unix_error _ ->
        Format.eprintf "*** omake: fatal error: current directory does not exist@.";
        exit 1  in
  let len = String.length cwd in
  let start = Lm_filename_util.drive_skip cwd in

  let dir, rest =
    if Sys.file_exists (Filename.concat cwd Omake_state.makeroot_name) || Sys.file_exists
      (Filename.concat cwd Omake_state.makeroot_short_name) then
      cwd, "."
    else
      search start cwd len (len - 1)
  in
  if rest <> "." then
    Format.eprintf "*** omake: changing directory to %s@." dir;
  Unix.chdir dir;
  Omake_node.Dir.reset_cwd ();
  rest

