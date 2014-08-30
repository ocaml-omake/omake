
include Omake_pos.MakePos (struct let name = "Omake_shell" end)


(*
 * Empty environment.
 *)
let node_empty = Omake_node.Node.create_phony_global "interactive shell input"

(*
 * The parser.
 *)
let parse_string venv s =
   let ast = Omake_ast_lex.parse_string s in
   let _ =
      if Lm_debug.debug Omake_eval.print_ast then
         Lm_printf.eprintf "@[<v 3>AST:@ %a@]@." Omake_ast_print.pp_print_prog ast
   in
   let senv = Omake_ir_ast.penv_of_vars (Omake_eval.eval_open_file venv) venv node_empty (Omake_env.venv_include_scope venv IncludePervasives) in
   let _, ir = Omake_ir_ast.compile_exp_list senv ast in
      Omake_eval.postprocess_ir venv ir

let parse_ir state venv senv prompt =
   let ast = Omake_ast_lex.parse_shell state prompt in
   let _ =
      if Lm_debug.debug Omake_eval.print_ast then
         Lm_printf.eprintf "@[<v 3>AST:@ %a@]@." Omake_ast_print.pp_print_prog ast
   in
   let senv, ir = Omake_ir_ast.compile_exp_list senv ast in
   let e =
      (* We are interested in not hiding top-level values. *)
      match ir.ir_exp with
         SequenceExp (_, [e]) -> e
       | e -> e
   in
   let ir = { ir with ir_exp = e } in
      senv, Omake_eval.postprocess_ir venv ir

(*
 * The result printer.
 *)
let print_result result =
  match result with
  | Omake_value_type.ValNone
  | ValQuote []
  | ValSequence []
  | ValArray []
  | ValString ""
  | ValWhite _
  | ValClass _
  | ValOther (ValExitCode 0) ->
    ()
  | ValInt _
  | ValFloat _
  | ValSequence _
  | ValArray _
  | ValData _
  | ValQuote _
  | ValQuoteString _
  | ValString _
  | ValNode _
  | ValDir _
  | ValMaybeApply _
  | ValVar _
  | ValObject _
  | ValMap _
  | ValChannel _
  | ValFun _
  | ValFunCurry _
  | ValPrim _
  | ValPrimCurry _
  | ValStringExp _
  | ValBody _
  | ValRules _
  | ValOther _
  | ValCases _
  | ValDelayed _ ->
    Lm_printf.printf "- : %a@." Omake_value_print.pp_print_value result

(*
 * Load a history file when the variable changes.
 *)
let load_history_file =
  let existing_file = ref None in
  let load venv pos =
    try
      let v = Omake_env.venv_find_var_exn venv Omake_var.history_file_var in
      match v with
        ValNone ->
        ()
      | _ ->
        let node = Omake_value.file_of_value venv pos v in
        let filename = Omake_node.Node.fullname node in
        if !existing_file <> Some filename then begin
          Omake_readline.load filename;
          existing_file := Some filename
        end
    with
      Not_found ->
      ()
    | _ ->
      Lm_printf.eprintf "*** osh: error loading history-file@."
  in
  load

(*
 * Set the history length when the variable changes.
 *)
let set_history_length =
   let existing_length = ref 0 in
   let set venv pos =
      try
         let v = Omake_env.venv_find_var_exn venv Omake_var.history_length_var in
         let i = Omake_value.int_of_value venv pos v in
            if !existing_length <> i then begin
               Omake_readline.set_length i;
               existing_length := i
            end
      with
         Not_found ->
            ()
       | _ ->
            Lm_printf.eprintf "*** omake: error setting history-length@."
   in
      set

(*
 * Tell readline about the current directory.
 *)
let set_current_directory venv =
  let cwd = Omake_env.venv_dir venv in
  Omake_readline.set_directory (Omake_node.Dir.absname cwd)

(*
 * Save the history when exiting.
 *)
let exit code =
   Omake_readline.save ();
   Pervasives.exit code

(*
 * Abort if asked.
 *)
let maybe_exit_on_exception pos venv =
  let abort =
    try Omake_value.bool_of_value venv pos (Omake_env.venv_find_var_exn venv Omake_var.exit_on_uncaught_exception_var) with
      Not_found ->
      false
  in
  if abort then
    exit Omake_state.exn_error_code

(*
 * The shell main loop.
 *)
let rec main state senv venv result =
  (* Prompt for input *)
  let loc = Omake_ast_lex.current_location state in
  let pos = string_pos "shell" (loc_exp_pos loc) in

  let () =
    (* Cleanup any jobs that have finished *)
    Omake_shell_job.cleanup venv;

    (* Save any static values *)
    Omake_env.venv_save_static_values venv;

    (* Load from the history file if the variable has changed *)
    load_history_file venv pos;

    (* Set the length of the history file *)
    set_history_length venv pos;

    (* Set the current directory *)
    set_current_directory venv;

    (* Install the callback for command completion *)
    Omake_shell_completion.set_completion_functions venv pos loc
  in

  let prompt =
    try
      let prompt = Omake_value_type.ValStringExp (Omake_env.venv_get_env venv, ApplyString (loc, VarVirtual (loc, Omake_symbol.prompt_sym), [], [])) in
      Omake_value.string_of_value venv pos prompt
    with
      Omake_value_type.OmakeException _
    | Omake_value_type.UncaughtException _
    | Omake_value_type.RaiseException _
    | Unix.Unix_error _
    | Sys_error _
    | Failure _
    | Not_found
    | Omake_value_type.Return _ ->
      "% "
  in

  (* Evaluate it *)
  let senv, venv, result =
    try
      let senv, ir = parse_ir state venv senv prompt in
      let venv, result = Omake_eval.eval_exp venv result ir.ir_exp in
      senv, venv, result
    with
      End_of_file ->
      if Omake_env.venv_defined venv Omake_var.ignoreeof_var then begin
        Lm_printf.eprintf "^D@.Use \"exit\" leave osh.@.";
        senv, venv, result
      end
      else
        exit 0
    | Unix.Unix_error _
    | Invalid_argument _
    | Sys_error _
    | Failure _
    | Not_found as exn ->
      Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn (Omake_value_type.UncaughtException (pos, exn));
      maybe_exit_on_exception pos venv;
      senv, venv, ValNone
    | Omake_value_type.ExitException (_, code) ->
      exit code
    | exn ->
      Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
      maybe_exit_on_exception pos venv;
      senv, venv, ValNone
  in
  print_result result;
  main state senv venv result

(*
 * Run an interactive shell.
 *)
let shell_interactive venv =
  (* Interactive mode *)
  Omake_shell_sys.set_interactive true;

  let state = Omake_ast_lex.create_shell () in
  let () =
    if Sys.os_type <> "Win32" then
      let _ = Sys.signal Sys.sigttou Sys.Signal_ignore in
      let _ = Sys.signal Sys.sigint  Sys.Signal_ignore in
      let _ = Sys.signal Sys.sigquit Sys.Signal_ignore in
      let _ = Sys.signal Sys.sigtstp Sys.Signal_ignore in
      ()
  in

  (* Set up the environment *)
  let venv = Omake_env.venv_add_var venv Omake_var.argv_var (ValString Sys.argv.(0)) in
  let venv = Omake_env.venv_add_var venv Omake_var.star_var ValNone in
  let venv = Omake_env.venv_add_var venv Omake_var.file_var (ValNode node_empty) in
  let senv = Omake_ir_ast.penv_of_vars (Omake_eval.eval_open_file venv) venv node_empty (Omake_env.venv_include_scope venv IncludeAll) in
  main state senv venv ValNone

(*
 * Non-interactive shell to run some files.
 *)
let shell_script venv scriptname args =
  (* Non-interactive mode *)
  Omake_shell_sys.set_interactive false;

  let loc = Lm_location.bogus_loc scriptname in
  let pos = string_pos "shell_targets" (loc_exp_pos loc) in
  let node = Omake_env.venv_intern venv PhonyProhibited scriptname in

  (* Add the command line to the environment *)
  let argv = scriptname :: args in
  let argv_val = Omake_value_type.ValArray (List.map (fun s -> Omake_value_type.ValString s) argv) in
  let venv = Omake_env.venv_add_var venv Omake_var.argv_var argv_val in
  let star_val = Omake_value_type.ValArray (List.map (fun s -> Omake_value_type.ValString s) args) in
  let venv = Omake_env.venv_add_var venv Omake_var.star_var star_val in
  let venv, _ =
    List.fold_left (fun (venv, i) s ->
      let v = Omake_var.create_numeric_var i in
      let venv = Omake_env.venv_add_var venv v (ValString s) in
      venv, succ i) (venv, 0) argv
  in
  (* Evaluate the file *)
  if !Omake_shell_type.debug_shell then
    Lm_printf.eprintf "@[<3>shell_script (pid=%i): running script@ %a@]@." (**)
      (Unix.getpid()) Omake_node.pp_print_node node;
  try ignore (Omake_eval.eval_include_file venv IncludeAll pos loc node) with
    End_of_file ->
    if !Omake_shell_type.debug_shell then
      Lm_printf.eprintf "@[<3>shell_script (pid=%i): script@ %a:@ got EOF, exiting@]@." (**)
        (Unix.getpid()) Omake_node.pp_print_node node;
    exit 0
  | Omake_value_type.Return _
  | Omake_value_type.OmakeException _
  | Omake_value_type.UncaughtException _
  | Omake_value_type.RaiseException _ as exn ->
    if !Omake_shell_type.debug_shell then
      Lm_printf.eprintf "@[<3>shell_script (pid=%i): script@ %a:@ got exception, exiting@]@." (**)
        (Unix.getpid()) Omake_node.pp_print_node node;
    Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
    exit Omake_state.exn_error_code
  | Omake_value_type.ExitException (_, code) ->
    if !Omake_shell_type.debug_shell then
      Lm_printf.eprintf "@[<3>shell_script (pid=%i): script@ %a:@ got exit exception (code = %i), exiting@]@." (**)
        (Unix.getpid()) Omake_node.pp_print_node node code;
    exit code
  | exn ->
    Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn (Omake_value_type.UncaughtException (pos, exn));
    maybe_exit_on_exception pos venv

(*
 * Evaluate a string.
 *)
let shell_string venv s =
   (* Non-interactive mode *)
   Omake_shell_sys.set_interactive false;

   (* Evaluate the string *)
   try ignore (Omake_eval.eval_exp venv ValNone (parse_string venv s).ir_exp) with
      End_of_file ->
         Lm_printf.eprintf "Empty command: %s@." s;
         exit 1
    | Omake_value_type.Return _
    | Omake_value_type.OmakeException _
    | Omake_value_type.UncaughtException _
    | Omake_value_type.RaiseException _ as exn ->
         Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
         exit Omake_state.exn_error_code
    | Omake_value_type.ExitException (_, code) ->
         exit code
    | exn ->
         Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
         maybe_exit_on_exception (string_exp_pos "shell_string") venv

(*
 * Get the initial environment.
 *)
let create_venv options targets =
   (* Non-interactive mode *)
   Omake_shell_sys.set_interactive false;

   (* Move to ~/.omake *)
   let cwd = Omake_node.Dir.cwd () in
   let () =
      Unix.chdir (Omake_state.omake_dir ());
      Omake_node.Dir.reset_cwd ()
   in

   (* Now start creating *)
   let exec  = Omake_exec.Exec.create cwd options in
   let cache = Omake_cache.create () in
   let venv  = Omake_env.create options "." exec cache in
   let venv  = Omake_env.venv_chdir_tmp venv cwd in
   let venv  = Omake_builtin.venv_add_command_defs venv in
   let venv  = Omake_env.venv_add_var venv Omake_var.targets_var (ValString (String.concat " " targets)) in
   let venv  = Omake_builtin.venv_add_builtins venv in
   let venv  = Omake_builtin.venv_include_rc_file venv Omake_state.omakeinit_file in
   let venv  = Omake_builtin.venv_add_pervasives venv in
   let venv  = Omake_builtin.venv_add_command_defs venv in
   let venv  = Omake_builtin.venv_include_rc_file venv Omake_state.oshrc_file in
      venv

(*
 * Run the shell.
 *)
let shell options command targets =
  let options = Omake_options.set_osh_opt options in
  let venv =
    try create_venv options targets with
      exn when not (Omake_options.opt_allow_exceptions options) ->
      Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
      exit Omake_state.exn_error_code
  in
  match command with
    Some command ->
    shell_string venv command
  | None ->
    match targets with
      [] ->
      shell_interactive venv
    | filename :: args ->
      shell_script venv filename args
