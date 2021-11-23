let debug_hash = ref false

(* List of targets to build. *)
let targets = ref []



let server_flag = ref None

let shell_flag = ref false
let command_string = ref None
let install_flag = ref false
let install_subdirs = ref false
let install_force = ref false
let extended_rusage = ref false


let is_white = function
  | '\t' | '\r' | '\n' | ' ' -> true
  | _ -> false

let wrap_string ~first_line_prefix ~next_line_prefix ~line_length ~initial_offset a_string =
  let string_length = String.length a_string in
    let index = ref initial_offset
    and lookahead_index = ref 0
    and column = ref 0
    and output = Buffer.create (16 + string_length + string_length / line_length) in
      Buffer.add_string output first_line_prefix;
      column := String.length first_line_prefix;
      while !index < string_length do
        while !index < string_length && is_white a_string.[!index] do incr index done;
        lookahead_index := !index + 1;
        while !lookahead_index < string_length && not (is_white a_string.[!lookahead_index]) do
          incr lookahead_index
        done;
        let word_length = !lookahead_index - !index in
          if !column = 0 then
            ()
          else if !column + 1 + word_length > line_length then
            begin
              Buffer.add_char output '\n';
              Buffer.add_string output next_line_prefix;
              column := String.length next_line_prefix
            end
          else
            begin
              Buffer.add_char output ' ';
              incr column
            end;
          Buffer.add_substring output a_string !index word_length;
          column := !column + word_length;
          index := !lookahead_index + 1
      done;
      Buffer.contents output

let negateable_option_marker = "<N>"

let print_help_intro () =
  print_endline ("Usage: " ^ Sys.argv.(0) ^ " [OPTIONS...] [TARGET...]");
  print_endline "Run OMake in the current directory to build TARGET.";
  print_endline "If called as osh start the OMake shell Osh.";
  print_newline ();
  print_endline ("A `" ^ negateable_option_marker ^ "' symbol indicates options that can be negated by");
  print_endline "prefixing them with `--no' as, e.g., `--no-S' or";
  print_endline "`--no--print-status'.  Note the conserved inner double";
  print_endline "dash of the latter, though.";
  print_newline ();
  print_string "Options:"
and print_help_outro () =
  print_newline ();
  print_endline "Report bugs to <https://github.com/ocaml-omake/omake/issues>"

let print_help_on_option (option_name, option_poly_spec, option_description) =
  let is_negateable = function
    | Lm_arg.Set _ | Clear _ | SetFold _ | ClearFold _ -> true
    | _ -> false in
    let initial_indent_length = 2
    and name_column_width = 28
    and total_width = 79
    and name_spec = Buffer.create 32
    and description_offset = if is_white option_description.[0] then 0
                             else
                               let index = ref 1 in
                                 while !index < String.length option_description &&
                                         not (is_white option_description.[!index]) do
                                   incr index
                                 done;
                                 !index
    in
      Buffer.add_string name_spec (String.make initial_indent_length ' ');
      Buffer.add_string name_spec option_name;
      if description_offset > 0 then
        begin
          Buffer.add_char name_spec ' ';
          Buffer.add_substring name_spec option_description 0 description_offset
        end;
      let pad_length = name_column_width + initial_indent_length - Buffer.length name_spec in
        if pad_length < 1 then
          begin
            print_endline (Buffer.contents name_spec);
            Buffer.clear name_spec;
            Buffer.add_string name_spec (String.make (initial_indent_length + name_column_width) ' ');
          end
        else
          Buffer.add_string name_spec (String.make pad_length ' ');

        print_endline
          (wrap_string
             ~first_line_prefix:(Buffer.contents name_spec)
             ~next_line_prefix:(String.make (initial_indent_length + name_column_width + 1) ' ')
             ~line_length:total_width
             ~initial_offset:description_offset
             option_description ^
             (if is_negateable option_poly_spec then " " ^ negateable_option_marker else ""))

let print_help_on_option_group group_name spec =
  print_newline ();
  print_char ' ';
  print_string group_name;
  print_endline ":";
  List.iter print_help_on_option spec

let show_version () =
  print_endline Omake_magic.version_message;
  print_string "Default library directory: ";
  print_endline Omake_magic.lib_dir;
  if Omake_state.lib_dir_reason <> "" then
    begin
      print_string "Using library directory: ";
      print_string Omake_state.lib_dir;
      print_string " as specified by the ";
      print_endline Omake_state.lib_dir_reason
    end;
  print_newline ();
  print_endline "License GPLv2: GNU GPL version 2 <http://gnu.org/licenses/gpl.html>";
  print_endline "This is free software: you are free to change and redistribute it.";
  print_endline "There is NO WARRANTY, to the extent permitted by law.";
  exit 0

(*
 * Arguments.
 *)
let header = "OMake generic system builder, version " ^ Omake_magic.version

let rec debug_spec =
  ["-print-ast", Lm_arg.Set Omake_eval.print_ast, (**)
   " print the AST after parsing";
   "-print-ir", Lm_arg.Set Omake_eval.print_ir, (**)
   " print the IR";
   "-print-loc", Lm_arg.Set Omake_ast_print.print_location, (**)
   " also print locations";
   "-print-rules", Lm_arg.Set Omake_eval.print_rules, (**)
   " print the rules after evaluation";
   "-print-files", Lm_arg.Set Omake_eval.print_files, (**)
   " print the files as they are read";
   "-debug-deps", Lm_arg.Set Omake_build.debug_deps, (**)
   " display dependency information as scanned";
   "-debug-ast-lex", Lm_arg.Set Omake_ast_lex.debug_lex, (**)
   " print tokens as they are scanned";
   "-debug-cache", Lm_arg.Set Omake_cache.debug_cache, (**)
   " display cache debugging information";
   "-debug-exec", Lm_arg.Set Omake_exec_util.debug_exec, (**)
   " display execution debugging information";
   "-debug-rule", Lm_arg.Set Omake_build.debug_rule, (**)
   " display debugging information about rule execution";
   "-debug-build", Lm_arg.Set Omake_build.debug_build, (**)
   " display debugging information during the build";
   "-debug-scanner", Lm_arg.Set Omake_env.debug_scanner, (**)
   " display debugging information for scanner selection";
   "-debug-implicit", Lm_arg.Set Omake_env.debug_implicit, (**)
   " display debugging information for implicit rule selection";
   "-debug-pos", Lm_arg.Set Lm_position.debug_pos, (**)
   " print source position information on error";
   "-trace-pos", Lm_arg.Set Lm_position.trace_pos, (**)
   " trace the program execution";
   "-debug-remote", Lm_arg.Set Omake_exec_remote.debug_remote, (**)
   " debug remote execution";
   "-debug-active-rules", Lm_arg.Set Omake_rule.debug_active_rules, (**)
   " debug active rules";
   "-debug-shell", Lm_arg.Set Omake_shell_type.debug_shell, (**)
   " debug shell operations";
   "-debug-eval", Lm_arg.Set Omake_eval.debug_eval, (**)
   " debug the evaluator";
   "-debug-lex", Lm_arg.Set Lm_lexer.debug_lex, (**)
   " debug the lexer";
   "-debug-lexgen", Lm_arg.Set Lm_lexer.debug_lexgen, (**)
   " debug the lexer generator";
   "-debug-parse", Lm_arg.Set Lm_parser.debug_parse, (**)
   " debug the parser";
   "-debug-parsegen", Lm_arg.Set Lm_parser.debug_parsegen, (**)
   " debug the parser generator";
   "-debug-parsing", Lm_arg.Set Omake_builtin_io_fun.debug_parsing, (**)
   " debug OMake parsing operations";
   "-debug-notify", Lm_arg.Set Lm_notify.debug_notify, (**)
   " debug the FAM (-p filesystem watch) operations";
   "-debug-db", Lm_arg.Set Omake_env.debug_db, (**)
   " debug the file database";
   "-debug-hash", Lm_arg.Set debug_hash, (**)
   " show Lm_hash statistics";
   "-debug-thread", Lm_arg.Set Lm_thread_pool.debug_thread, (**)
   " show thread operations";
   "-allow-exceptions", Lm_arg.SetFold Omake_options.set_allow_exceptions_opt, (**)
   " do not catch top-level exceptions (for use with OCAMLRUNPARAM=b)";
   "-extended-rusage", Lm_arg.Set extended_rusage, (**)
   " print more about resource usage";
   "-instrument", Lm_arg.Set Lm_instrument.enabled, (**)
   " do instrument functions"]
and cache_spec =
  ["--save-interval", Lm_arg.Float (fun f -> Omake_build.save_interval := f), (**)
   (Lm_printf.sprintf
      "DURATION  save the build DB (\".omakedb\") every DURATION seconds (0 disables, default: %.3f)" (**)
      Omake_magic.default_save_interval);
   "--force-dotomake", Lm_arg.Set Omake_state.always_use_dotomake, (**)
   " always use \"$HOME/.omake\" for .omc cache files";
   "--dotomake", Lm_arg.String Omake_state.set_omake_dir, (**)
   " use the specified directory in place of \"$HOME/.omake\""]
and helper_spec =
  ["--install", Lm_arg.Set install_flag, (**)
   " install an OMake project into the current directory";
   "--install-all", Lm_arg.SetFold (fun opts b -> install_flag := b; install_subdirs := b; opts), (**)
   " install an OMake project into the current directory and all subdirectories";
   "--install-force", Lm_arg.SetFold (fun opts b -> install_flag := b; install_force := b; opts), (**)
   " force overwriting of files during installation; implies --install";
   "-server", Lm_arg.String (fun s -> server_flag := Some s), "SERVER-NAME  run as remote server";
   "--help", (**)
   Lm_arg.Unit (fun () ->
       print_help_intro ();
       print_help_on_option_group "Build Control" Omake_options.options_spec;
       print_help_on_option_group "Output" Omake_options.output_spec;
       print_help_on_option_group "Cache Management" cache_spec;
       print_help_on_option_group "Shell Related" shell_spec;
       print_help_on_option_group "Miscellaneous" helper_spec;
       print_help_outro ();
       exit 0),
   " display help message and exit";
   "--help-debug", (**)
   Lm_arg.Unit (fun () ->
       print_help_intro ();
       print_help_on_option_group "Debug Options" debug_spec;
       print_help_outro ();
       exit 0),
   " display help message just for the debugging-related options and exit";
   "--help-all", (**)
   Lm_arg.Unit (fun () ->
       print_help_intro ();
       print_help_on_option_group "Build Control" Omake_options.options_spec;
       print_help_on_option_group "Output" Omake_options.output_spec;
       print_help_on_option_group "Cache Management" cache_spec;
       print_help_on_option_group "Shell Related" shell_spec;
       print_help_on_option_group "Debugging" debug_spec;
       print_help_on_option_group "Miscellaneous" helper_spec;
       print_help_outro ();
       exit 0),
   " display help message for all options and exit";
   "--version", Lm_arg.Unit show_version, (**)
   " print the version string and exit"]
and shell_spec =
  ["--shell", Lm_arg.Set shell_flag, (**)
   " run the OMake shell: osh(1)";
   "-i", Lm_arg.SetFold (fun opts b -> Lm_readline.set_interactive b; opts), (**)
   " treat the session as interactive";
   "-c", Lm_arg.String (fun s -> command_string := Some s), (**)
   "COMMAND  evaluate the COMMANDs from the string"]

let spec =
  Lm_arg.StrictOptions, (**)
  ["Make flags", (**)
   Omake_options.options_spec;
   "Output flags", (**)
   Omake_options.output_spec;
   "Cache management", (**)
   cache_spec;
   "Helper flags", (**)
   helper_spec;
   "Shell flags", (**)
   shell_spec;
   "Debugging flags", (**)
   debug_spec]



(*
 * Main program.
 *)
let main (options : Omake_options.t) =
  begin
    Sys.catch_break true ;
    (if Sys.os_type <> "Win32" then
       Sys.set_signal Sys.sigpipe Signal_ignore );
    let path = Omake_main_util.chroot () in
    Omake_build.build options
      (if  options.cd_root then
         "."
       else
         path)
      (match !targets with
       | [] -> [".DEFAULT"]
       | l -> List.rev l);
    if !debug_hash then
      Omake_main_util.print_hash_stats ();
    if !extended_rusage then (
      let r = Unix.times() in
      let open Unix in
      Lm_printf.eprintf "Resources used by main process:     \
                         user %.2fseconds, system %.2fseconds\n"
                        r.tms_utime r.tms_stime;
      Lm_printf.eprintf "Resources used incl. sub processes: \
                         user %.2fseconds, system %.2fseconds\n"
                        r.tms_cutime r.tms_cstime;
    );
    if !Lm_instrument.enabled then
      Lm_instrument.report()
  end

let _ =
  let add_unknown options s =
    begin
      ( match Lm_string_util.bi_split  '=' s  with
        | (v,x) ->
          Omake_build_util.add_command_def v x
        | exception Not_found -> targets := s :: !targets);
      options, !shell_flag
    end
  in
  let exe = Lm_filename_util.root (Filename.basename (Sys.argv.(0))) in
  let () =
    if exe = "osh" then
      shell_flag := true in

  (* Parse all the options *)
  let options = Omake_options.default_options in
  let options =
    try
      let s = Sys.getenv "OMAKEFLAGS" in
      let argv = Array.of_list (Sys.argv.(0) :: Lm_string_util.tokens_std s) in
      Lm_arg.fold_argv argv spec options add_unknown header
    with
    | Not_found | Lm_arg.UsageError -> options
  in
  let options =
    try Lm_arg.fold spec options add_unknown header with
    | Lm_arg.UsageError -> exit 0
    | Lm_arg.BogusArg s ->
      Lm_printf.eprintf "@\n@[<hv 3>*** omake fatal error:@ %s@]@." s;
      exit 3  in
  Lm_thread_core.debug_mutex := !Lm_thread_pool.debug_thread;
  Lm_thread.debug_lock := !Lm_thread_pool.debug_thread;
  (* Run it *)
  match !server_flag with
  | Some cwd -> Omake_main_util.main_remote cwd options !targets
  | None ->
    if !shell_flag then
      Omake_shell.shell options !command_string (List.rev !targets)
    else if !install_flag then
      if !install_subdirs then
        Omake_install.install_subdirs !install_force
      else
        Omake_install.install_current !install_force
    else if Omake_options.opt_allow_exceptions options then
      main options
    else
      Omake_exn_print.catch main options (* Main entry point *)
