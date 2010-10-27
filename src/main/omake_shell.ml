(*
 * Implement a shell.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, California Institute of Technology, and
 * HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_debug
open Lm_printf
open Lm_location

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_eval
open Omake_exec
open Omake_node_sig
open Omake_node
open Omake_value
open Omake_state
open Omake_symbol
open Omake_ir_ast
open Omake_options
open Omake_exn_print
open Omake_shell_job
open Omake_shell_type
open Omake_value_type
open Omake_value_print
open Omake_shell_completion

module Pos = MakePos (struct let name = "Omake_shell" end)
open Pos;;

(*
 * Empty environment.
 *)
let node_empty = Node.create_phony_global "interactive shell input"

(*
 * The parser.
 *)
let parse_string venv s =
   let ast = Omake_ast_lex.parse_string s in
   let _ =
      if debug print_ast then
         eprintf "@[<v 3>AST:@ %a@]@." Omake_ast_print.pp_print_prog ast
   in
   let senv = penv_of_vars (eval_open_file venv) venv node_empty (venv_include_scope venv IncludePervasives) in
   let _, ir = Omake_ir_ast.compile_exp_list senv ast in
      postprocess_ir venv ir

let parse_ir state venv senv prompt =
   let ast = Omake_ast_lex.parse_shell state prompt in
   let _ =
      if debug print_ast then
         eprintf "@[<v 3>AST:@ %a@]@." Omake_ast_print.pp_print_prog ast
   in
   let senv, ir = Omake_ir_ast.compile_exp_list senv ast in
   let e =
      (* We are interested in not hiding top-level values. *)
      match ir.ir_exp with
         SequenceExp (_, [e]) -> e
       | e -> e
   in
   let ir = { ir with ir_exp = e } in
      senv, postprocess_ir venv ir

(*
 * The result printer.
 *)
let print_result result =
   match result with
      ValNone
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
         printf "- : %a@." pp_print_value result

(*
 * Load a history file when the variable changes.
 *)
let load_history_file =
   let existing_file = ref None in
   let load venv pos =
      try
         let v = venv_find_var_exn venv history_file_var in
            match v with
               ValNone ->
                  ()
             | _ ->
                  let node = file_of_value venv pos v in
                  let filename = Node.fullname node in
                     if !existing_file <> Some filename then begin
                        Omake_readline.load filename;
                        existing_file := Some filename
                     end
      with
         Not_found ->
            ()
       | _ ->
            eprintf "*** osh: error loading history-file@."
   in
      load

(*
 * Set the history length when the variable changes.
 *)
let set_history_length =
   let existing_length = ref 0 in
   let set venv pos =
      try
         let v = venv_find_var_exn venv history_length_var in
         let i = int_of_value venv pos v in
            if !existing_length <> i then begin
               Omake_readline.set_length i;
               existing_length := i
            end
      with
         Not_found ->
            ()
       | _ ->
            eprintf "*** omake: error setting history-length@."
   in
      set

(*
 * Tell readline about the current directory.
 *)
let set_current_directory venv =
   let cwd = venv_dir venv in
      Omake_readline.set_directory (Dir.absname cwd)

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
      try bool_of_value venv pos (venv_find_var_exn venv exit_on_uncaught_exception_var) with
         Not_found ->
            false
   in
      if abort then
         exit exn_error_code

(*
 * The shell main loop.
 *)
let rec main state senv venv result =
   (* Prompt for input *)
   let loc = Omake_ast_lex.current_location state in
   let pos = string_pos "shell" (loc_exp_pos loc) in

   let () =
      (* Cleanup any jobs that have finished *)
      cleanup venv;

      (* Save any static values *)
      venv_save_static_values venv;

      (* Load from the history file if the variable has changed *)
      load_history_file venv pos;

      (* Set the length of the history file *)
      set_history_length venv pos;

      (* Set the current directory *)
      set_current_directory venv;

      (* Install the callback for command completion *)
      set_completion_functions venv pos loc
   in

   let prompt =
      try
         let prompt = ValStringExp (venv_get_env venv, ApplyString (loc, VarVirtual (loc, prompt_sym), [], [])) in
            string_of_value venv pos prompt
      with
         OmakeException _
       | UncaughtException _
       | RaiseException _
       | Unix.Unix_error _
       | Sys_error _
       | Failure _
       | Not_found
       | Return _ ->
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
            if venv_defined venv ignoreeof_var then begin
               eprintf "^D@.Use \"exit\" leave osh.@.";
               senv, venv, result
            end
            else
               exit 0
       | Unix.Unix_error _
       | Invalid_argument _
       | Sys_error _
       | Failure _
       | Not_found as exn ->
            eprintf "%a@." pp_print_exn (UncaughtException (pos, exn));
            maybe_exit_on_exception pos venv;
            senv, venv, ValNone
       | ExitException (_, code) ->
            exit code
       | exn ->
            eprintf "%a@." pp_print_exn exn;
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
   let venv = venv_add_var venv argv_var (ValString Sys.argv.(0)) in
   let venv = venv_add_var venv star_var ValNone in
   let venv = venv_add_var venv file_var (ValNode node_empty) in
   let senv = penv_of_vars (eval_open_file venv) venv node_empty (venv_include_scope venv IncludeAll) in
      main state senv venv ValNone

(*
 * Non-interactive shell to run some files.
 *)
let shell_script venv scriptname args =
   (* Non-interactive mode *)
   Omake_shell_sys.set_interactive false;

   let loc = bogus_loc scriptname in
   let pos = string_pos "shell_targets" (loc_exp_pos loc) in
   let node = venv_intern venv PhonyProhibited scriptname in

   (* Add the command line to the environment *)
   let argv = scriptname :: args in
   let argv_val = ValArray (List.map (fun s -> ValString s) argv) in
   let venv = venv_add_var venv argv_var argv_val in
   let star_val = ValArray (List.map (fun s -> ValString s) args) in
   let venv = venv_add_var venv star_var star_val in
   let venv, _ =
      List.fold_left (fun (venv, i) s ->
            let v = create_numeric_var i in
            let venv = venv_add_var venv v (ValString s) in
               venv, succ i) (venv, 0) argv
   in
      (* Evaluate the file *)
      if !debug_shell then
         eprintf "@[<3>shell_script (pid=%i): running script@ %a@]@." (**)
            (Unix.getpid()) pp_print_node node;
      try ignore (eval_include_file venv IncludeAll pos loc node) with
         End_of_file ->
            if !debug_shell then
               eprintf "@[<3>shell_script (pid=%i): script@ %a:@ got EOF, exiting@]@." (**)
                  (Unix.getpid()) pp_print_node node;
            exit 0
       | Return _
       | OmakeException _
       | UncaughtException _
       | RaiseException _ as exn ->
            if !debug_shell then
               eprintf "@[<3>shell_script (pid=%i): script@ %a:@ got exception, exiting@]@." (**)
                  (Unix.getpid()) pp_print_node node;
            eprintf "%a@." pp_print_exn exn;
            exit exn_error_code
       | ExitException (_, code) ->
            if !debug_shell then
               eprintf "@[<3>shell_script (pid=%i): script@ %a:@ got exit exception (code = %i), exiting@]@." (**)
                  (Unix.getpid()) pp_print_node node code;
            exit code
       | exn ->
            eprintf "%a@." pp_print_exn (UncaughtException (pos, exn));
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
         eprintf "Empty command: %s@." s;
         exit 1
    | Return _
    | OmakeException _
    | UncaughtException _
    | RaiseException _ as exn ->
         eprintf "%a@." pp_print_exn exn;
         exit exn_error_code
    | ExitException (_, code) ->
         exit code
    | exn ->
         eprintf "%a@." pp_print_exn exn;
         maybe_exit_on_exception (string_exp_pos "shell_string") venv

(*
 * Get the initial environment.
 *)
let create_venv options targets =
   (* Non-interactive mode *)
   Omake_shell_sys.set_interactive false;

   (* Move to ~/.omake *)
   let cwd = Dir.cwd () in
   let () =
      Unix.chdir (omake_dir ());
      Dir.reset_cwd ()
   in

   (* Now start creating *)
   let exec  = Exec.create cwd options in
   let cache = Omake_cache.create () in
   let venv  = Omake_env.create options "." exec cache in
   let venv  = venv_chdir_tmp venv cwd in
   let venv  = Omake_builtin.venv_add_command_defs venv in
   let venv  = Omake_env.venv_add_var venv targets_var (ValString (String.concat " " targets)) in
   let venv  = Omake_builtin.venv_add_builtins venv in
   let venv  = Omake_builtin.venv_include_rc_file venv omakeinit_file in
   let venv  = Omake_builtin.venv_add_pervasives venv in
   let venv  = Omake_builtin.venv_add_command_defs venv in
   let venv  = Omake_builtin.venv_include_rc_file venv oshrc_file in
      venv

(*
 * Run the shell.
 *)
let shell options command targets =
   let options = set_osh_opt options in
   let venv =
      try create_venv options targets with
         exn when not (opt_allow_exceptions options) ->
            eprintf "%a@." pp_print_exn exn;
            exit exn_error_code
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

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
