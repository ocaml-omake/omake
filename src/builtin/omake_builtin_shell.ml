(*
 * Builtin shell operations.
 *
 * \begin{doc}
 * \input{omake-shell}
 *
 * \section{Basic builtin functions}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 Jason Hickey, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_location

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_node_sig
open Omake_node
open Omake_value
open Omake_state
open Omake_symbol
open Omake_builtin
open Omake_shell_type
open Omake_value_type
open Omake_builtin_type
open Omake_builtin_util

module Pos = MakePos (struct let name = "Omake_builtin_io" end)
open Pos

(*
 * The string should be a job identifier.
 *)
let pid_of_string pos s =
   try int_of_string s with
      Invalid_argument _
    | Failure _ ->
         let pos = string_pos "pid_of_string" pos in
            raise (OmakeException (pos, StringStringError ("not a process identifier", s)))

(*
 * Signal numbers.
 *)
let signal_of_string s =
   match String.uppercase s with
      "-ABRT" -> SigAbrt
    | "-ALRM" -> SigAlrm
    | "-HUP"  -> SigHup
    | "-ILL"  -> SigIll
    | "-INT"  -> SigInt
    | "-KILL" -> SigKill
    | "-QUIT" -> SigQuit
    | "-SEGV" -> SigSegv
    | "-TERM" -> SigTerm
    | "-USR1" -> SigUsr1
    | "-USR2" -> SigUsr2
    | "-CHLD" -> SigChld
    | "-STOP" -> SigStop
    | "-TSTP" -> SigTstp
    | "-TTIN" -> SigTtin
    | "-TTOU" -> SigTtou
    | "-VTALRM" -> SigVTAlrm
    | "-PROF" -> SigProf
    | _ ->
         let len = String.length s in
            if len > 1 && s.[0] = '-' then
               SigNum (int_of_string (String.sub s 1 (len - 1)))
            else
               raise (Failure "signal_of_string")

let signal_of_string pos s =
   try signal_of_string s with
      Failure _ ->
         let pos = string_pos "signal_of_string" pos in
            raise (OmakeException (pos, StringStringError ("not a signal", s)))

(*
 * Print some text.
 *
 * \begin{doc}
 * \fun{echo}
 *
 * The \verb+echo+ function prints a string.
 *
 * \begin{verbatim}
 * $(echo <args>)
 * echo <args>
 * \end{verbatim}
 * \end{doc}
 *)
let echo_fun venv pos loc args =
   let pos = string_pos "echo" pos in
   let args = List.map (strings_of_value venv pos) args in
   let args = List.flatten args in
   let outx = channel_of_var venv pos loc stdout_var in
   let rec echo args =
      match args with
         [arg] ->
            Lm_channel.output_string outx arg
       | arg :: args ->
            Lm_channel.output_string outx arg;
            Lm_channel.output_char outx ' ';
            echo args
       | [] ->
            ()
   in
      echo args;
      Lm_channel.output_char outx '\n';
      Lm_channel.flush outx;
      ValNone

(*
 * \begin{doc}
 * \fun{cd}
 *
 * The \verb+cd+ function changes the current directory.
 *
 * \begin{verbatim}
 *     cd(dir)
 *        dir : Dir
 * \end{verbatim}
 *
 * The \verb+cd+ function also supports a 2-argument form:
 *
 * \begin{verbatim}
 *     $(cd dir, e)
 *        dir : Dir
 *        e : expression
 * \end{verbatim}
 *
 * In the two-argument form, expression \verb+e+ is evaluated
 * in the directory \verb+dir+.  The current directory is not
 * changed otherwise.
 *
 * The behavior of the \verb+cd+ function can be changed with the
 * \verb+CDPATH+ variable, which specifies a search path for
 * directories.  This is normally useful only in the \Prog{osh}
 * command interpreter.
 *
 * \begin{verbatim}
 *     CDPATH : Dir Sequence
 * \end{verbatim}
 *
 * For example, the following will change directory to the first
 * directory \verb+./foo+, \verb+~/dir1/foo+, \verb+~/dir2/foo+.
 *
 * \begin{verbatim}
 *     CDPATH[] =
 *        .
 *        $(HOME)/dir1
 *        $(HOME)/dir2
 *     cd foo
 * \end{verbatim}
 *
 * \end{doc}
 *)
type cd_status =
   CdFail
 | CdFile
 | CdSuccess

let dir_test name =
   Sys.os_type = "Win32" || (Unix.access name [Unix.X_OK]; true)

let cd_test venv pos loc dir =
   let name = Dir.fullname dir in
      try
         let stat = Unix.LargeFile.stat name in
            if stat.Unix.LargeFile.st_kind <> Unix.S_DIR then
               CdFile
            else if dir_test name then
               CdSuccess
            else
               CdFail
      with
         Unix.Unix_error _ ->
            CdFail

let cd_dir venv pos loc dir =
   match cd_test venv pos loc dir with
      CdFail ->
         let relname = Dir.name (venv_dir venv) dir in
            raise (OmakeException (loc_pos loc pos, StringStringError ("no such directory", relname)))
    | CdFile ->
         let relname = Dir.name (venv_dir venv) dir in
            raise (OmakeException (loc_pos loc pos, StringStringError ("not a directory", relname)))
    | CdSuccess ->
         dir

let rec cd_search venv cdpath pos loc name =
   match cdpath with
      [] ->
         raise (OmakeException (loc_pos loc pos, StringStringError ("no such directory", name)))
    | cd_dir :: cd_path ->
         let dir = Dir.chdir cd_dir name in
            match cd_test venv pos loc dir with
               CdFail
             | CdFile ->
                  cd_search venv cd_path pos loc name
             | CdSuccess ->
                  dir

let cd_aux venv cd_path pos loc arg =
   match values_of_value venv pos arg with
      [dir] ->
         (match eval_prim_value venv pos dir with
             ValDir dir ->
                cd_dir venv pos loc dir
           | _ ->
                let name = string_of_value venv pos dir in
                   if Filename.is_relative name then
                      cd_search venv cd_path pos loc name
                   else
                      cd_dir venv pos loc (Dir.chdir Dir.root name))
    | [] ->
         cd_dir venv pos loc (venv_intern_dir venv home_dir)
    | args ->
         raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let cd_fun venv pos loc args kargs =
   let pos = string_pos "cd" pos in
   let cd_path =
      try venv_find_var_exn venv cdpath_var with
         Not_found ->
            ValString "."
   in
   let cd_path = values_of_value venv pos cd_path in
   let cd_path = List.map (dir_of_value venv pos) cd_path in
      match args, kargs with
         [arg], [] ->
            let dir = cd_aux venv cd_path pos loc arg in
            let venv = venv_chdir_tmp venv dir in
               venv, ValDir dir
       | [dir; e], [] ->
            (* Change temporarily and evaluate the exp *)
            let dir = cd_aux venv cd_path pos loc dir in
            let venv_new = venv_chdir_tmp venv dir in
            let values = values_of_value venv_new pos e in
               venv, concat_array values
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))

(*
 * \begin{doc}
 * \section{Job control builtin functions}
 * \fun{jobs}
 *
 * The \verb+jobs+ function prints a list of jobs.
 *
 * \verb+jobs+
 * \end{doc}
 *)
let jobs_fun venv pos loc args =
   let _pos = string_pos "jobs" pos in
      Omake_shell_job.jobs venv;
      ValNone

(*
 * \begin{doc}
 * \fun{bg}
 *
 * The \verb+bg+ function places a job in the background.
 *
 * \verb+bg <pid...>+
 * \end{doc}
 *)
let jobs_iter_fun f venv pos loc args =
   let pos = string_pos "iter" pos in
      match args with
         [arg] ->
            let pids = strings_of_value venv pos arg in
            let pids = List.map (pid_of_string pos) pids in
               List.iter (f venv pos) pids
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let bg_fun venv pos loc args =
   let pos = string_pos "bg" pos in
      jobs_iter_fun Omake_shell_job.bg venv pos loc args;
      ValNone

(*
 * \begin{doc}
 * \fun{fg}
 *
 * The \verb+fg+ function brings a job to the foreground.
 *
 * \verb+fg <pid...>+
 * \end{doc}
 *)
let fg_fun venv pos loc args =
   let pos = string_pos "fg" pos in
      jobs_iter_fun Omake_shell_job.fg venv pos loc args;
      ValNone

(*
 * \begin{doc}
 * \fun{stop}
 *
 * The \verb+stop+ function suspends a job.
 *
 * \verb+stop <pid...>+
 * \end{doc}
 *)
let stop_fun venv pos loc args =
   let pos = string_pos "stop" pos in
      jobs_iter_fun Omake_shell_job.stop venv pos loc args;
      ValNone

(*
 * \begin{doc}
 * \fun{wait}
 *
 * The \verb+wait+ function waits for a job to finish.
 * If no process identifiers are given, the shell waits for
 * all jobs to complete.
 *
 * \verb+wait <pid...>+
 * \end{doc}
 *)
let wait_fun venv pos loc args =
   let pos = string_pos "wait" pos in
      jobs_iter_fun Omake_shell_job.wait venv pos loc args;
      ValNone

(*
 * \begin{doc}
 * \fun{kill}
 *
 * The \verb+kill+ function signals a job.
 *
 * \verb+kill [signal] <pid...>+
 * \end{doc}
 *)
let kill_fun venv pos loc args =
   let pos = string_pos "kill" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let signal, args =
               match args with
                  signal :: args' ->
                     if String.length signal > 1 && signal.[0] = '-' then
                        signal_of_string pos signal, args'
                     else
                        SigInt, args
                | [] ->
                     SigInt, []
            in
            let args = List.map (pid_of_string pos) args in
               List.iter (fun pid -> Omake_shell_job.kill venv pos pid signal) args;
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \section{Command history}
 * \fun{history}
 *
 * \begin{verbatim}
 *     $(history-index) : Int
 *     $(history) : String Sequence
 *     history-file : File
 *     history-length : Int
 * \end{verbatim}
 *
 * The history variables manage the command-line history in \Prog{osh}.  They have no effect
 * in \Prog{omake}.
 *
 * The \verb+history-index+ variable is the current index into the command-line history.
 * The \verb+history+ variable is the current command-line history.
 *
 * The \verb+history-file+ variable can be redefined if you want the command-line history
 * to be saved.  The default value is \verb+~/.omake/osh_history+.
 *
 * The \verb+history-length+ variable can be redefined to specify the maximum number of
 * lines in the history that you want saved.  The default value is \verb+100+.
 * \end{doc}
 *)
let history_index venv pos loc args =
   let pos = string_pos "history-index" pos in
      match args with
         [] ->
            ValInt (Omake_readline.where ())
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

let history venv pos loc args =
    let pos = string_pos "history" pos in
      match args with
         [] ->
            let strings = Omake_readline.history () in
            let strings = Array.to_list strings in
            let strings = List.map (fun s -> ValData s) strings in
               ValArray strings
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(************************************************************************
 * Tables.
 *)

let () =
   let builtin_vars =
      ["history-file",   (fun _ -> ValNode (Node.create_node no_mount_info Mount.empty (Dir.cwd ()) (Omake_state.history_file ())));
       "history-length", (fun _ -> ValInt 100);
       "CDPATH",         (fun _ -> ValArray [ValString "."])]
   in

   let builtin_funs =
      [true,  "echo",                  echo_fun,                 ArityAny;
       true,  "jobs",                  jobs_fun,                 ArityExact 1;
       true,  "bg",                    bg_fun,                   ArityExact 1;
       true,  "fg",                    fg_fun,                   ArityExact 1;
       true,  "stop",                  stop_fun,                 ArityExact 1;
       true,  "wait",                  wait_fun,                 ArityExact 1;
       true,  "kill",                  kill_fun,                 ArityExact 1;
       true,  "history-index",         history_index,            ArityExact 0;
       true,  "history",               history,                  ArityExact 0;
      ]
   in
   let builtin_kfuns =
      [false, "cd",                    cd_fun,                   ArityRange (1, 2);
      ]
   in

   let builtin_info =
      { builtin_empty with builtin_vars = builtin_vars;
                           builtin_funs = builtin_funs;
                           builtin_kfuns = builtin_kfuns
      }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
