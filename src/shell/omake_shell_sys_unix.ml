(*
 * System calls.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2006 Mojave Group, Caltech
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_debug

open Omake_pos
open Omake_exn_print
open Omake_shell_type
open Omake_shell_sys_type

(*
 * These functions are directly exported.
 *)
external ext_set_tty_pgrp   : pgrp -> unit                = "omake_shell_sys_set_tty_pgrp"
external ext_setpgid        : pid -> pid -> unit          = "omake_shell_sys_setpgid"

let interact = ref (Omake_readline.isatty ())

let set_interactive flag =
   interact := flag

let set_tty_pgrp pgrp =
   if !interact then
      ext_set_tty_pgrp pgrp

let setpgid pid1 pid2 =
   if !interact then
      ext_setpgid pid1 pid2

(*
 * Close-on-exec flags.
 * We actually want a close-on-fork, so we keep track of
 * these descriptors.
 *)
module FdCompare =
struct
   type t = Unix.file_descr
   let compare = Pervasives.compare
end

module FdSet = Lm_set.LmMake (FdCompare);;

let close_on_fork = ref FdSet.empty

let close_fd fd =
   Unix.close fd;
   close_on_fork := FdSet.remove !close_on_fork fd

let set_close_on_exec fd =
   Unix.set_close_on_exec fd;
   close_on_fork := FdSet.add !close_on_fork fd

let clear_close_on_exec fd =
   Unix.clear_close_on_exec fd;
   close_on_fork := FdSet.remove !close_on_fork fd

let do_close_on_fork () =
   FdSet.iter Unix.close !close_on_fork;
   close_on_fork := FdSet.empty

(*
 * Send a signal to a process.
 *)
let signo_of_signal = function
   SigAbrt -> Sys.sigabrt
 | SigAlrm -> Sys.sigalrm
 | SigFPE  -> Sys.sigfpe
 | SigHup  -> Sys.sighup
 | SigIll  -> Sys.sigill
 | SigInt  -> Sys.sigint
 | SigKill -> Sys.sigkill
 | SigPipe -> Sys.sigpipe
 | SigQuit -> Sys.sigquit
 | SigSegv -> Sys.sigsegv
 | SigTerm -> Sys.sigterm
 | SigUsr1 -> Sys.sigusr1
 | SigUsr2 -> Sys.sigusr2
 | SigChld -> Sys.sigchld
 | SigCont -> Sys.sigcont
 | SigStop -> Sys.sigstop
 | SigTstp -> Sys.sigtstp
 | SigTtin -> Sys.sigttin
 | SigTtou -> Sys.sigttou
 | SigVTAlrm -> Sys.sigvtalrm
 | SigProf   -> Sys.sigprof
 | SigNum i  -> i

let kill pgrp signal =
   Unix.kill pgrp (signo_of_signal signal)

(*
 * Wait for a process to exit.
 * The leader flag indicates whether to wait for the leader.
 *)
let wait pgrp leader nohang =
   let flags =
      if !interact then
         [Unix.WUNTRACED]
      else
         []
   in
   let flags =
      if nohang then
         Unix.WNOHANG :: flags
      else
         flags
   in
   let pid =
      if pgrp = 0 then
         -1
      else if leader then
         pgrp
      else if !interact then
         -pgrp
      else
         -1
   in
      Unix.waitpid flags pid

(*
 * Duplicate file descriptors onto
 * their standard places.
 *)
let dup stdin stdout stderr =
   let stdin'  = Unix.dup stdin in
   let stdout' = Unix.dup stdout in
   let stderr' = Unix.dup stderr in
      Unix.close stdin;
      if (stdin <> stdout) then Unix.close stdout;
      if (stdin <> stderr && stdout <> stderr) then Unix.close stderr;
      Unix.dup2 stdin'  Unix.stdin;
      Unix.dup2 stdout' Unix.stdout;
      Unix.dup2 stderr' Unix.stderr;
      Unix.close stdin';
      Unix.close stdout';
      Unix.close stderr'

(*
 * Create a thread.
 * This actually creates a process on Unix.
 *)
let create_thread info =
   let { create_thread_stdin = stdin;
         create_thread_stdout = stdout;
         create_thread_stderr = stderr;
         create_thread_pgrp = pgrp;
         create_thread_fun = f;
         create_thread_background = bg
       } = info
   in
   Pervasives.flush_all();
   let pid = Unix.fork () in
      if pid = 0 then
         let code =
            try
               let pgrp =
                  if pgrp = 0 then
                     let pid = Unix.getpid () in
                        setpgid pid pid;
                        if not bg then
                           set_tty_pgrp pgrp;
                        pid
                  else
                     pgrp
               in
               dup stdin stdout stderr;
               do_close_on_fork ();
               ignore (Sys.signal Sys.sigint  Sys.Signal_default);
               ignore (Sys.signal Sys.sigquit Sys.Signal_default);
               ignore (Sys.signal Sys.sigtstp Sys.Signal_default);
               f Unix.stdin Unix.stdout Unix.stderr pgrp
            with
               Omake_value_type.ExitException (_, code) ->
                  code
             | exn ->
                  let () =
                     try eprintf "%a@." pp_print_exn exn with _ -> ()
                  in
                     Omake_state.exn_error_code
         in
            exit code
      else
         pid

(*
 * Create a process.
 *)
let create_process info =
   let { create_process_stdin = stdin;
         create_process_stdout = stdout;
         create_process_stderr = stderr;
         create_process_pgrp = pgrp;
         create_process_dir = dir;
         create_process_env = env;
         create_process_exe = exe;
         create_process_argv = argv;
         create_process_background = bg
       } = info
   in
(*
      eprintf "@[<v 3>";
      Array.iter (fun s ->
            eprintf "@ %s" s) argv;
      eprintf "@]@.";
 *)
   let pid = Unix.fork () in
      if pid = 0 then
         try
            let () =
               if pgrp = 0 then
                  let pid = Unix.getpid () in
                     setpgid pid pid;
                     if not bg then
                        set_tty_pgrp pgrp;
            in
               dup stdin stdout stderr;
               Unix.handle_unix_error Unix.chdir dir;
               Unix.handle_unix_error (fun () -> Unix.execve exe argv env) ()
         with _ ->
            exit Omake_state.exn_error_code
      else
         pid

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
