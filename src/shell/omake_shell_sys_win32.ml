(*
 * System calls.
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
open Lm_printf
open Lm_debug

open Omake_shell_type
open Omake_shell_sys_type

(*
 * These functions are directly exported.
 *)
external set_tty_pgrp   : pgrp -> unit                    = "omake_shell_sys_set_tty_pgrp"
external create_process : create_process -> pid           = "omake_shell_sys_create_process"

(*
 * Internal.
 *)
external create_thread_pid  : pgrp -> pid                 = "omake_shell_sys_create_thread_pid"
external release_thread_pid : pid -> int -> unit          = "omake_shell_sys_release_thread_pid"
external init_thread_pid    : pid -> unit                 = "omake_shell_sys_init_thread_pid"
external check_thread       : unit -> bool                = "omake_shell_sys_check_thread"
external suspend            : pgrp -> unit                = "omake_shell_sys_suspend"
external resume             : pgrp -> unit                = "omake_shell_sys_resume"
external kill               : pgrp -> unit                = "omake_shell_sys_kill"
external ext_wait           : pgrp -> bool -> bool -> bool * pid * int = "omake_shell_sys_wait"

external init_shell         : unit -> unit                = "omake_shell_sys_init"
external close              : unit -> unit                = "omake_shell_sys_close"

let () =
   init_shell ();
   let pid = Unix.getpid () in
   let do_close () =
      if pid == Unix.getpid () then
         close ()
   in
      Pervasives.at_exit do_close

let set_interactive _ = ()
let set_close_on_exec = Unix.set_close_on_exec
let clear_close_on_exec = Unix.clear_close_on_exec
let close_fd = Unix.close

(*
 * Termination signal.
 *)
exception Terminated

(*
 * The operation depends on the signal number.
 *)
let kill pgrp signo =
   match signo with
      SigStop
    | SigTstp ->
         suspend pgrp
    | SigCont ->
         resume pgrp
    | _ ->
         kill pgrp

(*
 * Wait is blocking.
 *)
let unix_wait pgrp leader nohang =
   let exited, pid, code = ext_wait pgrp leader nohang in
   let status =
      if exited then
         Unix.WEXITED code
      else
         Unix.WSTOPPED code
   in
      pid, status

let wait pgrp leader nohang =
   Lm_thread_pool.blocking_section (unix_wait pgrp leader) nohang

(*
 * Try to close a descriptor.
 * This is kind of bad, because some other thread
 * may have allocated that descriptor by the time we get
 * to it, but this should never happen because the thread
 * should be catching all its exceptions.
 *)
let try_close fd =
   try close_fd fd with
      Unix.Unix_error _ ->
         ()

(*
 * Create a thread.  This is a real thread, but it
 * should look as much like a process as possible.
 * For this reason, we dup the stdio handles.
 *)
let create_thread info =
   let { create_thread_stdin = stdin;
         create_thread_stdout = stdout;
         create_thread_stderr = stderr;
         create_thread_pgrp = pgrp;
         create_thread_fun = f
       } = info
   in
   Pervasives.flush_all();
   let pid    = create_thread_pid pgrp in
   let stdin  = Unix.dup stdin  in
   let stdout = Unix.dup stdout in
   let stderr = Unix.dup stderr in
   let cleanup () =
      try_close stdin;
      try_close stdout;
      try_close stderr
   in
   let _ =
      Lm_thread_pool.create false (fun () ->
            init_thread_pid pid;
            let code =
               try
                  f stdin stdout stderr pid
               with
                  Omake_value_type.ExitException (_, code) ->
                     cleanup ();
                     code
                | Sys.Break as exn ->
                     cleanup();
                     raise exn
                | exn ->
                     eprintf "@[<v 3>%a@ Thread failed with an exception, cleaning up@]@." Omake_exn_print.pp_print_exn exn;
                     cleanup ();
                     Omake_state.exn_error_code
            in
               release_thread_pid pid code)
   in
      pid

(*
 * Create a new process group.
 *)
let create_process_group = create_thread

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
