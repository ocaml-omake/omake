(*
 * Architecture-independent process management.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Omake_shell_sys_type
open Omake_shell_type

(*
 * Set whether we are in interactive mode.
 * Defaults to true.
 *)
val set_interactive : bool -> unit

(*
 * Set the process group for the current terminal.
 * Does nothing if there is no terminal, or the
 * session is not interactive.
 *)
val set_tty_pgrp   : pgrp -> unit

(*
 * Send a signal to a process.
 *)
val kill           : pgrp -> signal -> unit

(*
 * Set/clear the close on exec flags.
 *)
val close_fd            : Unix.file_descr -> unit
val set_close_on_exec   : Unix.file_descr -> unit
val clear_close_on_exec : Unix.file_descr -> unit

(*
 * Wait:
 *    wait pgrp leader nohang
 *       pgrp: if 0, wait for all groups
 *             otherwise, wait for the specific group
 *       leader: if true, wait only for process group leaders
 *               if false, wait only for process group children
 *       nohang: if true, do not block
 *                  may raise Not_found
 *               if false, block until something happens
 *)
val wait : pgrp -> bool -> bool -> pid * Unix.process_status

(*
 * Create a thread or a process executing the function.
 * Note, the called thread should close the channels it is passed.
 *)
val create_thread : create_thread -> pid

(*
 * Create an actual process.
 *)
val create_process : create_process -> pid

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
