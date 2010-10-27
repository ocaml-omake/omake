(*
 * Execution codes.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology
 * and HRL Laboratories, LLC
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
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_location

open Omake_node
open Omake_exec_id
open Omake_options
open Omake_command_type

(*
 * Type of process codes.
 *)
type process_code =
   ProcessFailed
 | ProcessStarted of id

(*
 * Print flags.
 *)
type ('exp, 'pid, 'value) print_flag =
   PrintEager of 'exp
 | PrintLazy  of 'exp
 | PrintExit  of 'exp * int * 'value * float

(*
 * Internal wait status.
 *)
type ('exp, 'pid, 'value) wait_internal_code =
   WaitInternalExited of id * int * 'value
 | WaitInternalNotify of Lm_notify.event
 | WaitInternalStarted of bool
 | WaitInternalNone

(*
 * External wait status.
 * WaitServer count: a new server started, willing to serve "count" more jobs
 *)
type ('exp, 'pid, 'value) wait_code =
   WaitExited of id * int * 'value
 | WaitServer of int
 | WaitNotify of Lm_notify.event
 | WaitNone

(*
 * Types of upcalls.
 *)
type ('exp, 'pid, 'value) shell =
   { shell_eval           : Unix.file_descr -> Unix.file_descr -> 'exp -> 'pid;
     shell_info           : 'exp -> command_flag list * Dir.t * Node.t;
     shell_kill           : 'pid -> unit;
     shell_wait           : 'pid -> Unix.process_status * 'value;
     shell_error_value    : 'value;
     shell_print_exp      : out_channel -> 'exp -> unit;
     shell_print_exn      : out_channel -> exn -> unit;
     shell_is_failure_exn : exn -> bool
   }

type ('exp, 'pid, 'value) status_fun = id -> ('exp, 'pid, 'value) print_flag -> unit
type output_fun = id -> string -> int -> int -> unit

(*
 * Internal execution server has a few extra functions.
 *)
module type ExecServer =
sig
   (*
    * Command processor.
    *)
   type ('exp, 'pid, 'value) t

   (*
    * Create the processor.
    * The directory is the current working root directory.
    *)
   val create : string -> ('exp, 'pid, 'value) t

   (*
    * Close it, and possibly deallocate state.
    *)
   val close : ('exp, 'pid, 'value) t -> unit

   (*
    * Start a command, and return the process ID.
    *)
   val spawn :
      ('exp, 'pid, 'value) t ->                 (* Current state *)
      ('exp, 'pid, 'value) shell ->             (* The shell that does evaluation *)
      id ->                                     (* Id for the new process *)
      output_fun ->                             (* Function to handle output from stdout *)
      output_fun ->                             (* Function to handle output from stderr *)
      ('exp, 'pid, 'value) status_fun ->        (* Function to handle status commands *)
      Node.t ->                                 (* Target being built *)
      'exp list ->                              (* Commands to execute *)
      process_code                              (* The process id *)

   (*
    * The internal versions are polled using select.
    *)
   val descriptors : ('exp, 'pid, 'value) t -> Unix.file_descr list

   (*
    * Handle input from one of the descriptors.
    *)
   val handle : ('exp, 'pid, 'value) t -> omake_options -> Unix.file_descr -> unit

   (*
    * Wait for any one of the commands to finish.
    *)
   val wait : ('exp, 'pid, 'value) t -> omake_options -> ('exp, 'pid, 'value) wait_internal_code
end

(*
 * The execution service.
 *)
module type ExecService =
sig
   (*
    * Command processor.
    *)
   type ('exp, 'pid, 'value) t

   (*
    * Create the processor.
    * The directory is the current working root directory.
    *)
   val create : Dir.t -> omake_options -> ('exp, 'pid, 'value) t

   (*
    * Close it, and possibly deallocate state.
    *)
   val close : ('exp, 'pid, 'value) t -> unit

   (*
    * Start a command, and return the process ID.
    *)
   val spawn :
      ('exp, 'pid, 'value) t ->                 (* Current state *)
      ('exp, 'pid, 'value) shell ->             (* Evaluate a shell command *)
      omake_options ->                          (* Current options in effect *)
      output_fun ->                             (* Function to handle the OMake messages meant for stdout *)
      output_fun ->                             (* Function to handle output from stdout *)
      output_fun ->                             (* Function to handle output from stderr *)
      string ->                                 (* Name of this command *)
      Node.t ->                                 (* Target being built *)
      'exp list ->                              (* Commands to execute *)
      process_code                              (* The process id *)

   (*
    * Wait for any one of the commands to finish.
    *)
   val wait : ('exp, 'pid, 'value) t -> omake_options -> ('exp, 'pid, 'value) wait_code

   (*
    * Notify when a file changes.
    *)
   val monitor : ('exp, 'pid, 'value) t -> Node.t -> unit
   val monitor_tree : ('exp, 'pid, 'value) t -> Dir.t -> unit

   (*
    * Get the next file change notification.
    * This function blocks.  Use wait if you want
    * nonblocking behavior.
    *)
   val pending : ('exp, 'pid, 'value) t -> bool
   val next_event : ('exp, 'pid, 'value) t -> Lm_notify.event
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
