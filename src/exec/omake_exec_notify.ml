(*
 * File notification server.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, Caltifornia Institute of Technology,
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

open Omake_node
open Omake_exec_type

module Notify =
struct
   (*
    * Notification services are performed by Lm_notify.
    *)
   type ('exp, 'pid, 'value) t =
      { mutable notify_server : Lm_notify.t option;
        mutable notify_event  : Lm_notify.event option
      }

   (*
    * Create the server.
    * Initially disconnected.
    *)
   let create _ =
      { notify_server = None;
        notify_event = None
      }

   (*
    * Close it.
    *)
   let close notify =
      match notify.notify_server with
         Some server ->
            Lm_notify.close server;
            notify.notify_server <- None;
            notify.notify_event <- None
       | None ->
            ()

   (*
    * Monitor a directory.
    *)
   let start notify =
      match notify.notify_server with
         Some server ->
            server
       | None ->
            let server = Lm_notify.create () in
               notify.notify_server <- Some server;
               server

   let monitor notify node =
      let dir = Dir.absname (Node.dir node) in
      let server = start notify in
         Lm_notify.monitor server dir false

   let monitor_tree notify dir =
      let dir = Dir.absname dir in
      let server = start notify in
         Lm_notify.monitor server dir true

   (*
    * Get the next event.
    * This assumes a notification thread is not currently running.
    *)
   let pending notify =
      match notify with
         { notify_event = Some _ } ->
            true
       | { notify_server = Some server } ->
            Lm_thread_pool.blocking_section Lm_notify.pending server
       | { notify_server = None } ->
            false

   let next_event notify =
      match notify with
         { notify_event = Some event } ->
            notify.notify_event <- None;
            event
       | { notify_server = Some server } ->
            Lm_thread_pool.blocking_section (Lm_thread.Thread.raise_ctrl_c_wrapper Lm_notify.next_event) server
       | { notify_server = None } ->
            raise (Failure "Omake_exec_notify.next_event: no monitors")

   (*
    * Notify server does not implement processes.
    *)
   let spawn _ _ _ _ _ _ _ =
      raise (Invalid_argument "Omake_exec_notify.spawn: processes are not supported")

   (*
    * File descriptors.
    *)
   let descriptors notify =
      match Lm_notify.enabled, notify with
         true, { notify_event = None;
                 notify_server = Some server
         } ->
            begin match Lm_notify.file_descr server with
               Some fd ->
                  [fd]
             | None ->
                  (* We are probabaly on Win32? *)
                  [Unix.stdin]
            end
       | _ ->
            []

   (*
    * Handle input from the descriptor.
    *)
   let handle notify _ _ =
      match notify with
         { notify_event = None;
           notify_server = Some server
         } when Lm_notify.pending server ->
            let event = Lm_thread_pool.blocking_section Lm_notify.next_event server in
               notify.notify_event <- Some event
       | _ ->
            ()

   (*
    * Wait for a command to finish.
    *)
   let wait notify _ =
      handle notify () (); (* XXX HACK: nogin *)
      match notify.notify_event with
         Some event ->
            notify.notify_event <- None;
            WaitInternalNotify event
       | None ->
            WaitInternalNone
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
