(*
 * File change notifications.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
open Omake_node
open Omake_exec_type

(*
 * Local server.
 *)
module Notify :
sig
   include ExecServer

   (*
    * Watch a node for changes.
    *)
   val monitor      : ('exp, 'pid, 'value) t -> Node.t -> unit
   val monitor_tree : ('exp, 'pid, 'value) t -> Dir.t -> unit

   (*
    * Get the next notification event.
    *)
   val pending    : ('exp, 'pid, 'value) t -> bool
   val next_event : ('exp, 'pid, 'value) t -> Lm_notify.event
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
