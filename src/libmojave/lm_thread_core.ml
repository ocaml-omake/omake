(*
 * An null implementation of threads.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology, and
 * HRL Laboratories, LLC
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_debug

let debug_mutex =
   create_debug (**)
      { debug_name = "mutex";
        debug_description = "Show Mutex locking operations";
        debug_value = false
      }

(*
 * Locks are not required when not using threads.
 * We only track the "locked" state to produce the correct results in the try_lock function.
 *)
module MutexCore =
struct
   (* true = free; false = locked *)
   type t = bool ref

   let create _ =
      ref true

   let lock l =
      l := false 

   let try_lock l =
      let res = !l in
         l := false;
         res 

   let unlock l =
      l := true
end

(*
 * Conditions are not required when not using threads.
 *)
module ConditionCore =
struct
   type t = unit
   type mutex = MutexCore.t

   let create () =
      ()

   let wait _ l =
      l := false

   let signal () =
      ()

   let broadcast () =
      ()
end

module MutexCoreDebug = MutexCore
module ConditionCoreDebug = ConditionCore

(*
 * Threads are null.  The create function doesn't work without
 * threads, so raise an exception.
 *)
module ThreadCore =
struct
   type t = unit
   type id = unit

   let enabled = false

   let create _f _x =
      raise (Invalid_argument "Lm_thread.Thread.create: threads are not enabled in this application")

   let join _t =
      raise (Invalid_argument "Lm_thread.Thread.join: threads are not enabled in this application")

   let self () =
      ()

   let id () =
      0

   let sigmask _ mask =
      mask

   let raise_ctrl_c_wrapper f x = f x
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
