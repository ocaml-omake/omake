(*
 * On Win32, select does not work on pipes.  Instead, we use
 * threads to call all the handlers.  We keep a thread pool.
 * When a thread makes progress, it wakes up the main process,
 * and returns to the pool.  Each file descriptor is assigned
 * a thread.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2005 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_debug

(*
 * Build debugging.
 *)
let debug_thread =
   create_debug (**)
      { debug_name = "thread";
        debug_description = "Display thread debugging";
        debug_value = false
      }

(*
 * Threads are not enabled.
 *)
let enabled = false

(*
 * Temporarily unlock the pool while performing IO.
 *)
let blocking_section f x =
   f x

let resume_inner_section f x =
   f x

(*
 * Start a thread doing something.
 *)
let create _ =
   raise (Invalid_argument "Lm_thread_pool_null.create: threads are not enabled")

(*
 * Wait until something happens.
 *)
let wait () =
   raise (Invalid_argument "Lm_thread_pool_null.wait: threads are not enabled")

let waitpid _ =
   raise (Invalid_argument "Lm_thread_pool_null.waitpid: threads are not enabled")

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
