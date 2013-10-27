(*
 * Our personal implementation of threads.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Instititute of Technology, and
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
open Lm_thread_sig

module Mutex     : MutexSig
module Condition : ConditionSig with type mutex = Mutex.t
module Thread    : ThreadSig
module State     : StateSig

val debug_lock : bool ref

module MutexDebug     : MutexSig
module ConditionDebug : ConditionSig with type mutex = MutexDebug.t

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
