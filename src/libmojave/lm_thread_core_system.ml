(*
 * Our personal implementation of threads.  Each thread has
 * thread-local state.
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
 * Authors: Jason Hickey @email{jyh@cs.caltech.edu}
 *          Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_debug
open Lm_printf

module MutexCore =
struct
   type t = Mutex.t

   let create _ = Mutex.create ()
   let lock     = Mutex.lock
   let try_lock = Mutex.try_lock
   let unlock   = Mutex.unlock
end

module ConditionCore =
struct
   type t     = Condition.t
   type mutex = Mutex.t

   let create    = Condition.create
   let wait      = Condition.wait
   let signal    = Condition.signal
   let broadcast = Condition.broadcast
end

(*
 * Thread implementation.
 *)
module ThreadCore =
struct
   type t = Thread.t
   type id = int

   type 'a result =
      Result of 'a
    | Exn of exn

   let enabled = true
   let create = Thread.create
   let self = Thread.self
   let id = Thread.id
   let join = Thread.join
   let sigmask =
      if Sys.os_type = "Win32" then
         (fun _ mask -> mask)
      else
         Thread.sigmask

   let raise_ctrl_c_wrapper f x =
      let running = ref true in
      let result = ref (Exn (Invalid_argument "Lm_thread_core.raise_ctrl_c_wrapper")) in
      let run () =
         begin try
            result := Result (f x);
         with exn ->
            result := Exn exn
         end;
         running := false
      in
      let rec wait () =
         if !running then begin
            Thread.delay 0.1;
            wait ()
         end else
            match !result with
               Result res -> res
             | Exn exn -> raise exn
      in
         ignore (Thread.create run ());
         wait ()
end

let debug_mutex =
   create_debug (**)
      { debug_name = "mutex";
        debug_description = "Show Mutex locking operations";
        debug_value = false
      }

module MutexCoreDebug =
struct
   type t = {
      lock: Mutex.t;
      id : string;
      mutable locked: int option
   }

   let my_id () = Thread.id (Thread.self ())

   let count =
      let count = ref 0 in
      let lock = Mutex.create () in
         fun () ->
            Mutex.lock lock;
            incr count;
            let res = !count in
               Mutex.unlock lock;
               res

   let create debug = { 
      lock = Mutex.create ();
      id = sprintf "%s.%i" debug (count ());
      locked = None
   }

   let try_lock l = 
      let was_unlocked = Mutex.try_lock l.lock in
         if was_unlocked then begin
            begin match l.locked with
               None -> ()
             | Some id ->
                  if !debug_mutex then
                     eprintf "!!! Lm_thread_core_system.MutexCore.lock: insonsistency! Thread %i found the lock %s unlocked, while we think it was locked by %i@." (my_id()) l.id id
            end;
            if !debug_mutex then
               eprintf "Mutex.[try_]lock: %i locked %s@." (my_id()) l.id;
            l.locked <- Some (my_id())
         end;
         was_unlocked 

   let lock l =
      if not (try_lock l) then begin
         let id =
            match l.locked with
               None -> "unknown"
             | Some i -> string_of_int i
         in
            if !debug_mutex then
               eprintf "Mutex.lock: %i will block on lock %s, held by %s@." (my_id()) l.id id;
            Mutex.lock l.lock;
            if !debug_mutex then
               eprintf "Mutex.lock: %i locked %s@." (my_id()) l.id;
            l.locked <- Some (my_id())
      end
      
   let unlock l =
      begin match l.locked with
         None ->
            if !debug_mutex then
               eprintf "!!! Lm_thread_core_system.MutexCore.unlock: insonsistency! Thread %i unlocking %s, which is already unlocked@." (my_id()) l.id
       | Some id ->
            if id = (my_id()) then
               if !debug_mutex then
                  eprintf "Mutex.unlock: %i unlocking %s@." (my_id()) l.id
            else
               if !debug_mutex then
                  eprintf "!!! Lm_thread_core_system.MutexCore.unlock: insonsistency! Thread %i unlocking %s, which was locked by a different thread %i@." (my_id()) l.id id
      end;
      l.locked <- None;
      Mutex.unlock l.lock
end

module ConditionCoreDebug =
struct
   open MutexCoreDebug

   type t     = Condition.t
   type mutex = MutexCoreDebug.t

   let create    = Condition.create
   let signal    = Condition.signal
   let broadcast = Condition.broadcast

   let wait cond l =
      begin match l.locked with
         None ->
            if !debug_mutex then
               eprintf "!!! Lm_thread_core_system.ConditionCore.wait: insonsistency! Thread %i unlocking %s, which is already unlocked@." (my_id()) l.id
       | Some id ->
            if id = (my_id()) then
               if !debug_mutex then
                  eprintf "Condition.wait: %i unlocking %s@." (my_id()) l.id
            else
               if !debug_mutex then
                  eprintf "!!! Lm_thread_core_system.ConditionCore.wait: insonsistency! Thread %i unlocking %s, which was locked by a different thread %i@." (my_id()) l.id id
      end;
      l.locked <- None;
      Condition.wait cond l.lock;
      begin match l.locked with
         None -> ()
       | Some id ->
            if !debug_mutex then
               eprintf "!!! Lm_thread_core_system.ConditionCore.wait: insonsistency! Thread %i receiving a lock %s, which is already locked by %i@." (my_id()) l.id id;
      end;
      l.locked <- Some (my_id())

end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
