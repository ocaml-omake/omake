(*
 * Our personal implementation of threads.  Each thread has
 * thread-local state.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Instititue of Technology, and
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
open Lm_printf
open Lm_thread_sig
open Lm_thread_core
open Lm_int_set

let debug_lock =
   create_debug (**)
      { debug_name = "lock";
        debug_description = "Show locking operations";
        debug_value = false
      }

module Mutex = MutexCore
module Condition = ConditionCore
module MutexDebug = MutexCoreDebug
module ConditionDebug = ConditionCoreDebug

(*
 * The state identifier is just an integer.
 *)
type state_id = int

(*
 * Each thread has a translation to a state,
 * and a condition variable for blocking the thread.
 * When the thread has a lock, the operation is set
 * to the state of the lock.
 *)
type thread =
   { thread_id            : int;
     thread_parent        : int;
     thread_cond          : Condition.t;
     mutable thread_state : state_id
   }

type thread_request =
   RequestRead of thread
 | RequestWrite of thread

(*
 * A handle is a particular copy of a value.
 *    handle_value: the actual data value
 *    handle_queue: the queue of waiting threads
 *    handle_state: the current active state
 *    handle_active: a table of the currently active threads
 *    handle_readers: the number of active readers
 *    handle_writers: the number of active and queued writers
 *)
type handle_state =
   HandleUnlocked
 | HandleReading
 | HandleWriting

type handle_mode =
   ModeReading
 | ModeWriting

type 'a handle =
   { handle_value              : 'a;
     handle_debug              : string;
     handle_queue              : thread_request Queue.t;
     mutable handle_state      : handle_state;
     mutable handle_readers    : int;
     mutable handle_writers    : int;
     mutable handle_active     : handle_mode IntTable.t
   }

(*
 * The info struct is the table of handles for all threads
 * for this specific value.
 *)
type 'a info =
   { info_default         : 'a;
     info_debug           : string;
     info_fork            : ('a -> 'a);
     mutable info_forks   : IntSet.t;
     mutable info_entries : 'a handle IntTable.t
   }

(*
 * An entry corresponds to a single data value.
 * The value can be shared by all threads, or
 * each thread may have a private copy.
 *)
type 'a entry =
   EntryShared of 'a handle
 | EntryPrivate of 'a info

(*
 * The state has:
 *    state_lock: a master lock
 *    state_index: the index for the next state to be allocated.
 *    state_threads: info for each thread we know about
 *)
type state =
   { state_lock               : Mutex.t;
     mutable state_index      : int;
     mutable state_threads    : thread IntTable.t
   }

let state =
   let thread_id = ThreadCore.id (ThreadCore.self ()) in
   let current =
      { thread_id     = thread_id;
        thread_parent = thread_id;
        thread_cond   = Condition.create ();
        thread_state  = 1
      }
   in
      { state_lock    = Mutex.create "Lm_thread.state";
        state_threads = IntTable.add IntTable.empty thread_id current;
        state_index   = 2
      }

(*
 * Debugging.
 *)
let print_thread_id out =
   fprintf out "%d" (ThreadCore.id (ThreadCore.self ()))

(*
 * Perform something with the state lock.
 *)
let with_lock debug f x =
   if !debug_lock then
      eprintf "\tLocking: %t: %s@." print_thread_id debug;
   Mutex.lock state.state_lock;
   try
      let result = f x in
         Mutex.unlock state.state_lock;
         if !debug_lock then
            eprintf "\tUnlocking: %t: %s@." print_thread_id debug;
         result
   with
      exn ->
         Mutex.unlock state.state_lock;
         if !debug_lock then
            eprintf "Unlocking: %t: %s (exception)@." print_thread_id debug;
         raise exn

(*
 * Release the lock temporarily.
 *)
let with_unlock debug f x =
   if !debug_lock then
      eprintf "\tUnlocking temporarily: %t: %s@." print_thread_id debug;
   Mutex.unlock state.state_lock;
   try
      let result = f x in
         Mutex.lock state.state_lock;
         if !debug_lock then
            eprintf "\tRelocking: %t: %s@." print_thread_id debug;
         result
   with
      exn ->
         Mutex.lock state.state_lock;
         if !debug_lock then
            eprintf "Relocking: %t: %s (exception)@." print_thread_id debug;
         raise exn

(*
 * Create a new thread.  Assign a new thread struct.
 * The thread uses the same state as its parent.
 *)
let create_thread f x =
   let parent = ThreadCore.id (ThreadCore.self ()) in
   let pthread =
      try IntTable.find state.state_threads parent with
         Not_found ->
            raise (Invalid_argument "Thread.create: current thread is unknown")
   in
   let start () =
      let id = ThreadCore.id (ThreadCore.self ()) in
      let cleanup () =
         with_lock "create_thread.cleanup" (fun () ->
               state.state_threads <- IntTable.remove state.state_threads id) ()
      in
         (* Set up the new state *)
         with_lock "create_thread.start" (fun () ->
               let thread =
                  { thread_id     = id;
                    thread_parent = parent;
                    thread_cond   = Condition.create ();
                    thread_state  = pthread.thread_state
                  }
               in
                  state.state_threads <- IntTable.add state.state_threads id thread) ();

         (* Run the function, and clean up afterwards *)
         try
            f x;
            cleanup ()
         with
            exn ->
               eprintf "Uncaught thread exception: %s@." (Printexc.to_string exn);
               cleanup ()
   in
   let child = ThreadCore.create start () in
      if !debug_lock then
         eprintf "Started child %i from thread %t@." (ThreadCore.id child) print_thread_id;
      child

(*
 * Get the thread info for the current thread.
 * Every thread must have some info.
 *)
let get_thread_info () =
   let tid = ThreadCore.id (ThreadCore.self ()) in
      try IntTable.find state.state_threads tid with
         Not_found ->
            raise (Invalid_argument "Lm_thread.get_thread_info: unknown thread")

let get_parent_info thread =
   try IntTable.find state.state_threads thread.thread_parent with
      Not_found ->
         raise (Invalid_argument "Lm_thread.get_thread_info: unknown parent thread")

(*
 * Create a new state identifier.
 *)
let create_state () =
   with_lock "create_state" (fun () ->
         let index = state.state_index in
            state.state_index <- succ index;
            index) ()

(*
 * Get the current state identifier.
 *)
let current_state () =
   let thread = get_thread_info () in
      thread.thread_state

(*
 * Set the state for the current thread.
 *)
let set_state id =
   let thread = get_thread_info () in
      thread.thread_state <- id

(*
 * Use the specified state for the current thread.
 *)
let with_state id f x =
   let thread = get_thread_info () in
   let oid = thread.thread_state in
      try
         thread.thread_state <- id;
         let result = f x in
            thread.thread_state <- oid;
            result
      with
         exn ->
            thread.thread_state <- oid;
            raise exn

(*
 * Once a thread finishes, unlock the remaining processes.
 * We assume a lock.
 *)
let release handle =
   let len = Queue.length handle.handle_queue in
      if len <> 0 then
         match Queue.take handle.handle_queue with
            RequestRead thread ->
               Condition.signal thread.thread_cond;
               let rec unblock i =
                  if i <> 0 then
                     let info = Queue.peek handle.handle_queue in
                        match info with
                           RequestRead thread ->
                              ignore (Queue.take handle.handle_queue);
                              Condition.signal thread.thread_cond;
                              unblock (pred i)
                         | RequestWrite _ ->
                              ()
               in
                  unblock (pred len)
          | RequestWrite thread ->
               Condition.signal thread.thread_cond

(*
 * Get the lock record.
 * We assume the state is locked.
 * Just in case, check the lock.
 *)
let rec fork_handle thread info =
   let state_id = thread.thread_state in
      try IntTable.find info.info_entries state_id with
         Not_found ->
            if !debug_lock then
               eprintf "Forking handle: %s: state=%d parent=%d this=%d@." (**)
                  info.info_debug
                  state_id
                  thread.thread_parent
                  thread.thread_id;

            (* Prevent recursion *)
            let () = info.info_forks <- IntSet.add info.info_forks state_id in

            (* Get the new value *)
            let handle_value =
               if state_id = 1 then
                  info.info_default
               else if thread.thread_parent = thread.thread_id then
                  with_unlock "fork_handle" info.info_fork info.info_default
               else
                  let parent = get_parent_info thread in
                  let parent_handle = fork_handle parent info in
                     with_unlock "fork_handle" info.info_fork parent_handle.handle_value
            in

            (* Call the fork function *)
            let handle =
               { handle_value      = handle_value;
                 handle_debug      = Printf.sprintf "%s[%d]" info.info_debug state_id;
                 handle_state      = HandleUnlocked;
                 handle_queue      = Queue.create ();
                 handle_readers    = 0;
                 handle_writers    = 0;
                 handle_active     = IntTable.empty
               }
            in
               if !debug_lock then
                  eprintf "Forked handle: %s: state=%d parent=%d this=%d@." (**)
                     handle.handle_debug
                     state_id
                     thread.thread_parent
                     thread.thread_id;
               info.info_entries <- IntTable.add info.info_entries state_id handle;
               info.info_forks <- IntSet.remove info.info_forks state_id;
               fork_handle thread info

let handle_of_info thread info =
   with_lock "handle_of_info" (fork_handle thread) info

let get_handle thread entry =
   let handle =
      match entry with
         EntryShared handle ->
            handle
       | EntryPrivate info ->
            handle_of_info thread info
   in
      if !debug_lock then
         eprintf "get_handle: %s@." handle.handle_debug;
      handle

(*
 * Reader operations.
 *)
let read_lock thread handle =
   with_lock "read_lock" (fun () ->
         if handle.handle_writers <> 0 then
            begin
               if !debug_lock then
                  eprintf "read_lock: %t: %s: waiting@." print_thread_id handle.handle_debug;
               Queue.add (RequestRead thread) handle.handle_queue;
               Condition.wait thread.thread_cond state.state_lock
            end;
         handle.handle_state   <- HandleReading;
         handle.handle_readers <- succ handle.handle_readers;
         handle.handle_active  <- IntTable.add handle.handle_active thread.thread_id ModeReading) ()

let read_unlock thread handle =
   with_lock "read_unlock" (fun () ->
         let count = pred handle.handle_readers in
            handle.handle_readers <- count;
            handle.handle_active <- IntTable.remove handle.handle_active thread.thread_id;
            if count = 0 then
               begin
                  handle.handle_state <- HandleUnlocked;
                  release handle
               end) ()

(*
 * Perform a read.
 * If we already have a read lock, just run the function.
 *)
let read_wrap thread handle f =
   read_lock thread handle;
   try
      let x = f handle.handle_value in
         read_unlock thread handle;
         x
   with
      exn ->
         read_unlock thread handle;
         raise exn

let read_thread_handle thread handle f =
   if IntTable.mem handle.handle_active thread.thread_id then
      f handle.handle_value
   else
      read_wrap thread handle f

let read entry f =
   if !debug_lock then
      eprintf "Read: %t: enter@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
   let x = read_thread_handle thread handle f in
      if !debug_lock then
         eprintf "Read: %t: %s: end@." print_thread_id handle.handle_debug;
      x

(*
 * Writer operations.
 *)
let write_lock thread handle =
   if !debug_lock then
      eprintf "write_lock: %t: begin@." print_thread_id;
   with_lock "write_lock" (fun () ->
         handle.handle_writers <- succ handle.handle_writers;
         if handle.handle_state <> HandleUnlocked then
            begin
               if !debug_lock then
                  eprintf "write_lock: %t: waiting@." print_thread_id;
               Queue.add (RequestWrite thread) handle.handle_queue;
               Condition.wait thread.thread_cond state.state_lock
            end;
         handle.handle_state <- HandleWriting;
         handle.handle_active <- IntTable.add handle.handle_active thread.thread_id ModeWriting) ()

let write_unlock thread handle =
   if !debug_lock then
      eprintf "write_unlock: %t: begin@." print_thread_id;
   with_lock "write_unlock" (fun () ->
         handle.handle_state <- HandleUnlocked;
         handle.handle_writers <- pred handle.handle_writers;
         handle.handle_active <- IntTable.remove handle.handle_active thread.thread_id;
         release handle) ()

let write_wrap thread handle f =
   if !debug_lock then
      eprintf "write_wrap: %t: begin@." print_thread_id;
   write_lock thread handle;
   try
      let x = f handle.handle_value in
         write_unlock thread handle;
         x
   with
      exn ->
         if !debug_lock then
            eprintf "write_wrap: %t: exception@." print_thread_id;
         write_unlock thread handle;
         raise exn

let write_thread_handle thread handle f =
   let locked =
      try
         match IntTable.find handle.handle_active thread.thread_id with
            ModeReading ->
               raise (Invalid_argument "State.write: handle already has a read lock")
          | ModeWriting ->
               true
      with
         Not_found ->
            false
   in
      if locked then
         if !debug_lock then
            begin
               eprintf "write_thread_handle: %t: begin: already locked@." print_thread_id;
               try
                  let x = f handle.handle_value in
                     eprintf "write_thread_handle: %t: end: already locked@." print_thread_id;
                     x
               with
                  exn ->
                     eprintf "write_thread_handle: %t: end: already locked (exception)@." print_thread_id;
                     raise exn
            end
         else
            f handle.handle_value
      else
         write_wrap thread handle f

let write entry f =
   if !debug_lock then
      eprintf "Write: %t: enter@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
      if !debug_lock then
         eprintf "Writing begin: %t: %s: writers = %d@." print_thread_id handle.handle_debug handle.handle_writers;
      let x = write_thread_handle thread handle f in
         if !debug_lock then
            eprintf "Writing end: %t: %s: writers = %d@." print_thread_id handle.handle_debug handle.handle_writers;
         x

(*
 * Unlock temporarily.
 *)
let unlock_unlock thread handle =
   let mode =
      try IntTable.find handle.handle_active thread.thread_id with
         Not_found ->
            raise (Invalid_argument "State.unlock: thread does not have a lock")
   in
   let () =
      match mode with
         ModeReading ->
            read_unlock thread handle
       | ModeWriting ->
            write_unlock thread handle
   in
      mode

let unlock_lock thread handle mode =
   match mode with
      ModeReading ->
         read_lock thread handle
    | ModeWriting ->
         write_lock thread handle

(*
 * Unlock function.
 *)
let unlock_thread_handle thread handle f =
   let mode = unlock_unlock thread handle in
      try
         let x = f () in
            unlock_lock thread handle mode;
            x
      with
         exn ->
            unlock_lock thread handle mode;
            raise exn

let unlock entry f =
   if !debug_lock then
      eprintf "Unlock: %t: begin@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
   let x = unlock_thread_handle thread handle f in
      if !debug_lock then
         eprintf "Unlock: %t: %s: end@." print_thread_id handle.handle_debug;
      x

(*
 * Get the value.
 * Verify that this thread has a lock on the object.
 *)
let get_thread_handle thread handle =
   if IntTable.mem handle.handle_active thread.thread_id then
      handle.handle_value
   else
      raise (Invalid_argument (Printf.sprintf "State.get: %s: entry is not locked" handle.handle_debug))

let get entry =
   if !debug_lock then
      eprintf "Get: %t: begin@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
   let x = get_thread_handle thread handle in
      if !debug_lock then
         eprintf "Get: %t: %s: end@." print_thread_id handle.handle_debug;
      x

(*
 * Create a shared value.
 *)
let shared_val debug x =
   let handle =
      { handle_value      = x;
        handle_debug      = debug;
        handle_state      = HandleUnlocked;
        handle_queue      = Queue.create ();
        handle_readers    = 0;
        handle_writers    = 0;
        handle_active     = IntTable.empty
      }
   in
      EntryShared handle

(*
 * Create a private value.
 * This means that each state has its own copy of the
 * data.
 *)
let private_val debug x fork =
   let info =
      { info_default = x;
        info_debug   = debug;
        info_fork    = fork;
        info_forks   = IntSet.empty;
        info_entries = IntTable.empty
      }
   in
      EntryPrivate info

(*
 * Thread implementation.
 *)
module Thread =
struct
   type t = ThreadCore.t
   type id = int

   let enabled = ThreadCore.enabled
   let create = create_thread
   let self = ThreadCore.self
   let join = ThreadCore.join
   let id = ThreadCore.id
   let sigmask = ThreadCore.sigmask
   let raise_ctrl_c_wrapper = ThreadCore.raise_ctrl_c_wrapper
end

(*
 * States are identified by integers.
 *)
module State =
struct
   type t = state_id

   (* Silly redefinition to break type recursion *)
   type 'a local_entry = 'a entry
   type 'a entry  = 'a local_entry

   (*
    * State operations.
    *)
   let current = current_state
   let create = create_state
   let set = set_state
   let with_state = with_state

   (*
    * Variables.
    *)
   let shared_val = shared_val
   let private_val = private_val
   let read = read
   let write = write
   let unlock = unlock
   let get = get
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
