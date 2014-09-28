(** Our personal implementation of threads.  Each thread has
    thread-local state. *)


let debug_lock =
   Lm_debug.create_debug (**)
      { debug_name = "lock";
        debug_description = "Show locking operations";
        debug_value = false
      }

module Mutex = Lm_thread_core.MutexCore
module Condition = Lm_thread_core.ConditionCore
(* module MutexDebug = MutexCoreDebug *)
(* module ConditionDebug = ConditionCoreDebug *)

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
     mutable handle_active     : handle_mode Lm_int_set.IntTable.t
   }

(*
 * The info struct is the table of handles for all threads
 * for this specific value.
 *)
type 'a info =
   { info_default         : 'a;
     info_debug           : string;
     info_fork            : ('a -> 'a);
     mutable info_forks   : Lm_int_set.IntSet.t;
     mutable info_entries : 'a handle Lm_int_set.IntTable.t
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
     mutable state_threads    : thread Lm_int_set.IntTable.t
   }

let state =
   let thread_id = Lm_thread_core.ThreadCore.id (Lm_thread_core.ThreadCore.self ()) in
   let current =
      { thread_id     = thread_id;
        thread_parent = thread_id;
        thread_cond   = Condition.create ();
        thread_state  = 1
      }
   in
      { state_lock    = Mutex.create "Lm_thread.state";
        state_threads = Lm_int_set.IntTable.add Lm_int_set.IntTable.empty thread_id current;
        state_index   = 2
      }

(*
 * Debugging.
 *)
let print_thread_id out =
   Format.fprintf out "%d" (Lm_thread_core.ThreadCore.id (Lm_thread_core.ThreadCore.self ()))

(*
 * Perform something with the state lock.
 *)
let with_lock debug f x =
   if !debug_lock then
      Format.eprintf "\tLocking: %t: %s@." print_thread_id debug;
   Mutex.lock state.state_lock;
   try
      let result = f x in
         Mutex.unlock state.state_lock;
         if !debug_lock then
            Format.eprintf "\tUnlocking: %t: %s@." print_thread_id debug;
         result
   with
      exn ->
         Mutex.unlock state.state_lock;
         if !debug_lock then
            Format.eprintf "Unlocking: %t: %s (exception)@." print_thread_id debug;
         raise exn

(*
 * Release the lock temporarily.
 *)
let with_unlock debug f x =
   if !debug_lock then
      Format.eprintf "\tUnlocking temporarily: %t: %s@." print_thread_id debug;
   Mutex.unlock state.state_lock;
   try
      let result = f x in
         Mutex.lock state.state_lock;
         if !debug_lock then
            Format.eprintf "\tRelocking: %t: %s@." print_thread_id debug;
         result
   with
      exn ->
         Mutex.lock state.state_lock;
         if !debug_lock then
            Format.eprintf "Relocking: %t: %s (exception)@." print_thread_id debug;
         raise exn

(*
 * Create a new thread.  Assign a new thread struct.
 * The thread uses the same state as its parent.
 *)
let create_thread f x =
   let parent = Lm_thread_core.ThreadCore.id (Lm_thread_core.ThreadCore.self ()) in
   let pthread =
      try Lm_int_set.IntTable.find state.state_threads parent with
         Not_found ->
            raise (Invalid_argument "Thread.create: current thread is unknown")
   in
   let start () =
      let id = Lm_thread_core.ThreadCore.id (Lm_thread_core.ThreadCore.self ()) in
      let cleanup () =
         with_lock "create_thread.cleanup" (fun () ->
               state.state_threads <- Lm_int_set.IntTable.remove state.state_threads id) ()
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
                  state.state_threads <- Lm_int_set.IntTable.add state.state_threads id thread) ();

         (* Run the function, and clean up afterwards *)
         try
            f x;
            cleanup ()
         with
            exn ->
               Format.eprintf "Uncaught thread exception: %s@." (Printexc.to_string exn);
               cleanup ()
   in
   let child = Lm_thread_core.ThreadCore.create start () in
      if !debug_lock then
         Format.eprintf "Started child %i from thread %t@." (Lm_thread_core.ThreadCore.id child) print_thread_id;
      child

(*
 * Get the thread info for the current thread.
 * Every thread must have some info.
 *)
let get_thread_info () =
   let tid = Lm_thread_core.ThreadCore.id (Lm_thread_core.ThreadCore.self ()) in
      try Lm_int_set.IntTable.find state.state_threads tid with
         Not_found ->
            raise (Invalid_argument "Lm_thread.get_thread_info: unknown thread")

let get_parent_info thread =
   try Lm_int_set.IntTable.find state.state_threads thread.thread_parent with
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
      try Lm_int_set.IntTable.find info.info_entries state_id with
         Not_found ->
            if !debug_lock then
               Format.eprintf "Forking handle: %s: state=%d parent=%d this=%d@." (**)
                  info.info_debug
                  state_id
                  thread.thread_parent
                  thread.thread_id;

            (* Prevent recursion *)
            let () = info.info_forks <- Lm_int_set.IntSet.add info.info_forks state_id in

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
                 handle_active     = Lm_int_set.IntTable.empty
               }
            in
               if !debug_lock then
                  Format.eprintf "Forked handle: %s: state=%d parent=%d this=%d@." (**)
                     handle.handle_debug
                     state_id
                     thread.thread_parent
                     thread.thread_id;
               info.info_entries <- Lm_int_set.IntTable.add info.info_entries state_id handle;
               info.info_forks <- Lm_int_set.IntSet.remove info.info_forks state_id;
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
         Format.eprintf "get_handle: %s@." handle.handle_debug;
      handle

(*
 * Reader operations.
 *)
let read_lock thread handle =
   with_lock "read_lock" (fun () ->
         if handle.handle_writers <> 0 then
            begin
               if !debug_lock then
                  Format.eprintf "read_lock: %t: %s: waiting@." print_thread_id handle.handle_debug;
               Queue.add (RequestRead thread) handle.handle_queue;
               Condition.wait thread.thread_cond state.state_lock
            end;
         handle.handle_state   <- HandleReading;
         handle.handle_readers <- succ handle.handle_readers;
         handle.handle_active  <- Lm_int_set.IntTable.add handle.handle_active thread.thread_id ModeReading) ()

let read_unlock thread handle =
   with_lock "read_unlock" (fun () ->
         let count = pred handle.handle_readers in
            handle.handle_readers <- count;
            handle.handle_active <- Lm_int_set.IntTable.remove handle.handle_active thread.thread_id;
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
   if Lm_int_set.IntTable.mem handle.handle_active thread.thread_id then
      f handle.handle_value
   else
      read_wrap thread handle f

let read entry f =
   if !debug_lock then
      Format.eprintf "Read: %t: enter@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
   let x = read_thread_handle thread handle f in
      if !debug_lock then
         Format.eprintf "Read: %t: %s: end@." print_thread_id handle.handle_debug;
      x

(*
 * Writer operations.
 *)
let write_lock thread handle =
   if !debug_lock then
      Format.eprintf "write_lock: %t: begin@." print_thread_id;
   with_lock "write_lock" (fun () ->
         handle.handle_writers <- succ handle.handle_writers;
         if handle.handle_state <> HandleUnlocked then
            begin
               if !debug_lock then
                  Format.eprintf "write_lock: %t: waiting@." print_thread_id;
               Queue.add (RequestWrite thread) handle.handle_queue;
               Condition.wait thread.thread_cond state.state_lock
            end;
         handle.handle_state <- HandleWriting;
         handle.handle_active <- Lm_int_set.IntTable.add handle.handle_active thread.thread_id ModeWriting) ()

let write_unlock thread handle =
   if !debug_lock then
      Format.eprintf "write_unlock: %t: begin@." print_thread_id;
   with_lock "write_unlock" (fun () ->
         handle.handle_state <- HandleUnlocked;
         handle.handle_writers <- pred handle.handle_writers;
         handle.handle_active <- Lm_int_set.IntTable.remove handle.handle_active thread.thread_id;
         release handle) ()

let write_wrap thread handle f =
   if !debug_lock then
      Format.eprintf "write_wrap: %t: begin@." print_thread_id;
   write_lock thread handle;
   try
      let x = f handle.handle_value in
         write_unlock thread handle;
         x
   with
      exn ->
         if !debug_lock then
            Format.eprintf "write_wrap: %t: exception@." print_thread_id;
         write_unlock thread handle;
         raise exn

let write_thread_handle thread handle f =
   let locked =
      try
         match Lm_int_set.IntTable.find handle.handle_active thread.thread_id with
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
               Format.eprintf "write_thread_handle: %t: begin: already locked@." print_thread_id;
               try
                  let x = f handle.handle_value in
                     Format.eprintf "write_thread_handle: %t: end: already locked@." print_thread_id;
                     x
               with
                  exn ->
                     Format.eprintf "write_thread_handle: %t: end: already locked (exception)@." print_thread_id;
                     raise exn
            end
         else
            f handle.handle_value
      else
         write_wrap thread handle f

let write entry f =
   if !debug_lock then
      Format.eprintf "Write: %t: enter@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
      if !debug_lock then
         Format.eprintf "Writing begin: %t: %s: writers = %d@." print_thread_id handle.handle_debug handle.handle_writers;
      let x = write_thread_handle thread handle f in
         if !debug_lock then
            Format.eprintf "Writing end: %t: %s: writers = %d@." print_thread_id handle.handle_debug handle.handle_writers;
         x

(*
 * Unlock temporarily.
 *)
let unlock_unlock thread handle =
   let mode =
      try Lm_int_set.IntTable.find handle.handle_active thread.thread_id with
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
      Format.eprintf "Unlock: %t: begin@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
   let x = unlock_thread_handle thread handle f in
      if !debug_lock then
         Format.eprintf "Unlock: %t: %s: end@." print_thread_id handle.handle_debug;
      x

(*
 * Get the value.
 * Verify that this thread has a lock on the object.
 *)
let get_thread_handle thread handle =
   if Lm_int_set.IntTable.mem handle.handle_active thread.thread_id then
      handle.handle_value
   else
      raise (Invalid_argument (Printf.sprintf "State.get: %s: entry is not locked" handle.handle_debug))

let get entry =
   if !debug_lock then
      Format.eprintf "Get: %t: begin@." print_thread_id;
   let thread = get_thread_info () in
   let handle = get_handle thread entry in
   let x = get_thread_handle thread handle in
      if !debug_lock then
         Format.eprintf "Get: %t: %s: end@." print_thread_id handle.handle_debug;
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
        handle_active     = Lm_int_set.IntTable.empty
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
        info_forks   = Lm_int_set.IntSet.empty;
        info_entries = Lm_int_set.IntTable.empty
      }
   in
      EntryPrivate info

(*
 * Thread implementation.
 *)
module Thread =
struct
   type t = Lm_thread_core.ThreadCore.t
   type id = int

   let enabled = Lm_thread_core.ThreadCore.enabled
   let create = create_thread
   let self = Lm_thread_core.ThreadCore.self
   let join = Lm_thread_core.ThreadCore.join
   let id = Lm_thread_core.ThreadCore.id
   let sigmask = Lm_thread_core.ThreadCore.sigmask
   let raise_ctrl_c_wrapper = Lm_thread_core.ThreadCore.raise_ctrl_c_wrapper
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
 * We need to work nicely with threads.
 *
 * Note that the reintern function may be recursive, so we need to account for cases,
 * where the current thread is already holding the lock.
 * Almost every accesses come from the main thread with very little if any contention from other
 * threads. This makes it more effiecient to use a single global lock (as opposed to having a
 * separate lock for each instance of the functor), so that mutually recursive reintern calls only
 * have to lock one lock, not all of them.
 *
 * Finally, we do not care about race conditions for the statistics
 *)
module Synchronize : sig
   val synchronize : ('a -> 'b) -> 'a -> 'b
end = struct
   let lock_mutex = Mutex.create "Lm_hash.Synchronize"
   let lock_id = ref None

   let unsynchronize () =
      lock_id := None;
      Mutex.unlock lock_mutex

   let synchronize f x =
      let id = Thread.id (Thread.self ()) in
         match !lock_id with
            Some id' when id = id' ->
               (*
                * We are already holding the lock. This means:
                *  - we do not have to do anything special
                *  - reading the shared lock_id ref could not have created a race condition
                *)
               f x
          | _ ->
               Mutex.lock lock_mutex;
               lock_id := Some id;
               try
                  let res = f x in
                     unsynchronize();
                     res
               with exn ->
                  unsynchronize();
                  raise exn
   let  synchronize =
     if Thread.enabled then
       synchronize
     else
       (fun f -> f)

end
