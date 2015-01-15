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
open Lm_printf
open Lm_debug
open Lm_thread

(*
module Mutex     = MutexDebug
module Condition = ConditionDebug
*)

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
 * Data structures.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end

module IntTable = Lm_map.LmMake (IntCompare)

(*
 * Jobs are identified by descriptor.
 * If the job is not visible, it is not reported
 * to wait.
 *)
type job =
   { job_id  : int;
     job_fun : unit -> unit;
     job_visible : bool
   }

(*
 * We keep a master lock: only one thread is allowed to run at
 * any given time.  This doesn't really affect performance,
 * since OCaml enforces this restriction anyway.  Note: the
 * threads should release the lock before they wait for I/O.
 *)
type pool =
   { mutable pool_pid          : int;
     mutable pool_size         : int;
     mutable pool_members      : Thread.t list;
     mutable pool_ready        : job list;
     mutable pool_ready_length : int;
     mutable pool_running      : job IntTable.t;
     mutable pool_finished     : job list;
     mutable pool_break        : bool;
     pool_finished_wait        : Condition.t;
     pool_consumer_wait        : Condition.t;
     pool_lock                 : Mutex.t
   }

(*
 * The pool.
 *)
let pool =
   { pool_pid           = 1;
     pool_size          = 0;
     pool_members       = [];
     pool_ready         = [];
     pool_ready_length  = 0;
     pool_running       = IntTable.empty;
     pool_finished      = [];
     pool_break         = false;
     pool_finished_wait = Condition.create ();
     pool_consumer_wait = Condition.create ();
     pool_lock          = Mutex.create "Lm_thread_pool"
   }

(*
 * Lock for the main thread.
 *)
let () = Mutex.lock pool.pool_lock

(*
 * Threads are enabled.
 *)
let enabled = true

(*
 * Temporarily unlock the pool while performing IO.
 * The check_status function may generate exceptions.
 *)
let blocking_section f x =
   Mutex.unlock pool.pool_lock;
   try
      let y = f x in
         Mutex.lock pool.pool_lock;
         y
   with
      exn ->
         Mutex.lock pool.pool_lock;
         raise exn

let resume_inner_section f x =
   Mutex.lock pool.pool_lock;
   try
      let y = f x in
         Mutex.unlock pool.pool_lock;
         y
   with
      exn ->
         Mutex.unlock pool.pool_lock;
         raise exn

(*
 * Thread main loop.
 *)
let thread_main_loop () =
   try
      let id = Thread.id (Thread.self ()) in
      if !debug_thread then
         eprintf "Thread %d: starting@." id;
      let _ = Thread.sigmask Unix.SIG_SETMASK [Sys.sigint; Sys.sigquit] in
         Mutex.lock pool.pool_lock;
         if !debug_thread then
            eprintf "Thread %d: entered main loop@." id;
         let rec loop () =
            match pool.pool_ready with
               job :: rest ->
                  pool.pool_ready <- rest;
                  pool.pool_ready_length <- pred pool.pool_ready_length;
                  pool.pool_running <- IntTable.add pool.pool_running job.job_id job;
                  if !debug_thread then
                     eprintf "Thread %d: calling function: %d@." id job.job_id;
                  (try job.job_fun () with
                      Sys.Break ->
                         if !debug_thread then
                            eprintf "Lm_thread_pool_system: %d: Break@." job.job_id;
                         pool.pool_break <- true;
                         Condition.signal pool.pool_finished_wait
                    | exn ->
                         eprintf "Lm_thread_pool_system: thread raised exception: %s: %d@." (Printexc.to_string exn) job.job_id);
                  pool.pool_running <- IntTable.remove pool.pool_running job.job_id;
                  if job.job_visible then begin
                     pool.pool_finished <- job :: pool.pool_finished;
                     Condition.signal pool.pool_finished_wait
                  end;
                  if pool.pool_break then
                     Mutex.unlock pool.pool_lock
                  else
                     loop ()
             | [] ->
                  if !debug_thread then
                     eprintf "Thread %d: waiting@." id;
                  Condition.wait pool.pool_consumer_wait pool.pool_lock;
                  if !debug_thread then
                     eprintf "Thread %d: waited@." id;
                  if not pool.pool_break then
                     loop ()
         in
            loop ()
   with Sys.Break ->
      pool.pool_break <- true;
      if !debug_thread then
         eprintf "Lm_thead_pool_system: break@.";
      Mutex.unlock pool.pool_lock;
      Condition.signal pool.pool_finished_wait;
      Condition.signal pool.pool_consumer_wait

(*
 * Start a thread doing something.
 *)
let create visible f =
   (*
    * XXX: TODO: we may want to support "restarting" a pool after it was killed
    * with a Ctrl-C, but for now the transition to pool_break state is a kiss of
    * death.
    *)
   if pool.pool_break then
      raise Sys.Break;

   let id = succ pool.pool_pid in
   let job =
      { job_id = id;
        job_fun = f;
        job_visible = visible
      }
   in
      pool.pool_pid <- id;
      pool.pool_ready <- job :: pool.pool_ready;
      pool.pool_ready_length <- succ pool.pool_ready_length;

      (* Enlarge the pool if needed *)
      if pool.pool_size < pool.pool_ready_length + IntTable.cardinal pool.pool_running then begin
         pool.pool_size <- succ pool.pool_size;
         if !debug_thread then
            eprintf "Starting a new worker thread, total worker threads: %d@." pool.pool_size;
         pool.pool_members <- (Thread.create thread_main_loop ()) :: pool.pool_members
      end;

      (* Wake up one of the waiters if they are waiting *)
      Condition.signal pool.pool_consumer_wait;
      if !debug_thread then
         eprintf "Create: %d@." id;
      id

(*
 * Wait until something happens, and return the identifier of
 * all the threads that completed.
 *)
let wait () =
   (* Wait until a thread finishes *)
   while pool.pool_finished = [] && not pool.pool_break do
      if !debug_thread then
         eprintf "Main: waiting: %d+%d@." pool.pool_ready_length (IntTable.cardinal pool.pool_running);
      Condition.wait pool.pool_finished_wait pool.pool_lock;
      if !debug_thread then
         eprintf "Main: waited@.";
   done;

   if pool.pool_break then
      raise Sys.Break;

   (* Return pids of all the threads that finished *)
   let pids = List.map (fun job -> job.job_id) pool.pool_finished in
      pool.pool_finished <- [];
      pids

(*
 * Wait until a specific pid disappears.
 *)
let waitpid id =
   (* Wait until a thread finishes *)
   while IntTable.mem pool.pool_running id && not pool.pool_break do
      if !debug_thread then
         eprintf "Main: waiting: %d+%d@." pool.pool_ready_length (IntTable.cardinal pool.pool_running);
      Condition.wait pool.pool_finished_wait pool.pool_lock;
      if !debug_thread then
         eprintf "Main: waited@.";
   done;

   if pool.pool_break then
      raise Sys.Break;

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
