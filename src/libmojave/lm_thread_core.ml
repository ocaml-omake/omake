let debug_mutex =
   Lm_debug.create_debug (**)
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
