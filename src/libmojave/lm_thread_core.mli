open Lm_thread_sig

module MutexCore     : MutexSig
module ConditionCore : ConditionSig with type mutex = MutexCore.t
module ThreadCore    : ThreadSig

(*
 * A debugging version that flags the errors.
 *)
val debug_mutex: bool ref

(* module MutexCoreDebug     : MutexSig *)
(* module ConditionCoreDebug : ConditionSig with type mutex = MutexCoreDebug.t *)
