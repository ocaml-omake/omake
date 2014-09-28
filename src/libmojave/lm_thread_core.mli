

module MutexCore     : Lm_thread_sig.MutexSig
module ConditionCore : Lm_thread_sig.ConditionSig with type mutex = MutexCore.t
module ThreadCore    : Lm_thread_sig.ThreadSig

(*
 * A debugging version that flags the errors.
 *)
val debug_mutex: bool ref

