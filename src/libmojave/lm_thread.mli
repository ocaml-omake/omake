open Lm_thread_sig

module Mutex     : MutexSig
module Condition : ConditionSig with type mutex = Mutex.t
module Thread    : ThreadSig
module State     : StateSig

val debug_lock : bool ref

module Synchronize : sig 
  val synchronize : ('a -> 'b) -> 'a -> 'b
end 
