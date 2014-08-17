open Lm_set_sig

module LmMake (Ord : OrderedType) : (LmSet with type elt = Ord.t)
module LmMakeDebug (Ord : OrderedTypeDebug) : (LmSetDebug with type elt = Ord.t)
