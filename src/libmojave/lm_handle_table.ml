(*
 * Table indexed by opaque handles.
 *)

(*
 * Handles.  These need to be heap allocated so that we can register
 * finalization functions.
 *)

type 'a t =
  { mutable table : 'a Lm_int_set.IntTable.t;
    mutable index : int
  }

(* This must be heap-allocated *)
type handle = { index : int }

let create () =
  { table = Lm_int_set.IntTable.empty;
    index = 0
  }

let free table hand =
  table.table <- Lm_int_set.IntTable.remove table.table hand.index

let add (table : 'a t) (x : 'a) =
  let i = table.index in
  let hand = { index = i } in
  Gc.finalise (free table) hand;
  table.index <- succ i;
  table.table <- Lm_int_set.IntTable.add table.table i x;
  hand

let find table hand =
  Lm_int_set.IntTable.find table.table hand.index
