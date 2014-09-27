
(**
 * A generic hash module to make comparisons faster.
 * This version uses a state for hash-consing.
 *)
(** Table-based hashing. *)
module Make (Arg : sig
   type t
   (* For debugging *)
   val debug : string

   (* The client needs to provide hash and comparison functions *)
   val hash : t -> int
   val compare : t -> t -> int
end
)  =
struct
   (* %%MAGICBEGIN%% *)
   type elt = Arg.t
   type t = int
   module KeyTable = Lm_map.LmMake (Arg);;

   (* bi-directions.    *)
   type state =
     { mutable keys : int KeyTable.t;
       mutable ints : elt array
     }
   (* %%MAGICEND%% *)

   let create_state () =
     { keys = KeyTable.empty;
       ints = [||]
     }

   let length state =
     KeyTable.cardinal state.keys

   let set state (i : int) (x : elt) =
     let table = state.ints in
     let len = Array.length table in
     if len = 0 then
       state.ints <- Array.create 32 x
     else if i = len then
       let table2 = Array.create (len * 2) x in
       Array.blit table 0 table2 0 len;
       state.ints <- table2
     else
       table.(i) <- x

   let create state (item : elt) : int =
     try KeyTable.find state.keys item with
       Not_found ->
       let index = KeyTable.cardinal state.keys in
       state.keys <- KeyTable.add state.keys item index;
       set state index item ;
       index

   let get state (index : int) : elt =
     state.ints.(index)

   let hash index = index

   let compare index1 index2 =
     index1 - index2

   let map_array f state =
     Array.mapi f
       (Array.sub state.ints 0 (KeyTable.cardinal state.keys))

   let fold f x state =
     let len = KeyTable.cardinal state.keys in
     let rec fold i x =
       if i = len then
         x
       else
         fold ( i + 1) (f x i) in
     fold 0 x
end
