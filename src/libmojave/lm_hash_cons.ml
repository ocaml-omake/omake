
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

   (* We need both directions.    *)
   type state =
     { mutable key_table : int KeyTable.t;
       mutable int_table : elt array
     }
   (* %%MAGICEND%% *)

   let create_state () =
     { key_table = KeyTable.empty;
       int_table = [||]
     }

   let length state =
     KeyTable.cardinal state.key_table

   let set state (i : int) (x : elt) =
     let table = state.int_table in
     let len = Array.length table in
     if len = 0 then
       state.int_table <- Array.create 32 x
     else if i = len then
       let table2 = Array.create (len * 2) x in
       Array.blit table 0 table2 0 len;
       state.int_table <- table2
     else
       table.(i) <- x

   let icreate state (item : elt) : int =
     try KeyTable.find state.key_table item with
       Not_found ->
       let index = KeyTable.cardinal state.key_table in
       state.key_table <- KeyTable.add state.key_table item index;
       set state index item ;
       index

   let create state x =
     icreate state x 

   let get state index =
     state.int_table.(index)

   let hash index = index

   let compare index1 index2 =
     index1 - index2

   let map_array f state =
     Array.mapi f
       (Array.sub state.int_table 0 (KeyTable.cardinal state.key_table))

   let fold f x state =
     let len = KeyTable.cardinal state.key_table in
     let rec fold i x =
       if i = len then
         x
       else
         fold ( i + 1) (f x i) in
     fold 0 x
end
