
(* %%MAGICBEGIN%% *)
type 'a marshal_item =
   { mutable item_ref : unit ref;
     mutable item_val : 'a;
     item_hash        : int
   }
(* %%MAGICEND%% *)
(*
 * A version with two equalities.
 * The fine equality is used for cons-hashing, but the coarse
 * version is used for external comparisons.  The fine equality
 * must be a refinement of the coarse equality.
 *)
(* %%MAGICBEGIN%% *)
type 'a marshal_eq_item =
  ('a * 'a marshal_item) marshal_item
(* %%MAGICEND%% *)

(*
 * Statistics.
 *)
type stat =
   { debug              : string;
     mutable reintern   : int;
     mutable compare    : int;
     mutable collisions : int
   }


let current_ref = ref ()

let stats = ref []

let pp_print_stat buf ({ debug      = debug;
         reintern   = reintern;
         compare    = compare;
         collisions = collisions
       } : stat) =
      Format.fprintf buf "@[<hv 3>%s: reintern = %d, compare = %d, collisions = %d@]@\n" (**)
         debug reintern compare collisions

let pp_print_stats buf =
   List.iter (pp_print_stat buf) !stats

(* let () = *)
(*    at_exit (fun () -> pp_print_stats Format.err_formatter) *)



module type MARSHAL = 
sig
  type t

  (* For debugging *)
  val debug : string

  (* The client needs to provide hash and comparison functions *)
  val hash : t -> int
  val compare : t -> t -> int
  val reintern : t -> t
end

module type MARSHAL_EQ = 
sig
   type t

   (* For debugging *)
   val debug : string

   (*
    * The client needs to provide the hash and the two comparison functions.
    *)
   val fine_hash      : t -> int
   val fine_compare   : t -> t -> int

   val coarse_hash    : t -> int
   val coarse_compare : t -> t -> int

   (* Rehash the value *)
   val reintern       : t -> t

end

(*
 * This is what we get.
 *)
module type HashMarshalSig =
sig
   type elt
   type t

   (* Creation *)
   val create   : elt -> t

   (* The intern function fails with Not_found if the node does not already exist *)
   val intern   : elt -> t

   (* Destructors *)
   val get      : t -> elt
   val hash     : t -> int

   (* Comparison *)
   val equal    : t -> t -> bool
   val compare  : t -> t -> int

   (* Rehash the value *)
   val reintern : t -> t
end

module type HashMarshalEqSig =
sig
   include HashMarshalSig (* The default equality is the coarse one *)

   val fine_hash    : t -> int
   val fine_compare : t -> t -> int
   val fine_equal   : t -> t -> bool
end

(*
 * Make a hash item.
 *)
module MakeCoarse (Arg : MARSHAL) =
struct
   type elt = Arg.t
   type t = elt marshal_item

   (* Keep a hash-cons table based on a weak comparison *)
   module WeakCompare =
   struct
      type t = elt marshal_item

      let compare (item1 : t) (item2 : t) =
         let hash1 = item1.item_hash in
         let hash2 = item2.item_hash in
            if hash1 < hash2 then
               -1
            else if hash1 > hash2 then
               1
            else
               Arg.compare item1.item_val item2.item_val
   end;;

   module Table = Lm_map.LmMake (WeakCompare);;

   let table = ref Table.empty

   (*
    * Keep track of collisions for debugging.
    *)
   let stat =
     { debug       = Arg.debug;
       reintern    = 0;
       compare     = 0;
       collisions  = 0
     }

   let () =
     stats := stat :: !stats


   let create = Lm_thread.Synchronize.synchronize begin function elt -> 
       let item : _ marshal_item =
         { item_ref  = current_ref;
           item_val  = elt;
           item_hash = Arg.hash elt
         }
       in
       try Table.find !table item with
         Not_found ->
         table := Table.add !table item item;
         item
     end

   let intern elt =
     Table.find !table { 
       item_ref  = current_ref;
       item_val  = elt;
       item_hash = Arg.hash elt
     }

   (*
    * Reintern.  This will take an item that may-or-may-not be hashed
    * and produce a new one that is hashed.
    *)
   let reintern = Lm_thread.Synchronize.synchronize begin function item1 -> 
       stat.reintern <-  stat.reintern + 1;
       match Table.find !table item1 with 
       | item2 -> 
         (* assert (item2 = item1 ); *)
         if item2 != item1 then begin
           item1.item_val <- item2.item_val;
           item1.item_ref <- current_ref
         end;
         item2
       | exception Not_found ->
         item1.item_val <- Arg.reintern item1.item_val;
         item1.item_ref <- current_ref;
         table := Table.add !table item1 item1;
         item1
     end


   (*
    * Access to the element.
    *)
   let get (item : _ marshal_item) =
     if item.item_ref == current_ref then
       item.item_val
     else
       (reintern item).item_val

   let hash item = item.item_hash

   (*
    * String pointer-based comparison.
    *)
   let compare (item1 : t) (item2 : t) =
     stat.compare <- succ stat.compare;
     let hash1 = item1.item_hash in
     let hash2 = item2.item_hash in
     if hash1 < hash2 then
       -1
     else if hash1 > hash2 then
       1
     else if item1.item_val == item2.item_val then
       0
     else
       let elt1 = get item1 in
       let elt2 = get item2 in
       if elt1 == elt2 then
         0
       else begin
         stat.collisions <- succ stat.collisions;
         let cmp = Arg.compare elt1 elt2 in
         if cmp = 0 then
           invalid_arg "Lm_hash is broken@.";
         cmp
       end

   let equal (item1 : t) (item2 : t) =
     (item1 == item2) || (item1.item_hash = item2.item_hash && get item1 == get item2)
end


module MakeFine (Arg : MARSHAL_EQ) =
struct
   type elt = Arg.t
   type t = elt marshal_eq_item

   module CoarseArg =
   struct
      type t     = Arg.t
      let debug  = Arg.debug ^ ":coarse"

      let hash     = Arg.coarse_hash
      let compare  = Arg.coarse_compare
      let reintern = Arg.reintern
   end;;

   module Coarse = MakeCoarse (CoarseArg);;

   (*
    * We fold the Coarse item into the fine
    * item only so we don't have to create three
    * modules (the final one being a pair of fine
    * and coarse).
    *)
   module FineArg =
   struct
      type t     = Arg.t * Coarse.t
      let debug  = Arg.debug ^ ":fine"

      (*
       * We're assuming that the fine hash is a
       * refinement of the coarse one.
       *)
      let hash (fine, _) =
         Arg.fine_hash fine

      let compare (fine1, _) (fine2, _) =
         Arg.fine_compare fine1 fine2

      let reintern ((fine, coarse) as item) =
         let fine' = Arg.reintern fine in
         let coarse' = Coarse.reintern coarse in
            if fine' == fine && coarse' == coarse then
               item
            else
               fine', coarse'
   end;;

   module Fine = MakeCoarse (FineArg);;

   let create x =
      Fine.create (x, Coarse.create x)

   let intern x =
      Fine.intern (x, Coarse.intern x)

   let get info =
      fst (Fine.get info)

   (*
    * The default functions are the coarse versions.
    *)
   let get_coarse (info : t) =
      snd (Fine.get info)

   let hash (info : t) : int =
      Coarse.hash (get_coarse info)

   let compare item1 item2 =
      Coarse.compare (get_coarse item1) (get_coarse item2)

   let equal item1 item2 =
      Coarse.equal (get_coarse item1) (get_coarse item2)

   (*
    * Also export the fine versions.
    *)
   let fine_hash = Fine.hash
   let fine_compare = Fine.compare
   let fine_equal = Fine.equal

   let reintern = Fine.reintern
end
