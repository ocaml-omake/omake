(*
 * Table indexed by opaque handles.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2007 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_int_set

(*
 * Handles.  These need to be heap allocated so that we can register
 * finalization functions.
 *)
module type HandleTableSig =
sig
   type 'a t
   type handle

   val create : unit -> 'a t
   val add : 'a t -> 'a -> handle
   val find : 'a t -> handle -> 'a
end;;

module HandleTable : HandleTableSig =
struct
   type 'a t =
      { mutable hand_table : 'a IntTable.t;
        mutable hand_index : int
      }

   (* This must be heap-allocated *)
   type handle = { handle_index : int }

   let create () =
      { hand_table = IntTable.empty;
        hand_index = 0
      }

   let free table hand =
      table.hand_table <- IntTable.remove table.hand_table hand.handle_index

   let add table x =
      let i = table.hand_index in
      let hand = { handle_index = i } in
         Gc.finalise (free table) hand;
         table.hand_index <- succ i;
         table.hand_table <- IntTable.add table.hand_table i x;
         hand

    let find table hand =
       IntTable.find table.hand_table hand.handle_index
end;;

(************************************************************************
 * Integer handles.
 *)
module type IntHandleTableSig =
sig
   type handle
   type 'a t

   (* Handles *)
   val create_handle : 'a t -> int -> handle
   val new_handle    : 'a t -> handle
   val int_of_handle : handle -> int

   (* Table *)
   val create : unit -> 'a t
   val add    : 'a t -> handle -> 'a -> unit
   val remove : 'a t -> handle -> unit
   val find   : 'a t -> handle -> 'a
   val find_any : 'a t -> handle -> 'a
   val find_any_handle : 'a t -> int -> handle
   val find_value : 'a t -> int -> 'a -> handle
end;;

module IntHandleTable : IntHandleTableSig =
struct
   module Table = IntTable;;

   type handle = int ref

   type 'a t =
      { mutable table_index   : int;
        mutable table_entries : (handle * 'a) list Table.t
      }

   (************************************************
    * Association lists.
    *)
   let replaceq entries key x =
      let rec loop entries1 entries2 =
         match entries2 with
            (key', x') :: entries2 when key' == key ->
               List.rev_append entries1 ((key, x) :: entries2)
          | h :: entries2 ->
               loop (h :: entries1) entries2
          | [] ->
               (key, x) :: entries
      in
         loop [] entries

   let rec assq_value entries x =
      match entries with
         (key, x') :: _ when x' == x -> key
       | _ :: t -> assq_value entries x
       | [] -> raise Not_found

   (************************************************
    * Handle operations.
    *)
   let create_handle table index =
      let index2 = table.table_index in
         table.table_index <- max (index + 1) index2;
         ref index

   let new_handle table =
      let index = table.table_index in
         table.table_index <- succ index;
         ref index

   let int_of_handle = (!)

   (************************************************
    * Table operations.
    *)
   let create () =
      { table_index   = 0;
        table_entries = Table.empty
      }

   let add table hand x =
      let entries =
         Table.filter_add table.table_entries !hand (fun entries ->
               let entries =
                  match entries with
                     Some entries -> entries
                   | None -> []
               in
                  replaceq entries hand x)
      in
         table.table_entries <- entries

   let remove table hand =
      let entries =
         Table.filter_remove table.table_entries !hand (fun entries ->
               let entries = List.remove_assq hand entries in
                  match entries with
                     [] -> None
                   | _ :: _ -> Some entries)
      in
         table.table_entries <- entries

   let find table hand =
      List.assq hand (Table.find table.table_entries !hand)

   let find_any table hand =
      match Table.find table.table_entries !hand with
         (_, x) :: _ -> x
       | [] -> raise Not_found

   let find_any_handle table index =
      match Table.find table.table_entries index with
         (hand, _) :: _ -> hand
       | [] -> raise Not_found

   let find_value table index x =
      assq_value (Table.find table.table_entries index) x
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
