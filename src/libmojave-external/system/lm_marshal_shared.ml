(*
 * This is a "shared" marshaler.  We keep a table that maps
 * OCaml blocks to string representations.  The objects are saved
 * with weak pointers.  The marshaler is intended to represent a
 * distributed shared memory.
 *     o When a value is stored in the memory, entries
 *       are saved for all the new values, and a string
 *       is created that can be broadcast to all copies
 *       of the marshaler.
 *     o Periodically, the table can be scanned for values
 *       that have been locally garbage collected, and a string
 *       is created to broadcast those entries.
 *     o If a value is garbage collected by all copies of the
 *       marshaler, it can be removed from the table.
 *
 * Each value is associated with a number that uniquely identifies it.
 *
 * Each marshaler is identified by a unique string, and the marshalers
 * form a group.
 *
 * Invariants:
 *     o The marshalers agree on the group membership.
 *     o If a value is in multiple tables, it is identified
 *       by the same number.
 *
 * In order to get the second invariant, and also allow the group
 * membership to change, we use a page-table scheme.  Each marshaler
 * has a local address space, and all marshalers agree on a global
 * address space.  Each marshaler "owns" a set of pages, and allocates
 * addresses only from its own pages.  There is a translation-lookaside-
 * buffer to translate between global and local addresses.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1999-2005 PRL Group, Cornell University and Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
open Lm_large_array
open Lm_large_array_weak
open Lm_marshal_sig

(*
 * Tags for references to the table.
 *)
let shared_get = 0
let shared_set = 1

(*
 * We need some C helper functions.
 *)
type pointer_hash_type

external create_hash : unit -> pointer_hash_type = "ml_create_pointer_hash"
external insert : pointer_hash_type -> int -> Obj.t -> unit = "ml_pointer_hash_insert"
external lookup : pointer_hash_type -> Obj.t LargeWeakArray.t -> Obj.t -> int = "ml_pointer_hash_lookup"
external local_write_block : Obj.t -> string -> int -> unit = "ml_write_block"

(*
 * Marshal module.
 *)
module Marshal (Buf : BufSig) =
struct
   (************************************************************************
    * CONSTANTS AND TYPES                                                  *
    ************************************************************************)

   (*
    * Page constants.
    *)
   let page_shift = 16
   let page_size = 1 lsl page_shift
   let max_pages = 1 lsl 13

   (*
    * This is the size of the words we save when
    * we marshal.
    *)
   let word_size = 4

   (*
    * This a a very general form of free list.
    *)
   type free_list =
      FreeFun of (unit -> int * free_list)

   (*
    * This is a local marshal/unmarshal buffer.
    * Here is how we manage allocation of new pages.
    *    The marshal_local_alloc_list is the list of pages that
    *    we have already allocated.  After a gc, we search through this
    *    list to find free entries.  Once this list is exhausted, we allocate
    *    pages from the global pool.  When a new value is marshaled, we send
    *    the new global allocations in the header, and the real allocation
    *    occurs when the message is returned (we require total ordering
    *    of updates).
    *
    * Fields:
    *    marshal_id : every marshaler has a unique id
    *    marshal_view : group membership (all options are (Some id))
    *
    *    marshal_values : this saves the local versions of the values.
    *      The index into the array is the unique identifier of
    *      the value.
    *    marshal_copies : this is a copy of the value,
    *      so that it can be resurrected if needed.
    *    marshal_hash : C structure for looking up objects
    *
    *    marshal_local_tlb : translation from local indices to global
    *    marshal_global_tlb : translation from global indices to local
    *    marshal_tmp_tlb : used during unmarshaling if there is a global reassignment
    *
    *    marshal_free_index : index of next possibly free entry in the local free list
    *    marshal_free_max : next index after current page
    *    marshal_free_list : list of pages we own that we are searching
    *    marshal_alloc_list : list of pages we own in local address space
    *
    *    marshal_local_free_list : list of free pages in local space
    *    marshal_global_free_list : list of free pages in global space
    *    marshal_lookahead_free_list : list of free pages in global space
    *       This list is used to temporarily allocate pages from the global
    *       space when an object is marshaled.  The list of lookahead pages
    *       is sent with the message.  When the message is unmarshaled, the
    *       pages are allocated from the global space, and a translation
    *       occurs.
    *    marshal_lookahead_pages : list of global pages that have been
    *       allocated but not committed.
    *
    *    marshal_page_table : owner assignment to pages, index is in global space
    *)
   type 'a t =
      { marshal_id : Lm_id.t;
        mutable marshal_view : Lm_id.t option array;

        mutable marshal_values : Obj.t LargeWeakArray.t;
        mutable marshal_copies : string LargeArray.t;
        mutable marshal_hash : pointer_hash_type;

        marshal_local_tlb : int array;
        marshal_global_tlb : int array;
        marshal_tmp_tlb : int array;

        mutable marshal_free_index : int;
        mutable marshal_free_max : int;
        mutable marshal_free_list : int list;
        mutable marshal_alloc_list : int list;

        mutable marshal_local_free_list : free_list;
        mutable marshal_global_free_list : free_list;
        mutable marshal_lookahead_free_list : free_list;
        mutable marshal_lookahead_pages : int list;

        mutable marshal_page_table : Lm_id.t option array
      }

   (************************************************************************
    * FREE LIST IMPLEMENTATIONS                                            *
    ************************************************************************)

   (*
    * Free list counting up from the given number.
    * This list should be functional.
    *)
   let rec tail_free_list i =
      FreeFun (fun () -> i, tail_free_list (succ i))

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Empty string is used to initialize object copies.
    *)
   let null_string = ""

   (*
    * Create a marshal buffer.
    * We always pre-allocate the first page.
    *)
   let create () =
      let id = Lm_id.create () in
         { marshal_id = id;
           marshal_view = [| Some id |];

           marshal_values = LargeWeakArray.create ();
           marshal_copies = LargeArray.create null_string;
           marshal_hash = create_hash ();

           marshal_local_tlb = Array.create max_pages 0;
           marshal_global_tlb = Array.create max_pages 0;
           marshal_tmp_tlb = Array.create max_pages 0;

           marshal_free_index = 0;
           marshal_free_max = 0;
           marshal_free_list = [];
           marshal_alloc_list = [];

           marshal_local_free_list = tail_free_list 0;
           marshal_global_free_list = tail_free_list 0;
           marshal_lookahead_free_list = tail_free_list 0;
           marshal_lookahead_pages = [];

           marshal_page_table = Array.create max_pages None
         }

   (************************************************************************
    * MARSHALING                                                           *
    ************************************************************************)

   (*
    * Marshal an object.
    *)
   let marshal info buf obj =
      (*
       * Break apart the info.
       *)
      let { marshal_values = marshal_values;
            marshal_copies = marshal_copies;
            marshal_hash = marshal_hash;
          } = info
      in

      (*
       * Keep a list of global pages we are using for this object.
       *)
      let allocated_global_pages = ref [] in

      (*
       * Global buffer operations.
       *)
      let global_write_string buf' =
         Buf.write buf buf' 0 (String.length buf')
      in
      let global_write_int i =
         Buf.write_int buf i
      in
      let global_write_shared_get i =
         Buf.write_int2 buf shared_get i
      in
      let global_write_shared_set i =
         Buf.write_int2 buf shared_set i
      in
      let global_write_header tag i =
         Buf.write_tag buf tag i
      in

      (*
       * Local buffer operations.
       *)
      let local_write_int i buf index =
         buf.[index] <- Char.chr ((i lsr 23) land 255);
         buf.[index + 1] <- Char.chr ((i lsr 15) land 255);
         buf.[index + 2] <- Char.chr ((i lsr 7) land 255);
         buf.[index + 3] <- Char.chr (((i lsl 1) land 255) + 1)
      in
      let local_write_shared_get i buf index =
         buf.[index] <- Char.chr ((i lsr 21) land 255);
         buf.[index + 1] <- Char.chr ((i lsr 13) land 255);
         buf.[index + 2] <- Char.chr ((i lsr 5) land 255);
         buf.[index + 3] <- Char.chr (((i lsl 3) land 255) + (shared_get lsl 2) + 2)
      in
      let local_write_header tag i buf index =
         buf.[index] <- Char.chr tag;
         buf.[index + 1] <- Char.chr ((i lsr 14) land 255);
         buf.[index + 2] <- Char.chr ((i lsr 6) land 255);
         buf.[index + 3] <- Char.chr ((i lsl 2) land 255)
      in

      (*
       * Find a free location somewhere in the table.
       *)
      let rec alloc_search index max =
         if index = max then
            (* Current page has been fully searched *)
            match info.marshal_free_list with
               page :: tl ->
                  (* There is another local page to search *)
                  let index = page lsl page_shift in
                  let max = index + page_size in
                     info.marshal_free_list <- tl;
                     info.marshal_free_index <- index;
                     info.marshal_free_max <- max;
                     alloc_search index max
             | [] ->
                  (* Allocate a global page *)
                  let page, glob =
                     match info.marshal_global_free_list with
                        FreeFun f ->
                           f ()
                  in
                  let index = page lsl page_shift in
                  let max = index + page_size in
                     info.marshal_global_free_list <- glob;
                     info.marshal_alloc_list <- page :: info.marshal_alloc_list;
                     info.marshal_free_index <- succ index;
                     info.marshal_free_max <- max;
                     allocated_global_pages := page :: !allocated_global_pages;
                     index
         else if LargeArray.get marshal_copies index == null_string then
            alloc_search (succ index) max
         else
            begin
               info.marshal_free_index <- succ index;
               index
            end
      in
      let alloc_slot () =
         alloc_search info.marshal_free_index info.marshal_free_max
      in

      (*
       * For now, we allocate buffers on the heap.
       *)
      let alloc_buffer words =
         String.create (words * word_size)
      in

      (*
       * Save a value in the marshal table.
       * This constructs a string that should be broadcast
       * to all marshalers in the group.  The lbuf is
       * a local buffer that is used to construct copies of
       * the objects being marshaled.
       *)
      let rec marshal_value obj lbuf lindex =
         if Obj.is_block obj then
            let i = lookup marshal_hash marshal_values obj in
               if i >= 0 then
                  begin
                     global_write_shared_get i;
                     local_write_shared_get i lbuf lindex
                  end
               else
                  let tag = Obj.tag obj in
                  let count = Obj.size obj in
                     if count = 0 then
                        (* Atoms are never saved to the table *)
                        begin
                           global_write_header tag 0;
                           local_write_header tag 0 lbuf lindex
                        end
                     else if tag < Obj.no_scan_tag && tag != Obj.infix_tag then
                        (*
                         * This is a tuple that has not been saved in the table yet.
                         * Allocate a free slot, and save the object.  Then marshal
                         * all the subvalues, and construct the string representation.
                         *)
                        let index = alloc_slot () in
                        let lbuf' = alloc_buffer (succ count) in
                           (* Save the object *)
                           insert marshal_hash index obj;
                           LargeWeakArray.set marshal_values index obj;
                           LargeArray.set marshal_copies index lbuf';

                           (* Indicate that we are creating a value *)
                           global_write_shared_set index;

                           (* Write the header word into the new local buffer *)
                           local_write_header tag count lbuf' 0;

                           (* Write the subwords into the global and new local buffers *)
                           marshal_value_entries obj 0 count lbuf' word_size;

                           (* Copy the string to the output buffer *)
                           global_write_string lbuf';

                           (* Write the new tuple into the enclosing block *)
                           local_write_shared_get index lbuf lindex

                     else if tag = Obj.string_tag || tag = Obj.double_tag || tag = Obj.double_array_tag then
                        (*
                         * This is an abstract block.
                         * We have to create a marshaled version unfortunately.
                         *)
                        let index = alloc_slot () in
                        let lbuf' = alloc_buffer (succ count) in
                           (* Save the object *)
                           insert marshal_hash index obj;
                           LargeWeakArray.set marshal_values index obj;
                           LargeArray.set marshal_copies index lbuf';

                           (* Indicate that we are creating a value *)
                           global_write_shared_set index;

                           (* Write a header into the new local buffer *)
                           local_write_header tag count lbuf' 0;

                           (* Copy the data *)
                           local_write_block obj lbuf' word_size;

                           (* Copy the string to the output buffer *)
                           global_write_string lbuf';

                           (* Write the tuple into the enclosing header *)
                           local_write_shared_get index lbuf lindex
                     else
                        (* This is something we're not familiar with *)
                        raise (Invalid_argument (Lm_printf.sprintf "Obj_marshal.marshal: unknown object with tag %d" tag))
         else
            let i = ((Obj.magic obj) : int) in
               global_write_int i;
               local_write_int i lbuf lindex

      and marshal_value_entries obj i count lbuf lindex =
         if i != count then
            begin
               marshal_value (Obj.field obj i) lbuf lindex;
               marshal_value_entries obj (succ i) count lbuf (lindex + word_size)
            end
      in
      let lbuf = "XXXXXXXXXXXXXXXX" in
         marshal_value (Obj.repr obj) lbuf 0;
         !allocated_global_pages

   (************************************************************************
    * UNMARSHALING                                                         *
    ************************************************************************)

   (*
    * Take the string and reconstruct the object that it refers to.
    *)
   (*
   let unmarshal info buf =
      (*
       * Break apart the info.
       *)
      let { marshal_values = marshal_values;
            marshal_copies = marshal_copies;
            marshal_hash = marshal_hash;
            marshal_rank = marshal_rank;
            marshal_group_size = marshal_group_size
          } = info
      in

      (*
       * Global buffer operations.
       *)
      let global_read_int = Buf.read_int buf in
      let global_read_shared = Buf.read_int2_tag buf in
      let global_read_index = Buf.read_int2_value buf in
      let global_read_tag = Buf.read_value_tag buf in
      let global_read_size = Buf.read_value_value buf in
      let global_read_type = Buf.read_value_type buf in

      (*
       * Local buffer operations.
       *)
      let local_write_int i buf index =
         buf.[index] <- Char.chr ((i lsr 23) land 255);
         buf.[index + 1] <- Char.chr ((i lsr 15) land 255);
         buf.[index + 2] <- Char.chr ((i lsr 7) land 255);
         buf.[index + 3] <- Char.chr (((i lsl 1) land 255) + 1)
      in
      let local_write_shared_get i buf index =
         buf.[index] <- Char.chr ((i lsr 21) land 255);
         buf.[index + 1] <- Char.chr ((i lsr 13) land 255);
         buf.[index + 2] <- Char.chr ((i lsr 5) land 255);
         buf.[index + 3] <- Char.chr (((i lsl 3) land 255) + 2)
      in
      let local_write_shared_set i buf index =
         buf.[index] <- Char.chr ((i lsr 21) land 255);
         buf.[index + 1] <- Char.chr ((i lsr 13) land 255);
         buf.[index + 2] <- Char.chr ((i lsr 5) land 255);
         buf.[index + 3] <- Char.chr (((i lsl 3) land 255) + 3)
      in
      let local_write_header tag i buf index =
         buf.[index] <- Char.chr tag;
         buf.[index + 1] <- Char.chr ((i lsr 14) land 255);
         buf.[index + 2] <- Char.chr ((i lsr 6) land 255);
         buf.[index + 3] <- Char.chr ((i lsl 2) land 255)
      in

      (*
       * Unmarshal a string, and store the objects it
       * refers to in a table.
       *)
      let rec unmarshal_value () =
         match global_read_type () with
            IntValue ->
               global_read_int ()
          | Int2Value ->
               let tag = global_read_shared () in
               let index = global_read_index () in
                  if tag = shared_set then
                     (* The next object in the buffer is an object that should be saved in the table *)
                  if get_flag then
                     (* Get a value from the table *)
                     match LargeWeakArray.get marshal_values index with
                        Some obj ->
                           obj
                      | None ->
                           (* We accidentally collected this value *)
                           resurrect index
                  else
                     (* The buffer contains a formatted value that should be saved at the index *)

         if Obj.is_block obj then
            let i = lookup marshal_hash marshal_values obj in
               if i >= 0 then
                  begin
                     global_write_shared_get i;
                     local_write_shared_get i lbuf lindex
                  end
               else
                  let tag = Obj.tag obj in
                  let count = Obj.size obj in
                     if count = 0 then
                        (* Atoms are never saved to the table *)
                        begin
                           global_write_header tag 0;
                           local_write_header tag 0 lbuf lindex
                        end
                     else if tag < Obj.no_scan_tag && tag != Obj.infix_tag then
                        (*
                         * This is a tuple that has not been saved in the table yet.
                         * Allocate a free slot, and save the object.  Then marshal
                         * all the subvalues, and construct the string representation.
                         *)
                        let index = alloc_slot () in
                        let lbuf' = alloc_buffer (succ count) in
                           (* Save the object *)
                           insert marshal_hash obj index;
                           LargeWeakArray.set marshal_values index obj;
                           LargeArray.set marshal_copies index lbuf';

                           (* Write the header word into the new local buffer *)
                           local_write_header tag count lbuf' 0;

                           (* Write the subwords into the global and new local buffers *)
                           marshal_value_entries obj 0 count lbuf' word_size;

                           (* Copy the string to the output buffer *)
                           global_write_shared_set index;
                           global_write_string lbuf';

                           (* Write the new tuple into the enclosing block *)
                           local_write_shared_get index lbuf lindex

                     else if tag = Obj.string_tag || tag = Obj.double_tag || tag = Obj.double_array_tag then
                        (*
                         * This is an abstract block.
                         * We have to create a marshaled version unfortunately.
                         *)
                        let index = alloc_slot () in
                        let lbuf' = alloc_buffer (succ count) in
                           (* Save the object *)
                           insert marshal_hash obj index;
                           LargeWeakArray.set marshal_values index obj;
                           LargeArray.set marshal_copies index lbuf';

                           (* Write a header into the new local buffer *)
                           local_write_header tag count lbuf' 0;

                           (* Copy the data *)
                           local_write_block obj lbuf' word_size;

                           (* Copy the string to the output buffer *)
                           global_write_shared_set index;
                           global_write_string lbuf';

                           (* Write the tuple into the enclosing header *)
                           local_write_shared_get index lbuf lindex
                     else
                        (* This is something we're not familiar with *)
                        raise (Invalid_argument (Lm_printf.sprintf "Obj_marshal.marshal: unknown object with tag %d" tag))
         else
            let i = ((Obj.magic obj) : int) in
               global_write_int i;
               local_write_int i lbuf lindex

      and marshal_value_entries obj i count lbuf lindex =
         if i != count then
            begin
               marshal_value (Obj.field obj i) lbuf lindex;
               marshal_value_entries obj (succ i) count lbuf (lindex + word_size)
            end
      in
      let lbuf = "XXXXXXXXXXXXXXXX" in
         marshal_value (Obj.repr obj) lbuf 0
   *)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
