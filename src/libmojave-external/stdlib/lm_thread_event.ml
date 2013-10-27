(*
 * This is a reimplementation of the OCaml Event module.
 * We have weaker requirements on synchronization.
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
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
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
open Lm_debug
open Lm_printf
open Lm_thread

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A channel queues the senders and receivers.
 * There invariant is that either the readers or
 * the writers queue is empty.
 *
 * The reader and writer contain the condition
 * that the blocked process is waiting on, the
 * cell for the return value, and a flag for if the
 * action has already been performed.
 *
 * The performed flag is a different reference
 * because of the wrap function, which uses a local
 * reference to capture the return value.
 *)
type lock = Condition.t

type 'a reader =
   { read_lock : lock;
     read_value : 'a option ref;
     read_performed : bool ref
   }

type 'a writer =
   { write_lock : lock;
     write_value : 'a;
     write_performed : bool ref
   }

type 'a channel =
   { chan_readers : 'a reader Queue.t;
     chan_writers : 'a writer Queue.t
   }

(*
 * We want to be able to wrap values of receive events.
 * The type system won't allow the desired type quantification,
 * so we wrap the actions in closures.  The poll event
 * looks for an available value, and the block event
 * waits for a communication, given a function to do the
 * actual blocking.
 *)
type 'a event_info =
   { event_poll : unit -> 'a option;
     event_block : (unit -> unit) -> lock -> bool ref -> 'a option ref -> unit
   }

(*
 * An event is a function to compute a flattened
 * list of events.
 *)
type 'a event = 'a event_info list -> 'a event_info list

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * For speed, keep a queue of free condition variables.
 * This list never grows larger than the number of threads.
 *)
let alloc_lock, free_lock =
   let lock_list = ref [] in
   let alloc () =
      match !lock_list with
         lock :: tl ->
            lock_list := tl;
            lock
       | [] ->
            Condition.create ()
   in
   let free lock =
      lock_list := lock :: !lock_list
   in
      alloc, free

(*
 * Keep a global mutex for mutual exclusion on all
 * sync operations.
 *)
let global_lock = Mutex.create "Lm_thread_event"

let sync_lock () =
   eprintf "Lm_thread_event.lock@.";
   Mutex.lock global_lock

let sync_unlock () =
   eprintf "Lm_thread_event.unlock@.";
   Mutex.unlock global_lock

let sync_wait lock =
   Condition.wait lock global_lock

let sync_signal lock =
   Condition.signal lock

(*
 * Create a new channel.
 *)
let new_channel () =
   { chan_readers = Queue.create ();
     chan_writers = Queue.create ()
   }

(*
 * Polling.
 *)
let rec try_send chan x () =
   try
      let reader = Queue.take chan.chan_readers in
         if !(reader.read_performed) then
            try_send chan x ()
         else
            begin
               reader.read_performed := true;
               reader.read_value := Some x;
               sync_signal reader.read_lock;
               Some ()
            end
   with
      Queue.Empty ->
         None

let rec try_receive chan () =
   try
      let writer = Queue.take chan.chan_writers in
         if !(writer.write_performed) then
            try_receive chan ()
         else
            let x = writer.write_value in
               writer.write_performed := true;
               sync_signal writer.write_lock;
               Some x
   with
      Queue.Empty ->
         None

let try_wrap f poll () =
   match poll () with
      Some x ->
         Some (f x)
    | None ->
         None

(*
 * Place a request on a queue.
 * The `block' argument is the function to perform the suspend.
 *)
let block_send chan x block cond performed slot =
   let write = { write_lock = cond; write_value = x; write_performed = performed } in
      Queue.add write chan.chan_writers;
      block ();
      slot := Some ()

let block_receive chan block cond performed slot =
   let read = { read_lock = cond; read_value = slot; read_performed = performed } in
      Queue.add read chan.chan_readers;
      block ()

let block_wrap f block' block cond performed slot =
   let slot' = ref None in
      block' block cond performed slot';
      match !slot' with
         Some x ->
            slot := Some (f x)
       | None ->
            ()

(*
 * Basic events.
 *)
let always x =
   let event =
      { event_poll = (fun () -> Some x);
        event_block = (fun _ _ _ -> raise (Invalid_argument "Event.always"))
      }
   in
      (fun events -> event :: events)

let send chan x =
   let event =
      { event_poll = try_send chan x;
        event_block = block_send chan x;
      }
   in
      (fun events -> event :: events)

let receive chan =
   let rec event =
      { event_poll = try_receive chan;
        event_block = block_receive chan
      }
   in
      (fun events -> event :: events)

let wrap event f events =
   let wrap_event { event_poll = poll; event_block = block } =
      { event_poll = try_wrap f poll;
        event_block = block_wrap f block
      }
   in
      (List.map wrap_event (event [])) @ events

let rec choose events events' =
   match events with
      event :: events ->
         choose events (event events')
    | [] ->
         events'

let delay f events =
   (f () events)

let sequence events =
   let cell = ref events in
   let wrap_event events' =
      match !cell with
         event :: events ->
            let pop x =
               cell := events;
               x
            in
               wrap event pop events'
       | [] ->
            events'
   in
      wrap_event

let sequence_final events =
   let cell = ref events in
   let wrap_event events' =
      match !cell with
         [event] ->
            event events'
       | event :: events ->
            let pop x =
               cell := events;
               x
            in
               wrap event pop events'
       | [] ->
            events'
   in
      wrap_event

(*
 * Poll for a value in the event list.
 *)
let rec poll_events = function
   { event_poll = poll' } :: events ->
      begin
         match poll' () with
            (Some _) as x ->
               x
          | None ->
               poll_events events
      end
 | [] ->
      None

(*
 * Block until an event happens.
 *)
let block events =
   let lock = alloc_lock () in
   let slot = ref None in
   let performed = ref false in
   let rec block = function
      { event_block = block' } :: events ->
         block' (fun () -> block events) lock performed slot
    | [] ->
         sync_wait lock
   in
      block events;
      free_lock lock;
      !slot

(*
 * Poll for available input.
 *)
let poll event =
   let events = event [] in
      sync_lock ();
      let x = poll_events events in
         sync_unlock ();
         x

(*
 * Complete the specified event.
 *)
let level = ref 0

let sync _pid event =
   let events = event [] in
      sync_lock ();
      let x =
         match poll_events events with
            Some x ->
               x
          | None ->
               match block events with
                  Some x ->
                     x
                | None ->
                     eprintf "Failed%t" eflush;
                     raise (Invalid_argument "Event.sync")
      in
         sync_unlock ();
         x

(*
 * Shorthand.
 *)
let select pid events =
   sync pid (choose events)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
