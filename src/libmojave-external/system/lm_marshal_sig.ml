(*
 * Modules used in the marshaler.
 *
 * ----------------------------------------------------------------
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

(*
 * There are several formats for words.
 *
 *         ---------------------
 *    int: | value           |1|
 *         ---------------------
 *         31                1 0
 *
 *         ---------------------
 *   int2: | value         |X10|
 *         ---------------------
 *         31              3   0
 *
 *         ---------------------
 *    tag: | tag  | value   |00|
 *         ---------------------
 *         31     23        2  0
 *)
type value_type =
   IntValue
 | TaggedValue
 | Int2Value

module type BufSig =
sig
   type buf
   type wbuf
   type rbuf

   (* Data is written in this many bytes *)
   val word_size : int

   (* Creation/deletion *)
   val create : unit -> wbuf
   val to_static : wbuf -> buf
   val to_read : buf -> rbuf
   val free : rbuf -> unit

   (*
    * Write values to the buffer.
    * This never fails because the buffer is infinite.
    *)
   val write : wbuf -> string -> int -> int -> unit
   val write_int : wbuf -> int -> unit
   val write_int2 : wbuf -> int -> int -> unit
   val write_tag : wbuf -> int -> int -> unit

   (*
    * Read from the buffer.
    *)
   val read : rbuf -> string -> int -> int -> unit
   val read_value_type : rbuf -> value_type
   val read_int : rbuf -> int
   val read_int2_tag : rbuf -> int
   val read_int2_value : rbuf -> int
   val read_value_tag : rbuf -> int
   val read_value_value : rbuf -> int
end

(*
 * This is a "shared" marshaler, meaning that there can be several
 * marshaler that provide a model of shared memory over a communication
 * channel.
 *
 *      -------------               -------------
 *      | Marshal 1 |     ...       | Marshal n |
 *      -------------               -------------
 *           |             |              |
 *           \             |              /
 *            ----------\  |  /-----------
 *                      |  |  |
 *                ---------------------
 *                | Broadcast channel |
 *                ---------------------
 *
 * The application interface (upper) interface has two functions:
 *    marshal: format a value to be sent on the channel.
 *       The value to broadcast and a copy is saved at every
 *       marshaler.
 *    unmarshal: handle a value sent on the communication
 *       channel.  The value is returned as the result of
 *       this function.
 *
 * The membership to the communication channel is allowed to change.
 * We require these invariants:
 *    1. All broadcast messages are received in the same order
 *    2. The channel membership is known to all marshalers
 *
 * The view management is handled in these phases:
 *    1. Initially, a marshaler is the only member of its communication channel
 *    2. Channels may split and merge.
 *       These are the phases of the membership change:
 *
 *       a. When the membership is going to change, all marshalers that
 *          are involved are notified with the "block" function.  If the
 *          client is threaded, all "marshal" calls are blocked.
 *       b. When the membership is determined, all marshalers are notified with
 *          the number of channel members.  Each one broadcasts a view_membership
 *          message of all marshalers it has known in the past.
 *       c. When a marshaler receives all the view_membership messages, it
 *          determines which values it is going to act as "owner" for,
 *          and it broadcasts a view_summary of all the values.
 *       d. When a marshaler receives a view_summary, it broadcasts
 *          a view_additional message with all the values the originator
 *          did not know about.
 *       d. When all marshalers have received all summaries and additional
 *          values, the global state is consistent, and the view change
 *          is complete.
 * The view change is not required to complete: another view change may start
 * before the current change is complete.  the new view change cancels the old
 * change.  In general, marshaling is blocked wheil the view change is coccurring.
 *
 * There is also distributed garbage collection.  A client can initiate
 * garbage collection at any time except during a view change.  Here are the
 * phases:
 *       a. The "gc" function runs the collector locally,
 *          and broadcasts a gc_notify message that summarizes
 *          the values that have been collected.
 *       b. When a remote marshaler receives the notification,
 *          it sends a gc_response with the objects that it does
 *          not want to be collected.
 *       c. When the originating marshaler gets responses from
 *          all remote marshalers, it cancels collection of values
 *          in the responses, and it broadcasts a gc_update with
 *          a list of values that _really_ should be collected.
 * The garbage collector may be interrupted at any time by a view change.
 * Also, marshaling is not halted by the garbage collector, except during
 * the function calls themselves.
 *)
module type MarshalSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Marshal objects.  The argument type is the type of objects
    * being marshaled.
    *)
   type 'a t

   (*
    * Values are marshaled to/from buffers.
    *)
   type buf
   type wbuf
   type rbuf

   (************************************************************************
    * MANAGEMENT                                                           *
    ************************************************************************)

   (*
    * Create a new instance of the marshaler.
    *)
   val create : unit -> 'a t

   (************************************************************************
    * MARSHALING                                                           *
    ************************************************************************)

   type marshal_msg

   (*
    * Share the value, marshaling a message to the buffer.
    *)
   val marshal : 'a t -> wbuf -> 'a -> marshal_msg

   (*
    * Handle a marshal message.
    *)
   val unmarshal : 'a t -> rbuf -> marshal_msg -> 'a

   (*
    * Publish a value at a manifest name.
    * This is meant to be used for values that are already
    * know about before the marshaler was created.  These
    * are "manifest" values: every instance of the marshaler
    * should publish the same values.
    *
    * Should this be a global function?
    *)
   val register : 'a t -> string -> 'a -> unit

   (************************************************************************
    * GARBAGE COLLECTION                                                   *
    ************************************************************************)

   type gc_notice
   type gc_response
   type gc_update

   (*
    * Invoke the garbage collector.
   val gc : 'a t -> gc_notice
    *)

   (*
    * Handle a collection message.
    * The response should be passed back to the sender of the message,
    * not broadcast.
    *)
   val handle_gc_notice : 'a t -> gc_notice -> gc_response

   (*
    * Handle a gc response.
    * When all responses have been received, the marshaler
    * generates a summary of the items that are _really_
    * removed.
    *)
   val handle_gc_response : 'a t -> gc_response -> gc_update option

   (*
    * Handle the update.
    *)
   val handle_gc_update : 'a t -> gc_update -> unit

   (************************************************************************
    * GROUP MEMBERSHIP                                                     *
    ************************************************************************)

   type view_membership
   type view_summary
   type view_additional_summary
   type view_additional

   (*
    * A view change is starting.
    * This call is optional.
    *)
   val block : 'a t -> unit

   (*
    * Initiate a new view.
    * This generates a list of all the marshalers this marshaler
    * has ever known about.
    *)
   val start_view : 'a t -> view_membership

   (*
    * Handle a notification of the view membership from one of the
    * marshalers.  When all remote marshalers have responded, the marshaler
    * generates a summary of the values it is responsible for.
    *)
   val handle_view_membership : 'a t -> view_membership -> view_summary option

   (*
    * Handle a view summary.  If the summary needs to be augmented, this
    * marshaler generates a summary of additional values it would like
    * to provide.  This should be broadcast.
    *)
   val handle_view_summary : 'a t -> view_summary -> view_additional_summary

   (*
    * Once all these additional values are recieved, the marshaler
    * broadcasts the additional values.
    *)
   val handle_view_additional_summary : 'a t -> view_additional_summary -> view_additional option

   (*
    * Once all the additional values have been received, the marshaler is
    * free to continue.
    *)
   val handle_view_additional : 'a t -> view_additional -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
