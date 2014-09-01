(*
 * Our personal implementation of threads.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
module type MutexSig =
sig
   type t

   val create      : string -> t
   val lock        : t -> unit
   val try_lock    : t -> bool
   val unlock      : t -> unit
end

module type ConditionSig =
sig
   type t
   type mutex

   val create    : unit -> t
   val wait      : t -> mutex -> unit
   val signal    : t -> unit
   val broadcast : t -> unit
end

(*
 * The "state" provides thread-local storage with
 * read and write locks.  By default, all threads
 * share the same state.  All elements that are
 * added are available in all threads, but
 * each thread may have a different value for
 * the element.
 *
 * The read and write functions are locked
 * using a fair readers-writers protocol.
 *)
module type StateSig =
sig
   type t
   type 'a entry

   (* State operations *)
   val create     : unit -> t
   val current    : unit -> t
   val set        : t -> unit
   val with_state : t -> ('a -> 'b) -> 'a -> 'b

   (* Global variables *)
   val shared_val  : string -> 'a -> 'a entry
   val private_val : string -> 'a -> ('a -> 'a) -> 'a entry
   val read        : 'a entry -> ('a -> 'b) -> 'b
   val write       : 'a entry -> ('a -> 'b) -> 'b
   val unlock      : 'a entry -> (unit -> 'b) -> 'b

   (* This function is only valid within a lock *)
   val get         : 'a entry -> 'a
end

module type ThreadSig =
sig
   type t
   type id

   val enabled : bool
   val create : ('a -> 'b) -> 'a -> t
   val self : unit -> t
   val join : t -> unit
   val id : t -> int
   val sigmask : Unix.sigprocmask_command -> int list -> int list

   (*
    * XXX: This is a hack to address the "Sys.Break does not get raised inside
    * C blocking sections" problem.
    * See http://caml.inria.fr/pub/ml-archives/caml-list/2007/07/3662ad69f77253674f580b174c85dfbb.en.html for detail.
    * The (raise_ctrl_c_wrapper f x) behaves like (f x), except that it makes
    * sure that any Ctrl-C event quickly results in a Sys.Break exception, even
    * if f spends "forever" inside a C blocking section.
    *)
   val raise_ctrl_c_wrapper: ('a -> 'b) -> 'a -> 'b
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
