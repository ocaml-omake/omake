(*
 * A simple process-id interface to threads.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
type pid

type job_type =
   HiddenJob
 | VisibleJob

val create       : string -> job_type -> pid
val set_pid      : pid -> unit
val get_pid      : unit -> pid
val get_pids     : unit -> pid list
val with_pid     : pid -> ('a -> 'b) -> 'a -> 'b
val with_current : ('a -> 'b) -> 'a -> 'b

(*
 * Raises Failure if the string is not well-formed.
 * Raises Not_found if the string is not a current pid.
 *)
val make_pid : string -> int -> pid
val dest_pid : pid -> string * int

(*
 * Create, or find a previous job with this name.
 *)
val create_or_find : string -> int -> job_type -> pid

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
