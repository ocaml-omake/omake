(*
 * Doubly-linked lists.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001-2005 Mojave Group, Caltech
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

(*
 * Lists contain handles.
 *)
type 'a t
type 'a handle

(*
 * Creation.
 *)
val create : unit -> 'a t
val create_handle : 'a -> 'a handle
val data : 'a handle -> 'a

(*
 * List operations.
 *)
val is_empty : 'a t -> bool
val hd : 'a t -> 'a handle
val tl : 'a handle -> 'a handle
val no_tl : 'a handle -> bool
val to_list : 'a t -> 'a handle list

(*
 * Standard ops.
 *)
val length : 'a t -> int

(*
 * Iteration.
 *)
val iter : ('a handle -> unit) -> 'a t -> unit
val fold : ('a -> 'b handle -> 'a) -> 'a -> 'b t -> 'a

(*
 * Insertion/deletion.
 *)
val insert : 'a handle -> 'a t -> unit
val delete : 'a handle -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
