(*
 * This is just abstract interface to an array.
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
 * Normal arrays.
 *)
module type ArraySig =
sig
   type 'a t

   val create : 'a -> 'a t
   val set : 'a t -> int -> 'a -> unit
   val get : 'a t -> int -> 'a
   val length : 'a t -> int
end

(*
 * Weak arrays return an option.
 *)
module type WeakArraySig =
sig
   type 'a t

   val create : unit -> 'a t
   val set : 'a t -> int -> 'a -> unit
   val get : 'a t -> int -> 'a option
   val length : 'a t -> int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
