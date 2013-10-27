(*
 * This module provides a signature for a linearly ordered numbered set
 * with lazy application. Such sets are used, for example, to keep
 * hypothesis in a sequent
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
 * Author: Alexey Nogin
 *)

module type LinearSetSig =
sig
   type elt
   type t
   type index = int

   val empty : t
   val singleton : elt -> t
   val length : t -> int
   val get : t -> index -> elt
   val make : int -> elt -> t
   val create : int -> elt -> t
   val to_list : t -> elt list
   val of_list : elt list -> t
   val iter : (elt -> unit) -> t -> unit
   val map : (elt -> elt) -> t -> t
   val fold : ('a -> index -> elt -> 'a) -> 'a -> t -> 'a
   val split : t -> index -> t * elt * t
   val append : t -> elt -> t -> t
   val append_list : t -> elt list -> t -> t
   val concat : t -> t -> t
   val lazy_apply : (elt -> elt) -> t -> t
   val lazy_sub_map : (elt -> elt) -> t -> index -> index -> t

   val mapi : (index -> elt -> elt) -> t -> t
   val init : int -> (index -> elt) -> t
   val collect : (elt, t) Lm_array_util.array_part list -> t

   val for_all : (elt -> bool) -> t -> bool
   val exists : (elt -> bool) -> t -> bool
end

module type TypeSig =
sig
   type t
end
