(*
 * Map module based on red-black trees
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999-2005 Mojave Group, Caltech
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
 * jyh@cs.caltech.edu
 *)
open Lm_map_sig

module Make       (Ord : OrderedType) : (S         with type key = Ord.t)
module LmMake     (Ord : OrderedType) : (LmMap     with type key = Ord.t)
module LmMakeList (Ord : OrderedType) : (LmMapList with type key = Ord.t)

(*
 * This version includes a sharing constraint so that maps can
 * be used in recursive definitions.  This exposes the internal
 * representation, should you should avoid using it unless
 * absolutely necessary (like in a recursive type definition).
 *)
type ('key, 'value) tree

module LmMakeRec (Ord : OrderedType) : (LmMap     with type key = Ord.t with type 'a t = (Ord.t, 'a) tree)
