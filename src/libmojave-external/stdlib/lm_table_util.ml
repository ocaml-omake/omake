(*
 * Utilities on tables.
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
open Lm_map_sig

module MakeTable (Create : TableCreateSig) (Base : TableBaseSig) =
struct
   type elt = Base.elt
   type data = Base.data
   type t = (elt, data) Create.t

   (*
    * Get the methods.
    *)
   let methods = Create.create Base.print Base.compare Base.append

   (*
    * Now project them.
    *)
   let empty = methods.empty
   let is_empty = methods.is_empty
	let length = methods.cardinal
   let add = methods.add
   let replace = methods.replace
   let union = methods.union
   let mem = methods.mem
   let find = methods.find
   let find_all = methods.find_all
   let remove = methods.remove
   let iter = methods.iter
   let fold_map = methods.fold_map
   let map = methods.map
   let deletemax = methods.deletemax
   let list_of = methods.list_of
   let print = methods.print
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
