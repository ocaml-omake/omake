(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
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
 * Author: Alexey Nogin, Jason Hickey
 * nogin@cs.cornell.edu
 * jyh@cs.cornell.edu
 *)
open Lm_map_sig

(*
 * Create a table.
 *)
type ('elt, 'data) table

val create : ('elt, 'data, ('elt, 'data) table) table_create_type

(*
 * Build the table over an ordered type.
 *)
module MakeTable (Base : TableBaseSig)
: TableSig
  with type t = (Base.elt, Base.data) table
  with type elt = Base.elt
  with type data = Base.data

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
