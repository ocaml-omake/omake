(*
 * Extra functions on hashtables.
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

(*
 * Convert to an association list.
 * Order is random, but the is only one entry
 * for each key.
 *)
val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

(*
 * Add a list to a hashtbl.
 *)
val add_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list -> unit

(*
 * Add all the entries from the second table.
 * The original entries are removed.
 *)
val add_hashtbl : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
