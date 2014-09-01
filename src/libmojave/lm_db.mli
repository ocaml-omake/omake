(*
 * A simple database.  This is a low-level ionterface.
 * See, for example, omake_db.ml to see a higher-level
 * interface.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
val debug_db : bool ref

type t = Unix.file_descr

type tag = int
type magic = string
type digest = string
type hostname = string
type named_value = string * string

type entry_pred = tag -> named_value list -> hostname -> digest -> bool

(*
 * Some kinds of entries are host-independent.
 *)
type host =
   HostIndependent
 | HostDependent

(*
 * These functions assume that the file is locked.
 *    tag: the kind of entry
 *    magic: the magic number for this version
 *    digest: the source file digest (or use the empty string)
 *
 * These functions operate by side-effect, modifying the file.
 *    add: remove the old entry and add a new one
 *    find: find an existing entry, or raise Not_found if it doesn't exist
 *    remove: remove an old entry, does not fail.
 *)
val add    : t -> string -> tag * host -> magic -> digest -> 'a -> unit
val find   : t -> string -> tag * host -> magic -> digest -> 'a
val remove : t -> string -> tag * host -> magic -> unit

(*
 * Somewhat more general interface.
 *)
val first_entry_tag : tag

val append_entry : t -> string -> tag -> named_value list -> digest -> 'a -> unit
val find_entry   : t -> string -> entry_pred -> 'a
val remove_entry : t -> string -> entry_pred -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
