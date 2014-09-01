(*
 * File-change notification services.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, Caltech
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
 * Event manager.
 *)
type t

(*
 * Events.
 *)
type code =
   Changed
 | Deleted
 | StartExecuting
 | StopExecuting
 | Created
 | Moved
 | Acknowledge
 | Exists
 | EndExist
 | DirectoryChanged

type event =
   { notify_code : code;
     notify_name : string
   }

(*
 * Debugging.
 *)
val debug_notify      : bool ref
val string_of_code    : code -> string

(*
 * Methods.
 *)
val enabled           : bool
val create            : unit -> t
val close             : t -> unit
val file_descr        : t -> Unix.file_descr option
val monitor           : t -> string -> bool -> unit
val pending           : t -> bool
val next_event        : t -> event

val suspend           : t -> string -> unit
val resume            : t -> string -> unit
val cancel            : t -> string -> unit

val suspend_all       : t -> unit
val resume_all        : t -> unit
val cancel_all        : t -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
