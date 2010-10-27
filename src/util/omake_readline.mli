(*
 * Simple readline implementation.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, Caltech and HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * 
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)

val flush           : unit -> unit
val isatty          : unit -> bool
val readline        : string -> string
val readstring      : string -> string -> int -> int -> int
val set_interactive : bool -> unit
val is_interactive  : unit -> bool
val where           : unit -> int
val history         : unit -> string array
val load            : string -> unit
val save            : unit -> unit
val set_length      : int -> unit
val set_directory   : string -> unit
val prompt_invisible: (string * string) option

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
