(*
 * This is the main build loop.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech
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
 * Modified by: Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 *)
open Omake_env
open Omake_options
open Omake_build_type

(*
 * Debugging flags.
 *)
val debug_rule     : bool ref
val debug_build    : bool ref
val debug_deps     : bool ref

(*
 * .omakedb save interval (0 - disable)
 *)
val save_interval  : float ref

(*
 * Examining the state.
 * Note that in a non-standard build phase (such as .DUILD_SUCCESS),
 * this function will process _both_ the phase-specific worklist and the main worklist.
 *)
val command_fold   : env -> command_tag -> ('a -> command -> 'a) -> 'a -> 'a

(*
 * Build the system.
 *)
val build : omake_options -> string -> string list -> unit
val build_fun : venv -> string list -> bool

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
