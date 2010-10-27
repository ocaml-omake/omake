(*
 * Basic utilities for the builtin functions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol
open Lm_location

open Omake_env
open Omake_pos
open Omake_node
open Omake_value_type
open Omake_build_type

(*
 * Run-time symbols.
 *)
val defined_sym : venv -> pos -> loc -> string -> bool
val get_sym : venv -> pos -> loc -> string -> value
val add_sym : venv -> pos -> loc -> string -> value -> venv

(*
 * Add a command line variable definition.
 *)
val add_command_def : string -> string -> unit

(*
 * Check if there are command defs.
 *)
val command_defs_are_nonempty : unit -> bool

(*
 * Add all the command-line defs to the encironment.
 *)
val venv_add_command_defs : venv -> venv

(*
 * Map over a sequence and add separators.
 *)
val sequence_map : ('a -> value) -> 'a list -> value list
val sequence_list : value list -> value list

(*
 * Boolean values.
 *)
val val_true : value
val val_false : value
val val_of_bool : bool -> value

(*
 * Unfortunately, we have to specify the environment imperatively.
 *)
val set_env : env -> unit
val get_env : pos -> loc -> env
val is_build_phase : unit -> bool

(*
 * A node is a leaf if it has no dependencies and no commands.
 *)
val is_leaf_command : command -> bool
val is_leaf_node    : env -> Node.t -> bool

(*
 * Get the object from a file.
 *)
val object_of_file : venv -> pos -> loc -> string -> obj

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
