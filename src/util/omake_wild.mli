(*
 * Utilities, mostly on filenames.
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
open Lm_printf

(*
 * Wildcard string.
 *)
val wild_string : string

(*
 * Wildcard matching. "Incoming" patterns must have exactly one instance
 * of the pattern symbol %. "Outgoing" patterns may have any number.
 *)
type wild_in_patt
type wild_out_patt
type wild_subst
type wild_value

(*
 * Printing.
 *)
val pp_print_wild_in : formatter -> wild_in_patt -> unit
val pp_print_wild_out : formatter -> wild_out_patt -> unit

(*
 * Check if a string is a wild pattern.
 *)
val is_wild : string -> bool

(*
 * Compile a pattern.
 *)
val wild_compile_in : string -> wild_in_patt
val wild_compile_out : string -> wild_out_patt

(*
 * Perform a match.  Returns None if there
 * was no match.
 *)
val wild_matches : wild_in_patt -> string -> bool
val wild_match : wild_in_patt -> string -> wild_subst option
val wild_core : wild_subst -> string
val wild_of_core : string -> wild_subst

(*
 * Perform a substitution.
 *)

val wild_subst_in : wild_subst -> wild_in_patt -> string
val wild_subst : wild_subst -> wild_out_patt -> string
(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
