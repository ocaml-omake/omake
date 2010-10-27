(*
 * Cache information about past runs.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech and HRL Laboratories, LLC
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
open Lm_printf
open Lm_string_set

open Omake_options
open Omake_node
open Omake_cache_type
open Omake_value_type
open Omake_command_type

(* Debugging *)
val debug_cache : bool ref

(* The abstract type for the cache database *)
type t

(* Keys for various functions *)
type key

(* Directory listing *)
type dir_listing
type exe_listing

(* Manifest keys *)
val scanner_fun         : key
val rule_fun            : key
val env_fun             : key
val include_fun         : key

val env_target  : Node.t

(* Fetch the cache *)
val create       : unit -> t
val from_channel : omake_options -> in_channel -> t
val to_channel   : Pervasives.out_channel -> t -> unit

(*
 * Stats.
 *)
val stats : t -> int * int

(*
 * File digest information.
 * The reset functions indicate that stat info may need to be recomputed.
 * The stat functions get the digest.
 * The ?force flag, if set, forces the restat in case we cached a _negative_
 * information on the file (file did not exist before).
 *)
val stat             : t -> Node.t -> digest
val stat_set         : t -> NodeSet.t -> digest NodeTable.t
val stat_table       : t -> 'b NodeTable.t -> digest NodeTable.t
val stat_unix        : t -> ?force:bool -> ?follow_symlinks:bool -> Node.t -> Unix.LargeFile.stats
val is_dir           : t -> ?force:bool -> ?follow_symlinks:bool -> Node.t -> bool

val reset            : t -> Node.t -> unit
val reset_set        : t -> NodeSet.t -> unit
val reset_table      : t -> 'b NodeTable.t -> unit

val force_stat       : t -> Node.t -> digest
val force_stat_set   : t -> NodeSet.t -> digest NodeTable.t
val force_stat_table : t -> 'b NodeTable.t -> digest NodeTable.t

val stat_changed     : t -> Node.t -> bool

(*
 * Check if a file exists.
 *)
val exists           : t -> ?force:bool -> Node.t -> bool
val exists_dir       : t -> ?force:bool -> Dir.t -> bool

(*
 * Directory listings.  The Boolean in ls_path and ls_exe_path should
 * be true for those directory collections where auto-rehashing is to
 * be performed.
 *)
val rehash           : t -> unit

val ls_dir           : t -> bool -> Dir.t -> dir_listing
val ls_path          : t -> (bool * Dir.t list) list -> dir_listing
val listing_find     : t -> dir_listing -> string -> dir_entry

val ls_exe_path      : t -> (bool * Dir.t list) list -> exe_listing
val exe_find         : t -> exe_listing -> string -> Node.t
val exe_find_all     : t -> exe_listing -> string -> Node.t list
val exe_complete     : t -> exe_listing -> string -> StringSet.t

val exe_suffixes     : string list

(*
 * Memoizing commands.
 *
 * add                : key targets sources commands result
 * up_to_date         : key sources commands
 * find_result        : key sources commands
 * find_result_sloppy : key target
 *
 * The sloppy function just returns the results for the target
 * without even considering the command and dependencies.
 *)
val add                : t -> key -> Node.t -> NodeSet.t -> NodeSet.t -> command_digest -> memo_deps_result -> unit
val up_to_date         : t -> key -> NodeSet.t -> command_digest -> bool
val up_to_date_status  : t -> key -> NodeSet.t -> command_digest -> memo_status
val find_result        : t -> key -> NodeSet.t -> command_digest -> memo_deps
val find_result_sloppy : t -> key -> Node.t -> memo_deps

(*
 * Similar functions for values. The bool flag indicates whether we want a static value.
 *)
val find_value         : t -> value -> bool -> NodeSet.t -> command_digest -> obj
val add_value          : t -> value -> bool -> NodeSet.t -> command_digest -> memo_obj_result -> unit

(*
 * Printing.
 *)
val pp_print_digest            : formatter -> digest -> unit
val pp_print_node_digest_table : formatter -> digest NodeTable.t -> unit
val pp_print_memo_result       : (formatter -> NodeSet.t NodeTable.t -> unit) -> formatter -> memo_deps_result -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
