(*
 * Types used to represent commands and the cache.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_node
open Omake_value_type

(* %%MAGICBEGIN%% *)
(*
 * File digest is an option, in case the file does not exist.
 *)
type digest = Digest.t option

(*
 * The memo result is used only for the scanner,
 * whice produces a table of dependencies.
 *)
type 'a memo_result =
   MemoFailure of int
 | MemoSuccess of 'a

type memo_deps = NodeSet.t NodeTable.t

type memo_deps_result = memo_deps memo_result

type memo_obj_result = obj memo_result

(*
 * Status query.
 *)
type memo_status =
   StatusSuccess
 | StatusFailure of int
 | StatusUnknown

(*
 * A directory entry is a node or directory.
 *)
type dir_entry =
   NodeEntry of Node.t
 | DirEntry of Dir.t
(* %%MAGICEND%% *)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
