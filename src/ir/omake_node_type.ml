(*
 * Types of internal representations of nodes.
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
open Lm_filename_util

(*
 * Internally, we represent pathnames as absolute paths.
 * We keep a hashed integer for quick equality testing.
 *    dir_hash : the quick hash
 *    dir_root : the root of this name
 *    dir_key  : the path in canonical form (lowercase on Windows)
 *    dir_name : the actual path will full capitalization
 *)
type dir =
   DirRoot of int * root
 | DirSub of int * string * string * dir

(*
 * Possible node flags.
 *)
type node_flag =
   NodeIsOptional
 | NodeIsExisting
 | NodeIsSquashed
 | NodeIsScanner

(*
 * A node is a phony, or it is a filename.
 *)
type node =
   NodeFile        of int * dir * string * string
 | NodePhonyGlobal of int * string
 | NodePhonyDir    of int * dir * string * string
 | NodePhonyFile   of int * dir * string * string * string
 | NodeFlagged     of node_flag * node

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
