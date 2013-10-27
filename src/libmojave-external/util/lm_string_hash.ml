(*
 * A path is like a Node, but it represents a name within
 * a search path.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2007 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_hash
open Lm_printf
open Lm_location
open Lm_hash_sig

(*
 * Compare two paths.
 *)
module StringCompare =
struct
   type t = string

   let debug = "StringHash"

   let hash = Hashtbl.hash

   let compare = String.compare

   let reintern s = s
end

module StringHash : HashMarshalSig with type elt = string =
   MakeHashMarshal (StringCompare);;

module StringHashSet = Lm_set.LmMake (StringHash);;
module StringHashTable = Lm_map.LmMake (StringHash);;

let pp_print_string_hash buf s =
   pp_print_string buf (StringHash.get s)

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
