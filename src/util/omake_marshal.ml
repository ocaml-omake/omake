(*
 * Marshaling of messages.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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
open Fmarshal

let version_number = Hashtbl.hash "$Id$"

type magic =
   LocationMagic
 | IdMagic
 | NullRootMagic
 | DriveRootMagic
 | DirRootMagic
 | DirSubMagic
 | NodeFileMagic
 | NodePhonyGlobalMagic
 | NodePhonyDirMagic
 | NodePhonyFileMagic
 | NodeFlaggedMagic
 | NodeIsOptionalMagic
 | NodeIsExistingMagic
 | NodeIsSquashedMagic
 | NodeIsScannerMagic
 | QuietFlagMagic
 | AllowFailureFlagMagic
 | AllowOutputFlagMagic
 | CommandLineMagic
 | PrintEagerMagic
 | PrintLazyMagic
 | PrintExitMagic
 | RequestSpawnMagic
 | ResponseCreateMagic
 | ResponseExitedMagic
 | ResponseStdoutMagic
 | ResponseStderrMagic
 | ResponseStatusMagic
 | MaxMagic

type msg = magic item

exception MarshalError

(*
 * Magic numbers.
 * We cheat a little here.
 *)
let int_of_magic magic =
   (Obj.magic magic : int)

let max_magic = int_of_magic MaxMagic

let magic_of_int i =
   if i < 0 || i >= max_magic then
      raise (Failure "magic_of_int");
   (Obj.magic i : magic)

(*
 * Some common marshalers.
 *)
let marshal_string_list l =
   List (List.map (fun s -> String s) l)

let unmarshal_string_list l =
   match l with
      List l ->
         List.map (function
            String s -> s
          | _ -> raise MarshalError) l
    | _ ->
         raise MarshalError

(*
 * Locations.
 *)
let marshal_loc loc =
   let file, sline, schar, eline, echar = Lm_location.dest_loc loc in
   let file = Lm_symbol.to_string file in
      List [Magic LocationMagic; String file; Int sline; Int schar; Int eline; Int echar]

let unmarshal_loc l =
   match l with
      List [Magic LocationMagic; String file; Int sline; Int schar; Int eline; Int echar] ->
         Lm_location.create_loc (Lm_symbol.add file) sline schar eline echar
    | _ ->
         raise MarshalError

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
