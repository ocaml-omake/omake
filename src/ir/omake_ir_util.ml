(*
 * Utilities for IR expressions.
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
open Omake_ir

let loc_of_exp e =
   match e with
      LetVarExp (loc, _, _, _, _)
    | KeyExp (loc, _)
    | LetKeyExp (loc, _, _, _)
    | LetFunExp (loc, _, _, _, _, _, _, _)
    | LetObjectExp (loc, _, _, _, _, _)
    | LetThisExp (loc, _)
    | ShellExp (loc, _)
    | IfExp (loc, _)
    | SequenceExp (loc, _)
    | SectionExp (loc, _, _, _)
    | OpenExp (loc, _)
    | IncludeExp (loc, _, _)
    | ApplyExp (loc, _, _, _)
    | SuperApplyExp (loc, _, _, _, _)
    | MethodApplyExp (loc, _, _, _, _)
    | StaticExp (loc, _, _, _)
    | ReturnBodyExp (loc, _, _)
    | StringExp (loc, _)
    | ReturnExp (loc, _, _)
    | ReturnObjectExp (loc, _)
    | ReturnSaveExp loc ->
         loc

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
