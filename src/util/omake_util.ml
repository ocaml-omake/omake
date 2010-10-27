(*
 * Basic utilities.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2007 Mojave Group, California Institute of Technology and
 * HRL Laboratories, LLC
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

(*
 * Test a string for being false.
 *)
let bool_of_string s =
   match String.lowercase s with
      ""
    | "0"
    | "no"
    | "nil"
    | "false"
    | "undefined" ->
         false
    | _ ->
         true

(*
 * Path separator.
 *)
let pathsep =
   if Sys.os_type = "Win32" then
      ";"
   else
      ":"

let pp_time buf secs =
   if secs < 60. then
      fprintf buf "%0.2f sec" secs
   else
      let subsec, sec = modf secs in
      let sec = int_of_float sec in
      let h = sec / 3600 in
      let m = (sec / 60) mod 60 in
      let s = sec mod 60 in
         if h > 0 then
            fprintf buf "%d hrs %02d min %05.2f sec" h m (float s +. subsec)
         else
            fprintf buf "%d min %05.2f sec" m (float s +. subsec)

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
