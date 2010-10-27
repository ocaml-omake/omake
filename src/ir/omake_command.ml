(*
 * Command utilities.
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
open Lm_printf

open Fmarshal

open Omake_node
open Omake_marshal
open Omake_ir_print
open Omake_shell_type
open Omake_command_type

(************************************************************************
 * Argument collapsing.
 *)
type arg_buffer = arg_string list

let arg_buffer_empty =
   []

let arg_buffer_add_string buf s =
   ArgString s :: buf

let arg_buffer_add_data buf s =
   ArgData s :: buf

let rec collect_string buf args =
   match args with
      ArgString s :: args ->
         Buffer.add_string buf s;
         collect_string buf args
    | _ ->
         args

let rec collect_data buf args =
   match args with
      ArgData s :: args ->
         Buffer.add_string buf s;
         collect_data buf args
    | _ ->
         args

let arg_buffer_contents args =
   let buf = Buffer.create 32 in
   let rec collect args' args =
      match args with
         ArgString s :: ((ArgString _ :: _) as tl) ->
            Buffer.add_string buf s;
            let args = collect_string buf tl in
            let s = Buffer.contents buf in
               Buffer.clear buf;
               collect (ArgString s :: args') args
       | ArgData s :: ((ArgData _ :: _) as tl) ->
            Buffer.add_string buf s;
            let args = collect_data buf tl in
            let s = Buffer.contents buf in
               Buffer.clear buf;
               collect (ArgData s :: args') args
       | h :: args ->
            collect (h :: args') args
       | [] ->
            List.rev args'
   in
      collect [] (List.rev args)

(************************************************************************
 * Command utilities
 *)

(*
 * Parse the command lines from the strings.
 *)
let parse_command venv dir target loc flags line =
   { command_loc    = loc;
     command_dir    = dir;
     command_target = target;
     command_flags  = flags;
     command_venv   = venv;
     command_inst   = line
   }

let parse_commands venv dir target loc lines =
   List.map (fun (flags, line) -> parse_command venv dir target loc flags line) lines

(*
 * Allow output in the command.
 *)
let command_allow_output command =
   { command with command_flags = AllowOutputFlag :: command.command_flags }

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
