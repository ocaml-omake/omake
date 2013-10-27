(*
 * Simple readline interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
 * Copyright (C) 2000-2005 Mojave Group, Alexey Nogin, Cornell University
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *)


(* C readline function returns None at EOF *)
external caml_initialize_readline : unit -> unit = "caml_initialize_readline"
external caml_register_commands : string list -> unit = "caml_register_commands"
external caml_readline : string -> string option = "caml_readline"
external caml_read_history : string -> unit = "caml_read_history"
external caml_write_history : string -> unit = "caml_write_history"
external caml_history_truncate_file : string -> int -> unit = "caml_history_truncate_file"


(* initialize_readline ()
   Initialise the readline library.  *)
let initialize_readline () =
   caml_initialize_readline ()


(* register_commands commands
   Register a list of commands for tab completion.  This will clear
   the previous command-list; only one command-list may be installed
   at a time.  Command completion only applies to the first word on
   the command-line.  *)
let register_commands commands =
   caml_register_commands commands

let read_history filename =
   caml_read_history filename

let write_history filename =
   caml_write_history filename

let history_truncate_file filename nlines =
   caml_history_truncate_file filename nlines

(* readline prompt
   Displays a readline prompt, and accepts a line of input from the user.
   Tab completion will be enabled as approprate.  Be sure to call the
   initialize_readline () function before calling this function.  This
   will raise End_of_file if the user strikes ^D.  *)
let readline prompt =
   match caml_readline prompt with
      Some s ->
         s
    | None ->
         raise End_of_file
