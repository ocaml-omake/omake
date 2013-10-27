(*
 * Simple terminfo interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
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


(* The C function takes a string ID, and returns the escape sequence
   (or an empty string if the ID is not defined for this terminal).  *)
external caml_tgetstr_enabled : unit -> bool = "caml_tgetstr_enabled"
external caml_tgetstr : string -> string = "caml_tgetstr"


(* Tgetstr is enabled only if the terminal is defined *)
let tgetstr_enabled = caml_tgetstr_enabled ()

(* tgetstr id
   Lookup the terminal capability with indicated id.  This assumes the
   terminfo to lookup is given in the TERM environment variable.  This
   function returns None if the terminal capability is not defined.  *)
let tgetstr id =
   if tgetstr_enabled then
      let result = caml_tgetstr id in
         if result = "" then
            None
         else
            Some result
   else
      None


(* Various terminfo identifier names for use with tgetstr *)
let enter_bold_mode = "bold"
let exit_attribute_mode = "sgr0"


(* xterm_ok ()
   Check for an XTerm-compatible terminal, for the XTerm escapes.  *)

(* XXX: strictly speaking, we should be using the "tsl"/"fsl" capabilities here, but those are often missing *)
let xterm_ok () =
   try
      match Sys.getenv "TERM" with
         "xterm" | "color_xterm" | "xterm-color" | "konsole" | "rxvt" ->
            true
       | _ ->
            false
   with
      Not_found ->
         false


(* xterm_escape_begin ()
   Display XTerm title begin escape, if available.  *)
let xterm_escape_begin () =
   if xterm_ok () then
      Some "\027]0;"
   else
      None


(* xterm_escape_begin ()
   Display XTerm title end escape, if available.  *)
let xterm_escape_end () =
   if xterm_ok () then
      Some "\007"
   else
      None
