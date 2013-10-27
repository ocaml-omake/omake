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


(* tgetstr id
   Lookup the terminal capability with indicated id.  This assumes the
   terminfo to lookup is given in the TERM environment variable.  This
   function returns None if the terminal capability is not defined.  *)
val tgetstr : string -> string option


(* Various terminfo identifier names for use with tgetstr *)
val enter_bold_mode : string
val exit_attribute_mode : string


(* xterm_escape_begin ()
   Display XTerm title begin escape, if available.  *)
val xterm_escape_begin : unit -> string option


(* xterm_escape_begin ()
   Display XTerm title end escape, if available.  *)
val xterm_escape_end : unit -> string option
