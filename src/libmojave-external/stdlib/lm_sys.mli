(*
 * System interface.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Mojave Group, Caltech
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
 *
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *)

external argv              : string array = "%argv"
external file_exists       : string -> bool = "%file_exists"
external remove            : string -> unit = "%file_remove"
external rename            : string -> string -> unit = "%file_rename"
external getenv            : string -> string = "%getenv"
external time              : unit -> float = "%time"
external chdir             : string -> unit = "%chdir"
external getcwd            : unit -> string = "%getcwd"
external interactive       : bool ref = "%interactive"
external os_type           : string = "%os_type"
external word_size         : int = "%word_size"
external max_string_length : int = "%max_string_length"
external max_array_length  :  int = "%max_array_length"

external sigabrt   : int = "%sigabrt"
external sigalrm   : int = "%sigalrm"
external sigfpe    : int = "%sigfpe"
external sighup    : int = "%sighup"
external sigill    : int = "%sigill"
external sigint    : int = "%sigint"
external sigkill   : int = "%sigkill"
external sigpipe   : int = "%sigpipe"
external sigquit   : int = "%sigquit"
external sigsegv   : int = "%sigsegv"
external sigterm   : int = "%sigterm"
external sigusr1   : int = "%sigusr1"
external sigusr2   : int = "%sigusr2"
external sigchld   : int = "%sigchld"
external sigcont   : int = "%sigcont"
external sigstop   : int = "%sigstop"
external sigtstp   : int = "%sigtstp"
external sigttin   : int = "%sigttin"
external sigttou   : int = "%sigttou"
external sigvtalrm : int = "%sigvtalrm"
external sigprof   : int = "%sigprof"

type signal_behavior =
   Signal_default
 | Signal_ignore
 | Signal_handle of (int -> unit)

val signal : int -> signal_behavior -> signal_behavior
val set_signal : int -> signal_behavior -> unit

exception Break

val catch_break : bool -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
