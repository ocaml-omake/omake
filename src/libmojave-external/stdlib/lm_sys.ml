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

type signal = Pervasives_boot.signal
type sig_handle = Pervasives_boot.sig_handle

external set_signal : int -> signal -> signal = "%set_signal"
external handle_of_sig_handle : sig_handle -> (int -> unit) = "%sig_dest_handle"
external sig_handle_of_handle : (int -> unit) -> sig_handle = "%sig_make_handle"

let signal signo behavior =
   let signalb =
      match behavior with
         Signal_default ->
            Pervasives_boot.Signal_default
       | Signal_ignore ->
            Pervasives_boot.Signal_ignore
       | Signal_handle f ->
            Pervasives_boot.Signal_handle (sig_handle_of_handle f)
   in
      match set_signal signo signalb with
         Pervasives_boot.Signal_default ->
            Signal_default
       | Pervasives_boot.Signal_ignore ->
            Signal_ignore
       | Pervasives_boot.Signal_handle f ->
            Signal_handle (handle_of_sig_handle f)

let set_signal signo behavior =
   let signalb =
      match behavior with
         Signal_default ->
            Pervasives_boot.Signal_default
       | Signal_ignore ->
            Pervasives_boot.Signal_ignore
       | Signal_handle f ->
            Pervasives_boot.Signal_handle (sig_handle_of_handle f)
   in
      ignore (set_signal signo signalb)

exception Break

let catch_break flag =
   let signalb =
      if flag then
         Signal_handle (fun _ -> raise Break)
      else
         Signal_default
   in
      set_signal sigint signalb

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
