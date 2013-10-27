(*
 * Unique identifiers.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1999-2005 PRL Group, Cornell University and Caltech
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
 * jyh@cs.cornell.edu
 *)

(*
 * Use the null implementation of threads.
 * Don't kill this off.
 *)
open Lm_thread

(*
 * The identifier is just a string.
 *)
type t = string

(*
 * The identifier contains the:
 *    1. Machine name
 *    2. Process identifier
 *    3. A unique number in this process
 *)
let id_lock = Mutex.create "Lm_id"
let id_value = ref 0

let create () =
   let hostname =
      let name = Unix.gethostname () in
         try
            let host = Unix.gethostbyname name in
            let addrs = host.Unix.h_addr_list in
               if addrs = [||] then
                  host.Unix.h_name
               else
                  String.concat host.Unix.h_name (Array.to_list (Array.map Unix.string_of_inet_addr addrs))
         with
            Not_found ->
               name
   in
   let pid = Unix.getpid () in
   let index =
      Mutex.lock id_lock;
      let index = !id_value in
         incr id_value;
         Mutex.unlock id_lock;
         index
   in
      Lm_printf.sprintf "%s:%d:%d" hostname pid index

(*
 * Get a printable representation.
 *)
let string_of_id x =
   x

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
