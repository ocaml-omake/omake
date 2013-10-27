(*
 * Simplfiied socket interface.
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

type server
type client

(*
 * Access to descriptors.
 *)
val file_descr_of_client : client -> Unix.file_descr

(*
 * Create a server.
 * The argument is the port number.
 * If the port is not set, an arbitrary port is chosen.
 *)
val serve : int option -> server

(*
 * Close the server.
 *)
val close_server : server -> unit

(*
 * Accept a connection on the server port.
 *)
val accept : server -> client

(*
 * Create a connection to a server.
 *)
val connect : string -> int -> client

(*
 * Get the local info about the server.
 *)
val get_server_host : server -> string * int

(*
 * Get the local info about the client.
 *)
val get_client_host : client -> string * int

(*
 * Get the info about the peer.
 *)
val get_client_peer : client -> string * int

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
