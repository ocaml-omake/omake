(*
 * A pipe implemented with a memory-mapped file.
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
open Lm_printf
open Lm_debug

let debug_pipe =
   create_debug (**)
      { debug_name = "pipe";
        debug_description = "Debug shared memory pipe operations";
        debug_value = false
      }

let eflush out =
   output_char out '\n';
   flush out

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The pipe has three objects:
 *    1. a file, where the data is written
 *    2. a socket, for waking up the peer process
 *
 * The file is divided into two halves,
 * one for reading, and one for writing.
 *
 * To send a value:
 *    1. Wait until the write buffer is empty
 *    2. Write the value
 *    3. Send a char on the socket
 * To receive a value:
 *    1. Wait until the read buffer has a value
 *    2. Unmarshal the value from the read buffer
 *    3. Send a char on the socket
 *
 * The format of a buffer entry:
 *    full (4 byte): buffer contains a value if fourth byte is nonzero
 *    code (4 bytes): a number associated with the entry
 *    name_length (4 bytes): length of the name string
 *    name (string): a string for the entry
 *    data (marshaled value): the data associated with the entry
 *)
type t =
   { mmap_block_size : int;
     mmap_read_offset : int;
     mmap_write_offset : int;
     mmap_data : string;
     mmap_file : Lm_mmap.t;
     mmap_server : Unix.file_descr option;
     mutable mmap_socket : Unix.file_descr option
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Parameters.
 *)
let max_servers = 16
let sock_suffix = ".sock"
let buffer_suffix = ".buffer"

(*
 * Offsets into the buffers.
 *)
let full_offset = 0
let code_offset = 4
let data_length_offset = 8
let name_length_offset = 12
let name_offset = 16

(*
 * Four-byte boundaries.
 *)
let ceil_word i =
   (i + 3) land (lnot 3)

(*
 * Integer operations on string buffers.
 *)
let get_int buf offset =
   ((Char.code buf.[offset]) lsl 24)
   lor ((Char.code buf.[offset + 1]) lsl 16)
   lor ((Char.code buf.[offset + 2]) lsl 8)
   lor (Char.code buf.[offset + 3])

let set_int buf offset code =
   buf.[offset] <- Char.chr ((code lsr 24) land 255);
   buf.[offset + 1] <- Char.chr ((code lsr 16) land 255);
   buf.[offset + 2] <- Char.chr ((code lsr 8) land 255);
   buf.[offset + 3] <- Char.chr (code land 255)

(*
 * Character buffer for communication on the socket.
 *)
let char_buf = String.create 1

(*
 * Search for a free server.
 * We search for a free file in the directory
 * specified by:
 *    First, stat the socket.
 *    If it does not exist, then we have a new location.
 *    If it does exist, then try connecting:
 *       If the connect fails, the server is dead, and we use the file.
 *       If the connect succeeds, the server is alive.
 *          Send it a cancelation char, then try the next file.
 *)
let search_server dir =
   (* Make the directory in case it does not exist *)
   (try Unix.mkdir dir 511 with Unix.Unix_error _ -> ());

   (* Search for a free port *)
   let rec search i =
      if i = max_servers then
         raise (Failure "Lm_mmap_pipe.search_server: too many running servers");

      let filename = sprintf "%s/%d" dir i in
      let sockname = filename ^ sock_suffix in
         try
            let _ = Unix.LargeFile.stat sockname in

            (* File exists, so try connecting *)
            let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
               try
                  Unix.connect sock (Unix.ADDR_UNIX sockname);

                  (* Server is operational, so close the socket and keep trying *)
                  Unix.close sock;
                  search (succ i)
               with
                  Unix.Unix_error _ ->
                     (* No server here, so close the socket and return the file *)
                     Unix.unlink sockname;
                     Unix.close sock;
                     filename
         with
            Unix.Unix_error _ ->
               (* No socket, so return this file *)
               filename
   in
      search 1

let create_server dir =
   let _ = Sys.signal Sys.sigpipe Sys.Signal_ignore in
   let filename = search_server dir in
   let bufname = filename ^ buffer_suffix in
   let sockname = filename ^ sock_suffix in
   let _ =
      if !debug_pipe then
         eprintf "Creating buffer %s%t" bufname eflush
   in
   let file = Lm_mmap.create bufname [Unix.O_RDWR; Unix.O_CREAT] 438 in
   let buf = Lm_mmap.to_string file in
   let _ =
      if !debug_pipe then
         eprintf "Buffer size: 0x%08x %d%t" (Obj.magic buf) (String.length buf) eflush
   in
   let length = (String.length buf) lsr 1 in
   let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
   let at_exit () =
      eprintf "Lm_mmap_pipe: cleaning up server sockets%t" eflush;
      (try Unix.unlink bufname with Unix.Unix_error _ -> ());
      (try Unix.unlink sockname with Unix.Unix_error _ -> ())
   in
      Pervasives.at_exit at_exit;
      Unix.bind sock (Unix.ADDR_UNIX sockname);
      Unix.listen sock 1;
      set_int buf (0 + full_offset) 0;
      set_int buf (length + full_offset) 0;
      { mmap_block_size = length;
        mmap_read_offset = length;
        mmap_write_offset = 0;
        mmap_data = buf;
        mmap_file = file;
        mmap_server = Some sock;
        mmap_socket = None
      }

(*
 * Search for a free server.
 * Look through the directory, and try to connect to each of the servers.
 *)
let search_client dir =
   let rec search i =
      if i = max_servers then
         raise (Failure "Lm_mmap_pipe: no servers are online");
      let filename = sprintf "%s/%d" dir i in
      let sockname = filename ^ sock_suffix in
      let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
         try
            Unix.connect sock (Unix.ADDR_UNIX sockname);

            (* Connected to this server *)
            let count = Unix.read sock char_buf 0 1 in
               if count = 1 && char_buf.[0] <> Char.chr 0 then
                  filename, sock
               else
                  begin
                     (* Server is already busy *)
                     Unix.close sock;
                     search (succ i)
                  end
         with
            Unix.Unix_error _ ->
               Unix.close sock;
               search (succ i)
   in
      search 1

(*
 * Connect to the pipe.
 *)
let create_client dir =
   let filename, sock = search_client dir in
   let bufname = filename ^ buffer_suffix in
   let file = Lm_mmap.create bufname [Unix.O_RDWR] 438 in
   let buf = Lm_mmap.to_string file in
   let _ =
      if !debug_pipe then
         eprintf "Buffer size: 0x%08x %d%t" (Obj.magic buf) (String.length buf) eflush
   in
   let length = (String.length buf) lsr 1 in
      { mmap_block_size = length;
        mmap_read_offset = 0;
        mmap_write_offset = length;
        mmap_data = buf;
        mmap_file = file;
        mmap_server = None;
        mmap_socket = Some sock
      }

(*
 * Get the server socket for polling.
 *)
let server_socket { mmap_server = server } =
   match server with
      Some sock ->
         sock
    | None ->
         raise (Invalid_argument "server_socket")

(*
 * Get the client socket for polling.
 *)
let client_socket { mmap_socket = client } =
   match client with
      Some sock ->
         sock
    | None ->
         raise (Invalid_argument "client_socket")

let try_write sock buf =
   try
      let _ = Unix.write sock buf 0 1 in ()
   with Unix.Unix_error _ ->
      ()

(*
 * Open a client connection for the server.
 *)
let open_client mmap =
   match mmap with
      { mmap_server = Some server; mmap_socket = Some _ } ->
         (* Only one client is allowed *)
         let sock, _ = Unix.accept server in
            char_buf.[0] <- Char.chr 0;
            try_write sock char_buf;
            Unix.close sock;
            false

    | { mmap_server = Some server;
        mmap_socket = None;
        mmap_data = buf;
        mmap_block_size = length
      } ->
         (* We're free, so accept this client *)
         let sock, _ = Unix.accept server in
            char_buf.[0] <- Char.chr 1;
            try_write sock char_buf;
            set_int buf (0 + full_offset) 0;
            set_int buf (length + full_offset) 0;
            mmap.mmap_socket <- Some sock;
            true

    | _ ->
         raise (Invalid_argument "Lm_mmap_pipe.open_client")

(*
 * Close the client socket (to disconnect the client).
 * This is only allowed for servers.
 *)
let close_client mmap =
   eprintf "Lm_mmap_pipe: closing client%t" eflush;
   match mmap with
      { mmap_server = Some _; mmap_socket = Some sock; mmap_data = buf; mmap_block_size = length } ->
         set_int buf (0 + full_offset) 0;
         set_int buf (length + full_offset) 0;
         Unix.close sock;
         mmap.mmap_socket <- None
    | _ ->
         raise (Invalid_argument "close_client")

(*
 * Wait for something to happen to the buffer.
 *)
let block mmap =
   if !debug_pipe then
      eprintf "Lm_mmap_pipe.block: 0x%08x%t" (Obj.magic mmap.mmap_data) eflush;
   match mmap.mmap_socket with
      Some sock ->
         let count = Unix.read sock char_buf 0 1 in
            count = 1
    | None ->
         raise (Invalid_argument "block")

(*
 * Try to write a value into the buffer.
 * Return true iff successful.
 *)
let write mmap code name raw_write =
   if !debug_pipe then
      eprintf "Lm_mmap_pipe.write: begin 0x%08x%t" (Obj.magic mmap.mmap_data) eflush;
   match mmap.mmap_socket with
      Some sock ->
         let { mmap_write_offset = woffset;
               mmap_data = buf;
               mmap_block_size = length
             } = mmap
         in
         let full = get_int buf (woffset + full_offset) in
            if full = 0 then
               let name_length = String.length name in
               let data_size =
                  let offset = ceil_word (name_offset + name_length) in
                     raw_write buf (woffset + offset) (length - offset)
               in
               let data_size = ceil_word data_size in
                  set_int buf (woffset + code_offset) code;
                  set_int buf (woffset + name_length_offset) name_length;
                  String.blit name 0 buf (woffset + name_offset) name_length;
                  set_int buf (woffset + data_length_offset) data_size;
                  set_int buf (woffset + full_offset) 1;
                  try_write sock buf;
                  if !debug_pipe then
                     eprintf "Lm_mmap_pipe.write: succeeded%t" eflush;
                  true
            else
               begin
                  if !debug_pipe then
                     eprintf "Lm_mmap_pipe.write: failed%t" eflush;
                  false
               end
    | None ->
         raise (Invalid_argument "Lm_mmap_pipe.write")

(*
 * Try to read a value from a buffer.
 *)
let read mmap raw_read =
   if !debug_pipe then
      eprintf "Lm_mmap_pipe.read: begin 0x%08x%t" (Obj.magic mmap.mmap_data) eflush;
   match mmap.mmap_socket with
      Some sock ->
         let { mmap_read_offset = roffset;
               mmap_data = buf
             } = mmap
         in
         let full = get_int buf (roffset + full_offset) in
            if full = 0 then
               begin
                  if !debug_pipe then
                     eprintf "Lm_mmap_pipe.read: failed%t" eflush;
                  None
               end
            else
               let code = get_int buf (roffset + code_offset) in
               let name_length = get_int buf (roffset + name_length_offset) in
               let name = String.create name_length in
               let _ = String.blit buf (roffset + name_offset) name 0 name_length in
               let data_length = get_int buf (roffset + data_length_offset) in
               let offset = ceil_word (name_offset + name_length) in
               let data = raw_read buf (roffset + offset) data_length in
                  set_int buf (roffset + full_offset) 0;
                  try_write sock char_buf;
                  if !debug_pipe then
                     eprintf "Lm_mmap_pipe.read: succeeded%t" eflush;
                  Some (code, name, data)
    | None ->
         raise (Invalid_argument "Lm_mmap_pipe.read")

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
