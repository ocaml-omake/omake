(*
 * Implement the shared buffer.
 * If ENSEMBLE is defined, we use that implementation.
 * Values are always saved along 4-byte boundaries.
 *
 * ----------------------------------------------------------------
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
open Lm_marshal_sig

module Buf =
struct
   (*
    * This is the allocation unit size.
    *)
   let block_size = 65536

   (*
    * The unit size.
    *)
   let word_size = 4

(*
IFDEF ENSEMBLE THEN


   (*
    * We allocate in Iovec chunks, and we need to keep a subrange.
    *)
   type wbuf =
      { mutable wbuf_buf : string;
        mutable wbuf_index : int;
        mutable wbuf_stop : int;
        mutable wbuf_buffers : Iovecl.t list
      }

   type buf = Iovecl.t

   type rbuf =
      { mutable rbuf_buf : string;
        mutable rbuf_index : int;
        mutable rbuf_stop : int;
        mutable rbuf_iovecs : Iovec.t list;
        mutable rbuf_buffers : Iovecl.t
      }

   (*
    * We need an Mbuf to allocate iovec's.
    *)
   let mbuf = Mbuf.create "Shared_buf.Buf" (8 * block_size) block_size

   (*
    * Create a new buffer.
    * Allocate the first iovec.
    *)
   let create () =
      let alloc buf ofs len =
         (buf, ofs), block_size
      in
      let vecl, (buf, ofs) = Mbuf.alloc_fun "Shared_buf.buf.create" mbuf alloc in
         { wbuf_buf = buf;
           wbuf_index = ofs;
           wbuf_stop = ofs + block_size;
           wbuf_buffers = [vecl]
         }

   let flush_write info =
      let alloc buf ofs len =
         (buf, ofs), block_size
      in
      let vecl, (buf, ofs) = Mbuf.alloc_fun "Shared_buf.buf.flush_write" mbuf alloc in
         info.wbuf_buffers <- info.wbuf_buf :: info.wbuf_buffers;
         info.wbuf_buf <- buf;
         info.wbuf_index <- ofs;
         info.wbuf_stop <- ofs + block_size

   (*
    * Close the buffer and return the Iovecl.
    *)
   let to_static = function
      { wbuf_start = start; wbuf_index = index; wbuf_buffers = h :: t } ->
         let vecl = append_buffers (Iovecl.sub "Shared_buf.Buf.close" h 0 (Buf.ceil index) :: t) in
            Iovecl.free "Shared_buf.Buf.close" h;
            vecl

   (*
    * Create the read-only buffer.
    *)
   let to_read vecl =
      match Arrayf.to_list ((Obj.magic vecl) : Iovec.t Arrayf.t) with
         h :: t ->
            let read buf ofs len =
               ((Obj.magic buf) : string), ofs, len
            in
            let buf, ofs, len = Iovec.read "Shared_buf.Buf.read" h read in
               { rbuf_buf = buf;
                 rbuf_index = ofs;
                 rbuf_stop = ofs + len;
                 rbuf_iovecs = t;
                 rbuf_buffers = vecl
               }
       | [] ->
            { rbuf_buf = "";
              rbuf_index = 0;
              rbuf_stop = 0;
              rbuf_iovecs = [];
              rbuf_buffers = vecl
            }

   (*
    * Flush the current read buffer.
    *)
   let flush_read info =
      match info.rbuf_iovecs with
         h :: t ->
            let read buf ofs len =
               ((Obj.magic buf) : string), ofs, len
            in
            let buf, ofs, len = Iovec.read "Shared_buf.Buf.flush_read" h read in
               info.rbuf_buf <- buf;
               info.rbuf_index <- ofs;
               info.rbuf_stop <- ofs + len;
               info.rbuf_iovecs <- t
       | [] ->
            raise (Failure "flush_read")

   (*
    * Free the read buffer.
    *)
   let free { rbuf_buffers = vecl } =
      Iovecl.free "Shared_buf.Buf.free" vecl

ELSE (* ENSEMBLE undefined *)
*)

   (*
    * If not Ensemble, then we just create a simplified
    * form of the buffers.
    *)
   type wbuf =
      { mutable wbuf_buf : string;
        mutable wbuf_index : int;
        mutable wbuf_stop : int;
        mutable wbuf_buffers : string list
      }

   type buf = string list

   type rbuf =
      { mutable rbuf_buf : string;
        mutable rbuf_index : int;
        mutable rbuf_stop : int;
        mutable rbuf_buffers : string list
      }

   (*
    * Create a new buffer.
    *)
   let create () =
      { wbuf_buf = String.create block_size;
        wbuf_index = 0;
        wbuf_stop = 0;
        wbuf_buffers = []
      }

   let flush_write info =
      info.wbuf_buffers <- info.wbuf_buf :: info.wbuf_buffers;
      info.wbuf_buf <- String.create block_size;
      info.wbuf_index <- 0

   (*
    * Static buffer.
    *)
   let to_static { wbuf_buf = buf; wbuf_buffers = bufs } =
      List.rev (buf :: bufs)

   (*
    * Convert to a read buffer.
    *)
   let to_read = function
      h :: t ->
         { rbuf_buf = h;
           rbuf_index = 0;
           rbuf_stop = String.length h;
           rbuf_buffers = t
         }
    | [] ->
         { rbuf_buf = "";
           rbuf_index = 0;
           rbuf_stop = 0;
           rbuf_buffers = []
         }

   (*
    * Flush the current read buffer.
    *)
   let flush_read info =
      match info.rbuf_buffers with
         h :: t ->
            info.rbuf_buf <- h;
            info.rbuf_index <- 0;
            info.rbuf_stop <- String.length h;
            info.rbuf_buffers <- t
       | [] ->
            raise (Invalid_argument "read")

   (*
    * Free the buffer.
    *)
   let free _ =
      ()

(*
ENDIF
*)

   (*
    * Write a string to the buffer.
    *)
   let rec write info buf off len =
      let { wbuf_buf = buf';
            wbuf_index = start;
            wbuf_stop = stop
          } = info
      in
      let amount = stop - start in
         if len <= amount then
            begin
               (* Normal cas *)
               String.blit buf off buf' start len;
               info.wbuf_index <- start + len
            end
         else
            begin
               (* Write in multiple fragments *)
               String.blit buf off buf' start amount;
               flush_write info;
               write info buf (off + amount) (len - amount)
            end

   (*
    * Write an integer.
    * The LSB is set to 1.
    *)
   let rec write_int info i =
      let { wbuf_buf = buf;
            wbuf_index = start;
            wbuf_stop = stop
          } = info
      in
         if start = stop then
            begin
               flush_write info;
               write_int info i
            end
         else
            begin
               buf.[start] <- Char.chr ((i lsr 23) land 255);
               buf.[start + 1] <- Char.chr ((i lsr 15) land 255);
               buf.[start + 2] <- Char.chr ((i lsr 7) land 255);
               buf.[start + 3] <- Char.chr (((i lsl 1) land 255) + 1);
               info.wbuf_index <- start + word_size
            end

   (*
    * Write an integer.
    * The LSB is set to 10.
    *)
   let rec write_int2 info tag i =
      let { wbuf_buf = buf;
            wbuf_index = start;
            wbuf_stop = stop
          } = info
      in
         if start = stop then
            begin
               flush_write info;
               write_int info i
            end
         else
            begin
               buf.[start] <- Char.chr ((i lsr 21) land 255);
               buf.[start + 1] <- Char.chr ((i lsr 13) land 255);
               buf.[start + 2] <- Char.chr ((i lsr 5) land 255);
               buf.[start + 3] <- Char.chr (((i lsl 3) land 255) + (tag lsl 2) + 2);
               info.wbuf_index <- start + word_size
            end

   (*
    * Write a four-byte word.
    *)
   let rec write_tag info tag i =
      let { wbuf_buf = buf;
            wbuf_index = start;
            wbuf_stop = stop
          } = info
      in
         if start = stop then
            begin
               flush_write info;
               write_tag info tag i
            end
         else
            begin
               buf.[start] <- Char.chr tag;
               buf.[start + 1] <- Char.chr ((i lsr 14) land 255);
               buf.[start + 2] <- Char.chr ((i lsr 6) land 255);
               buf.[start + 3] <- Char.chr ((i lsl 2) land 255);
               info.wbuf_index <- start + word_size
            end

   (*
    * Read a string from the buffer.
    *)
   let rec read info buf off len =
      let { rbuf_buf = buf';
            rbuf_index = start;
            rbuf_stop = stop
          } = info
      in
      let amount = stop - start in
         if len <= amount then
            begin
               (* Usual case *)
               String.blit buf' start buf off len;
               info.rbuf_index <- start + len
            end
         else
            begin
               (* Fragment the read *)
               String.blit buf' start buf off amount;
               flush_read info;
               read info buf (off + amount) (len - amount)
            end

   (*
    * Check if it is a header word.
    *)
   let rec read_value_type info =
      let { rbuf_buf = buf;
            rbuf_index = start;
            rbuf_stop = stop
          } = info
      in
         if start = stop then
            begin
               flush_read info;
               read_value_type info
            end
         else
            match (Char.code buf.[start + 3]) land 3 with
               0 ->
                  TaggedValue
             | 2 ->
                  Int2Value
             | _ ->
                  IntValue

   (*
    * Read the int.
    *)
   let read_int info =
      let { rbuf_buf = buf; rbuf_index = start } = info in
      let i =
         ((Char.code buf.[start]) lsl 23) +
         ((Char.code buf.[start + 1]) lsl 15) +
         ((Char.code buf.[start + 2]) lsl 7) +
         ((Char.code buf.[start + 3]) lsr 1)
      in
         info.rbuf_index <- start + word_size;
         i

   (*
    * Read the int2.
    *)
   let read_int2_tag info =
      ((Char.code info.rbuf_buf.[info.rbuf_index + 3]) lsr 2) land 1

   let read_int2_value info =
      let { rbuf_buf = buf; rbuf_index = start } = info in
      let i =
         ((Char.code buf.[start]) lsl 21) +
         ((Char.code buf.[start + 1]) lsl 13) +
         ((Char.code buf.[start + 2]) lsl 5) +
         ((Char.code buf.[start + 3]) lsr 3)
      in
         info.rbuf_index <- start + word_size;
         i

   (*
    * Read the tag.
    *)
   let read_value_tag info =
      let { rbuf_buf = buf; rbuf_index = start } = info in
         Char.code buf.[start]

   let read_value_value info =
      let { rbuf_buf = buf; rbuf_index = start } = info in
      let i =
         ((Char.code buf.[start + 1]) lsl 14) +
         ((Char.code buf.[start + 2]) lsl 6) +
         ((Char.code buf.[start + 3]) lsr 2)
      in
         info.rbuf_index <- start + word_size;
         i
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
