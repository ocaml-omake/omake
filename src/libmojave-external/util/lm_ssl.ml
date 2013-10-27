(*
 * Interface to Open SSL.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
let eprintf = Printf.eprintf
let eflush out =
   output_char out '\n';
   flush out

open Lm_make_printf

(*
 * Define hooks to C code.
 *)
type t
type ssl = t

exception SSLSigPipe

external socket         : string -> t                        = "lm_ssl_socket"
external bind           : t -> Unix.inet_addr -> int -> unit = "lm_ssl_bind"
external getsockname    : t -> Unix.inet_addr * int          = "lm_ssl_get_addr"
external listen         : t -> string -> int -> unit         = "lm_ssl_listen"
external accept         : t -> t                             = "lm_ssl_accept"
external connect        : t -> Unix.inet_addr -> int -> unit = "lm_ssl_connect"
external shutdown       : t -> unit                          = "lm_ssl_shutdown"
external close          : t -> unit                          = "lm_ssl_close"

(*
 * For restarting from an existing socket.
 *)
external fd             : t -> int                           = "lm_ssl_fd"
external serve          : int -> string -> string -> t       = "lm_ssl_serve"

(*
 * Private functions.
 *)
external lm_ssl_enabled  : unit -> bool                       = "lm_ssl_enabled"
external lm_ssl_init     : unit -> unit                       = "lm_ssl_init"
external lm_ssl_read     : t -> string -> int -> int -> int   = "lm_ssl_read"
external lm_ssl_write    : t -> string -> int -> int -> int   = "lm_ssl_write"
external lm_ssl_flush    : t -> unit                          = "lm_ssl_flush"

(*
 * Initialize.
 *)
let () = lm_ssl_init ()
let enabled = lm_ssl_enabled ()

(*
 * Buffered output channel.
 *)
module OutChannel =
struct
   (* Some buffer type *)
   type t =
      { mutable windex : int;
        buffer : string;
        ssl    : ssl
      }

   type result = unit

   (* Buffer length is 1k *)
   let buf_length = 1 lsl 10

   (* Create the output channel *)
   let create ssl =
      { windex = 0;
        buffer = String.create buf_length;
        ssl = ssl
      }

   (* Flush the output *)
   let flush out =
      let { windex = windex;
            buffer = buf;
            ssl = ssl
          } = out
      in
      let rec flush off =
         if off < windex then
            let amount = lm_ssl_write ssl buf off (windex - off) in
               if amount <= 0 then
                  raise SSLSigPipe;
               flush (off + amount)
      in
         flush 0;
         out.windex <- 0

   (* Close the output channel *)
   let close out =
      flush out;
      close out.ssl

   (*
    * Add a single character.
    *)
   let print_char out c =
      let { windex = windex;
            buffer = buf
          } = out
      in
      let windex' = succ windex in
         buf.[windex] <- c;
         out.windex <- windex';
         if windex' = buf_length then
            flush out

   (*
    * Add a string.
    *)
   let rec output out s off len =
      if len <> 0 then
         let { windex = windex;
               buffer = buf
             } = out
         in
         let amount = min len (buf_length - windex) in
         let windex' = windex + amount in
            String.blit s off buf windex amount;
            out.windex <- windex';
            if windex' = buf_length then
               flush out;
            output out s (off + amount) (len - amount)

   let print_string out s =
      output out s 0 (String.length s)

   (*
    * Formatting functions are ignored.
    *)
   let open_box _ _ = ()
   let open_hbox _ = ()
   let open_vbox _ _ = ()
   let open_hvbox _ _ = ()
   let open_hovbox _ _ = ()
   let close_box _ = ()

   (*
    * These formatting actions are partially handled.
    *)
   let print_cut _ =
      ()

   let print_space out =
      print_char out ' '

   let force_newline out =
      print_char out '\n'

   let print_break _ _ _ =
      ()

   let print_flush out =
      flush out;
      lm_ssl_flush out.ssl

   let print_newline =
      force_newline

   let exit _out =
      ()
end

module Printf = MakePrintf (OutChannel)

type ssl_out = OutChannel.t

let out_channel_of_ssl = OutChannel.create
let output_char = OutChannel.print_char
let output_string = OutChannel.print_string
let output_buffer out buf =
   output_string out (Buffer.contents buf)
let output = OutChannel.output
let flush = OutChannel.flush
let close_out = OutChannel.close

let fprintf = Printf.fprintf

(*
 * Input channel.
 *)
module InChannel =
struct
   type t =
      { mutable rindex : int;
        mutable length : int;
        buffer : string;
        ssl : ssl
      }

   (*
    * Input buffer is 1k.
    *)
   let buf_length = 1 lsl 10

   (*
    * Create the buffer.
    *)
   let create ssl =
      { rindex = 0;
        length = 0;
        buffer = String.create buf_length;
        ssl = ssl
      }

   let close inx =
      close inx.ssl

   (*
    * Fill input.
    *)
   let rec fill inx =
      let { rindex = rindex;
            length = length;
            buffer = buf;
            ssl = ssl
          } = inx
      in
         if length <> 0 && rindex = length then
            begin
               inx.rindex <- 0;
               inx.length <- 0;
               fill inx
            end
         else if length <> buf_length then
            begin
               let amount = lm_ssl_read ssl buf length (buf_length - length) in
                  if amount <= 0 then
                     raise End_of_file;
                  inx.length <- length + amount
            end

   (*
    * Get a character.
    *)
   let rec input_char inx =
      let { rindex = rindex;
            length = length;
            buffer = buf
          } = inx
      in
         if rindex = length then
            begin
               fill inx;
               input_char inx
            end
         else
            let c = buf.[rindex] in
               inx.rindex <- succ rindex;
               c

   (*
    * Get a line.
    *)
   let rec input_line_aux out inx =
      let { rindex = rindex;
            length = length;
            buffer = buf
          } = inx
      in
      let rec collect rindex =
         if rindex = length then
            let filled =
               inx.rindex <- rindex;
               try fill inx; true with
                  End_of_file ->
                     false
            in
               if filled then
                  input_line_aux out inx
               else
                  Buffer.contents out
         else
            let c = buf.[rindex] in
               if c = '\n' then
                  begin
                     inx.rindex <- succ rindex;
                     Buffer.contents out
                  end
               else
                  begin
                     Buffer.add_char out c;
                     collect (succ rindex)
                  end
      in
         collect rindex

   let input_line inx =
      if inx.rindex = inx.length then
         fill inx;
      input_line_aux (Buffer.create 64) inx

   (*
    * Get some input.
    * Try to optimize this case a little, and
    * copy directly into the buffer if possible.
    *)
   let rec really_input_start inx s off len =
      let { rindex = rindex;
            length = length;
            buffer = buf
          } = inx
      in
      let amount = min len (length - rindex) in
      let () =
         String.blit buf rindex s off amount;
         inx.rindex <- rindex + amount
      in
         let off = off + amount in
         let len = len - amount in
            if len <> 0 then
               really_input_middle inx s off len

   and really_input_middle inx s off len =
      if len < buf_length then
         really_input_end inx s off len
      else
         let amount = lm_ssl_read inx.ssl s off len in
         let () =
            if amount <= 0 then
               raise End_of_file
         in
         let off = off + amount in
         let len = len - amount in
            really_input_middle inx s off len

   and really_input_end inx s off len =
      let { rindex = rindex;
            length = length;
            buffer = buf
          } = inx
      in
         if length - rindex < len then
            begin
               fill inx;
               really_input_end inx s off len
            end
         else
            begin
               String.blit buf rindex s off len;
               inx.rindex <- rindex + len
            end

   let really_input = really_input_start
end

type ssl_in = InChannel.t

let in_channel_of_ssl = InChannel.create
let input_char = InChannel.input_char
let input_line = InChannel.input_line
let really_input = InChannel.really_input
let close_in = InChannel.close

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
