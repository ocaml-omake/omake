(*
 * Simple buffered IO.
 *
 * We have to deal with select on Win32 in some sensible way.
 * The problem is that select does not work with files or
 * pipes.  Here is what we do:
 *
 *    1. If a channel is a file, IO is always possible
 *    2. If polling for read, and the buffer is nonempty, return true
 *    3. If polling for write, and the buffer is empty, return true
 *
 *    Main polling loop:
 *       4. If polling for read on a pipe:
 *          Use PeekNamedPipe
 *       5. If polling for write on a pipe:
 *          Create a thread to perform the write
 *       6. For sockets, use select with a small timeout so
 *          so we can check the pipes periodically.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology
 * and HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol
open Lm_location

open Lm_thread_pool

(*
 * The channel may be a file, pipe, or socket.
 * !!!The ordering of these fields is important!!!
 * If you update this, fix the function omake_shell_pipe_kind()
 * in omake_shell_csys.c
 *)
type kind =
   FileChannel
 | PipeChannel
 | SocketChannel

type mode =
   InChannel
 | OutChannel
 | InOutChannel

(*
 * Canonical names.
 *)
let string_sym = Lm_symbol.add ".string"
let fun_sym    = Lm_symbol.add ".fun"

(*
 * The channel has an input and output buffer.
 * In addition, on Win32, we may have threads
 * to perform asynchronous writes on pipes.
 *)
type channel =
   { mutable channel_id   : int;
     channel_fd           : Unix.file_descr option;
     channel_kind         : kind;
     channel_mode         : mode;
     mutable channel_file : symbol;

     (* If not channel_binary, then perform win32 line-ending transformation *)
     mutable channel_binary : bool;

     (*
      * We keep track of line number information.
      * The start_* fields give the line and character
      * number at the beginning of the buffer.
      *
      * The middle_* fields give the information at some
      * arbitrary point between 0 and in_max.
      *)
     mutable start_line    : int;
     mutable start_char    : int;

     mutable middle_index  : int;
     mutable middle_line   : int;
     mutable middle_char   : int;

     (*
      * in_index is the index of the next character in the input.
      * in_max is the total amount of data in the input buffer.
      *)
     mutable in_index     : int;
     mutable in_max       : int;
     mutable in_buffer    : string;

     (*
      * For lexing from the buffer.
      * lex_index is the current input position when lexing.
      * Invariant: in_index <= lex_index <= in_max
      *)
     mutable lex_index    : int;

     (*
      * out_max is the total amount of data in the output buffer.
      * If out_expand is true, the outbuffer is expanded instead
      * instead of flushing.
      *)
     out_expand           : bool;
     mutable out_max      : int;
     mutable out_buffer   : string;

     (*
      * In text mode, the output is double-buffered because
      * of line ending translation.  
      * INVARIANT: In binary mode, the write_buffer is the same as the out_buffer.
      *
      * write_pid is the pid of the output thread, or 0 if there is no output thread.
      * write_index is the amount of data that has been written to the file.
      * write_max is the total amount of data in the write_buffer.
      *)
     mutable write_pid    : int;
     mutable write_index  : int;
     mutable write_max    : int;
     mutable write_buffer : string;

     (*
      * These are the actual read and write functions.
      *)
     mutable read_fun     : string -> int -> int -> int;
     mutable write_fun    : string -> int -> int -> int
   }

type t = channel

(*
 * This is the normal buffer size.
 *)
let buf_size     = 4096

(*
 * This is the maximum size we allow
 * the input buffer to grow when lexing.
 *)
let lex_buf_size = 1 lsl 18

(*
 * We may be told that this is a pipe, but really figure out what kind it is.
 *)
external pipe_kind : Unix.file_descr -> kind = "omake_shell_pipe_kind"

(*
 * Default readers and writers.
 *
 * XXX: We treat the "broken pipe" errors as EOFs.
 *)
let default_reader fd buf off len =
   match fd with
      Some fd ->
         blocking_section (fun () ->
               try Unix.read fd buf off len with
                  Unix.Unix_error (Unix.EUNKNOWNERR 0, _, _)
                | Unix.Unix_error (Unix.EPIPE, _, _) ->
                     0) ()
    | None ->
         raise (Unix.Unix_error (Unix.EINVAL, "default_reader", ""))

(*
 * Convert a fd to an integer (for debugging).
 *)
external int_of_fd : Unix.file_descr -> int = "int_of_fd"

let default_writer fd buf off len =
   match fd with
      Some fd ->
         blocking_section (fun () ->
               Unix.write fd buf off len) ()
    | None ->
         raise (Unix.Unix_error (Unix.EINVAL, "default_writer", ""))

(*
 * Readers and writers for string channels.
 *)
let null_reader _ _ _ =
   0

let null_writer _ _ _ =
   0

(*
 * Empty buffer.
 *)
let create file kind mode binary fd =
   let kind =
      match fd with
         Some fd ->
            if kind = PipeChannel then
               pipe_kind fd
            else
               kind
       | None ->
            kind
   in
   let binary = Sys.os_type <> "Win32" || binary in
   let out_buffer = String.create buf_size in
   let write_buffer =
      if binary then
         out_buffer
      else
         String.create (buf_size * 2)
   in
      { channel_id     = 0;
        channel_fd     = fd;
        channel_kind   = kind;
        channel_mode   = mode;
        channel_file   = Lm_symbol.add file;
        channel_binary = Sys.os_type <> "Win32" || binary;

        start_line     = 1;
        start_char     = 0;
        middle_index   = 0;
        middle_line    = 1;
        middle_char    = 0;

        in_index       = 0;
        in_max         = 0;
        in_buffer      = String.create (succ buf_size);
        lex_index      = 0;

        out_max        = 0;
        out_expand     = false;
        out_buffer     = out_buffer;

        write_pid      = 0;
        write_index    = 0;
        write_max      = 0;
        write_buffer   = write_buffer;

        read_fun       = default_reader fd;
        write_fun      = default_writer fd
      }

let set_id info id =
   info.channel_id <- id

let of_string file line char s =
   let len = String.length s in
      { channel_id     = 0;
        channel_fd     = None;
        channel_kind   = FileChannel;
        channel_mode   = InChannel;
        channel_file   = file;
        channel_binary = true;

        start_line     = line;
        start_char     = char;
        middle_index   = 0;
        middle_line    = line;
        middle_char    = char;

        in_index     = 0;
        in_max       = len;
        in_buffer    = s;
        lex_index    = 0;

        out_max      = 0;
        out_expand   = false;
        out_buffer   = "";

        write_pid    = 0;
        write_index  = 0;
        write_max    = 0;
        write_buffer = "";

        read_fun     = null_reader;
        write_fun    = null_writer
      }

let of_fun read write =
   let out_buffer = String.create buf_size in
   { channel_id     = 0;
     channel_fd     = None;
     channel_kind   = FileChannel;
     channel_mode   = InOutChannel;
     channel_file   = fun_sym;
     channel_binary = true;

     start_line     = 1;
     start_char     = 0;
     middle_index   = 0;
     middle_line    = 1;
     middle_char    = 0;

     in_index     = 0;
     in_max       = 0;
     in_buffer    = String.create buf_size;
     lex_index    = 0;

     out_max      = 0;
     out_expand   = false;
     out_buffer   = out_buffer;

     write_pid    = 0;
     write_index  = 0;
     write_max    = 0;
     write_buffer = out_buffer;

     read_fun     = read;
     write_fun    = write
   }

let of_loc_string file line char s =
   of_string (Lm_symbol.add file) line char s

let of_substring s off len =
   of_string string_sym 1 0 (String.sub s off len)

let of_string s =
   of_string string_sym 1 0 (String.copy s)

let info channel =
   let { channel_id = id;
         channel_kind = kind;
         channel_mode = mode;
         channel_binary = binary
       } = channel
   in
      id, kind, mode, binary

let name channel =
   Lm_symbol.to_string channel.channel_file

let descr channel =
   match channel.channel_fd with
      Some fd ->
         fd
    | None ->
         raise (Unix.Unix_error (Unix.EINVAL, "descr", ""))

let set_binary_mode =
   if Sys.os_type = "Win32" then
      (fun info flag -> info.channel_binary <- flag)
   else
      (fun _ _ -> ())

let set_io_functions info reader writer =
   info.read_fun <- reader;
   info.write_fun <- writer

let create_loc_string_aux file line char =
   let out_buffer = String.create buf_size in
   { channel_id     = 0;
     channel_fd     = None;
     channel_kind   = FileChannel;
     channel_mode   = OutChannel;
     channel_file   = file;
     channel_binary = true;

     start_line     = line;
     start_char     = char;
     middle_index   = 0;
     middle_line    = line;
     middle_char    = char;

     in_index     = 0;
     in_max       = 0;
     in_buffer    = "";
     lex_index    = 0;

     out_max      = 0;
     out_expand   = true;
     out_buffer   = out_buffer;

     write_pid    = 0;
     write_index  = 0;
     write_max    = 0;
     write_buffer = out_buffer;

     read_fun     = null_reader;
     write_fun    = null_writer
   }

let create_loc_string file line char =
   create_loc_string_aux (Lm_symbol.add file) line char

let create_string () =
   create_loc_string_aux string_sym 1 0

(************************************************************************
 * Line envding translation.
 *)
let debug_get s i =
   eprintf "String.get: %d[%d]@." (String.length s) i;
   String.get s i

let debug_set s i c =
   eprintf "String.set: %d[%d]@." (String.length s) i;
   String.set s i c

let string_get = String.unsafe_get
let string_set = String.unsafe_set

let rec expand_text obuffer omax wbuffer =
   assert (omax >= 0 && omax <= String.length obuffer && omax * 2 <= String.length wbuffer);
   let rec copy1 src dst =
      if src = omax then
         dst
      else
         match string_get obuffer src with
            '\n' ->
               string_set wbuffer dst '\r';
               string_set wbuffer (succ dst) '\n';
               copy1 (succ src) (dst + 2)
          | c ->
               string_set wbuffer dst c;
               copy1 (succ src) (succ dst)
   in
      copy1 0 0

let squash_text buffer off amount =
   assert (off >= 0 && amount >= 0 && off + amount <= String.length buffer);
   if amount = 0 then
      0
   else
      let max = off + amount in
      let rec copy2 dst src =
         if src = max then
            dst - off
         else if src = max - 1 then
            begin
               string_set buffer dst (string_get buffer src);
               succ dst - off
            end
         else
            match string_get buffer src with
               '\r' when string_get buffer (succ src) = '\n' ->
                  string_set buffer dst '\n';
                  copy2 (succ dst) (src + 2)
             | _ ->
                  string_set buffer dst (string_get buffer src);
                  copy2 (succ dst) (succ src)
      in
         copy2 off off

(************************************************************************
 * Line numbers.
 *)

(*
 * Get the line/char for a particular point in the input buffer.
 *)
let line_of_index info buffer index =
   let { start_line   = start_line;
         start_char   = start_char;
         middle_index = middle_index;
         middle_line  = middle_line;
         middle_char  = middle_char
       } = info
   in
   let rec search line char i =
      if i = index then
         begin
            info.middle_index <- index;
            info.middle_line <- line;
            info.middle_char <- char;
            line, char
         end
      else if buffer.[i] = '\n' then
         search (succ line) 0 (succ i)
      else
         search line (succ char) (succ i)
   in
      if index >= middle_index then
         search middle_line middle_char middle_index
      else
         search start_line start_char 0

(*
 * Reset the input buffer.
 * This resets the start line and position.
 *)
let reset_input_buffer info =
   let line, char = line_of_index info info.in_buffer info.in_max in
      info.start_line   <- line;
      info.start_char   <- char;
      info.middle_index <- 0;
      info.middle_line  <- line;
      info.middle_char  <- char;
      info.in_max       <- 0;
      info.in_index     <- 0;
      info.lex_index    <- 0

let shift_input_buffer info =
   let { in_buffer = in_buffer;
         in_index  = in_index;
         lex_index = lex_index;
         in_max    = in_max
       } = info
   in
   let line, char = line_of_index info in_buffer in_index in
      String.blit in_buffer in_index in_buffer 0 (in_max - in_index);
      info.start_line   <- line;
      info.start_char   <- char;
      info.middle_index <- 0;
      info.middle_line  <- line;
      info.middle_char  <- char;
      info.in_index     <- 0;
      info.in_max       <- in_max - in_index;
      info.lex_index    <- lex_index - in_index

let set_line info name line =
   shift_input_buffer info;
   info.start_line   <- line;
   info.start_char   <- 0;
   info.middle_line  <- line;
   info.middle_char  <- 0;
   info.channel_file <- Lm_symbol.add name

(*
 * Reset the output buffer.
 * This resets the start line and position if this is a file.
 * For pipes and sockets, input and output are independent.
 *)
let reset_output_buffer info =
   if info.channel_kind = FileChannel then
      begin
         let line, char = line_of_index info info.out_buffer info.out_max in
            info.start_line   <- line;
            info.start_char   <- char;
            info.middle_index <- 0;
            info.middle_line  <- line;
            info.middle_char  <- char
      end;
   info.out_max      <- 0;
   info.write_index  <- 0;
   info.write_max    <- 0

(************************************************************************
 * Output string buffers.
 *)

(*
 * For string buffers, expand the output instead of
 * flushing.
 *)
let expand_output info =
   let { out_buffer = buffer;
         out_max    = max
       } = info
   in
      if max = String.length buffer then begin
         let buffer2 = String.create (max * 2) in
            String.blit buffer 0 buffer2 0 max;
            info.out_buffer <- buffer2;
            if info.channel_binary then
               info.write_buffer <- buffer2;
      end

let to_string info =
   let { out_buffer = buffer;
         out_max = max
       } = info
   in
      String.sub buffer 0 max

(************************************************************************
 * Flushing and filling.
 *)

(*
 * Flush the input.
 * When output is started, the remaining input is discarded.
 * Update the current position and line number information.
 *)
let flush_input info =
   if info.channel_kind = FileChannel && info.in_max <> 0 then
      reset_input_buffer info

(*
 * Start the write buffer.
 *)
let setup_write_buffer info =
   if info.write_max = 0 then
      if info.channel_binary then
         begin
            info.write_index <- 0;
            info.write_max <- info.out_max
         end
      else
         let wmax = expand_text info.out_buffer info.out_max info.write_buffer in
            info.write_index <- 0;
            info.write_max <- wmax

(*
 * Flush the buffer, but write only once.
 *)
let flush_output_once info =
   setup_write_buffer info;
   let { write_index  = off;
         write_max    = max;
         write_buffer = buf;
         write_fun    = write
       } = info
   in
   let count = write buf off (max - off) in
   let off' = off + count in
      if off' = max then
         reset_output_buffer info
      else
         info.write_index <- off'

(*
 * Flush the buffer.
 *)
let flush_aux info =
   setup_write_buffer info;
   let { write_buffer = buf;
         write_fun    = writer
       } = info
   in

   (* Now write the data directly *)
   let rec write () =
      let { write_index = index;
            write_max = max
          } = info
      in
      let len = max - index in
         if len <> 0 then
            let count = writer buf index len in
               info.write_index <- index + count;
               write ()
   in
      write ();
      reset_output_buffer info

let flush_output info =
   let pid = info.write_pid in
      if pid <> 0 then
         Lm_thread_pool.waitpid pid;
      if info.out_expand then
         expand_output info
      else
         flush_aux info

(*
 * Start an output thread trying to write the data to the pipe.
 *)
let start_output_thread info =
   let pid = info.write_pid in
      if pid = 0 then
         let pid =
            Lm_thread_pool.create false (fun () ->
                  let () =
                     try flush_aux info with
                        Unix.Unix_error _ ->
                           ()
                  in
                     info.write_pid <- 0)
         in
            info.write_pid <- pid

let start_output_threads wfd_pipe =
   List.iter start_output_thread wfd_pipe

(*
 * Flush and close the channel.
 *)
let close info =
   flush_input info;
   let () =
      try flush_output info with
         Unix.Unix_error _ ->
            ()
   in
      match info.channel_fd with
         Some fd ->
            Unix.close fd
       | None ->
            ()

(*
 * Print a byte.
 *)
let rec output_char info c =
   let { out_max = max;
         out_buffer = buffer
       } = info
   in
      flush_input info;
      if max = String.length buffer then
         begin
            flush_output info;
            output_char info c
         end
      else
         begin
            buffer.[max] <- c;
            info.out_max <- succ max
         end

let output_byte info c =
   output_char info (Char.chr c)

(*
 * Write a substring.
 *)
let rec output_buffer info buf off len =
   let { out_max = max;
         out_buffer = buffer
       } = info
   in
   let avail = String.length buffer - max in
      flush_input info;
      if len <> 0 then
         if avail = 0 then
            begin
               flush_output info;
               output_buffer info buf off len
            end
         else
            let amount = min avail len in
               String.blit buf off buffer max amount;
               info.out_max <- max + amount;
               output_buffer info buf (off + amount) (len - amount)

let output_string info buf =
   output_buffer info buf 0 (String.length buf)

(*
 * Write allows for partial writes.
 * This is always in binary mode.
 *)
let write info buf off len =
   flush_input info;
   flush_output info;
   info.write_fun buf off len

(*
 * Check if there is input already in the buffer.
 *)
let poll info =
   let { in_index = index;
         in_max = max
       } = info
   in
      index <> max

(*
 * Get data when the buffer is empty.
 *)
let fillbuf info =
   let { channel_binary = binary;
         in_buffer = buf;
         read_fun = reader
       } = info
   in
   let count = reader buf 0 buf_size in
   let count =
      if count = 0 then
         raise End_of_file;

      if binary then
         count
      else
         let extra =
            (* Read one extra char if we got an unfortunate read *)
            if buf.[pred count] = '\r' then
               reader buf count 1
            else
               0
         in
            squash_text buf 0 (count + extra)
   in
      info.in_index <- 0;
      info.in_max <- count

(*
 * Get a single char.
 *)
let rec input_char info =
   let { in_index = index;
         in_max = max;
         in_buffer = buf;
       } = info
   in
      flush_output info;
      if index = max then
         begin
            fillbuf info;
            input_char info
         end
      else
         let c = buf.[index] in
            info.in_index <- succ index;
            c

(*
 * Translate to an integer.
 *)
let input_byte info =
   Char.code (input_char info)

(*
 * Read data into a buffer.
 *)
let rec input_buffer info s off len =
   let { in_index = index;
         in_max = max;
         in_buffer = buf
       } = info
   in
   let avail = max - index in
      flush_output info;
      if len <> 0 then
         if avail = 0 then
            begin
               fillbuf info;
               input_buffer info s off len
            end
         else
            let amount = min avail len in
            let new_len = len - amount in
            let new_off = off + amount in
               String.blit buf index s off amount;
               info.in_index <- index + amount;
               input_buffer info s new_off new_len

(*
 * Read a line, do not include the line-ending.
 *)
let input_line info =
   let buf = Buffer.create 80 in
   let rec collect () =
      let c = input_char info in
         if c = '\n' then
            Buffer.contents buf
         else
            begin
               Buffer.add_char buf c;
               collect ()
            end
   in
      try collect () with
         End_of_file when Buffer.length buf <> 0 ->
            Buffer.contents buf

(*
 * Read a line, include the line-ending.
 *)
let input_entire_line info =
   let buf = Buffer.create 80 in
   let rec collect () =
      let c = input_char info in
         Buffer.add_char buf c;
         if c = '\n' then
            Buffer.contents buf
         else
            collect ()
   in
      try collect () with
         End_of_file when Buffer.length buf <> 0 ->
            Buffer.contents buf

(*
 * Read allows for partial reading.
 *)
let read info s off len =
   let { in_index = index;
         in_max = max;
         in_buffer = buf;
         read_fun = reader
       } = info
   in
   let avail = max - index in
      flush_output info;
      if avail = 0 then
         reader s off len
      else
         let amount = min len avail in
            String.blit buf index s off amount;
            info.in_index <- index + amount;
            amount

(*
 * Export the flusher.
 *)
let flush = flush_output

(*
 * Positioning.
 * The tell function is unreliable on text files.
 *)
let tell info =
   let pos = Unix.lseek (descr info) 0 Unix.SEEK_CUR in
      if info.out_max <> 0 then
         pos + info.out_max
      else
         pos + info.in_index

let seek info pos whence =
   flush_output info;
   flush_input info;
   Unix.lseek (descr info) pos whence

(*
 * Get the current location.
 *)
let loc info =
   let { out_max = out_max;
         in_index = in_index;
         in_buffer = in_buffer;
         out_buffer = out_buffer;
         channel_file = file
       } = info
   in
   let line, char =
      if out_max <> 0 then
         line_of_index info out_buffer out_max
      else
         line_of_index info in_buffer in_index
   in
      create_loc file line char line char

(************************************************************************
 * Select.
 * Bah, this is tough on Win32.
 *)
let rec classify files pipes sockets fdl =
   match fdl with
      fd :: fdl ->
         let files, pipes, sockets =
            match fd.channel_kind with
               FileChannel ->
                  fd :: files, pipes, sockets
             | PipeChannel ->
                  files, fd :: pipes, sockets
             | SocketChannel ->
                  files, pipes, fd :: sockets
         in
            classify files pipes sockets fdl
    | [] ->
         files, pipes, sockets

(*
 * Find input channels with nonempty buffers.
 *)
let rec find_read_nonempty l rfd =
   match rfd with
      fd :: rfd ->
         let l =
            if fd.in_max <> fd.in_index then
               fd :: l
            else
               l
         in
            find_read_nonempty l rfd
    | [] ->
         l

(*
 * Find output channels with empty buffers.
 *)
let rec find_write_empty l wfd =
   match wfd with
      fd :: wfd ->
         let l =
            if fd.out_max = 0 || fd.out_expand then
               fd :: l
            else
               l
         in
            find_write_empty l wfd
    | [] ->
         l

(*
 * Look at all the input pipes and see if any data is available.
 *)
external peek_pipe : Unix.file_descr -> bool = "omake_shell_peek_pipe"

let rec peek_pipes l pipes =
   match pipes with
      pipe :: pipes ->
         let l =
            if peek_pipe (descr pipe) then
               pipe :: l
            else
               l
         in
            peek_pipes l pipes
    | [] ->
         l

(*
 * Aux function to translate between descriptors and channels.
 *)
let select_aux rfd_sockets wfd_sockets efd_sockets timeout =
   let rfd = List.map descr rfd_sockets in
   let wfd = List.map descr wfd_sockets in
   let efd = List.map descr efd_sockets in
   let rfd, wfd, efd = Unix.select rfd wfd efd timeout in
   let rfd_sockets = List.filter (fun fd -> List.mem (descr fd) rfd) rfd_sockets in
   let wfd_sockets, wrote =
      List.fold_left (fun (wfd_sockets, wrote) fd ->
            if List.mem (descr fd) wfd then
               let wrote =
                  if fd.out_max <> 0 then
                     begin
                        flush_output_once fd;
                        true
                     end
                  else
                     wrote
               in
                  fd :: wfd_sockets, wrote
            else
               wfd_sockets, wrote) ([], false) wfd_sockets
   in
   let efd_sockets = List.filter (fun fd -> List.mem (descr fd) efd) efd_sockets in
      rfd_sockets, wfd_sockets, efd_sockets, wrote

(*
 * Periodically poll to see if something has happened.
 * If necessary, poll interval is 50msec.
 *)
let poll_interval = 0.050

let rec select_poll rfd_pipes rfd_sockets wfd_pipes wfd_sockets efd_sockets expire =
   let wfd_empty = find_write_empty [] wfd_pipes in
      if wfd_empty <> [] then
         [], wfd_empty, []
      else
         (* Peek at the pipes and see if they are ready for reading *)
         let rfd_pipes_ready = peek_pipes [] rfd_pipes in
            if rfd_pipes_ready <> [] then
               rfd_pipes_ready, [], []
            else
               (* Start the writer threads for the pipes *)
               let () = start_output_threads wfd_pipes in

               (* Compute the nest polling interval *)
               let timeout, final_attempt =
                  if rfd_pipes = [] && wfd_pipes = [] then
                     let now = Unix.gettimeofday () in
                     let timeout = expire -. now in
                        max timeout 0.0, true
                  else if expire < 0.0 then
                     poll_interval, false
                  else
                     let now = Unix.gettimeofday () in
                     let timeout = expire -. now in
                        if timeout < poll_interval then
                           max timeout 0.0, true
                        else
                           poll_interval, false
               in

               (* Perform the select on the sockets *)
               let rfd, wfd, efd, wrote = select_aux rfd_sockets wfd_sockets efd_sockets timeout in
                  if rfd <> [] || wfd <> [] || efd <> [] then
                     (* Success with a socket *)
                     rfd, wfd, efd

                  else if final_attempt && not wrote then
                     (* Reached the timeout *)
                     [], [], []

                  else
                     (* Timeout occurred, try again *)
                     select_poll rfd_pipes rfd_sockets wfd_pipes wfd_sockets efd_sockets expire

(*
 * If there are no pipes, just call select directly.
 *)
let select rfd wfd efd timeout =
   let rfd_files, rfd_pipes, rfd_sockets = classify [] [] [] rfd in
   let wfd_files, wfd_pipes, wfd_sockets = classify [] [] [] wfd in
   let efd_files, _efd_pipes, efd_sockets = classify [] [] [] efd in
      if rfd_files <> [] || wfd_files <> [] || efd_files <> [] then
         rfd_files, wfd_files, efd_files
      else
         let rfd_nonempty = find_read_nonempty [] rfd_pipes in
         let rfd_nonempty = find_read_nonempty rfd_nonempty rfd_sockets in
            if rfd_nonempty <> [] then
               rfd_nonempty, [], []
            else
               let expire =
                  if timeout <= 0.0 then
                     timeout
                  else
                     Unix.gettimeofday () +. timeout
               in
                  select_poll rfd_pipes rfd_sockets wfd_pipes wfd_sockets efd_sockets expire

(************************************************************************
 * Lexing functions.
 *
 * When the lexer is working, it needs to buffer *all* the input,
 * so we never throw the input away.
 *)
module LexerInput =
struct
   type t = channel

   (*
    * These are special characters used to identify begin-of-file
    * and end-of-file conditions.
    *)
   let eof = -1
   let bof = -2

   (*
    * Start lex mode.
    *)
   let lex_start channel =
      let { in_index = index;
            in_buffer = buffer
          } = channel
      in
      let prev =
         if index = 0 then
            bof
         else
            Char.code buffer.[pred index]
      in
         channel.lex_index <- channel.in_index;
         prev

   (*
    * Restart at a previous position.
    *)
   let lex_restart channel pos =
      let { in_max = max;
            in_index = index
          } = channel
      in
         assert (pos >= 0 && pos <= max - index);
         channel.lex_index <- index + pos

   (*
    * Stop lexing.
    * The argument is how much data was read in lex mode.
    *)
   let lex_stop channel pos =
      channel.in_index <- channel.in_index + pos;
      assert(channel.in_index <= channel.in_max)

   (*
    * Get the string matched by the lexer.
    *)
   let lex_string channel pos =
      let { in_index = start;
            in_buffer = buffer
          } = channel
      in
         String.sub buffer start pos

   (*
    * Get the string matched by the lexer.
    *)
   let lex_substring channel off len =
      let { in_index = start;
            in_buffer = buffer
          } = channel
      in
         String.sub buffer (start + off) len

   (*
    * Fill the buffer in lex mode.
    * We can't discard any of the existing data.
    *)
   let rec lex_fill channel =
      let { in_max         = max;
            in_buffer      = buffer;
            in_index       = start;
            read_fun       = reader;
            channel_binary = binary
          } = channel
      in
      let len = String.length buffer in
      let amount = len - max in
         (* If we have space, fill it *)
         if amount > 1 then
            let count = reader buffer max (pred amount) in
               if count = 0 then
                  eof
               else
                  let count =
                     if binary then
                        count
                     else
                        let extra =
                           if buffer.[max + count - 1] = '\r' then
                              reader buffer (max + count) 1
                           else
                              0
                        in
                           squash_text buffer max (count + extra)
                  in
                  let c = buffer.[max] in
                     channel.in_max <- max + count;
                     channel.lex_index <- succ max;
                     Char.code c

         (* If we can shift left, do it *)
         else if start <> 0 then
            begin
               shift_input_buffer channel;
               lex_fill channel
            end

         (* If the buffer is already too big, return eof *)
         else if len >= lex_buf_size then
            eof

         (*
          * Otherwise grow it.
          *)
         else
            let new_buffer = String.create (Pervasives.max (len * 2) 32) in
               String.blit buffer 0 new_buffer 0 max;
               channel.in_buffer <- new_buffer;
               lex_fill channel

   (*
    * Get the next character in lex mode.
    *)
   let lex_next channel =
      let { in_max = max;
            in_buffer = buffer;
            lex_index = index
          } = channel
      in
         if index = max then
            lex_fill channel
         else
            let c = buffer.[index] in
               channel.lex_index <- succ index;
               Char.code c

   (*
    * Get the current position in lex mode.
    *)
   let lex_pos channel =
      channel.lex_index - channel.in_index

   (*
    * Get the location of the buffer.
    *)
   let lex_loc channel off =
      let { start_line = line;
            start_char = char;
            channel_file = file;
            lex_index = index;
            in_buffer = buffer;
            in_max = max
          } = channel
      in
      let line1, char1 =
         if index > max then
            line, char
         else
            line_of_index channel buffer index
      in
      let line2, char2 =
         if index + off > max then
            line1, char1
         else
            line_of_index channel buffer (index + off)
      in
         create_loc file line1 char1 line2 char2

   (*
    * Add any remaining buffered text to a buffer.
    *)
   let lex_buffer channel buf =
      let { in_max    = max;
            in_buffer = buffer;
            in_index  = start
          } = channel
      in
         Buffer.add_substring buf buffer start (max - start);
         channel.in_index <- max
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
