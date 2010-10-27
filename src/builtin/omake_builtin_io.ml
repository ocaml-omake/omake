(*
 * Builtin file operations.
 *
 * \begin{doc}
 * \section{IO functions}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_parser
open Lm_location

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_eval
open Omake_node
open Omake_value
open Omake_lexer
open Omake_parser
open Omake_printf
open Omake_symbol
open Omake_builtin
open Omake_value_type
open Omake_value_print
open Omake_builtin_type
open Omake_builtin_util

module Pos = MakePos (struct let name = "Omake_builtin_io" end)
open Pos

(*
 * Table of variables.
 *
 * \begin{doc}
 * \subsection{Standard channels}
 *
 * The following variables define the standard channels.
 *
 * \var{stdin}
 *
 * \begin{verbatim}
 * stdin : InChannel
 * \end{verbatim}
 *
 * The standard input channel, open for reading.
 *
 * \var{stdout}
 * \begin{verbatim}
 * stdout : OutChannel
 * \end{verbatim}
 *
 * The standard output channel, open for writing.
 *
 * \var{stderr}
 * \begin{verbatim}
 * stderr : OutChannel
 * \end{verbatim}
 *
 * The standard error channel, open for writing.
 * \end{doc}
 *)
let builtin_vars =
   ["nl",     (fun _ -> ValString "\n");
    "stdin",  (fun _ -> ValChannel (InChannel,  venv_stdin));
    "stdout", (fun _ -> ValChannel (OutChannel, venv_stdout));
    "stderr", (fun _ -> ValChannel (OutChannel, venv_stderr))]

(*
 * \begin{doc}
 * \fun{open-in-string}
 * The \verb+open-in-string+ treats a string as if it were a file
 * and returns a channel for reading.
 *
 * \begin{verbatim}
 *    $(open-in-string s) : Channel
 *        s : String
 * \end{verbatim}
 * \end{doc}
 *)
let open_in_string venv pos loc args =
   let pos = string_pos "open-in-string" pos in
      match args with
         [arg] ->
            let s = string_of_value venv pos arg in
            let fd = Lm_channel.of_string s in
            let chan = venv_add_channel venv fd in
               ValChannel (Lm_channel.InChannel, chan)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \twofuns{open-out-string}{out-contents}
 * The \verb+open-out-string+ creates a channel that writes to a
 * string instead of a file.  The string may be retrieved with the
 * \verb+out-contents+ function.
 *
 * \begin{verbatim}
 *    $(open-out-string) : Channel
 *    $(out-contents chan) : String
 *        chan : OutChannel
 * \end{verbatim}
 * \end{doc}
 *)
let open_out_string venv pos loc args =
   let pos = string_pos "open-in-string" pos in
      match args with
         [] ->
            let fd = Lm_channel.create_string () in
            let chan = venv_add_channel venv fd in
               ValChannel (Lm_channel.OutChannel, chan)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

let out_contents venv pos loc args =
   let pos = string_pos "out-contents" pos in
      match args with
         [fd] ->
            let outp = prim_channel_of_value venv pos fd in
            let outx = venv_find_channel venv pos outp in
            let s = Lm_channel.to_string outx in
               ValString s
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Open a file.
 *
 * \begin{doc}
 * \fun{fopen}
 *
 * The \verb+fopen+ function opens a file for reading or writing.
 *
 * \begin{verbatim}
 *    $(fopen file, mode) : Channel
 *       file : File
 *       mode : String
 * \end{verbatim}
 *
 * The \verb+file+ is the name of the file to be opened.
 * The \verb+mode+ is a combination of the following characters.
 * \begin{description}
 * \item[r] Open the file for reading; it is an error if the file does not exist.
 * \item[w] Open the file for writing; the file is created if it does not exist.
 * \item[a] Open the file in append mode; the file is created if it does not exist.
 * \item[+] Open the file for both reading and writing.
 * \item[t] Open the file in text mode (default).
 * \item[b] Open the file in binary mode.
 * \item[n] Open the file in nonblocking mode.
 * \item[x] Fail if the file already exists.
 * \end{description}
 *
 * Binary mode is not significant on Unix systems, where
 * text and binary modes are equivalent.
 * \end{doc}
 *)
let read_mode     = 1
let write_mode    = 2
let create_mode   = 4
let append_mode   = 8
let binary_mode   = 16
let text_mode     = 32
let nonblock_mode = 64
let excl_mode     = 128

let fopen_mode pos loc s =
   let len = String.length s in
   let rec collect mode i =
      if i = len then
         mode
      else
         let bit =
         match s.[i] with
            ' '
          | '\t' ->
               mode
          | 'r' ->
               read_mode
          | 'w' ->
               write_mode lor create_mode
          | 'a' ->
               append_mode lor write_mode
          | '+' ->
               read_mode lor write_mode
          | 'b' ->
               binary_mode
          | 't' ->
               text_mode
          | 'n' ->
               nonblock_mode
          | 'x' ->
               excl_mode
          | _ ->
               raise (OmakeException (loc_pos loc pos, StringStringError ("illegal file mode", s)))
         in
         let mode = mode lor bit in
            collect mode (succ i)
   in
   let mode = collect 0 0 in
   let () =
      if (mode land text_mode) <> 0 && (mode land binary_mode) <> 0 then
         raise (OmakeException (loc_pos loc pos, StringStringError ("can't specify both text and binary modes", s)))
   in
   let opt =
      if (mode land append_mode) <> 0 then
         [Unix.O_APPEND; Unix.O_CREAT]
      else if (mode land create_mode) <> 0 then
         [Unix.O_CREAT; Unix.O_TRUNC]
      else
         []
   in
   let kind, opt =
      if (mode land read_mode) <> 0 && (mode land write_mode) <> 0 then
         InOutChannel, Unix.O_RDWR :: opt
      else if (mode land write_mode) <> 0 then
         OutChannel, Unix.O_WRONLY :: opt
      else
         InChannel, Unix.O_RDONLY :: opt
   in
   let opt =
      if (mode land excl_mode) <> 0 then
         Unix.O_EXCL :: opt
      else
         opt
   in
   let opt =
      if (mode land nonblock_mode) <> 0 then
         Unix.O_NONBLOCK :: opt
      else
         opt
   in
      kind, (mode land binary_mode) <> 0, opt

let fopen venv pos loc args =
   let pos = string_pos "fopen" pos in
      match args with
         [node; flags] ->
            let name = filename_of_value venv pos node in
            let kind, binary, flags = fopen_mode pos loc (string_of_value venv pos flags) in
            let fd =
               try Lm_unix_util.openfile name flags 0o666 with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (loc_pos loc pos, exn))
            in
            let chan = Lm_channel.create name Lm_channel.FileChannel kind binary (Some fd) in
               ValChannel (kind, venv_add_channel venv chan)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Closing file descriptors.
 *
 * \begin{doc}
 * \fun{close}
 *
 * \begin{verbatim}
 *     $(close channel...)
 *        channel : Channel
 * \end{verbatim}
 *
 * The \verb+close+ function closes a file that was previously opened
 * with \verb+fopen+.
 * \end{doc}
 *)
let close venv pos loc args =
   let pos = string_pos "close" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
               List.iter (fun arg ->
                     match eval_prim_value venv pos arg with
                        ValChannel (_, channel) ->
                           venv_close_channel venv pos channel
                      | arg ->
                           raise (OmakeException (loc_pos loc pos, StringValueError ("not a file descriptor", arg)))) args;
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \twofuns{read}{input-line}
 *
 * \begin{verbatim}
 *    $(read channel, amount) : String
 *    $(input-line channel) : String
 *       channel : InChannel
 *       amount  : Int
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+read+ function reads up to \verb+amount+
 * bytes from an input channel, and returns
 * the data that was read. The \verb+input-line+ function reads a line from the file and returns the line read, without
 * the line terminator. If an end-of-file condition is reached, both functions raise a \verb+RuntimeException+
 * exception.
 * \end{doc}
 *)
let read venv pos loc args =
   let pos = string_pos "read" pos in
      match args with
         [fd; amount] ->
            let fd = channel_of_value venv pos fd in
            let amount = int_of_value venv pos amount in
            let s = String.make amount '\000' in
            let count =
               try Lm_channel.read fd s 0 amount with
                  Sys_error _
                | Invalid_argument _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               if count = amount then
                  ValData s
               else if count = 0 then
                  raise (UncaughtException (pos, End_of_file))
               else
                  ValData (String.sub s 0 count)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let input_line venv pos loc args =
   let pos = string_pos "input-line" pos in
      match args with
         [fd] ->
            let fd = channel_of_value venv pos fd in
            let s =
               try Lm_channel.input_line fd with
                  Sys_error _
                | End_of_file
                | Invalid_argument _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
                  ValData s
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{write}
 *
 * \begin{verbatim}
 *    $(write channel, buffer, offset, amount) : String
 *       channel : OutChannel
 *       buffer  : String
 *       offset  : Int
 *       amount  : Int
 *    $(write channel, buffer) : String
 *       channel : OutChannel
 *       buffer  : String
 *    raises RuntimeException
 * \end{verbatim}
 *
 * In the 4-argument form, the \verb+write+ function writes
 * bytes to the output channel \verb+channel+ from the \verb+buffer+,
 * starting at position \verb+offset+.  Up to \verb+amount+ bytes
 * are written.  The function returns the number of bytes that were
 * written.
 *
 * The 3-argument form is similar, but the \verb+offset+ is 0.
 *
 * In the 2-argument form, the \verb+offset+ is 0, and the \verb+amount+
 * if the length of the \verb+buffer+.
 *
 * If an end-of-file condition is reached,
 * the function raises a \verb+RuntimeException+ exception.
 * \end{doc}
 *)
let write venv pos loc args =
   let pos = string_pos "read" pos in
   let fd, buf, off, len =
      match args with
         [fd; buf] ->
            fd, buf, None, None
       | [fd; buf; len] ->
            fd, buf, None, Some len
       | [fd; buf; off; len] ->
            fd, buf, Some off, Some len
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (2, 4), List.length args)))
   in
   let fd = channel_of_value venv pos fd in
   let buf = string_of_value venv pos buf in
   let off =
      match off with
         Some off ->
            int_of_value venv pos off
       | None ->
            0
   in
   let len =
      match len with
         Some len ->
            int_of_value venv pos len
       | None ->
            String.length buf
   in
   let count =
      try Lm_channel.write fd buf off len with
         Sys_error _
       | Invalid_argument _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValInt count

(*
 * \begin{doc}
 * \fun{lseek}
 *
 * \begin{verbatim}
 *     $(lseek channel, offset, whence) : Int
 *        channel : Channel
 *        offset  : Int
 *        whence  : String
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+lseek+ function repositions the offset of the
 * channel \verb+channel+ according to the \verb+whence+ directive, as
 * follows:
 *
 * \begin{description}
 * \item[SEEK\_SET] The offset is set to \verb+offset+.
 * \item[SEEK\_CUR] The offset is set to its current position plus \verb+offset+ bytes.
 * \item[SEEK\_END] The offset is set to the size of the file plus \verb+offset+ bytes.
 * \end{description}
 *
 * The \verb+lseek+ function returns the new position in the file.
 * \end{doc}
 *)
let lseek venv pos loc args =
   let pos = string_pos "lseek" pos in
      match args with
         [fd; off; whence] ->
            let fd = channel_of_value venv pos fd in
            let off = int_of_value venv pos off in
            let whence =
               match String.uppercase (string_of_value venv pos whence) with
                  "SET" | "SEEK_SET" ->
                     Unix.SEEK_SET
                | "CUR" | "CURRENT" | "SEEK_CUR" ->
                     Unix.SEEK_CUR
                | "END" | "SEEK_END" ->
                     Unix.SEEK_END
                | whence ->
                     raise (OmakeException (loc_pos loc pos, StringStringError ("illegal lseek parameter", whence)))
            in
            let off =
               try Lm_channel.seek fd off whence with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValInt off
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))

(*
 * \begin{doc}
 * \fun{rewind}
 *
 * \begin{verbatim}
 *    rewind(channel...)
 *       channel : Channel
 * \end{verbatim}
 *
 * The \verb+rewind+ function set the current file position to the
 * beginning of the file.
 * \end{doc}
 *)
let rewind venv pos loc args =
   let pos = string_pos "rewind" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let () =
               try
                  List.iter (fun arg ->
                        let fd = channel_of_value venv pos arg in
                           ignore (Lm_channel.seek fd 0 Unix.SEEK_SET)) args
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{tell}
 *
 * \begin{verbatim}
 *     $(tell channel...) : Int...
 *        channel : Channel
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+tell+ function returns the current position of the \verb+channel+.
 * \end{doc}
 *)
let tell venv pos loc args =
   let pos = string_pos "tell" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let args =
               try
                  List.map (fun arg ->
                        let fd = channel_of_value venv pos arg in
                           ValInt (Lm_channel.tell fd)) args
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Flush an output channel.
 *
 * \begin{doc}
 * \fun{flush}
 *
 * \begin{verbatim}
 *    $(flush channel...)
 *       channel : OutChannel
 * \end{verbatim}
 *
 * The \verb+flush+ function can be used only on files that are open for writing.
 * It flushes all pending data to the file.
 * \end{doc}
 *)
let flush venv pos loc args =
   let pos = string_pos "flush" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
               List.iter (fun s ->
                     let fd = channel_of_value venv pos s in
                        Lm_channel.flush fd) args;
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{channel-name}
 *
 * \begin{verbatim}
 *    $(channel-name channel...) : String
 *       channel : Channel
 * \end{verbatim}
 *
 * The \verb+channel-name+ function returns the name that is associated with the channel.
 * \end{doc}
 *)
let channel_name venv pos loc args =
   let pos = string_pos "channel-name" pos in
      match args with
         [arg] ->
            let fd = channel_of_value venv pos arg in
               ValData (Lm_channel.name fd)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{dup}
 *
 * \begin{verbatim}
 *     $(dup channel) : Channel
 *        channel : Channel
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+dup+ function returns a new channel referencing the
 * same file as the argument.
 * \end{doc}
 *)
let dup venv pos loc args =
   let pos = string_pos "dup" pos in
      match args with
         [arg] ->
            let channel = channel_of_value venv pos arg in
            let name = Lm_channel.name channel in
            let fd = Lm_channel.descr channel in
            let _, kind, mode, binary = Lm_channel.info channel in
            let fd =
               try Unix.dup fd with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
            let chan = Lm_channel.create name kind mode binary (Some fd) in
            let channel = venv_add_channel venv chan in
               ValChannel (mode, channel)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{dup2}
 *
 * \begin{verbatim}
 *    dup2(channel1, channel2)
 *       channel1 : Channel
 *       channel2 : Channel
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+dup2+ function causes \verb+channel2+ to refer to the same
 * file as \verb+channel1+.
 * \end{doc}
 *)
let dup2 venv pos loc args =
   let pos = string_pos "dup2" pos in
      match args with
         [arg1; arg2] ->
            let channel1 = channel_of_value venv pos arg1 in
            let channel2 = channel_of_value venv pos arg2 in
            let fd1 = Lm_channel.descr channel1 in
            let fd2 = Lm_channel.descr channel2 in
            let () =
               try Unix.dup2 fd1 fd2 with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{set-nonblock}
 *
 * \begin{verbatim}
 *    set-nonblock-mode(mode, channel...)
 *       channel : Channel
 *       mode : String
 * \end{verbatim}
 *
 * The \verb+set-nonblock-mode+ function sets the nonblocking flag on the
 * given channel.  When IO is performed on the channel, and the operation
 * cannot be completed immediately, the operations raises a \verb+RuntimeException+.
 * \end{doc}
 *)
let set_nonblock_mode venv pos loc args =
   let pos = string_pos "set_nonblock_mode" pos in
      match args with
         [mode; channel] ->
            let set_mode =
               if bool_of_value venv pos mode then
                  Unix.set_nonblock
               else
                  Unix.clear_nonblock
            in
            let channels = values_of_value venv pos channel in
            let () =
               try
                  List.iter (fun channel ->
                        let channel = channel_of_value venv pos channel in
                        let fd = Lm_channel.descr channel in
                           set_mode fd) channels
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{set-close-on-exec-mode}
 *
 * \begin{verbatim}
 *    set-close-on-exec-mode(mode, channel...)
 *       channel : Channel
 *       mode : String
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+set-close-on-exec-mode+ function sets the close-on-exec
 * flags for the given channels.  If the close-on-exec flag is set, the channel
 * is not inherited by child processes.  Otherwise it is.
 * \end{doc}
 *)
let set_close_on_exec_mode venv pos loc args =
   let pos = string_pos "set-close-on-exec-mode" pos in
      match args with
         [mode; channel] ->
            let set_mode =
               if bool_of_value venv pos mode then
                  Unix.set_close_on_exec
               else
                  Unix.clear_close_on_exec
            in
            let channels = values_of_value venv pos channel in
            let () =
               try
                  List.iter (fun channel ->
                        let channel = channel_of_value venv pos channel in
                        let fd = Lm_channel.descr channel in
                           set_mode fd) channels
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{pipe}
 *
 * \begin{verbatim}
 *    $(pipe) : Pipe
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+pipe+ function creates a \verb+Pipe+ object, which has two
 * fields.  The \verb+read+ field is a channel that is opened for
 * reading, and the \verb+write+ field is a channel that is opened
 * for writing.
 * \end{doc}
 *)
let pipe venv pos loc args =
   let pos = string_pos "pipe" pos in
      match args with
         [] ->
            let fd_read, fd_write =
               try Unix.pipe () with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
            let read = Lm_channel.create "<readpipe>" Lm_channel.PipeChannel InChannel false (Some fd_read) in
            let write = Lm_channel.create "<writepipe>" Lm_channel.PipeChannel OutChannel false (Some fd_write) in
            let fd_read = ValChannel (InChannel, venv_add_channel venv read) in
            let fd_write = ValChannel (OutChannel, venv_add_channel venv write) in
            let obj = venv_find_object_or_empty venv pipe_object_var in
            let obj = venv_add_field_internal obj read_sym fd_read in
            let obj = venv_add_field_internal obj write_sym fd_write in
               ValObject obj
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(*
 * \begin{doc}
 * \fun{mkfifo}
 *
 * \begin{verbatim}
 *    mkfifo(mode, node...)
 *       mode : Int
 *       node : Node
 * \end{verbatim}
 *
 * The \verb+mkfifo+ function creates a named pipe.
 * \end{doc}
 *)
let mkfifo venv pos loc args =
   let pos = string_pos "mkfifo" pos in
      match args with
         [mode; nodes] ->
            let mode = int_of_value venv pos mode in
            let nodes = values_of_value venv pos nodes in
            let () =
               try
                  List.iter (fun node ->
                        Unix.mkfifo (filename_of_value venv pos node) mode) nodes
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{select}
 *
 * \begin{verbatim}
 *    $(select rfd..., wfd..., wfd..., timeout) : Select
 *       rfd : InChannel
 *       wfd : OutChannel
 *       efd : Channel
 *       timeout : float
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+select+ function polls for possible IO on a set of channels.
 * The \verb+rfd+ are a sequence of channels for reading, \verb+wfd+ are a
 * sequence of channels for writing, and \verb+efd+ are a sequence of
 * channels to poll for error conditions.  The \verb+timeout+ specifies
 * the maximum amount of time to wait for events.
 *
 * On successful return, \verb+select+ returns a \verb+Select+ object,
 * which has the following fields:
 * \begin{description}
 * \item[read] An array of channels available for reading.
 * \item[write] An array of channels available for writing.
 * \item[error] An array of channels on which an error has occurred.
 * \end{description}
 * \end{doc}
 *)
let select venv pos loc args =
   let pos = string_pos "select" pos in
      match args with
         [rfd; wfd; efd; timeout] ->
            let rfd = values_of_value venv pos rfd in
            let wfd = values_of_value venv pos wfd in
            let efd = values_of_value venv pos efd in
            let rfd = List.map (channel_of_value venv pos) rfd in
            let wfd = List.map (channel_of_value venv pos) wfd in
            let efd = List.map (channel_of_value venv pos) efd in
            let timeout = float_of_value venv pos timeout in
            let rfd, wfd, efd =
               try Lm_channel.select rfd wfd efd timeout with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
            let reintern_channel fdl =
               List.map (fun fd ->
                     let fd = venv_find_channel_by_channel venv pos fd in
                     let channel = venv_find_channel venv pos fd in
                     let _, _, mode, _ = Lm_channel.info channel in
                        ValChannel (mode, fd)) fdl
            in
            let rfd = reintern_channel rfd in
            let wfd = reintern_channel wfd in
            let efd = reintern_channel efd in
            let obj = venv_find_object_or_empty venv select_object_var in
            let obj = venv_add_field_internal obj read_sym  (ValArray rfd) in
            let obj = venv_add_field_internal obj write_sym (ValArray wfd) in
            let obj = venv_add_field_internal obj error_sym (ValArray efd) in
               ValObject obj
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 4, List.length args)))

(*
 * \begin{doc}
 * \fun{lockf}
 *
 * \begin{verbatim}
 *     lockf(channel, command, len)
 *        channel : Channel
 *        command : String
 *        len : Int
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+lockf+ function places a lock on a region of the channel.
 * The region starts at the current position and extends for \verb+len+
 * bytes.
 *
 * The possible values for \verb+command+ are the following.
 * \begin{description}
 * \item[F\_ULOCK] Unlock a region.
 * \item[F\_LOCK] Lock a region for writing; block if already locked.
 * \item[F\_TLOCK] Lock a region for writing; fail if already locked.
 * \item[F\_TEST] Test a region for other locks.
 * \item[F\_RLOCK] Lock a region for reading; block if already locked.
 * \item[F\_TRLOCK] Lock a region for reading; fail is already locked.
 * \end{description}
 * \end{doc}
 *)
let lockf venv pos loc args =
   let pos = string_pos "lockf" pos in
      match args with
         [channel; command; len] ->
            let channel = channel_of_value venv pos channel in
            let command = string_of_value venv pos command in
            let len = int_of_value venv pos len in
            let command =
               match command with
                  "F_ULOCK" -> Unix.F_ULOCK
                | "F_LOCK"  -> Unix.F_LOCK
                | "F_TLOCK" -> Unix.F_TLOCK
                | "F_TEST"  -> Unix.F_TEST
                | "F_RLOCK" -> Unix.F_RLOCK
                | "F_TRLOCK" -> Unix.F_TRLOCK
                | _ ->
                     raise (OmakeException (loc_pos loc pos, StringStringError ("lockf: illegal command", command)))
            in
            let fd = Lm_channel.descr channel in
            let () =
               try Unix.lockf fd command len with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))

(************************************************************************
 * Databases.
 *)

let addr_of_value venv pos arg =
   let host = string_of_value venv pos arg in
      try Unix.inet_addr_of_string host with
         Failure _ ->
            let entry = Unix.gethostbyname host in
               entry.Unix.h_addr_list.(0)

let proto_of_value venv pos arg =
   let proto = string_of_value venv pos arg in
      try Unix.getprotobynumber (int_of_string proto) with
         Failure _ ->
            Unix.getprotobyname proto

(*
(*
 * \begin{doc}
 * \obj{InetAddr}
 *
 * The \verb+InetAddr+ object describes an Internet address.
 * It contains the following fields.
 *
 * \begin{description}
 * \item[addr] \verb+String+: the Internet address.
 * \item[port] \verb+Int+: the port number.
 * \end{description}
 *
 * \obj{Host}
 *
 * A \verb+Host+ object contains the following fields.
 *
 * \begin{description}
 * \item[name] \verb+String+: the name of the host.
 * \item[aliases] \verb+String Array+: other names by which the host is known.
 * \item[addrtype] \verb+String+: the preferred socket domain.
 * \item[addrs] \verb+InetAddr Array+: an array of Internet addresses belonging to the host.
 * \end{description}
 *
 * \fun{gethostbyname}
 *
 * \begin{verbatim}
 *    $(gethostbyname host...) : Host...
 *       host : String
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+gethostbyname+ function returns a \verb+Host+ object
 * for the specified host.  The \verb+host+ may specify a domain name
 * or an Internet address.
 *
 * \end{doc}
 *)
let gethostbyname venv pos loc args =
   let pos = string_pos "gethostbyname" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let args =
               try
                  List.map (fun arg ->
                        let host = string_of_value venv pos arg in
                        let entry =
                           try
                              let addr = Unix.inet_addr_of_string host in
                                 Unix.gethostbyaddr addr
                           with
                              Failure _ ->
                                 Unix.gethostbyname host
                        in
                           make_host_entry venv pos entry) args
               with
                  Not_found
                | Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \obj{Protocol}
 *
 * The \verb+Protocol+ object represents a protocol entry.
 * It has the following fields.
 *
 * \begin{description}
 * \item[name] \verb+String+: the canonical name of the protocol.
 * \item[aliases] \verb+String Array+: aliases for the protocol.
 * \item[proto] \verb+Int+: the protocol number.
 * \end{description}
 *
 * \fun{getprotobyname}
 *
 * \begin{verbatim}
 *    $(getprotobyname name...) : Protocol...
 *       name : Int or String
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+getprotobyname+ function returns a \verb+Protocol+ object for the
 * specified protocol.  The \verb+name+ may be a protocol name, or a
 * protocol number.
 * \end{doc}
 *)
let getprotobyname venv pos loc args =
   let pos = string_pos "getprotobyname" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let args =
               try
                  List.map (fun arg ->
                        let proto = string_of_value venv pos arg in
                        let entry =
                           try Unix.getprotobynumber (int_of_string proto) with
                              Failure _ ->
                                 Unix.getprotobyname proto
                        in
                           make_proto_entry venv pos entry) args
               with
                  Not_found
                | Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \obj{Service}
 *
 * The \verb+Service+ object represents a network service.
 * It has the following fields.
 *
 * \begin{description}
 * \item[name] \verb+String+: the name of the service.
 * \item[aliases] \verb+String Array+: aliases for the service.
 * \item[port] \verb+Int+: the port number of the service.
 * \item[proto] \verb+Protocol+: the protocol for the service.
 * \end{description}
 *
 * \fun{getservbyname}
 *
 * \begin{verbatim}
 *    $(getservbyname service...) : Service...
 *       service : String or Int
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+getservbyname+ function gets the information for a network service.
 * The \verb+service+ may be specified as a service name or number.
 * \end{doc}
 *)
let getprotobyname venv pos loc args =
   let pos = string_pos "getprotobyname" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let args =
               try
                  List.map (fun arg ->
                        let proto = string_of_value venv pos arg in
                        let entry =
                           try Unix.getservbyprt (int_of_string proto) with
                              Failure _ ->
                                 Unix.getservbyname proto
                        in
                           make_serv_entry venv pos entry) args
               with
                  Not_found
                | Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
*)

(*
 * \begin{doc}
 * \fun{socket}
 *
 * \begin{verbatim}
 *    $(socket domain, type, protocol) : Channel
 *       domain : String
 *       type : String
 *       protocol : String
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+socket+ function creates an unbound socket.
 *
 * The possible values for the arguments are as follows.
 *
 * The \verb+domain+ may have the following values.
 * \begin{description}
 * \item[PF\_UNIX or unix] Unix domain, available only on Unix systems.
 * \item[PF\_INET or inet] Internet domain, IPv4.
 * \item[PF\_INET6 or inet6] Internet domain, IPv6.
 * \end{description}
 *
 * The \verb+type+ may have the following values.
 * \begin{description}
 * \item[SOCK\_STREAM or stream] Stream socket.
 * \item[SOCK\_DGRAM or dgram] Datagram socket.
 * \item[SOCK\_RAW or raw] Raw socket.
 * \item[SOCK\_SEQPACKET or seqpacket] Sequenced packets socket
 * \end{description}
 *
 * The \verb+protocol+ is an \verb+Int+ or \verb+String+ that specifies
 * a protocol in the protocols database.
 * \end{doc}
 *)
let socket venv pos loc args =
   let pos = string_pos "socket" pos in
      match args with
         [domain; ty; proto] ->
            let domain =
               match String.uppercase (string_of_value venv pos domain) with
                  "PF_UNIX"
                | "UNIX" ->
                     Unix.PF_UNIX
                | "PF_INET"
                | "INET"
                | "IP" ->
                     Unix.PF_INET

                  (* If you are compiling with OCaml-3.07 or earlier, comment out these lines *)
                | "PF_INET6"
                | "INET6"
                | "IP6" ->
                     Unix.PF_INET6
                | domain ->
                     raise (OmakeException (loc_pos loc pos, StringStringError ("bad domain", domain)))
            in
            let ty =
               match String.uppercase (string_of_value venv pos ty) with
                  "SOCK_STREAM"
                | "STREAM" ->
                     Unix.SOCK_STREAM
                | "SOCK_DGRAM"
                | "DGRAM" ->
                     Unix.SOCK_DGRAM
                | "SOCK_RAW"
                | "RAW" ->
                     Unix.SOCK_RAW
                | "SOCK_SEQPACKET"
                | "SEQPACKET" ->
                     Unix.SOCK_SEQPACKET
                | ty ->
                     raise (OmakeException (loc_pos loc pos, StringStringError ("bad type", ty)))
            in
            let proto = proto_of_value venv pos proto in
            let socket =
               try Unix.socket domain ty proto.Unix.p_proto with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
            let channel = Lm_channel.create "<socket>" Lm_channel.SocketChannel Lm_channel.InOutChannel false (Some socket) in
            let channel = venv_add_channel venv channel in
               ValChannel (InOutChannel, channel)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))

(*
 * \begin{doc}
 * \fun{bind}
 *
 * \begin{verbatim}
 *    bind(socket, host, port)
 *       socket : InOutChannel
 *       host : String
 *       port : Int
 *    bind(socket, file)
 *       socket : InOutChannel
 *       file : File
 *    raise RuntimeException
 * \end{verbatim}
 *
 * The \verb+bind+ function binds a socket to an address.
 *
 * The 3-argument form specifies an Internet connection, the \verb+host+ specifies a host name
 * or IP address, and the \verb+port+ is a port number.
 *
 * The 2-argument form is for \verb+Unix+ sockets.  The \verb+file+ specifies the filename
 * for the address.
 * \end{doc}
 *)
let bind venv pos loc args =
   let pos = string_pos "bind" pos in
   let socket, addr =
      match args with
         [socket; host; port] ->
            let host = addr_of_value venv pos host in
            let port = int_of_value venv pos port in
            let addr = Unix.ADDR_INET (host, port) in
               socket, addr
       | [socket; name] ->
            let name = filename_of_value venv pos name in
            let addr = Unix.ADDR_UNIX name in
               socket, addr
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (2, 3), List.length args)))
   in
   let socket = channel_of_value venv pos socket in
   let socket = Lm_channel.descr socket in
   let () =
      try Unix.bind socket addr with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

(*
 * \begin{doc}
 * \fun{listen}
 *
 * \begin{verbatim}
 *    listen(socket, requests)
 *       socket : InOutChannel
 *       requests : Int
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+listen+ function sets up the socket for receiving up to \verb+requests+ number
 * of pending connection requests.
 * \end{doc}
 *)
let listen venv pos loc args =
   let pos = string_pos "listen" pos in
      match args with
         [socket; requests] ->
            let socket = channel_of_value venv pos socket in
            let socket = Lm_channel.descr socket in
            let requests = int_of_value venv pos requests in
            let () =
               try Unix.listen socket requests with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \fun{accept}
 *
 * \begin{verbatim}
 *    $(accept socket) : InOutChannel
 *       socket : InOutChannel
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+accept+ function accepts a connection on a socket.
 * \end{doc}
 *)
let accept venv pos loc args =
   let pos = string_pos "accept" pos in
      match args with
         [socket] ->
            let socket = channel_of_value venv pos socket in
            let socket = Lm_channel.descr socket in
            let socket, _ =
               try Unix.accept socket with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
            let channel = Lm_channel.create "<socket>" Lm_channel.SocketChannel Lm_channel.InOutChannel false (Some socket) in
            let channel = venv_add_channel venv channel in
               ValChannel (InOutChannel, channel)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{connect}
 *
 * \begin{verbatim}
 *     connect(socket, addr, port)
 *        socket : InOutChannel
 *        addr : String
 *        port : int
 *     connect(socket, name)
 *        socket : InOutChannel
 *        name : File
 *     raise RuntimeException
 * \end{verbatim}
 *
 * The \verb+connect+ function connects a socket to a remote address.
 *
 * The 3-argument form specifies an Internet connection.
 * The \verb+addr+ argument is the Internet address of the remote host,
 * specified as a domain name or IP address.  The \verb+port+ argument
 * is the port number.
 *
 * The 2-argument form is for Unix sockets.  The \verb+name+ argument
 * is the filename of the socket.
 * \end{doc}
 *)
let connect venv pos loc args =
   let pos = string_pos "connect" pos in
   let socket, addr =
      match args with
         [socket; host; port] ->
            let host = addr_of_value venv pos host in
            let port = int_of_value venv pos port in
            let addr = Unix.ADDR_INET (host, port) in
               socket, addr
       | [socket; name] ->
            let name = filename_of_value venv pos name in
            let addr = Unix.ADDR_UNIX name in
               socket, addr
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (2, 3), List.length args)))
   in
   let socket = channel_of_value venv pos socket in
   let socket = Lm_channel.descr socket in
   let () =
      try Unix.connect socket addr with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

(************************************************************************
 * Buffered IO.
 *)

(*
 * Get the next character.
 *
 * \begin{doc}
 * \fun{getchar}
 *
 * \begin{verbatim}
 *     $(getc) : String
 *     $(getc file) : String
 *        file : InChannel or File
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+getc+ function returns the next character of a file.
 * If the argument is not specified, \verb+stdin+ is used as input.
 * If the end of file has been reached, the function returns \verb+false+.
 * \end{doc}
 *)
let getc venv pos loc args =
   let pos = string_pos "getc" pos in
   let arg =
      match args with
         [] ->
            venv_find_var venv pos loc stdin_var
       | [arg] ->
            arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (0, 1), List.length args)))
   in
   let inp, close_flag = in_channel_of_any_value venv pos arg in
   let inx = venv_find_channel venv pos inp in
   let s =
      try String.make 1 (Lm_channel.input_char inx) with
         End_of_file ->
            "false"
   in
      if close_flag then
         venv_close_channel venv pos inp;
      ValData s


(*
 * Get the next line.
 *
 * \begin{doc}
 * \fun{gets}
 *
 * \begin{verbatim}
 *    $(gets) : String
 *    $(gets channel) : String
 *       channel : InChannel or File
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+gets+ function returns the next line from a file.
 * The function returns the empty string if the end of file has been reached.
 * The line terminator is removed.
 * \end{doc}
 *)
let gets venv pos loc args =
   let pos = string_pos "gets" pos in
   let arg =
      match args with
         [] ->
            venv_find_var venv pos loc stdin_var
       | [arg] ->
            arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (0, 1), List.length args)))
   in
   let inp, close_flag = in_channel_of_any_value venv pos arg in
   let inx = venv_find_channel venv pos inp in
   let s =
      try Lm_channel.input_line inx with
         End_of_file ->
            ""
   in
      if close_flag then
         venv_close_channel venv pos inp;
      ValString s

(*
 * Get the next line.
 *
 * \begin{doc}
 * \fun{fgets}
 *
 * \begin{verbatim}
 *    $(fgets) : String
 *    $(fgets channel) : String
 *       channel : InChannel or File
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+fgets+ function returns the next line from a file that has been
 * opened for reading with \verb+fopen+.  The function returns the empty string
 * if the end of file has been reached.  The returned string is returned as
 * literal data.  The line terminator is not removed.
 * \end{doc}
 *)
let fgets venv pos loc args =
   let pos = string_pos "fgets" pos in
   let arg =
      match args with
         [] ->
            venv_find_var venv pos loc stdin_var
       | [arg] ->
            arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (0, 1), List.length args)))
   in
   let inp, close_flag = in_channel_of_any_value venv pos arg in
   let inx = venv_find_channel venv pos inp in
   let s =
      try Lm_channel.input_entire_line inx with
         End_of_file ->
            ""
   in
      if close_flag then
         venv_close_channel venv pos inp;
      ValData s

(*
 * \begin{doc}
 * \section{Printing functions}
 * \funref{fprint}
 * \funref{print}
 * \funref{eprint}
 * \funref{fprintln}
 * \funref{println}
 * \funref{eprintln}
 *
 * Output is printed with the \verb+print+ and \verb+println+ functions.
 * The \verb+println+ function adds a terminating newline to the value being
 * printed, the \verb+print+ function does not.
 *
 * \begin{verbatim}
 *     fprint(<file>, <string>)
 *     print(<string>)
 *     eprint(<string>)
 *     fprintln(<file>, <string>)
 *     println(<string>)
 *     eprintln(<string>)
 * \end{verbatim}
 *
 * The \verb+fprint+ functions print to a file that has been previously opened with
 * \verb+fopen+.  The \verb+print+ functions print to the standard output channel, and
 * the \verb+eprint+ functions print to the standard error channel.
 * \end{doc}
 *)
let print_aux venv pos loc nl args =
   match args with
      [fd; s] ->
         let outp, close_flag = out_channel_of_any_value venv pos fd in
         let outx = venv_find_channel venv pos outp in
         let s = string_of_value venv pos s in
            Lm_channel.output_string outx s;
            Lm_channel.output_string outx nl;
            Lm_channel.flush outx;
            if close_flag then
               venv_close_channel venv pos outp;
            ValNone
    | _ ->
         raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let fprint venv pos loc args =
   let pos = string_pos "fprint" pos in
      print_aux venv pos loc "" args

let print venv pos loc args =
   let pos = string_pos "print" pos in
   let stdout_fd = venv_find_var venv pos loc stdout_var in
      fprint venv pos loc (stdout_fd :: args)

let eprint venv pos loc args =
   let pos = string_pos "eprint" pos in
   let stderr_fd = venv_find_var venv pos loc stderr_var in
      fprint venv pos loc (stderr_fd :: args)

let fprintln venv pos loc args =
   let pos = string_pos "fprintln" pos in
      print_aux venv pos loc "\n" args

let println venv pos loc args =
   let pos = string_pos "println" pos in
   let stdout_fd = venv_find_var venv pos loc stdout_var in
     fprintln venv pos loc (stdout_fd :: args)

let eprintln venv pos loc args =
   let pos = string_pos "eprintln" pos in
   let stderr_fd = venv_find_var venv pos loc stderr_var in
      fprintln venv pos loc (stderr_fd :: args)

(*
 * \begin{doc}
 * \section{Value printing functions}
 * \funref{fprintv}
 * \funref{printv}
 * \funref{eprintv}
 * \funref{fprintvln}
 * \funref{printvln}
 * \funref{eprintvln}
 *
 * Values can be printed with the \verb+printv+ and \verb+printvln+ functions.
 * The \verb+printvln+ function adds a terminating newline to the value being
 * printed, the \verb+printv+ function does not.
 *
 * \begin{verbatim}
 *     fprintv(<file>, <string>)
 *     printv(<string>)
 *     eprintv(<string>)
 *     fprintvln(<file>, <string>)
 *     printvln(<string>)
 *     eprintvln(<string>)
 * \end{verbatim}
 *
 * The \verb+fprintv+ functions print to a file that has been previously opened with
 * \verb+fopen+.  The \verb+printv+ functions print to the standard output channel, and
 * the \verb+eprintv+ functions print to the standard error channel.
 * \end{doc}
 *)
let printv_aux venv pos loc nl args =
   match args with
      [fd; s] ->
         let outp, close_flag = out_channel_of_any_value venv pos fd in
         let outx = venv_find_channel venv pos outp in
         let s =
            pp_print_value stdstr s;
            flush_stdstr ()
         in
            Lm_channel.output_string outx s;
            Lm_channel.output_string outx nl;
            Lm_channel.flush outx;
            if close_flag then
               venv_close_channel venv pos outp;
            ValNone
    | _ ->
         raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let fprintv venv pos loc args =
   let pos = string_pos "fprintv" pos in
      printv_aux venv pos loc "" args

let printv venv pos loc args =
   let pos = string_pos "printv" pos in
   let stdout_fd = venv_find_var venv pos loc stdout_var in
      fprintv venv pos loc (stdout_fd :: args)

let eprintv venv pos loc args =
   let pos = string_pos "eprintv" pos in
   let stderr_fd = venv_find_var venv pos loc stderr_var in
      fprintv venv pos loc (stderr_fd :: args)

let fprintvln venv pos loc args =
   let pos = string_pos "fprintvln" pos in
      printv_aux venv pos loc "\n" args

let printvln venv pos loc args =
   let pos = string_pos "printvln" pos in
   let stdout_fd = venv_find_var venv pos loc stdout_var in
     fprintvln venv pos loc (stdout_fd :: args)

let eprintvln venv pos loc args =
   let pos = string_pos "eprintvln" pos in
   let stderr_fd = venv_find_var venv pos loc stderr_var in
      fprintvln venv pos loc (stderr_fd :: args)

(************************************************************************
 * Printf.
 *)
module Args =
struct
   type t =
      { print_venv    : venv;
        print_pos     : pos;
        print_loc     : loc;
        print_fmt     : Format.formatter;
        print_fd      : prim_channel;
        print_channel : Lm_channel.t
      }

   type value = Omake_value_type.value

   (*
    * Create the buffers and channels.
    *)
   let create_channel venv pos loc channel =
      let fmt = Format.make_formatter (Lm_channel.output_buffer channel) (fun () -> Lm_channel.flush channel) in
      let fd = venv_add_formatter_channel venv fmt in
      let channel = venv_find_channel venv pos fd in
         { print_venv    = venv;
           print_pos     = pos;
           print_loc     = loc;
           print_fmt     = fmt;
           print_fd      = fd;
           print_channel = channel
         }

   let create_buffer venv pos loc buf =
      let fmt = Format.formatter_of_buffer buf in
      let fd = venv_add_formatter_channel venv fmt in
      let channel = venv_find_channel venv pos fd in
         { print_venv    = venv;
           print_pos     = pos;
           print_loc     = loc;
           print_fmt     = fmt;
           print_fd      = fd;
           print_channel = channel
         }

   (*
    * When done, close the channels, and get the string.
    *)
   let close info =
      let { print_fd = fd;
            print_venv = venv;
            print_pos = pos;
            print_fmt = fmt
          } = info
      in
         venv_close_channel venv pos fd;
         Format.pp_print_flush fmt ()

   (*
    * The printers.
    *)
   let print_char info c =
      Lm_channel.output_char info.print_channel c

   let print_string info s =
      Lm_channel.output_string info.print_channel s

   (*
    * Formatter flushes the buffer.
    *)
   let flush info =
      Lm_channel.flush info.print_channel

   let open_box info i =
      flush info;
      Format.pp_open_box info.print_fmt i

   let open_hbox info =
      flush info;
      Format.pp_open_hbox info.print_fmt ()

   let open_vbox info i =
      flush info;
      Format.pp_open_vbox info.print_fmt i

   let open_hvbox info i =
      flush info;
      Format.pp_open_hvbox info.print_fmt i

   let open_hovbox info i =
      flush info;
      Format.pp_open_hovbox info.print_fmt i

   let close_box info =
      flush info;
      Format.pp_close_box info.print_fmt ()

   let print_cut info =
      flush info;
      Format.pp_close_box info.print_fmt ()

   let print_space info =
      flush info;
      Format.pp_print_space info.print_fmt ()

   let force_newline info =
      flush info;
      Format.pp_force_newline info.print_fmt ()

   let print_break info i j =
      flush info;
      Format.pp_print_break info.print_fmt i j

   let print_flush info =
      flush info;
      Format.pp_print_flush info.print_fmt ()

   let print_newline info =
      flush info;
      Format.pp_print_newline info.print_fmt ()

   (*
    * Converters.
    *)
   let bool_of_value info v =
      let { print_venv = venv;
            print_pos = pos
          } = info
      in
         bool_of_value venv pos v

   let char_of_value info v =
      let { print_venv = venv;
            print_pos = pos
          } = info
      in
      let s = string_of_value venv pos v in
         if String.length s <> 1 then
            raise (OmakeException (pos, StringStringError ("not a character", s)));
         s.[0]

   let int_of_value info v =
      let { print_venv = venv;
            print_pos = pos
          } = info
      in
         int_of_value venv pos v

   let float_of_value info v =
      let { print_venv = venv;
            print_pos = pos
          } = info
      in
         float_of_value venv pos v

   let string_of_value info v =
      let { print_venv = venv;
            print_pos = pos
          } = info
      in
         string_of_value venv pos v

   let print_value info v =
      flush info;
      pp_print_value (out_channel_of_formatter info.print_fmt) v

   (*
    * Applications.
    *)
   let apply1 info arg1 =
      let { print_venv = venv;
            print_pos = pos;
            print_loc = loc;
            print_fd = fd
          } = info
      in
         ignore (eval_apply venv pos loc arg1 [ValChannel (OutChannel, fd)] [])

   let apply2 info arg1 arg2 =
      let { print_venv = venv;
            print_pos = pos;
            print_loc = loc;
            print_fd = fd
          } = info
      in
         ignore (eval_apply venv pos loc arg1 [ValChannel (OutChannel, fd); arg2] [])

   (*
    * Catch too many arguments.
    *)
   let exit info args =
      match args with
         [] ->
            ValNone
       | arg :: _ ->
            let { print_venv = venv;
                  print_pos = pos
                } = info
            in
               raise (OmakeException (pos, StringValueError ("too many arguments to printf", arg)))
end

module Printf = MakePrintf (Args);;

let fprintf_aux venv pos loc channel fmt args =
   let fmt = string_of_value venv pos fmt in
   let buf = Args.create_channel venv pos loc channel in
   let result =
      try Printf.fprintf buf fmt args with
         exn ->
            Args.close buf;
            raise exn
   in
      Args.close buf;
      result

let printf_fun venv pos loc args =
   let pos = string_pos "printf" pos in
      match args with
         fmt :: args ->
            let stdout = venv_find_var venv pos loc stdout_var in
            let stdout = channel_of_value venv pos stdout in
               fprintf_aux venv pos loc stdout fmt args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let eprintf_fun venv pos loc args =
   let pos = string_pos "eprintf" pos in
      match args with
         fmt :: args ->
            let stderr = venv_find_var venv pos loc stderr_var in
            let stderr = channel_of_value venv pos stderr in
               fprintf_aux venv pos loc stderr fmt args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let fprintf_fun venv pos loc args =
   let pos = string_pos "fprintf" pos in
      match args with
         fd :: fmt :: args ->
            let channel = channel_of_value venv pos fd in
               fprintf_aux venv pos loc channel fmt args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let sprintf_fun venv pos loc args =
   let pos = string_pos "sprintf" pos in
      match args with
         fmt :: args ->
            let fmt = string_of_value venv pos fmt in
            let buf = Buffer.create 100 in
            let info = Args.create_buffer venv pos loc buf in
            let _ =
               try Printf.fprintf info fmt args with
                  exn ->
                     Args.close info;
                     raise exn
            in
               Args.close info;
               ValData (Buffer.contents buf)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \subsection{Miscellaneous functions}
 * \subsubsection{set-channel-line}
 *
 * \begin{verbatim}
 *     set-channel-line(channel, filename, line)
 *         channel : Channel
 *         filename : File
 *         line : int
 * \end{verbatim}
 *
 * Set the line number information for the channel.
 * \end{doc}
 *)
let set_channel_line_fun venv pos loc args =
   let pos = string_pos "set-channel-line" pos in
   let chan, file, line =
      match args with
         [chan; file; line] ->
            chan, file, line
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))
   in
   let chan = channel_of_value venv pos chan in
   let file = string_of_value venv pos file in
   let line = int_of_value venv pos line in
      Lm_channel.set_line chan file line;
      ValNone

(************************************************************************
 * Tables.
 *)

let () =
   let builtin_vars =
      ["nl",     (fun _ -> ValString "\n");
       "stdin",  (fun _ -> ValChannel (InChannel,  venv_stdin));
       "stdout", (fun _ -> ValChannel (OutChannel, venv_stdout));
       "stderr", (fun _ -> ValChannel (OutChannel, venv_stderr))]
   in
   let builtin_funs =
      [true, "open-in-string",        open_in_string,       ArityExact 1;
       true, "open-out-string",       open_out_string,      ArityExact 0;
       true, "out-contents",          out_contents,         ArityExact 1;
       true, "fopen",                 fopen,                ArityExact 2;
       true, "close",                 close,                ArityExact 1;
       true, "read",                  read,                 ArityExact 2;
       true, "input-line",            input_line,           ArityExact 1;
       true, "write",                 write,                ArityRange (2, 4);
       true, "lseek",                 lseek,                ArityExact 3;
       true, "rewind",                rewind,               ArityExact 1;
       true, "tell",                  tell,                 ArityExact 1;
       true, "flush",                 flush,                ArityExact 1;
       true, "channel-name",          channel_name,         ArityExact 1;
       true, "dup",                   dup,                  ArityExact 1;
       true, "dup2",                  dup2,                 ArityExact 2;
       true, "set-nonblock-mode",     set_nonblock_mode,    ArityExact 2;
       true, "set-close-on-exec",     set_close_on_exec_mode, ArityExact 2;
       true, "pipe",                  pipe,                 ArityExact 0;
       true, "mkfifo",                mkfifo,               ArityExact 2;
       true, "select",                select,               ArityExact 4;
       true, "lockf",                 lockf,                ArityExact 3;
       true, "socket",                socket,               ArityExact 3;
       true, "bind",                  bind,                 ArityRange (2, 3);
       true, "listen",                listen,               ArityExact 2;
       true, "accept",                accept,               ArityExact 1;
       true, "connect",               connect,              ArityExact 1;
       true, "getc",                  getc,                 ArityRange (0, 1);
       true, "gets",                  gets,                 ArityRange (0, 1);
       true, "fgets",                 fgets,                ArityRange (0, 1);
       true, "print",                 print,                ArityExact 1;
       true, "eprint",                eprint,               ArityExact 1;
       true, "fprint",                fprint,               ArityExact 2;
       true, "println",               println,              ArityExact 1;
       true, "eprintln",              eprintln,             ArityExact 1;
       true, "fprintln",              fprintln,             ArityExact 2;
       true, "printv",                printv,               ArityExact 1;
       true, "eprintv",               eprintv,              ArityExact 1;
       true, "fprintv",               fprintv,              ArityExact 2;
       true, "printvln",              printvln,             ArityExact 1;
       true, "eprintvln",             eprintvln,            ArityExact 1;
       true, "fprintvln",             fprintvln,            ArityExact 2;
       true, "printf",                printf_fun,           ArityAny;
       true, "eprintf",               eprintf_fun,          ArityAny;
       true, "fprintf",               fprintf_fun,          ArityAny;
       true, "sprintf",               sprintf_fun,          ArityAny;
       true, "set-channel-line",      set_channel_line_fun, ArityExact 3]
   in
   let builtin_info =
      { builtin_empty with builtin_vars = builtin_vars;
                           builtin_funs = builtin_funs
      }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
