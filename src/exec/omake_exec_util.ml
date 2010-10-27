(*
 * Utilities for execution.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 *)
open Lm_printf

open Lm_debug

open Omake_node
open Omake_state
open Omake_cache_type
open Omake_exec_print

(*
 * Build debugging.
 *)
let debug_exec =
   create_debug (**)
      { debug_name = "exec";
        debug_description = "Display execution debugging";
        debug_value = false
      }

(*
 * Table based on integers.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end

module IntTable = Lm_map.LmMake (IntCompare)

(*
 * Table based on file descriptor.
 *)
module FdCompare =
struct
   type t = Unix.file_descr
   let compare = Pervasives.compare
end

module FdTable = Lm_map.LmMake (FdCompare);;

(*
 * Create some pipes, and close them if an exception is raised.
 *)
let unix_close fd =
   try Unix.close fd with
      Unix.Unix_error _ ->
         ()

let with_pipe f =
   let read, write = Unix.pipe () in
      try f read write with
         exn ->
            unix_close read;
            unix_close write;
            raise exn

(*
 * Write the data in the buffer to the channel.
 *)
let rec write_all name fd id buf off len =
   if len <> 0 then
      let amount =
         try Unix.write fd buf off len with
            Unix.Unix_error (err1, err2, err3) ->
               eprintf "Writing to %s resulted in an error: %s: %s: %s@." name err2 err3 (Unix.error_message err1);
               0
      in
         if (amount <> 0) && (amount <> len) then begin
            eprintf "Writing to %s was only partially successful (%i out of %i written)@." name amount len;
            raise (Invalid_argument "Omake_exec_util.write_all")
         end

let copy_stdout = write_all "Unix.stdout" Unix.stdout
let copy_stderr = write_all "Unix.stderr" Unix.stderr

(*
 * Copy output to a file.
 *)
let copy_file name =
   let fd_out = Lm_unix_util.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
   let () = Unix.set_close_on_exec fd_out in
   let copy id buf off len =
      if len = 0 then
         Unix.close fd_out
      else
         write_all name fd_out id buf off len
   in
      copy

(*
 * Tee the output to a file if any occurs.
 * The files are created only if there is output.
 *)
type tee_info =
   TeeChannel of string * Pervasives.out_channel
 | TeeFile of string
 | TeeMaybe
 | TeeNever

type tee = tee_info ref

let tee_file tee =
   match !tee with
      TeeChannel (name, _)
    | TeeFile name ->
         Some name
    | TeeMaybe
    | TeeNever ->
         None

let tee_channel tee =
   match !tee with
      TeeChannel (_, outx) ->
         Some outx
    | TeeMaybe ->
         let filename, outx = Filename.open_temp_file ~mode:[Open_binary] "omake" ".divert" in
            tee := TeeChannel (filename, outx);
            Some outx
    | TeeFile _
    | TeeNever ->
         None

let tee_close tee =
   match !tee with
      TeeChannel (name, outx) ->
         Pervasives.close_out outx;
         tee := TeeFile name
    | TeeFile _
    | TeeMaybe
    | TeeNever ->
         ()

let tee_none = ref TeeNever

let tee_create b =
   if b then
      ref TeeMaybe
   else
      tee_none

let tee_copy name fd flush_flag tee tee_only id buf off len =
   if len = 0 then begin
      if not tee_only then
         flush_flag := true;
      match !tee with
         TeeChannel (_, outx) ->
            Pervasives.flush outx
      | _ ->
            ()
   end else begin
      if not tee_only then begin
         if !flush_flag then begin
            progress_flush ();
            flush_flag := false;
         end;
         write_all name fd id buf off len;
      end;
      match tee_channel tee with
         Some outx ->
            Pervasives.output outx buf off len
       | None ->
            ()
   end

let tee_stdout = tee_copy "Unix.stdout" Unix.stdout (ref true)
let tee_stderr = tee_copy "Unix.stderr" Unix.stderr (ref true)

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
