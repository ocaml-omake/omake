(*
 * Simple value database.  The entries in the database have
 * the following format.
 *
 *     - Field label (int)
 *     - Hostname (string)
 *     - Magic number (16 bytes)
 *     - Digest (used on the source file, for up-to-date info)
 *     - Value (marshaled)
 *
 * Invariant:
 *     - There is at most one entry for each host/field label.
 *
 *       If the magic number doesn't match, then the entry is
 *       out-of-date, and should be replaced.
 *
 * In some cases, the hostname doesn't matter.  Even so, if there
 * is an entry with the current hostname, and the magic number
 * doesn't match, it is out-of-date.
 *
 * NOTE: This has been updated to allowed for key-value pairs
 * in the header.  It looks pretty dumb, but I (jyh) want to keep
 * the file format backward-compatible.  So we stuff all the key/value
 * pairs in the magic number.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, California Institute of Technology and
 * HRL Laboratories, LLC
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
open Lm_debug

let debug_db =
   create_debug (**)
      { debug_name = "db";
        debug_description = "Display debugging information for marshaling operations";
        debug_value = false
      }

type t = Unix.file_descr

type tag = int
type magic = string
type digest = string
type hostname = string
type named_value = string * string

type entry_pred = tag -> named_value list -> hostname -> digest -> bool

let first_entry_tag = 1000

(*
 * Some kinds of entries are host-independent.
 *)
type host =
   HostIndependent
 | HostDependent

(*
 * Codes during unmarshaling.
 *)
type 'a unmarshal =
   UnmarshalValue of 'a
 | UnmarshalNext

(*
 * Codes during removal.
 *)
type remove =
   RemoveEntry of int
 | RemoveNext
 | RemoveRest

(*
 * Version number.
 *)
let magic = 0x56e50f8b

(*
 * Marshaling.
 *)
let hostname = Unix.gethostname ()
let digest_length = 16

(*
 * File operations.
 *)

(*
 * Win32 doesn't have a general truncate, so seek and truncate.
 *)
let seek_and_truncate fd pos =
   let _ = Unix.lseek fd pos Unix.SEEK_SET in
      Lm_unix_util.ftruncate fd

(*
 *
 * When an entry is removed, copy the remaining parts of
 * the file.
 *)
let bufsize = 4096

let file_shift fd pos1 pos2 =
   let buf = String.create bufsize in
   let rec copy pos1 pos2 =
      let _ = Unix.lseek fd pos2 Unix.SEEK_SET in
      let amount = Unix.read fd buf 0 bufsize in
         if amount <> 0 then
            let _ = Unix.lseek fd pos1 Unix.SEEK_SET in
               assert (Unix.write fd buf 0 amount = amount);
               copy (pos1 + amount) (pos2 + amount)
         else
            pos1
   in
   let pos = copy pos1 pos2 in
      seek_and_truncate fd pos;
      ignore (Unix.lseek fd pos1 Unix.SEEK_SET)

(*
 * If some kind of error happens while removing an entry,
 * truncate the file at this point.
 *)
let remove_entry fd pos off =
   try file_shift fd pos off with
      Unix.Unix_error _ ->
         seek_and_truncate fd pos

(*
 * Unmarshaling.
 *)
let unmarshal_magic inx =
   try input_binary_int inx = magic with
      End_of_file ->
         false

let unmarshal_tag inx =
   input_binary_int inx

let unmarshal_digest inx =
   let s = String.create digest_length in
      really_input inx s 0 digest_length;
      s

let unmarshal_string inx =
   let len = input_binary_int inx in
      if len < 0 || len >= 1024 then
         raise (Failure "unmarshal_string")
      else
         let s = String.create len in
            really_input inx s 0 len;
            s

let unmarshal_strings_old inx =
   let magic = unmarshal_string inx in
      ["MAGIC", magic]

let unmarshal_strings_new inx =
   (* Total size of all the entries *)
   let _ = input_binary_int inx in

   (* Number of key/value pairs *)
   let len = input_binary_int inx in
      (* Read the key/value pairs *)
      if len < 0 || len >= 1024 then
         raise (Failure "unmarshal_string")
      else
         let rec loop strings i =
            if i = len then
               List.rev strings
            else
               let key = unmarshal_string inx in
               let value = unmarshal_string inx in
                  loop ((key, value) :: strings) (i + 1)
         in
            loop [] 0

let unmarshal_strings inx tag =
   if tag < first_entry_tag then
      unmarshal_strings_old inx
   else
      unmarshal_strings_new inx

(*
 * Search for the appropriate entry.
 *)
let find_entry fd filename test =
   let _ = Unix.lseek fd 0 Unix.SEEK_SET in
   let inx = Unix.in_channel_of_descr fd in
   let head = String.create Marshal.header_size in

   (* Find the appropriate entry *)
   let unmarshal_entry () =
      (* Get the header *)
      let tag = unmarshal_tag inx in
      let host = unmarshal_string inx in
      let strings = unmarshal_strings inx tag in
      let digest = unmarshal_digest inx in
         if test tag strings host digest then begin
            (* Found a matching entry *)
            if !debug_db then
               eprintf "@[<v 3>Marshal.from_channel: %s@ save tag/digest: %d/%s@." (**)
                  filename
                  tag (Lm_string_util.hexify digest);
            let x = UnmarshalValue (Marshal.from_channel inx) in
               if !debug_db then
                  eprintf "Marshal.from-channel: done@.";
               x
         end
         else
            (* Skip over this entry *)
            let () = really_input inx head 0 Marshal.header_size in
            let size = Marshal.data_size head 0 in
            let pos = pos_in inx + size in
               seek_in inx pos;
               UnmarshalNext
   in

   (*
    * Search through the entries.  If an exception is raised,
    * truncate the file at the start of the entry.
    *)
   let rec search () =
      let start = pos_in inx in
      let code =
         try unmarshal_entry () with
            End_of_file
          | Failure _
          | Sys_error _
          | Invalid_argument _ ->
               if !debug_db then
                  eprintf "Lm_db.find: %s: failed@." filename;
               seek_and_truncate fd start;
               raise Not_found
      in
         match code with
            UnmarshalValue x ->
               x
          | UnmarshalNext ->
               search ()
   in
      if unmarshal_magic inx then
         search ()
      else
         raise Not_found

let find fd filename (tag, host_mode) magic digest =
   let test tag' strings host' digest' =
      match strings with
         ["MAGIC", magic'] ->
            tag' = tag && magic' = magic && digest' = digest && (host_mode = HostIndependent || host' = hostname)
       | _ ->
            false
   in
      find_entry fd filename test

(*
 * Remove an entry.  Search through the existing entries
 * to find one with the same tag.  If the host is significant,
 * remove only the entry with the same hostname.  Otherwise,
 * remove the entry with the same magic number.
 *)
let marshal_magic fd =
   seek_and_truncate fd 0;
   let outx = Unix.out_channel_of_descr fd in
      output_binary_int outx magic;
      Pervasives.flush outx

let remove_entry fd _filename test =
   let head = String.create Marshal.header_size in

   (* Find the appropriate entry *)
   let unmarshal_entry inx =
      (* Get the header *)
      let tag = unmarshal_tag inx in
      let host = unmarshal_string inx in
      let strings = unmarshal_strings inx tag in
      let digest = unmarshal_digest inx in
      let () = really_input inx head 0 Marshal.header_size in
      let size = Marshal.data_size head 0 in
      let pos = pos_in inx + size in
         if test tag strings host digest then
            RemoveEntry pos
         else begin
            seek_in inx pos;
            RemoveNext
         end
   in

   (*
    * Search through the entries.  If an exception is raised,
    * truncate the file at the start of the entry.
    *)
   let rec search inx =
      let start = pos_in inx in
      let code =
         try unmarshal_entry inx with
            End_of_file
          | Failure _
          | Sys_error _
          | Invalid_argument _ ->
               RemoveRest
      in
         match code with
            RemoveEntry pos ->
               remove_entry fd start pos;
               ignore(Unix.lseek fd 0 Unix.SEEK_SET);
               let inx = Unix.in_channel_of_descr fd in
                  seek_in inx start;
                  search inx
          | RemoveNext ->
               search inx
          | RemoveRest ->
               seek_and_truncate fd start
   in
   let _ = Unix.lseek fd 0 Unix.SEEK_SET in
   let inx = Unix.in_channel_of_descr fd in
      if unmarshal_magic inx then
         search inx
      else
         marshal_magic fd

let remove fd filename (tag, host_mode) magic =
   let test tag' strings host' _digest' =
      match strings with
         ["MAGIC", magic'] ->
            tag' = tag && (host' = hostname || host_mode = HostIndependent && magic' = magic)
       | _ ->
            false
   in
      remove_entry fd filename test

(*
 * Add an entry.
 * Remove any existing entry, and add the new one to the end of the
 * file.
 *)
let marshal_tag outx tag =
   output_binary_int outx tag

let marshal_digest outx digest =
   assert (String.length digest = digest_length);
   Pervasives.output_string outx digest

let marshal_string outx s =
   let len = String.length s in
      output_binary_int outx len;
      Pervasives.output_string outx s

let marshal_strings outx sl =
   let len =
      List.fold_left (fun len (key, value) ->
            len + String.length key + String.length value + 8) 4 sl
   in
      output_binary_int outx len;
      output_binary_int outx (List.length sl);
      List.iter (fun (key, value) ->
            marshal_string outx key;
            marshal_string outx value) sl

let marshal_entry fd filename tag magic_number digest x =
   let outx = Unix.out_channel_of_descr fd in
      marshal_tag outx tag;
      marshal_string outx hostname;
      marshal_string outx magic_number;
      marshal_digest outx digest;
      if !debug_db then
         eprintf "@[<v 3>Marshal.to_channel: %s@ tag/digest: %d/%s@]@." (**)
            filename
            tag (Lm_string_util.hexify digest);
      Marshal.to_channel outx x [];
      if !debug_db then
         eprintf "Marshal.to_channel: %s: done@." filename;
      Pervasives.flush outx

let add fd filename ((code, _) as tag) magic digest x =
   remove fd filename tag magic;
   marshal_entry fd filename code magic digest x

let append_entry fd filename tag strings digest x =
   let _ = Unix.lseek fd 0 Unix.SEEK_END in
   let outx = Unix.out_channel_of_descr fd in
      marshal_tag outx tag;
      marshal_string outx hostname;
      marshal_strings outx strings;
      marshal_digest outx digest;
      if !debug_db then
         eprintf "@[<v 3>Marshal.to_channel: %s@ tag/digest: %d/%s@]@." (**)
            filename
            tag (Lm_string_util.hexify digest);
      Marshal.to_channel outx x [];
      if !debug_db then
         eprintf "Marshal.to_channel: %s: done@." filename;
      Pervasives.flush outx

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
