(*
 * Extra unix utilities.
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
 * Copyright (C) 1998-2007 PRL Group, Cornell University, California
 * Institute of Technology and HRL Laboratories, LLC
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <anogin@hrl.com>
 *)
open Lm_printf

(*
 * Registry handles.
 *)
type registry_hkey =
    HKEY_CLASSES_ROOT
  | HKEY_CURRENT_CONFIG
  | HKEY_CURRENT_USER
  | HKEY_LOCAL_MACHINE
  | HKEY_USERS

external print_stack_pointer : unit -> unit = "lm_print_stack_pointer"
external registry_find   : registry_hkey -> string -> string -> string = "caml_registry_find"
external getpwents : unit -> Unix.passwd_entry list = "lm_getpwents"

(*
 * Read the exact amount.
 *)
let rec really_read fd buf off len =
   if len <> 0 then
      let amount = Unix.read fd buf off len in
      let () =
         if amount = 0 then
            raise (Failure "really_read")
      in
      let off = off + amount in
      let len = len - amount in
         really_read fd buf off len

(*
 * Copy a file.
 *)
let rec complete_write fd buf off len =
   let count = Unix.write fd buf off len in
      if count < len then
         complete_write fd buf (off + count) (len - count)

let rec copy_file_fd buffer from_fd to_fd =
   let count = Unix.read from_fd buffer 0 (String.length buffer) in
      if count > 0 then
         begin
            complete_write to_fd buffer 0 count;
            copy_file_fd buffer from_fd to_fd
         end

let copy_file from_name to_name mode =
   let from_fd = Unix.openfile from_name [Unix.O_RDONLY] 0o666 in
   let () =
      try
         let to_fd = Unix.openfile to_name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
         let () =
            try
               copy_file_fd (String.create 8192) from_fd to_fd;
               Unix.fchmod to_fd mode
            with
               x ->
                  Unix.close to_fd;
                  raise x
         in
            Unix.close to_fd
      with
         x ->
            Unix.close from_fd;
            raise x
   in
      Unix.close from_fd

(*
 * Make a directory hierarchy.
 *)
let mkdirhier name =
   let rec mkdir head path =
      match path with
         dir :: rest ->
            let filename = Filename.concat head dir in

            (* If it is already a directory, keep it *)
            let is_dir =
               try (Unix.LargeFile.stat filename).Unix.LargeFile.st_kind = Unix.S_DIR with
                  Unix.Unix_error _ ->
                     false
            in
               if not is_dir then
                  Unix.mkdir filename 0o777;
               mkdir filename rest
       | [] ->
            ()
   in
   let head =
      if String.length name = 0 || name.[0] <> '/' then
         "."
      else
         "/"
   in
   let path = Lm_filename_util.split_path name in
   let path = Lm_filename_util.simplify_path path in
      mkdir head path

(*
 * Compatibility initializer.
 *)
external init : unit -> unit = "lm_compat_init"

let () = init ()

(*
 * Get the pid of the process holding the lock
 *)
external lm_getlk : Unix.file_descr -> Unix.lock_command -> int = "lm_getlk"

let getlk fd cmd =
   let res = lm_getlk fd cmd in
      if res = 0 then None else Some res

(*
 * Convert a fd to an integer (for debugging).
 *)
external int_of_fd : Unix.file_descr -> int = "int_of_fd"

(*
 * Win32 functions.
 *)
external home_win32       : unit -> string = "home_win32"
external lockf_win32      : Unix.file_descr -> Unix.lock_command -> int -> unit = "lockf_win32"
external ftruncate_win32  : Unix.file_descr -> unit = "ftruncate_win32"

(*
 * Try to figure out the home directory as best as possible.
 *)
let find_home_dir () =
   try Sys.getenv "HOME" with
      Not_found ->
         let home =
            try (Unix.getpwnam (Unix.getlogin ())).Unix.pw_dir with
               Not_found
             | Unix.Unix_error _ ->
                 eprintf "!!! Lm_unix_util.find_home_dir:@.";
                 eprintf "!!! You have no home directory.@.";
                 eprintf "!!! Please set the HOME environment variable to a suitable directory.@.";
                 raise (Invalid_argument "Lm_unix_util.find_home_dir")
         in
            Unix.putenv "HOME" home;
            home

let application_dir =
   if Sys.os_type = "Win32" then
      try home_win32 () with
         Failure _ ->
            find_home_dir ()
   else
      find_home_dir ()

let home_dir =
   if Sys.os_type = "Win32" then
      try Sys.getenv "HOME" with
         Not_found ->
            let home = application_dir in
               Unix.putenv "HOME" home;
               home
   else
      application_dir

let lockf =
   if Sys.os_type = "Win32" then
      (fun fd cmd off ->
         try lockf_win32 fd cmd off with
            Failure "lockf_win32: already locked" ->
               raise (Unix.Unix_error(Unix.EAGAIN, "lockf", ""))
          | Failure "lockf_win32: possible deadlock" ->
               raise (Unix.Unix_error(Unix.EDEADLK, "lockf", "")))
   else
      Unix.lockf

let ftruncate =
   if Sys.os_type = "Win32" then
      ftruncate_win32
   else
      (fun fd -> Unix.ftruncate fd (Unix.lseek fd 0 Unix.SEEK_CUR))

type flock_command =
   LOCK_UN
 | LOCK_SH
 | LOCK_EX
 | LOCK_TSH
 | LOCK_TEX

external flock : Unix.file_descr -> flock_command -> unit = "lm_flock"

(*
 * Open a file descriptor.
 * This hook is here so you can add print statements to
 * help find file descriptor leaks.
 *)
let openfile = Unix.openfile

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
