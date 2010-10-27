(*
 * Configuration variables.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology and
 * HRL Laboratories, LLC
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
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_filename_util

(*
 * Error codes for various actions.
 *)
let signal_error_code   = 127
let fork_error_code     = 126
let internal_error_code = 125
let deadlock_error_code = 124
let exn_error_code      = 123
let scanner_error_code  = 122

(*
 * Name of the database.
 *)
let db_name = ".omakedb"

(*
 * Name of the makefiles.
 *)
let makefile_name = "OMakefile"
let makeroot_name = "OMakeroot"
let omake_file_suffix = ".om"
let makeroot_short_name = "Root" ^ omake_file_suffix

let omake_dir_ref = ref None
let cache_dir_ref = ref None
let always_use_dotomake = ref false

let set_omake_dir dir =
   let () =
      try Unix.mkdir dir 0o777 with
         Unix.Unix_error _ ->
            ()
   in
   let dir =
      if Filename.is_relative dir then
         try
            Filename.concat (Unix.getcwd ()) dir
         with Unix.Unix_error _ ->
            dir
      else
         dir
   in
      omake_dir_ref := Some dir;
      cache_dir_ref := None

(*
 * Directories.
 *)
let lib_dir, lib_dir_reason =
   let key_name = "SOFTWARE\\MetaPRL\\OMake" in
   let field_name = "OMAKELIB" in
      try
         Sys.getenv field_name, "OMAKELIB environment variable"
      with Not_found ->
         try
            Lm_unix_util.registry_find Lm_unix_util.HKEY_CURRENT_USER key_name field_name,
               "HKEY_CURRENT_USER\\" ^ key_name ^ "\\" ^ field_name ^ " registry key"
         with Not_found ->
            try
               Lm_unix_util.registry_find Lm_unix_util.HKEY_LOCAL_MACHINE key_name field_name,
                  "HKEY_LOCAL_MACHINE\\" ^ key_name ^ "\\" ^ field_name ^ " registry key"
            with Not_found ->
               Omake_magic.lib_dir, ""

let home_dir = Lm_unix_util.home_dir
let application_dir = Lm_unix_util.application_dir

let omakeinit_file = Filename.concat home_dir ".omakeinit"
let omakerc_file = Filename.concat home_dir ".omakerc"
let oshrc_file = Filename.concat home_dir ".oshrc"

let omake_dir () =
   match !omake_dir_ref with
      Some dir ->
         dir
    | None ->
         let dirname = Filename.concat application_dir ".omake" in
            set_omake_dir dirname;
            dirname

(*
 * Cache directory is separate for each host.
 *)
let cache_dir () =
   match !cache_dir_ref with
      Some dir ->
         dir
    | None ->
         let dirname = Filename.concat (omake_dir ()) "cache" in
         let () =
            try Unix.mkdir dirname 0o777 with
               Unix.Unix_error _ ->
                  ()
         in
            cache_dir_ref := Some dirname;
            dirname

(* Create cache file hierarchy under the HOME directory *)
let cache_file dir name =
   let dir =
      match Lm_filename_util.filename_string dir with
         AbsolutePath (DriveRoot c, name) ->
            Filename.concat (String.make 1 c) name
       | AbsolutePath (NullRoot, name) ->
            name
       | RelativePath path ->
            raise (Invalid_argument ("Omake_state.cache_file: received a relative path: " ^ path))
   in
   let dirname = Filename.concat (cache_dir ()) dir in
      Lm_filename_util.mkdirhier dirname 0o777;
      Filename.concat dirname name

let open_cache_file dir name =
   let filename = cache_file dir name in
      filename, Lm_unix_util.openfile filename [Unix.O_RDWR; Unix.O_CREAT] 0o666

let get_cache_file dir name =
   if !always_use_dotomake then
      open_cache_file dir name
   else
      let filename = Filename.concat dir name in
         try filename, Lm_unix_util.openfile filename [Unix.O_RDWR; Unix.O_CREAT] 0o666 with
            Unix.Unix_error _ ->
               open_cache_file dir name

(*
 * XXX: TODO: We use lockf, but it is not NFS-safe if filesystem is mounted w/o locking.
 * Also, lockf is not always supported, so we may raise an exception for a "wrong" reason.
 * May be we should implement a "sloppy" locking as well - see
 * also the mailing list discussions:
 *    - http://lists.metaprl.org/pipermail/omake/2005-November/thread.html#744
 *    - http://lists.metaprl.org/pipermail/omake-devel/2005-November/thread.html#122
 *)
let lock_file fd mode =
   Lm_unix_util.lockf fd mode 0

let db_file () =
   Filename.concat (omake_dir ()) db_name

let history_file () =
   Filename.concat (omake_dir ()) "osh_history"

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
