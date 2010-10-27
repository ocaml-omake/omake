(*
 * Generate magic numbers.
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
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Printf

(************************************************************************
 * Options.
 *)
type mode =
   CacheFiles
 | OmcFiles
 | OmoFiles
 | NoFiles

let mode        = ref NoFiles
let make_magic  = ref false
let make_root   = ref None
let output_file = ref None
let cache_files = ref []
let omc_files   = ref []
let omo_files   = ref []
let libdir      = ref None
let version_txt = ref "version.txt"
let default_save_interval = ref 15.0

let anon s =
   let p =
      match !mode with
         CacheFiles ->
            cache_files
       | OmcFiles ->
            omc_files
       | OmoFiles ->
            omo_files
       | NoFiles ->
            eprintf "You specified an anonymous file.  Use --help for help.";
            exit 3
   in
      p := s :: !p

let spec =
   ["--magic",       Arg.Set make_magic, "generate the omake_magic.ml file";
    "--lib",         Arg.String (fun s -> libdir := Some s), "specify the location of the library directory";
    "--root",        Arg.String (fun s -> make_root := Some s),  "generate the OMakeroot file";
    "--cache-files", Arg.Unit (fun () -> mode := CacheFiles), "specify the magic files for the cache magic number";
    "--omc-files",   Arg.Unit (fun () -> mode := OmcFiles), "specify the files to scan for the IR magic number";
    "--omo-files",   Arg.Unit (fun () -> mode := OmoFiles), "specify the files to scan for the object magic number";
    "--version",     Arg.String (fun s -> version_txt := s), "specify the version.txt file";
    "--default_save_interval", 
         Arg.Float (fun f -> default_save_interval := f), "specify the default .omakedb save interval";
    "-o",            Arg.String (fun s -> output_file := Some s), "set the output file"]

let usage = "Generate special files"

(************************************************************************
 * Version numbers.
 *)

(*
 * Get the version from the version.txt file.
 *)
let rec read_version_from_file inx =
   let line = Lm_string_util.trim (input_line inx) in
      if line = "" then
         raise End_of_file;
      line

let read_version () =
   let file = !version_txt in
   let inx = open_in file in
   let version =
      try read_version_from_file inx with
         End_of_file ->
            eprintf "The %s file does not contain a version number\n" file;
            flush stderr;
            exit 1
   in
      close_in inx;
      if Lm_string_util.contains_any version "()$\" " then
         version
      else if (String.contains version '-') then
         let dash = String.index version '-' in
         let release = String.sub version (dash + 1) ((String.length version) - dash - 1) in
            if String.length release > 4 && String.sub release 0 4 = "0.rc" then
               let release = String.sub release 4 ((String.length release) - 4) in
                  sprintf "%s (release candidate %s)" (String.sub version 0 dash) release
            else
               sprintf "%s (release %s)" (String.sub version 0 dash) release
      else
         version ^ " (development snapshot)"

(************************************************************************
 * File copying.
 *)

(*
 * Copy a file from input to output.
 *)
let rec copy_file buf inx =
   let rec copy () =
      let line = input_line inx in
         output_string buf line;
         output_char buf '\n';
         copy ()
   in
      try copy () with
         End_of_file ->
            ()

(************************************************************************
 * Magic numbers.
 *)

type magic =
   MagicBegin
 | MagicEnd
 | MagicNone

type magic_mode =
   MagicIdle
 | MagicStart
 | MagicScanning

(*
 * Figure out if the line is a magic directive.
 *)
let buf = Buffer.create 200

let magic_line_type line =
   let len = String.length line in
   let rec loop mode i =
      if i <> len then
         let c = line.[i] in
            if c = '%' then
               match mode with
                  MagicIdle ->
                     loop MagicStart (succ i)
                | MagicStart ->
                     loop MagicScanning (succ i)
                | MagicScanning ->
                     ()
            else begin
               if mode = MagicScanning then
                  Buffer.add_char buf c;
               loop mode (succ i)
            end
   in
   let () = loop MagicIdle 0 in
   let code = Buffer.contents buf in
      Buffer.clear buf;
      match code with
         "MAGICBEGIN" ->
            MagicBegin
       | "MAGICEND" ->
            MagicEnd
       | _ ->
            MagicNone

(*
 * Copy magic data from some files.
 *)
let copy_magic_file outx filename =
   let inx =
      try open_in filename with
         Sys_error _ ->
            eprintf "Can't open %s\n" filename;
            flush stderr;
            exit 1
   in
   let rec copy magic_flag =
      let line = input_line inx in
         match magic_line_type line with
            MagicBegin ->
               copy true
          | MagicEnd ->
               copy false
          | MagicNone ->
               if magic_flag then begin
                  output_string outx line;
                  output_char outx '\n'
               end;
               copy magic_flag
   in
      try copy false with
         End_of_file ->
            close_in inx

let digest_files filename code filenames =
   let outx =
      try open_out_bin filename with
         Sys_error _ ->
            eprintf "Can't open temporary_file %s\n" filename;
            flush stderr;
            exit 2
   in
   let digest =
      List.iter (copy_magic_file outx) filenames;
      close_out outx;
      Digest.file filename
   in
      String.escaped (code ^ digest)

(************************************************************************
 * omake_magic.ml file.
 *)

(*
 * Save the date in the version string.
 *)
let wday_names =
   [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]

let mon_names =
   [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

let digest_len = 4 + String.length (Digest.string "hello world")

let shorten_version s =
   if Lm_string_util.contains_any s " -" then
      String.sub s 0 (Lm_string_util.index_set s " -")
   else
      s

let omake_magic buf =
   let libdir = 
      match !libdir with 
         Some s -> Filename.concat s "omake"
       | None -> Filename.concat (Filename.dirname (Unix.getcwd ())) "lib"
   in
   let version = read_version () in
   let now = Unix.time () in
   let { Unix.tm_year = year;
         Unix.tm_mon = mon;
         Unix.tm_mday = mday;
         Unix.tm_wday = wday;
         Unix.tm_hour = hour;
         Unix.tm_min = min;
         Unix.tm_sec = sec
       } = Unix.localtime now
   in
      fprintf buf "let default_save_interval = %F\n" !default_save_interval;
      fprintf buf "let input_magic inx = let s = String.make %d ' ' in really_input inx s 0 %d; s\n" digest_len digest_len;
      fprintf buf "let output_magic = output_string\n";
      fprintf buf "let cache_magic = \"%s\"\n" (digest_files ".cache.magic" ".odb" !cache_files);
      fprintf buf "let ir_magic = \"%s\"\n"    (digest_files ".omc.magic" ".omc" !omc_files);
      fprintf buf "let obj_magic = \"%s\"\n"   (digest_files ".omo.magic" ".omo" !omo_files);
      fprintf buf "let lib_dir = \"%s\"\n" (String.escaped libdir);
      fprintf buf "let version = \"%s\"\n" (String.escaped (shorten_version version));
      fprintf buf "let version_message = \"OMake %s:\\n\\tbuild [%s %s %d %02d:%02d:%02d %d]\\n\\ton %s\"\n"
         (String.escaped version)
         wday_names.(wday)
         mon_names.(mon)
         mday
         hour
         min
         sec
         (year + 1900)
         (String.escaped (Unix.gethostname ()));
      flush buf

(************************************************************************
 * OMakeroot file.
 *)
let omake_root buf name =
   let version = shorten_version (read_version ()) in
   let version =
      if Lm_string_util.contains_any version "()$\" " then
         sprintf "$'''%s'''" version
      else
         version
   in
   let inx = open_in name in
      fprintf buf "#\n# Required version of omake\n#\nOMakeVersion(%s, %s)\n\n" version version;
      copy_file buf inx;
      fprintf buf "\n";
      close_out buf;
      close_in inx

(************************************************************************
 * Main function.
 *)
let main () =
   Arg.parse spec anon usage;
   let buf =
      match !output_file with
         Some name ->
            open_out_bin name
       | None ->
            raise (Invalid_argument "use the -o option to specify an output file")
   in
      if !make_magic then
         omake_magic buf
      else
         match !make_root with
            Some name ->
               omake_root buf name
          | None ->
               Arg.usage spec usage;
               exit 2

let _ =
   Printexc.catch main ()

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
