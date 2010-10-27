(*
 * Functions to install OMakefiles into a project.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2006 Mojave Group, Caltech
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
open Lm_glob

open Omake_node_sig
open Omake_node

(*
 * Copy a file into a directory, but prompt the user if
 * the file already exists.
 *)
let copy_file force src dst =
   let prompt () =
      printf "%s already exists, overwrite (yes/no)? " dst;
      flush stdout;
      String.lowercase (input_line stdin) = "yes"
   in
      if force || not (Sys.file_exists dst) || prompt () then
         let () = printf "*** omake: creating %s@." dst in
         let inx = Pervasives.open_in src in
         let outx = Pervasives.open_out dst in
         let rec copy () =
            Pervasives.output_char outx (input_char inx);
            copy ()
         in
         let () =
            try copy () with
               End_of_file ->
                  ()
         in
            close_in inx;
            close_out outx
      else
         printf "*** omake: skipping %s@." dst

(*
 * Names of the standard files.
 *)
let omakeroot = Node.fullname (Node.create_node no_mount_info Mount.empty Dir.lib "OMakeroot.default")
let omakefile = Node.fullname (Node.create_node no_mount_info Mount.empty Dir.lib "OMakefile.default")

(*
 * Install just into the current directory.
 *)
let install_current force =
   copy_file force omakeroot "OMakeroot";
   copy_file force omakefile "OMakefile";
   printf "*** omake: project files OMakefile and OMakeroot have been installed\n";
   printf "*** omake: you should edit these files before continuing@."

(*
 * Install into all subdirectories.
 *)
let glob_cvs_ignore = Lm_glob.create_options [GlobCVSIgnore]

let install_subdirs force =
   let dirs = subdirs_of_dirs glob_cvs_ignore "" ["."] in
      copy_file force omakeroot "OMakeroot";
      List.iter (fun dir -> copy_file force omakefile (Filename.concat dir "OMakefile")) dirs;
      printf "*** omake: project files OMakefile and OMakeroot have been installed\n";
      printf "*** omake: OMakefiles have been installed into all subdirectories\n";
      printf "*** omake: you should edit these files before continuing@."

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
