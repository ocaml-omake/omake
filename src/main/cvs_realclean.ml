(*
 * This is a script to remove all files in a project that are
 * unknown to CVS.  This would be easier in a scripting language,
 * but Win32 is limited.
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
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 *)
open Lm_printf

open Lm_string_set

(*
 * Flags.
 *)

(* Forcably remove files *)
let rm_force = ref false

(*
 * List the entries in a directory.
 *)
let ls_aux dirname =
   let dir = Unix.opendir dirname in
   let rec collect entries =
      let entry =
      try Some (Unix.readdir dir) with
         End_of_file ->
            None
      in
         match entry with
            Some "."
          | Some ".."
          | Some "CVS" ->
               collect entries
          | Some name ->
               collect (name :: entries)
          | None ->
               entries
   in
   let entries = collect [] in
      Unix.closedir dir;
      entries

let ls dirname =
   try ls_aux dirname with
      Unix.Unix_error _ ->
         []

let ignore_files = ref (StringSet.empty)

let ignore_file name =
   ignore_files := StringSet.add !ignore_files name;
   if Filename.is_relative name then
      ignore_files := StringSet.add !ignore_files (Filename.concat Filename.current_dir_name name)

(*
 * Get the entries in a CVS/Entries file.
 *)
let cvs_entries dirname =
   let filename = dirname ^ "/CVS/Entries" in
   let inx = open_in_bin filename in
   let rec collect_entries files dirs =
      let line =
         try Some (input_line inx) with
            End_of_file ->
               None
      in
         match line with
            None ->
               files, dirs
          | Some line ->
               match Lm_string_util.split "/" line with
                  "D" :: name :: _ ->
                     let dirs = StringSet.add dirs name in
                        collect_entries files dirs
                | "" :: name :: _ ->
                     let files = StringSet.add files name in
                        collect_entries files dirs
                | _ ->
                     collect_entries files dirs
   in
   let files, dirs = collect_entries StringSet.empty StringSet.empty in
      close_in inx;
      files, dirs

(*
 * Arguments.
 *)
let args = [
   "-f", Arg.Set rm_force, "\tDo not ask whether to remove files";
   "-i", Arg.String ignore_file, "<file>\tIgnore the file";
]

let root_dirs = ref []

let add_dir s =
   root_dirs := s :: !root_dirs

(*
 * Main program starts from the current directory.
 *)
let main () =
   let rm_command =
      match Sys.os_type with
         "Win32" ->
            if !rm_force then
               "del /F "
            else
               "del /P "
       | "Unix"
       | "Cygwin" ->
            if !rm_force then
               "/bin/rm -rf "
            else
               "/bin/rm -ri "
       | s ->
            raise (Failure ("cvs_realclean: unknown architecture " ^ s))
   in
   (*
    * Remove a file/directory.
    *)
   let rm dirname name =
      let name = Filename.concat dirname name in
         if not (StringSet.mem !ignore_files name) then begin
            printf "removing %s...@." name;
            ignore (Unix.system (rm_command ^ (Filename.quote name)))
         end
   in
   (*
    * Remove all entries that are not known to CVS.
    *)
   let rec clean dirname =
      match
         try Some (cvs_entries dirname)
         with Sys_error _ ->
            eprintf "WARNING: Directory ``%s'' does not have a readable CVS/Entries file!\n         Ignoring (skipping) directory ``%s''.\n" dirname dirname;
            None
      with
         Some (files, dirs) ->
            let names = ls dirname in
            let subdirs =
               List.fold_left (fun subdirs name ->
                     if StringSet.mem files name then
                        subdirs
                     else if StringSet.mem dirs name then
                        Filename.concat dirname name :: subdirs
                     else
                        begin
                           rm dirname name;
                           subdirs
                        end) [] names
            in
               List.iter clean subdirs
       | None ->
            ()
   in
   let dirs =
      match !root_dirs with
         [] -> [Filename.current_dir_name]
       | l -> List.rev l
   in
      List.iter clean dirs

let _ =
   Arg.parse args add_dir "cvs_realclean removes all files not known by CVS.\nUsage: cvs_realclean <options> [dir1 [dir2 ...]]\nOptions are:";
   Printexc.catch main ()

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
