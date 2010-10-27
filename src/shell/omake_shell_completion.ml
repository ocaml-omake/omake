(*
 * Completion functions for the interactive shell.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_node
open Omake_value
open Omake_symbol
open Omake_node_sig
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_shell_job" end)
open Pos;;

(*
 * Filename operations not already implemented in Lm_filename_util.
 *)
let is_compound_path name =
   Lm_string_util.contains_any name Lm_filename_util.separators

let split_last name =
   let i = Lm_string_util.rindex_set name Lm_filename_util.separators in
   let prefix = String.sub name 0 (i + 1) in
   let head = String.sub name 0 i in
   let tail = String.sub name (i + 1) (String.length name - i - 1) in
      prefix, head, tail

let split_relative name =
   if is_compound_path name then
      split_last name
   else
      "", "", name

let split_triple name =
   let i = Lm_string_util.index_set name Lm_filename_util.separators in
   let j = Lm_string_util.rindex_set name Lm_filename_util.separators in
   let prefix = String.sub name 0 (j + 1) in
   let first = String.sub name 0 i in
   let dir =
      if i = j then
         ""
      else
         String.sub name (i + 1) (j - i - 1)
   in
   let name = String.sub name (j + 1) (String.length name - j - 1) in
      prefix, first, dir, name

(*
 * We are given a partial username  ~xy
 * Find all the usernames with prefix xy.
 *)
let username_completion_exn s =
   let name = String.sub s 1 (String.length s - 1) in
   let users = Lm_glob.getusers () in
      List.fold_left (fun users user ->
            if Lm_string_util.is_string_prefix name user then
               StringSet.add users ("~" ^ user)
            else
               users) StringSet.empty users

(*
 * List the filenames in the directory that match the
 * given name.
 *)
let list_completion_exn prefix dir name =
   let names = Lm_filename_util.lsdir dir in
      List.fold_left (fun names n ->
            if Lm_string_util.is_string_prefix name n then
               StringSet.add names (prefix ^ n)
            else
               names) StringSet.empty names

(*
 * We are given an absolute name /ab/cd/ef.
 * A path /abc is a special case.
 *)
let absolute_completion_exn s =
   let prefix, dir, name = split_last s in
   let dir = if dir = "" then "/" else dir in
      list_completion_exn prefix dir name

(*
 * We are given a relative name ab/cd/ef.
 * Compute it relative to the current directory.
 *)
let relative_completion_exn venv pos loc s =
   let cwd = Dir.fullname (venv_dir venv) in
   let prefix, dir, name = split_relative s in
   let dir = Filename.concat cwd dir in
      list_completion_exn prefix dir name

(*
 * We are given a home directory name ~abc/def/geh
 *)
let homedir_completion_exn s =
   let prefix, user, dir, name = split_triple s in
   let home =
      if user = "~" then
         Lm_glob.home_dir
      else
         Lm_glob.gethomedir (String.sub user 1 (String.length user - 1))
   in
   let dir = Filename.concat home dir in
      list_completion_exn prefix dir name

(*
 * Filename completion has several cases:
 *    ~xy        : partial username should be completed
 *    ~xy/foo    : partial filename should be completed
 *    /foo/bar   : partial absolute filename should be completed
 *    foo/bar    : partial relative filename should be completed
 *)
let filename_completion_exn venv pos loc s =
   let pos = string_pos "filename_completion" pos in
   let len = String.length s in
   let () =
      if len = 0 then
         raise Not_found
   in
      if s.[0] = '~' then
         if is_compound_path s then
            homedir_completion_exn s
         else
            username_completion_exn s
      else if Lm_filename_util.is_absolute s then
         absolute_completion_exn s
      else
         relative_completion_exn venv pos loc s

(*
 * Command completion uses the Omake_cache.
 *)
let command_completion_exn venv pos loc s =
   let pos = string_pos "command_completion" pos in

   (* Aliases *)
   let shell_obj = venv_find_var_exn venv shell_object_var in
   let items1 =
      match eval_single_value venv pos shell_obj with
         ValObject obj ->
            venv_object_fold_internal (fun items v _ ->
                  let s2 = Lm_symbol.to_string v in
                     if Lm_string_util.is_string_prefix s s2 then
                        StringSet.add items s2
                     else
                        items) StringSet.empty obj
       | _ ->
            StringSet.empty
   in

   (* Commands *)
   let cache  = venv_cache venv in
   let path   = venv_find_var_exn venv path_var in
   let path   = Omake_eval.path_of_values venv pos (values_of_value venv pos path) "." in
   let path   = Omake_cache.ls_exe_path cache path in
   let items2 = Omake_cache.exe_complete cache path s in
      StringSet.union items2 items1

(*
 * For readline, the result requires that the first entry
 * be the maximal prefix of all the entries.
 *)
let rec char_matches c i names =
   match names with
      name :: names ->
         name.[i] = c && char_matches c i names
    | [] ->
         true

let rec min_string_length i names =
   match names with
      name :: names ->
         min_string_length (min i (String.length name)) names
    | [] ->
         i

let rec search_matches i len name names =
   if i = len then
      i
   else if char_matches name.[i] i names then
      search_matches (i + 1) len name names
   else
      i

let complete_names s names =
   let names = StringSet.to_list names in
   let off = String.length s in
      match names with
         [name] ->
            (* Optimization *)
            [| name; name |]
       | name :: rest ->
            let len = min_string_length (String.length name) rest in
            let len = search_matches off len name rest in
            let prefix = String.sub name 0 len in
               Array.of_list (prefix :: names)
       | [] ->
            [||]

(*
 * Catch all exceptions we might expect.
 *)
let catch f venv pos loc s =
   try complete_names s (f venv pos loc s) with
      Not_found
    | Sys_error _
    | OmakeException _ ->
         [||]

let set_completion_functions venv pos loc =
   let filename_completion s = catch filename_completion_exn venv pos loc s in
   let command_completion s = catch command_completion_exn venv pos loc s in
      Callback.register "omake_filename_completion" filename_completion;
      Callback.register "omake_command_completion" command_completion

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
