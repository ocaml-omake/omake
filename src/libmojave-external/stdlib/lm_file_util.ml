(*
 * Operations on files.
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
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Lm_debug

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Lm_file_util%t"

(* Can't open and can't find a file *)
exception CantOpen of string
exception CantFind of string

(*
 * Split the name into a list of names.
 * If there is an initial /, then this is an
 * absolute name.  Otherwise it is relative to the current module.
 *)
let parse_path dir name =
   let len = String.length name in
   let rec aux path i j =
      if i < len then
         if j = len or name.[j] = '/' then
            let word = Lm_string_util.sub "Lm_file_util.parse_path" name i (j - i) in
            let path' =
               match word with
                  "" | "." -> path
                | ".." ->
                     if path = [] then
                        []
                     else
                        List.tl path
                | _ -> word::path
            in
               aux path' (j + 1) (j + 1)
         else
            aux path i (j + 1)
      else
         path
   in
   let path =
      if len <> 0 & name.[0] = '/' then
         []
      else
         dir
   in
      List.rev (aux path 0 0)

(*
 * Build the path from the list.
 *)
let rec build_path = function
   [h] -> h
 | h::t ->
      h ^ "/" ^ (build_path t)
 | [] -> "."

(*
 * Get dir name.
 *)
let path_dir path =
   match parse_path [] path with
      [] -> "."
    | x ->
         let h, _ = Lm_list_util.split_last x in
            build_path h

(*
 * Get filename.
 *)
let path_file path =
   match parse_path [] path with
      [] -> "."
    | x ->
         Lm_list_util.last x

(*
 * Open a file somewhere in the path.
 *)
let open_in_path path name =
   let rec aux = function
      [] -> raise (CantFind name)
    | dir::rest ->
         let fullname = Filename.concat dir name in
            try let ifile = open_in fullname in (ifile, fullname)
               with Sys_error _ -> aux rest
   in
      aux path

(*
 * Safe file handler.
 *)
let with_input_file name f =
   let iport = open_in name in
   let a = try f iport with x -> close_in iport; raise x in
       close_in iport;
       a

let with_output_file name f =
   let oport = open_out name in
   let a = try f oport with x -> close_out oport; raise x in
      close_out oport;
      a

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
