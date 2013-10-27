(*
 * String buffers.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Mojave Group, Caltech
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
 * jyh@cs.caltech.edu
 *)

type entry =
   Char of char
 | SubString of string * int * int

type t = entry list ref

external screate : int -> string = "%screate"
external slength : string -> int = "%slength"
external sblit : string -> int -> string -> int -> int -> unit = "%sblit"

let create _ =
   ref []

let length buf =
   let rec loop i = function
      h :: t ->
         (match h with
             Char _ ->
                loop (succ i) t
           | SubString (_, _, len) ->
                loop (len + i) t)
    | [] ->
         i
   in
      loop 0 !buf

let contents buf =
   let len = length buf in
   let s = screate len in
   let rec loop i = function
      h :: t ->
         (match h with
             Char c ->
                let i = pred i in
                   s.[i] <- c;
                   loop i t
           | SubString (s', off, len) ->
                let i = i - len in
                   sblit s' off s i len;
                   loop i t)
    | [] ->
         s
   in
      loop len !buf

let clear buf =
   buf := []

let reset = clear

let add_char buf c =
   buf := (Char c) :: !buf

let add_string buf s =
   buf := (SubString (s, 0, slength s)) :: !buf

let add_substring buf s off len =
   buf := (SubString (s, off, len)) :: !buf

let add_buffer buf1 buf2 =
   buf1 := !buf2 @ !buf1

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
