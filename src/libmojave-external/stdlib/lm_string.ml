(*
 * String operations.
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

(* these will have to change if we start to use C strings (simply add an extra, final element '\000') *)
let length = Array.length
let get = Array.get
let set = Array.set
let create n = Array.create n '\000' (* '\000' is an "arbitrary character" *)
let make = Array.create
let copy = Array.copy
let sub = Array.sub
let fill = Array.fill
let blit = Array.blit
let concat = Array.append

(*
 * Escape the string.
 *)
let escaped s =
   let buf = Buffer.create 19 in
      for i = 0 to pred (length s) do
         Buffer.add_string buf (Char.escaped s.[i])
      done;
      Buffer.contents buf

(*
 * First occurrence of the char in the string.
 *)
let index s c =
   let len = length s in
   let rec loop i =
      if i = len then
         raise Not_found
      else if s.[i] = c then
         i
      else
         loop (succ i)
   in
      loop 0

(*
 * Last ocurrence of the char in the string.
 *)
let rindex s c =
   let rec loop i =
      if i <= 0 then
         raise Not_found;
      let i = pred i in
         if s.[i] = c then
            i
         else
            loop (pred i)
   in
      loop (length s)

(*
 * First occurrence of the char in the string.
 *)
let index_from s i c =
   let len = length s in
   let rec loop i =
      if i = len then
         raise Not_found;
      if s.[i] = c then
         i
      else
         loop (succ i)
   in
      if i < 0 || i > len then
         raise (Invalid_argument "index_from");
      loop i

(*
 * Last ocurrence of the char in the string.
 *)
let rindex_from s i c =
   let rec loop i =
      if i = 0 then
         raise Not_found;
      let i = pred i in
         if s.[i] = c then
            i
         else
            loop i
   in
   let len = length s in
      if i < 0 || i > len then
         raise (Invalid_argument "rindex_from");
      loop i

(*
 * String containment.
 *)
let contains s c =
   let len = length s in
   let rec loop i =
      if i = len then
         false
      else
         s.[i] = c || loop (succ i)
   in
      loop 0

let contains_from s i c =
   let len = length s in
   let rec loop i =
      if i = len then
         false
      else
         s.[i] = c || loop (succ i)
   in
      if i < 0 || i > len then
         raise (Invalid_argument "contains_from");
      loop i

let rcontains_from s i c =
   let rec loop i =
      if i = 0 then
         false
      else
         let i = pred i in
            s.[i] = c || loop i
   in
   let len = length s in
      if i < 0 || i > len then
         raise (Invalid_argument "rcontains_from");
      loop i

(*
 * Convert to uppercase.
 *)
let uppercase s =
   let s' = copy s in
      for i = 0 to pred (length s) do
         s'.[i] <- Char.uppercase s.[i]
      done;
      s'

let lowercase s =
   let s' = copy s in
      for i = 0 to pred (length s) do
         s'.[i] <- Char.lowercase s.[i]
      done;
      s'

let capitalize s =
   if s = "" then
      s
   else
      let s' = copy s in
         s'.[0] <- Char.uppercase s'.[0];
         s'

let uncapitalize s =
   if s = "" then
      s
   else
      let s' = copy s in
         s'.[0] <- Char.lowercase s'.[0];
         s'

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
