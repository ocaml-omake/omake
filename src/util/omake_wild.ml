(*
 * Some utilities, mostly on filenames.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech
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
 * Modified by: Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 *)
open Lm_printf

(*
 * The wildcard character.
 *)
let wild_char   = '%'
let wild_string = "%"

(*
 * We have very simple regular expressions of the form, where
 * a single % is a wildcard.
 *)
type wild_in_patt = int * string * int * string

type wild_out_patt = string list

type wild_subst = int * string

type wild_value = string

(*
 * Printing.
 *)
let pp_print_wild_in buf (_, s1, _, s2) =
   fprintf buf "%s%c%s" s1 wild_char s2

let pp_print_wild_out buf strs =
   pp_print_string buf (String.concat wild_string strs)

(*
 * Check if a string is a wild pattern.
 *)
let is_wild s =
   String.contains s wild_char

(*
 * Compile a pattern to make searching easier.
 *)
let wild_compile_in s =
   let len = String.length s in
      try
         let index = String.index s wild_char in
         let prefix = String.sub s 0 index in
         let slen = len - index - 1 in
         let suffix = String.sub s (succ index) slen in
            if String.contains suffix wild_char then
               raise (Failure "Only one wildcard symbol % allowed in a match pattern");
            index, prefix, slen, suffix
      with
         Not_found ->
            raise (Invalid_argument "Omake_wild.wild_compile")

let wild_compile_out s =
   Lm_string_util.split wild_string s

(*
 * Perform a match.
 *)
let rec string_match s1 off s2 len i =
   if i = len then
      true
   else
      s1.[off + i] = s2.[i] && string_match s1 off s2 len (succ i)

(*
 * Does the node match?
 *)
let wild_matches (plen, prefix, slen, suffix) s =
   let len = String.length s in
      len >= plen + slen && string_match s 0 prefix plen 0 && string_match s (len - slen) suffix slen 0

(*
 * Match the wild pattern, and return a subst.
 *)
let wild_match (plen, prefix, slen, suffix) s =
   let len = String.length s in
      if len >= plen + slen && string_match s 0 prefix plen 0 && string_match s (len - slen) suffix slen 0 then
         let len = len - plen - slen in
            Some (len, String.sub s plen len)
      else
         None

(*
 * Get the substitution value.
 *)
let wild_core (_, s) =
   s

let wild_of_core s =
   String.length s, s

(*
 * Perform a substitution.
 *)
let wild_subst_in (slen, s) (plen, prefix, sflen, suffix) =
   let res = String.create (slen + plen + sflen) in
      String.blit prefix 0 res 0 plen;
      String.blit s 0 res plen slen;
      String.blit suffix 0 res (plen + slen) sflen;
      res

let wild_subst (_, s) strs =
   String.concat s strs

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
