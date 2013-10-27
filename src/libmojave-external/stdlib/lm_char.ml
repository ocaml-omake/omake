(*
 * Character operations.
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

external code : char -> int = "%cord"
external chr : int -> char = "%char"

external make : int -> char -> string = "%smake"

let escaped = function
   '\n'   -> "\\n"
 | '\t'   -> "\\t"
 | '\r'   -> "\\r"
 | '\127' -> "\\127"
 | '\000' -> "\\000"
 | '\001' -> "\\001"
 | '\002' -> "\\002"
 | '\003' -> "\\003"
 | '\004' -> "\\004"
 | '\005' -> "\\005"
 | '\006' -> "\\006"
 | '\007' -> "\\007"
 | '\008' -> "\\008"
 | '\009' -> "\\009"
 | '\010' -> "\\010"
 | '\011' -> "\\011"
 | '\012' -> "\\012"
 | '\013' -> "\\013"
 | '\014' -> "\\014"
 | '\015' -> "\\015"
 | '\016' -> "\\016"
 | '\017' -> "\\017"
 | '\018' -> "\\018"
 | '\019' -> "\\019"
 | '\020' -> "\\020"
 | '\021' -> "\\021"
 | '\022' -> "\\022"
 | '\023' -> "\\023"
 | '\024' -> "\\024"
 | '\025' -> "\\025"
 | '\026' -> "\\026"
 | '\027' -> "\\027"
 | '\028' -> "\\028"
 | '\029' -> "\\029"
 | '\030' -> "\\030"
 | '\031' -> "\\031"
 | c      -> make 1 c

let lowercase = function
   'A' -> 'a'
 | 'B' -> 'b'
 | 'C' -> 'c'
 | 'D' -> 'd'
 | 'E' -> 'e'
 | 'F' -> 'f'
 | 'G' -> 'g'
 | 'H' -> 'h'
 | 'I' -> 'i'
 | 'J' -> 'j'
 | 'K' -> 'k'
 | 'L' -> 'l'
 | 'M' -> 'm'
 | 'O' -> 'o'
 | 'P' -> 'p'
 | 'Q' -> 'q'
 | 'R' -> 'r'
 | 'S' -> 's'
 | 'T' -> 't'
 | 'U' -> 'u'
 | 'V' -> 'v'
 | 'W' -> 'w'
 | 'X' -> 'x'
 | 'Y' -> 'y'
 | 'Z' -> 'z'
 | c -> c

let uppercase = function
   'a' -> 'A'
 | 'b' -> 'B'
 | 'c' -> 'C'
 | 'd' -> 'D'
 | 'e' -> 'E'
 | 'f' -> 'F'
 | 'g' -> 'G'
 | 'h' -> 'H'
 | 'i' -> 'I'
 | 'j' -> 'J'
 | 'k' -> 'K'
 | 'l' -> 'L'
 | 'm' -> 'M'
 | 'o' -> 'O'
 | 'p' -> 'P'
 | 'q' -> 'Q'
 | 'r' -> 'R'
 | 's' -> 'S'
 | 't' -> 'T'
 | 'u' -> 'U'
 | 'v' -> 'V'
 | 'w' -> 'W'
 | 'x' -> 'X'
 | 'y' -> 'Y'
 | 'z' -> 'Z'
 | c -> c

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
