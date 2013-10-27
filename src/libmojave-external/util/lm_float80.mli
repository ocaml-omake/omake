(*
   Support for 80-bit floating point values
   Copyright (C) 2001 Justin David Smith, Caltech

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation,
   version 2.1 of the License.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
   
   Additional permission is given to link this library with the
   OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
   and you may distribute the linked executables.  See the file
   LICENSE.libmojave for more details.
 *)


(* Lm_float80 type definition *)
type float80


(* Lm_float80 constants *)
val zero       : float80
val one        : float80
val minus_one  : float80


(* Lm_float80 coercions *)
external to_string : float80 -> string = "string_of_float80"
external to_float  : float80 -> float  = "float_of_float80"
external to_int    : float80 -> int    = "int_of_float80"
val      to_int64  : float80 -> int64
external of_string : string -> float80 = "float80_of_string"
external of_float  : float -> float80  = "float80_of_float"
external of_int    : int -> float80    = "float80_of_int"
val      of_int64  : int64 -> float80
external format    : string -> float80 -> string = "float80_format"


(* Lm_float80 arithmetic functions *)
external neg     : float80 -> float80 = "float80_neg"
external abs     : float80 -> float80 = "float80_abs"
external sin     : float80 -> float80 = "float80_sin"
external cos     : float80 -> float80 = "float80_cos"
external sqrt    : float80 -> float80 = "float80_sqrt"
external add     : float80 -> float80 -> float80 = "float80_add"
external sub     : float80 -> float80 -> float80 = "float80_sub"
external mul     : float80 -> float80 -> float80 = "float80_mul"
external div     : float80 -> float80 -> float80 = "float80_div"
external atan2   : float80 -> float80 -> float80 = "float80_atan2"
val      rem     : float80 -> float80 -> float80
val      succ    : float80 -> float80
val      pred    : float80 -> float80
val      is_zero : float80 -> bool
external compare : float80 -> float80 -> int = "float80_compare"
val      min     : float80 -> float80 -> float80
val      max     : float80 -> float80 -> float80

(*
 * BUG: these are hopefully temporary.
 * We use them to blit the float into a strng area.
 *)
val blit_float32 : float80 -> string -> int -> unit
val blit_float64 : float80 -> string -> int -> unit
val blit_float80 : float80 -> string -> int -> unit
