(*
   Support for raw floating point values
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

(*
 * Type of raw float numbers.
 *)
type rawfloat

(*
 * Various forms of raw floats.
 *)
type float_precision =
   Single
 | Double
 | LongDouble

(* Conversions *)
val precision : rawfloat -> float_precision

val to_string : rawfloat -> string
val to_float : rawfloat -> float
val to_float80 : rawfloat -> Lm_float80.float80
val to_int64 : rawfloat -> int64
val to_int : rawfloat -> int
val to_rawint : Lm_rawint.int_precision -> Lm_rawint.int_signed -> rawfloat -> Lm_rawint.rawint

val of_int : float_precision -> int -> rawfloat
val of_rawint : float_precision -> Lm_rawint.rawint -> rawfloat
val of_float : float_precision -> float -> rawfloat
val of_float80 : float_precision -> Lm_float80.float80 -> rawfloat
val of_rawfloat : float_precision -> rawfloat -> rawfloat
val of_string : float_precision -> string -> rawfloat

(*
 * Comparison.
 *)
val is_zero : rawfloat -> bool
val compare : rawfloat -> rawfloat -> int

(*
 * Basic arithmetic.
 *)
val neg : rawfloat -> rawfloat
val uminus : rawfloat -> rawfloat
val succ : rawfloat -> rawfloat
val pred : rawfloat -> rawfloat
val abs : rawfloat -> rawfloat
val sin : rawfloat -> rawfloat
val cos : rawfloat -> rawfloat
val sqrt : rawfloat -> rawfloat

val add : rawfloat -> rawfloat -> rawfloat
val sub : rawfloat -> rawfloat -> rawfloat
val mul : rawfloat -> rawfloat -> rawfloat
val div : rawfloat -> rawfloat -> rawfloat
val rem : rawfloat -> rawfloat -> rawfloat
val min : rawfloat -> rawfloat -> rawfloat
val max : rawfloat -> rawfloat -> rawfloat
val atan2 : rawfloat -> rawfloat -> rawfloat
