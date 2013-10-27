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
type float80 = string      (* TEMP: fix this so it is better *)


(* Lm_float80 coercions *)
external to_string : float80 -> string = "string_of_float80"
external to_float  : float80 -> float  = "float_of_float80"
external to_int    : float80 -> int    = "int_of_float80"
external of_string : string -> float80 = "float80_of_string"
external of_float  : float -> float80  = "float80_of_float"
external of_int    : int -> float80    = "float80_of_int"
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
external compare : float80 -> float80 -> int = "float80_compare"


(* Constants *)
let zero       = of_float 0.0
let one        = of_float 1.0
let minus_one  = of_float ~-.1.0


(* Implemented functions *)
let to_int64 f = Int64.of_float (to_float f)
let of_int64 i = of_float (Int64.to_float i)
let rem _ _    = raise (Failure "Not supported: Lm_float80.rem")
let succ f     = add f one
let pred f     = sub f one
let is_zero f  = (compare f zero) = 0
let min x y    = if compare x y < 0 then x else y
let max x y    = if compare x y > 0 then x else y

(* Check buffer bounds *)
external c_blit_float32 : float80 -> string -> int -> unit = "c_blit_float32"
external c_blit_float64 : float80 -> string -> int -> unit = "c_blit_float64"
external c_blit_float80 : float80 -> string -> int -> unit = "c_blit_float80"

let blit_float32 x buf off =
   let len = String.length buf in
      if off + 4 > len then
         raise (Invalid_argument "blit_float32");
      c_blit_float32 x buf off

let blit_float64 x buf off =
   let len = String.length buf in
      if off + 8 > len then
         raise (Invalid_argument "blit_float64");
      c_blit_float64 x buf off

let blit_float80 x buf off =
   let len = String.length buf in
      if off + 10 > len then
         raise (Invalid_argument "blit_float80");
      c_blit_float80 x buf off
