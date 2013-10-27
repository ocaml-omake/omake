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

(* Various forms of raw floats *)
type float_precision =
   Single
 | Double
 | LongDouble

type rawfloat = float_precision * Lm_float80.float80

(*
 * Operations.
 *)
let precision (pre, _) =
   pre

(*
 * Conversions.
 *)
let to_string (_, x) =
   Lm_float80.to_string x

let to_float (_, x) =
   Lm_float80.to_float x

let to_float80 (_, x) =
   x

let to_int64 (_, x) =
   Lm_float80.to_int64 x

let to_int (_, x) =
   Lm_float80.to_int x

let to_rawint p s (_, x) =
   Lm_rawint.of_int64 p s (Lm_float80.to_int64 x)

let of_int pre x =
   pre, Lm_float80.of_int x

let of_float pre x =
   pre, Lm_float80.of_float x

let of_float80 pre x =
   pre, x

let of_rawfloat pre (_, x) =
   pre, x

let of_rawint pre i =
   let j = Lm_rawint.to_int64 i in
   let signed = Lm_rawint.signed i in
   let x = Lm_float80.of_int64 j in
   let x =
      if not signed && j < Int64.zero then
         Lm_float80.neg x
      else
         x
   in
      pre, x

let of_string pre s =
   pre, Lm_float80.of_string s

(*
 * Arithmetic.
 *)
let neg (pre, x) = (pre, Lm_float80.neg x)
let abs (pre, x) = (pre, Lm_float80.abs x)
let sin (pre, x) = (pre, Lm_float80.sin x)
let cos (pre, x) = (pre, Lm_float80.cos x)
let sqrt (pre, x) = (pre, Lm_float80.sqrt x)
let add (pre, x) (_, y) = (pre, Lm_float80.add x y)
let sub (pre, x) (_, y) = (pre, Lm_float80.sub x y)
let mul (pre, x) (_, y) = (pre, Lm_float80.mul x y)
let div (pre, x) (_, y) = (pre, Lm_float80.div x y)
let rem (pre, x) (_, y) = (pre, Lm_float80.rem x y)
let atan2 (pre, x) (_, y) = (pre, Lm_float80.atan2 x y)
let succ (pre, x) = (pre, Lm_float80.succ x)
let pred (pre, x) = (pre, Lm_float80.pred x)

let uminus = neg

(*
 * Comparison.
 *)
let is_zero (_, x) =
   Lm_float80.is_zero x

let compare (_, x) (_, y) =
   Lm_float80.compare x y

let min x y =
   if compare x y < 0 then
      x
   else
      y

let max x y =
   if compare x y > 0 then
      x
   else
      y
