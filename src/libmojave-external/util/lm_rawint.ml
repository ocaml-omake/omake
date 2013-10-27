(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001-2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

(*
 * Various forms of raw integers.
 *)
type int_precision =
   Int8
 | Int16
 | Int32
 | Int64

(*
 * Signed or unsigned.
 *)
type int_signed = bool

(*
 * Actual numbers.
 * Invariant: if ((p, s, i) is a rawint) i = cast p s i
 *)
type rawint = int_precision * int_signed * int64

let precision (pre, _, _) = pre
let signed (_, signed, _) = signed

(*
 * Test for zero or one.
 *)
let is_zero (_, _, i) =
   i = Int64.zero

let is_one (_, _, i) =
   i = Int64.one

(*
 * Truncate the value.
 *)
let neg_int8_mask  = Int64.of_int (-1 lsl 8)
let neg_int16_mask = Int64.of_int (-1 lsl 16)
let neg_int32_mask = Int64.shift_left (Int64.of_int (-1)) 32

let pos_int8_mask  = Int64.lognot neg_int8_mask
let pos_int16_mask = Int64.lognot neg_int16_mask
let pos_int32_mask = Int64.lognot neg_int32_mask

let cast pre signed i =
   if signed && compare i Int64.zero < 0 then
      match pre with
         Int8 ->
            Int64.logor i neg_int8_mask
       | Int16 ->
            Int64.logor i neg_int16_mask
       | Int32 ->
            Int64.logor i neg_int32_mask
       | Int64 ->
            i
   else
      match pre with
         Int8 ->
            Int64.logand i pos_int8_mask
       | Int16 ->
            Int64.logand i pos_int16_mask
       | Int32 ->
            Int64.logand i pos_int32_mask
       | Int64 ->
            i

let of_rawint pre signed (_, _, i) =
  pre, signed, cast pre signed i

(*
 * Min and max.
 *)
let max_int8_unsigned  = pos_int8_mask
let min_int8_signed    = Int64.of_int (-1 lsl 7)
let max_int8_signed    = Int64.lognot min_int8_signed

let max_int16_unsigned = pos_int16_mask
let min_int16_signed   = Int64.of_int (-1 lsl 15)
let max_int16_signed   = Int64.lognot min_int16_signed

let max_int32_unsigned = pos_int32_mask
let min_int32_signed   = Int64.shift_left (Int64.of_int (-1)) 31
let max_int32_signed   = Int64.lognot min_int32_signed

let max_int64_unsigned = Int64.lognot Int64.zero
let min_int64_signed   = Int64.shift_left (Int64.of_int (-1)) 63
let max_int64_signed   = Int64.lognot min_int64_signed

let max_int pre signed =
   let i =
      match pre, signed with
         Int8,  true  -> max_int8_signed
       | Int8,  false -> max_int8_unsigned
       | Int16, true  -> max_int16_signed
       | Int16, false -> max_int16_unsigned
       | Int32, true  -> max_int32_signed
       | Int32, false -> max_int32_unsigned
       | Int64, true  -> max_int64_signed
       | Int64, false -> max_int64_unsigned
   in
      pre, signed, i

let min_int pre signed =
   let i =
      if signed then
         match pre with
            Int8  -> min_int8_signed
          | Int16 -> min_int16_signed
          | Int32 -> min_int32_signed
          | Int64 -> min_int64_signed
      else
         Int64.zero
   in
      pre, signed, i

(*
 * Constructors.
 *)
let of_int pre signed i =
   pre, signed, cast pre signed (Int64.of_int i)

let of_float pre signed x =
   pre, signed, cast pre signed (Int64.of_float x)

let of_int32 pre signed i =
   pre, signed, cast pre signed (Int64.of_int32 i)

let of_int64 pre signed i =
   pre, signed, cast pre signed i

let of_nativeint pre signed i =
   pre, signed, cast pre signed (Int64.of_nativeint i)

let of_string pre signed s =
   pre, signed, Int64.of_string s

let to_byte (_, signed, i) =
   Int64.to_int (cast Int8 signed i)

let to_short (_, signed, i) =
   Int64.to_int (cast Int16 signed i)

let to_int64 (_, _, i) = i

let to_int (_, _, i) =       Int64.to_int i
let to_float (_, _, i) =     Int64.to_float i
let to_int32 (_, _, i) =     Int64.to_int32 i
let to_nativeint (_, _, i) = Int64.to_nativeint i
let to_string (_, _, i) =    Int64.to_string i

(*
 * Coercions.
 * Sizes always go up.
 * Signed always becomes unsigned
 *)
let max_pre pre1 pre2 =
   match pre1, pre2 with
      Int8,  Int8 -> Int8
    | Int8,  Int16
    | Int16, Int8
    | Int16, Int16 -> Int16
    | Int8,  Int32
    | Int16, Int32
    | Int32, Int8
    | Int32, Int16
    | Int32, Int32 -> Int32
    | Int8,  Int64
    | Int16, Int64
    | Int32, Int64
    | Int64, Int8
    | Int64, Int16
    | Int64, Int32
    | Int64, Int64 -> Int64

let max_signed sign1 sign2 =
   sign1 && sign2

(*
 * Arithmetic.
 *)
let unop op (pre, signed, i) =
   pre, signed, cast pre signed (op i)

let neg    = unop Int64.neg
let uminus = neg
let succ   = unop Int64.succ
let pred   = unop Int64.pred
let abs    = unop Int64.abs
let lognot = unop Int64.lognot

(*
 * Binary operators.
 *)
let binop op (pre1, signed1, i1) (pre2, signed2, i2) =
   let pre = max_pre pre1 pre2 in
   let signed = max_signed signed1 signed2 in
   let i1 = cast pre signed i1 in
   let i2 = cast pre signed i2 in
      pre, signed, cast pre signed (op i1 i2)

let add = binop Int64.add
let sub = binop Int64.sub
let mul = binop Int64.mul
let div = binop Int64.div
let rem = binop Int64.rem

(*
 * Bitwise operations.
 *)
let logand = binop Int64.logand
let logor  = binop Int64.logor
let logxor = binop Int64.logxor

let shift_mask pre j =
   let mask =
      match pre with
         Int8 | Int16 | Int32 -> 31
       | Int64 -> 63
   in
      Int64.to_int j land mask

let shift_left (pre, signed, i) (_, _, j) =
   let j = shift_mask pre j in
      pre, signed, cast pre signed (Int64.shift_left i j)

let shift_left_int (pre, signed, i) j =
   pre, signed, cast pre signed (Int64.shift_left i j)

let shift_right (pre, signed, i) (_, _, j) =
   let j = shift_mask pre j in
   let i =
      if signed then
         Int64.shift_right i j
      else
         Int64.shift_right_logical i j
   in
      pre, signed, cast pre signed i

(*
 * bit fields.
 *)
let field (pre, signed, i) off len =
   let i = Int64.shift_right i off in
   let mask = Int64.shift_left (Int64.of_int (-1)) len in
      if signed then
         pre, signed, Int64.logor i mask
      else
         pre, signed, Int64.logand i (Int64.lognot mask)

let set_field (pre, signed, i) off len (_, _, j) =
   let mask1 = Int64.lognot (Int64.shift_left (Int64.of_int (-1)) len) in
   let mask2 = Int64.lognot (Int64.shift_left mask1 off) in
   let i = Int64.logand i mask2 in
   let i = Int64.logor i (Int64.shift_left (Int64.logand j mask1) off) in
      pre, signed, i

(*
 * Comparison.
 *)
let compare (pre1, signed1, i1) (pre2, signed2, i2) =
   let pre = max_pre pre1 pre2 in
   let signed = max_signed signed1 signed2 in
   let i1 = cast pre signed i1 in
   let i2 = cast pre signed i2 in
      if signed then
         compare i1 i2
      else
         let neg1 = Int64.to_int (Int64.shift_right_logical i1 63) = 1 in
         let neg2 = Int64.to_int (Int64.shift_right_logical i2 63) = 1 in
            if neg1 then
               if neg2 then
                  compare i2 i1
               else
                  1
            else if neg2 then
               -1
            else
               compare i1 i2

let min i1 i2 =
   if compare i1 i2 < 0 then
      i1
   else
      i2

let max i1 i2 =
   if compare i1 i2 > 0 then
      i1
   else
      i2

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
