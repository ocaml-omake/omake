(*
 * Bignums are represented as signed magnitude.
 * The magnitude is a list of digits, least-significant
 * first.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type big_int = bool * int list

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Maximum digit size.
 *)
let shift_int = 15
let carry_int = 1 lsl shift_int
let max_int   = pred carry_int

(*
 * Representation of zero.
 *)
let zero_big_int = true, []

(*
 * Most-significant digits are zeros.
 *)
let rec zeros = function
   0 :: t ->
      zeros t
 | _ :: _ ->
      false
 | [] ->
      true

(*
 * Build from an int.
 *)
let rec make_mag i =
   if i = 0 then
      []
   else
      i land max_int :: make_mag (i lsr shift_int)

let big_int_of_int i =
   if i < 0 then
      false, make_mag (-i)
   else
      true, make_mag i

(*
 * Integer testing is conservative.
 *)
let is_integer_big_int = function
   _, ([] | [_] | [_; _]) ->
      true
 | _, _ :: _ :: mag ->
      zeros mag

let rec collect_big_int = function
   digit :: mag ->
      digit + ((collect_big_int mag) lsl shift_int)
 | [] ->
      0

let integer_big_int ((sign, mag) as i) =
   if not (is_integer_big_int i) then raise (Invalid_argument "Lm_big_int.integer_big_int: overflow");
   let mag = collect_big_int mag in
      if sign then
         mag
      else
         -mag

(************************************************************************
 * COMPARISON                                                           *
 ************************************************************************)

(*
 * Compare two magnitudes.
 *)
let rec compare_mag val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         let comp = compare_mag val1 val2 in
            if comp = 0 then
               digit1 - digit2
            else
               comp
    | [], [] ->
         0
    | [], t ->
         if zeros t then 0 else -1
    | t, [] ->
         if zeros t then 0 else 1

let compare_big_int (sign1, val1) (sign2, val2) =
   if sign1 then
      if sign2 then
         compare_mag val1 val2
      else
         1
   else if sign2 then
      -1
   else
      compare_mag val2 val1

let rec eq_mag val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         digit1 = digit2 && eq_mag val1 val2
    | [], [] ->
         true
    | [], val2 ->
         zeros val2
    | val1, [] ->
         zeros val1

let eq_big_int (sign1, val1) (sign2, val2) =
   sign1 = sign2 && eq_mag val1 val2

(************************************************************************
 * ADDITION/SUBTRACTIOB                                                 *
 ************************************************************************)

(*
 * Addition of the magnitudes.
 *)
let rec add_mag carry val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         add_digit carry digit1 digit2 val1 val2
    | [], val2 ->
         add_carry carry val2
    | val1, [] ->
         add_carry carry val1

and add_digit carry digit1 digit2 val1 val2 =
   let z =
      if carry then
         digit1 + digit2 + 1
      else
         digit1 + digit2
   in
      if z > max_int then
         z land max_int :: add_mag true val1 val2
      else
         z :: add_mag false val1 val2

and add_carry carry digits =
   if carry then
      match digits with
         digit :: vals ->
            if digit = max_int then
               0 :: add_carry true vals
            else
               succ digit :: vals
       | [] ->
            [1]
   else
      digits

(*
 * Subtraction of magnitudes.
 *)
let rec sub_mag borrow val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         sub_digit borrow digit1 digit2 val1 val2
    | val1, [] ->
         sub_borrow borrow val1
    | [], val2 ->
         if (zeros val2) && (not borrow) then [] else raise (Invalid_argument "Lm_big_int.sub_mag")

and sub_digit borrow digit1 digit2 val1 val2 =
   let z =
      if borrow then
         digit1 - digit2 - 1
      else
         digit1 - digit2
   in
      if z < 0 then
         z land max_int :: sub_mag true val1 val2
      else
         z :: sub_mag false val1 val2

and sub_borrow borrow val1 =
   if borrow then
      match val1 with
         digit1 :: val1 ->
            if digit1 = 0 then
               max_int :: sub_borrow true val1
            else
               pred digit1 :: val1
       | [] ->
            raise (Invalid_argument "Lm_big_int.sub_borrow")
   else
      val1

(*
 * Bigint addition.
 *)
let add_big_int (sign1, val1) (sign2, val2) =
   if sign1 then
      if sign2 then
         true, add_mag false val1 val2
      else
         let comp = compare_mag val1 val2 in
            if comp = 0 then
               zero_big_int
            else if comp < 0 then
               false, sub_mag false val2 val1
            else
               true, sub_mag false val1 val2
   else if sign2 then
      let comp = compare_mag val1 val2 in
         if comp = 0 then
            zero_big_int
         else if comp < 0 then
            true, sub_mag false val2 val1
         else
            false, sub_mag false val1 val2
   else
      false, add_mag false val1 val2

(*
 * Bigint subtraction.
 *)
let sub_big_int val1 (sign2, val2) =
   add_big_int val1 (not sign2, val2)

(************************************************************************
 * MULTIPLICATION                                                       *
 ************************************************************************)

(*
 * Long multiplication.
 *)
let rec mult_mag total val1 = function
   digit2 :: val2 ->
      mult_mag (add_mag false (mult_digit 0 digit2 val1) total) (shift_mag val1) val2
 | [] ->
      total

and mult_digit carry digit2 = function
   digit1 :: val1 ->
      let z = digit2 * digit1 + carry in
         z land max_int :: mult_digit (z lsr shift_int) digit2 val1
 | [] ->
      if carry <> 0 then
         [carry]
      else
         []

and shift_mag val1 =
   0 :: val1

(*
 * Bigint multiplication.
 *)
let mult_big_int (sign1, val1) (sign2, val2) =
   (sign1 = sign2, mult_mag [] val1 val2)

(************************************************************************
 * DIVISION                                                             *
 ************************************************************************)

(*
 * Right shift by one bit.
 *)
let rec div2 = function
   [digit] ->
      [digit lsr 1]
 | digit :: ((digit' :: _) as vals) ->
      (((digit + (digit' lsl shift_int)) lsr 1) land max_int) :: div2 vals
 | [] ->
      []

(*
 * We use a simple, expensive binary search.
 *)
let rec div_mag min max num den =
   let mid = div2 (add_mag false min max) in
      if eq_mag mid min then
         mid
      else
         let prod = mult_mag [] mid den in
         let comp = compare_mag prod num in
            if comp = 0 then
               mid
            else if comp > 0 then
               div_mag min mid num den
            else
               div_mag mid max num den

let div_big_int (sign1, val1) (sign2, val2) =
   if zeros val2 then raise (Invalid_argument "Lm_big_int.div_big_int: division by zero");
   sign1 = sign2, div_mag [] val1 val1 val2

let mod_mag num den =
   let quo = div_mag [] num num den in
      sub_mag false num (mult_mag [] den quo)

let mod_big_int (sign1, val1) (sign2, val2) =
   if zeros val2 then raise (Invalid_argument "Lm_big_int.mod_big_int: division by zero");
   sign1 = sign2, mod_mag val1 val2

let quo_big_int = div_big_int
let rem_big_int = mod_big_int

(*
 * Test for zero.
 *)
let is_zero_big_int (_, mag) =
   zeros mag

(*
 * Absolute value.
 *)
let abs_big_int (_, mag) =
   (true, mag)

let neg_big_int (sign, mag) =
   (not sign || zeros mag, mag)

(************************************************************************
 * STRING CONVERSION                                                    *
 ************************************************************************)

(*
 * Divide the number by 10.
 * Magnitude is in reverse order.
 *)
let div10_rev_mag digits =
   let rec collect rem = function
      digit :: digits ->
         let digit = digit + rem * (succ max_int) in
         let rem, digits = collect (digit mod 10) digits in
            rem, (digit / 10) :: digits
    | [] ->
         rem, []
   in
      collect 0 digits

let div10 (sign, mag) =
   let rem, mag = div10_rev_mag (List.rev mag) in
      rem, (sign, List.rev mag)

(*
 * Multiply a mag by a small int.
 *)
let rec mult_mag_i i =
   let rec collect carry = function
      digit :: digits ->
         let z =
            digit * i + carry
         in
            z land max_int :: collect (z lsr shift_int) digits
    | [] ->
         if carry = 0 then
            []
         else
            [carry]
   in
      collect 0

let mult2_mag = mult_mag_i 2
let mult8_mag = mult_mag_i 8
let mult10_mag = mult_mag_i 10
let mult16_mag = mult_mag_i 16

let mult10 (sign, mag) =
   sign, mult10_mag mag

let zero_code = Char.code '0'

let add_int mag i =
   let rec collect carry digits =
      if carry = 0 then
         digits
      else
         match digits with
            digit :: digits ->
               let z = digit + carry in
                  z land max_int :: collect (z lsr shift_int) digits
          | [] ->
               [carry]
   in
      collect i mag

(*
 * Produce a string.
 *)
let string_of_big_int (sign, mag) =
   let flatten = function
      [] ->
         "0"
    | digits ->
         let s, start =
            if sign then
               String.create (List.length digits), 0
            else
               String.make (succ (List.length digits)) '-', 1
         in
         let rec collect i = function
            digit :: digits ->
               s.[i] <- Char.chr (Char.code '0' + digit);
               collect (succ i) digits
          | [] ->
               s
         in
            collect start digits
   in
   let rec collect digits mag =
      if zeros mag then
         flatten digits
      else
         let rem, quo = div10_rev_mag mag in
            collect (rem :: digits) quo
   in
      collect [] (List.rev mag)

(*
 * Produce it from a string.
 *)
let big_int_of_string =
   let inv () = raise (Invalid_argument "Lm_big_int.big_int_of_string") in
   let rec collect mult char_code s len i mag =
      if i = len then
         mag
      else
         collect mult char_code s len (succ i) (add_int (mult mag) (char_code s.[i]))
   in
   let char_code2 = function
      '0' -> 0
    | '1' -> 1
    | _ -> inv ()
   in
   let char_code8 = function
      '0'..'7' as c -> Char.code c - zero_code
    | _ -> inv ()
   in
   let char_code10 = function
      '0'..'9' as c -> Char.code c - zero_code
    | _ -> inv ()
   in
   let char_code16 = function
      '0'..'9' as c -> Char.code c - zero_code
    | 'a' | 'A' -> 10
    | 'b' | 'B' -> 11
    | 'c' | 'C' -> 12
    | 'd' | 'D' -> 13
    | 'e' | 'E' -> 14
    | 'f' | 'F' -> 15
    | _ -> inv ()
   in
   fun s ->
      let len = String.length s in
      if len = 0 then inv ();
      let sign = not (s.[0] = '-') in
      if not sign && len = 1 then inv ();
      sign, begin
         let i = if sign then 0 else 1 in
         if s.[i] = '0' && len >= (i+3) then
            match s.[i+1] with
              'b' | 'B' -> collect mult2_mag char_code2 s len (i+2) []
            | 'o' | 'O' -> collect mult8_mag char_code8 s len (i+2) []
            | 'x' | 'X' -> collect mult16_mag char_code16 s len (i+2) []
            | _         -> collect mult10_mag char_code10 s len (i+2) []
         else
            collect mult10_mag char_code10 s len i []
      end

(*
 * Additional names.
 *)
let to_string = string_of_big_int
let of_string = big_int_of_string

(*
 * Produce a int32.
 *)
let to_int32 = function
   _, [] ->
      Int32.zero
 | _, digits ->
      let rec collect shift i digits =
         match digits with
            digit :: digits when shift < 32 ->
               let i = Int32.logor i (Int32.shift_left (Int32.of_int digit) shift) in
                  collect (shift + shift_int) i digits
          | _ ->
               i
      in
         collect 0 Int32.zero digits

let of_int32 i =
   let rec make_mag i =
      if i = Int32.zero then
         []
      else
         (Int32.to_int i) land max_int :: make_mag (Int32.shift_right i shift_int)
   in
      if i < Int32.zero then
         false, make_mag (Int32.neg i)
      else
         true, make_mag i

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
