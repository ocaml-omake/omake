(*
 * A generic marshaler.
 * For marshaling, we need a
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001 Jason Hickey, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf

(*
 * All items eventually become ints, floats, or strings.
 *)
type 'a item =
   Bool of bool
 | Char of char
 | Code of int
 | Symbol of int
 | Int of int
 | Magic of 'a
 | Float of float
 | String of string
 | List of 'a item list

(************************************************************************
 * MARSHALING
 ************************************************************************)

(*
 * IO module.
 *)
module type MarshalIOSig =
sig
   type t
   type in_channel
   type out_channel

   (* Convert between magic ids *)
   val magic_of_int : int -> t
   val int_of_magic : t -> int

   (* IO *)
   val input_byte    : in_channel -> int
   val input_buffer  : in_channel -> string -> int -> int -> unit
   val output_byte   : out_channel -> int -> unit
   val output_buffer : out_channel -> string -> int -> int -> unit
end

(*
 * Marshal module.
 *)
module type MarshalSig =
sig
   type t
   type in_channel
   type out_channel

   val marshal : out_channel -> t item -> unit
   val unmarshal : in_channel -> t item
end

(*
 * Output routines.
 *)
module Make (IO : MarshalIOSig)
: MarshalSig
  with type t = IO.t
  with type in_channel = IO.in_channel
  with type out_channel = IO.out_channel =
struct
   type t = IO.t
   type in_channel = IO.in_channel
   type out_channel = IO.out_channel

   (*
    * Codes.
    *)
   let true_magic     = 0xe0
   let false_magic    = 0xe1
   let char_magic     = 0xe2
   let code_magic     = 0xe3
   let symbol_magic   = 0xe4
   let int_magic      = 0xe5
   let magic_magic    = 0xe6
   let rawint_magic   = 0xe7
   let float_magic    = 0xe8
   let rawfloat_magic = 0xe9
   let string_magic   = 0xea
   let list_magic     = 0xeb

   let version_number = Hashtbl.hash "$Id$"

   (************************************************************************
    * BASIC IO
    ************************************************************************)

   (*
    * Basic routines.
    *)
   let output_byte out i =
      IO.output_byte out (i land 0xff)

   let input_byte inc =
      IO.input_byte inc

   let output_bool out b =
      output_byte out (if b then 1 else 0)

   let input_bool inc =
      match input_byte inc with
         0 -> false
       | 1 -> true
       | _ -> raise (Failure "input_bool: input is not Boolean")

   let output_char out c =
      output_byte out (Char.code c)

   let input_char inc =
      Char.chr (input_byte inc)

   let output_int out i =
      output_byte out ((i lsr 24) land 0xff);
      output_byte out ((i lsr 16) land 0xff);
      output_byte out ((i lsr 8) land 0xff);
      output_byte out (i land 0xff)

   let input_int inc =
      let i = input_byte inc in
      let i = (i lsl 8) lor (input_byte inc) in
      let i = (i lsl 8) lor (input_byte inc) in
      let i = (i lsl 8) lor (input_byte inc) in
         i

   let output_int16 out i =
      output_byte out ((i lsr 8) land 0xff);
      output_byte out (i land 0xff)

   let input_int16 inc =
      let i1 = input_byte inc in
      let i2 = input_byte inc in
         (i1 lsl 8) lor i2

   let output_int32 out i =
      output_byte out ((Int32.to_int (Int32.shift_right i 24)) land 0xff);
      output_byte out ((Int32.to_int (Int32.shift_right i 16)) land 0xff);
      output_byte out ((Int32.to_int (Int32.shift_right i 8)) land 0xff);
      output_byte out ((Int32.to_int i) land 0xff)

   let input_int32 inc =
      let i = Int32.shift_left (Int32.of_int (input_byte inc)) 24 in
      let i = Int32.logor i (Int32.shift_left (Int32.of_int (input_byte inc)) 16) in
      let i = Int32.logor i (Int32.shift_left (Int32.of_int (input_byte inc)) 8) in
      let i = Int32.logor i (Int32.of_int (input_byte inc)) in
         i

   let output_int64 out i =
      output_byte out ((Int64.to_int (Int64.shift_right i 56)) land 0xff);
      output_byte out ((Int64.to_int (Int64.shift_right i 48)) land 0xff);
      output_byte out ((Int64.to_int (Int64.shift_right i 40)) land 0xff);
      output_byte out ((Int64.to_int (Int64.shift_right i 32)) land 0xff);
      output_byte out ((Int64.to_int (Int64.shift_right i 24)) land 0xff);
      output_byte out ((Int64.to_int (Int64.shift_right i 16)) land 0xff);
      output_byte out ((Int64.to_int (Int64.shift_right i 8)) land 0xff);
      output_byte out ((Int64.to_int i) land 0xff)

   let input_int64 inc =
      let i = Int64.shift_left (Int64.of_int (input_byte inc)) 56 in
      let i = Int64.logor i (Int64.shift_left (Int64.of_int (input_byte inc)) 48) in
      let i = Int64.logor i (Int64.shift_left (Int64.of_int (input_byte inc)) 40) in
      let i = Int64.logor i (Int64.shift_left (Int64.of_int (input_byte inc)) 32) in
      let i = Int64.logor i (Int64.shift_left (Int64.of_int (input_byte inc)) 24) in
      let i = Int64.logor i (Int64.shift_left (Int64.of_int (input_byte inc)) 16) in
      let i = Int64.logor i (Int64.shift_left (Int64.of_int (input_byte inc)) 8) in
      let i = Int64.logor i (Int64.of_int (input_byte inc)) in
         i

   (*
    * Floats.
    *)
   let output_float out x =
      output_int64 out (Int64.bits_of_float x)

   let input_float inc =
      Int64.float_of_bits (input_int64 inc)

   (*
    * Meta.
    *)
   let output_magic = output_byte
   let input_magic = input_byte

   let output_size out i =
      if i >= 0 && i < 255 then
         output_byte out i
      else
         begin
            output_byte out 255;
            output_int out i
         end

   let input_size inc =
      let i = input_byte inc in
         if i < 255 then
            i
         else
            input_int inc

   (************************************************************************
    * MARSHALING
    ************************************************************************)

   (*
    * Marshal base types.
    *)
   let marshal_bool out b =
      (* eprintf "Bool: %b%t" b eflush; *)
      if b then
         output_magic out true_magic
      else
         output_magic out false_magic

   let marshal_char out c =
      output_magic out char_magic;
      output_char out c

   let marshal_code out i =
      (* eprintf "Int: %d%t" i eflush; *)
      output_magic out code_magic;
      output_int16 out i

   let marshal_symbol out i =
      (* eprintf "Int: %d%t" i eflush; *)
      output_magic out symbol_magic;
      output_int16 out i

   let marshal_int out i =
      (* eprintf "Int: %d%t" i eflush; *)
      output_magic out int_magic;
      output_int out i

   let marshal_magic out x =
      let index = IO.int_of_magic x in
         (* eprintf "Magic: %d%t" index eflush; *)
         output_magic out magic_magic;
         output_int out index

   let marshal_float out x =
      (* eprintf "Float: %g%t" x eflush; *)
      output_magic out float_magic;
      output_float out x

   let marshal_string out s =
      let len = String.length s in
         (* eprintf "String: %s%t" s eflush; *)
         output_magic out string_magic;
         output_size out len;
         IO.output_buffer out s 0 len

   (*
    * Marshaler
    *)
   let rec marshal_item out x =
      match x with
         Bool b -> marshal_bool out b
       | Char c -> marshal_char out c
       | Code i -> marshal_code out i
       | Symbol i -> marshal_symbol out i
       | Int i -> marshal_int out i
       | Magic i -> marshal_magic out i
       | Float x -> marshal_float out x
       | String s -> marshal_string out s
       | List l -> marshal_list out l

   and marshal_list out l =
      let len = List.length l in
         (* eprintf "List: %d%t" len eflush; *)
         output_magic out list_magic;
         output_size out len;
         List.iter (marshal_item out) l

   (*
    * Save the version number.
    *)
   let marshal_version out =
      output_int out version_number

   (*
    * First collect strings and save them.
    *)
   let marshal out l =
         marshal_version out;
         marshal_item out l

   (************************************************************************
    * UNMARSHALING
    ************************************************************************)

   (*
    * Reading.
    *)
   let unmarshal_bool b =
      (* eprintf "Bool: %b%t" b eflush; *)
      Bool b

   let unmarshal_magic inc =
      let i = input_int inc in
         (* eprintf "Magic: %d%t" i eflush; *)
         Magic (IO.magic_of_int i)

   let unmarshal_char inc =
      let c = input_char inc in
         (* eprintf "Char: %c%t" c eflush; *)
         Char c

   let unmarshal_code inc =
      let i = input_int16 inc in
         (* eprintf "Int: %d%t" i eflush; *)
         Code i

   let unmarshal_symbol inc =
      let i = input_int16 inc in
         (* eprintf "Int: %d%t" i eflush; *)
         Symbol i

   let unmarshal_int inc =
      let i = input_int inc in
         (* eprintf "Int: %d%t" i eflush; *)
         Int i

   let unmarshal_float inc =
      let x = input_float inc in
         (* eprintf "Float: %g%t" x eflush; *)
         Float x

   let unmarshal_string inc =
      let len = input_size inc in
      let _ =
         if len < 0 then
            raise (Failure "unmarshal_string: string length is negative")
      in
      let s = String.create len in
         IO.input_buffer inc s 0 len;
         (* eprintf "String: %s%t" s eflush; *)
         String s

   (*
    * Build a value from the input.
    *)
   let rec unmarshal_item inc =
      let magic = input_magic inc in
         if magic = true_magic then
            unmarshal_bool true
         else if magic = false_magic then
            unmarshal_bool false
         else if magic = char_magic then
            unmarshal_char inc
         else if magic = code_magic then
            unmarshal_code inc
         else if magic = symbol_magic then
            unmarshal_symbol inc
         else if magic = int_magic then
            unmarshal_int inc
         else if magic = magic_magic then
            unmarshal_magic inc
         else if magic = float_magic then
            unmarshal_float inc
         else if magic = string_magic then
            unmarshal_string inc
         else if magic = list_magic then
            unmarshal_list inc
         else
            raise (Failure (sprintf "unmarshal: unexpected magic number 0x%02x" magic))

   and unmarshal_list inc =
      let len = input_size inc in
      (* let _ = eprintf "List: %d%t" len eflush in *)
      let rec collect i l =
         if i = 0 then
            List (List.rev l)
         else
            let x = unmarshal_item inc in
               collect (pred i) (x :: l)
      in
         collect len []

   (*
    * Read the version number.
    *)
   let unmarshal_version inc =
      let i = input_int inc in
         if i <> version_number then
            raise (Failure (sprintf "unmarshal_version: bogus version number: 0x%08x, should be 0x%08x" i version_number))

   (*
    * Now read the data.
    *)
   let unmarshal inc =
      unmarshal_version inc;
      unmarshal_item inc
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
