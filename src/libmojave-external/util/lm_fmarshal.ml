(*
 * A generic marshaler.
 * For marshaling, we need a
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
open Lm_printf

(*
 * All items eventually become ints, floats, or strings.
 *)
type 'a item =
   Bool of bool
 | Int of int
 | Magic of 'a
 | Lm_rawint of Lm_rawint.rawint
 | Float of float
 | Lm_rawfloat of Lm_rawfloat.rawfloat
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
   val input_byte : in_channel -> int
   val input_buffer : in_channel -> string -> int -> int -> unit
   val output_byte : out_channel -> int -> unit
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
   let int_magic      = 0xe2
   let magic_magic    = 0xe3
   let rawint_magic   = 0xe4
   let float_magic    = 0xe5
   let rawfloat_magic = 0xe6
   let string_magic   = 0xe7
   let list_magic     = 0xe8

   let version_number = Hashtbl.hash "$Id: lm_fmarshal.ml 9445 2006-07-15 17:23:37Z jyh $"

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
    * Rawints.
    *)
   let output_rawint_params out p s =
      let p =
         match p with
            Lm_rawint.Int8 -> 8
          | Lm_rawint.Int16 -> 16
          | Lm_rawint.Int32 -> 32
          | Lm_rawint.Int64 -> 64
      in
         output_byte out p;
         output_bool out s

   let output_rawfloat_params out p =
      let p =
         match p with
            Lm_rawfloat.Single -> 1
          | Lm_rawfloat.Double -> 2
          | Lm_rawfloat.LongDouble -> 3
      in
         output_byte out p

   let output_rawint out i =
      let p = Lm_rawint.precision i in
      let s = Lm_rawint.signed i in
         output_rawint_params out p s;
         match p with
            Lm_rawint.Int8  -> output_byte  out ((Lm_rawint.to_int i) land 0xff)
          | Lm_rawint.Int16 -> output_int16 out ((Lm_rawint.to_int i) land 0xffff)
          | Lm_rawint.Int32 -> output_int32 out (Lm_rawint.to_int32 i)
          | Lm_rawint.Int64 -> output_int64 out (Lm_rawint.to_int64 i)

   let output_rawfloat out x =
      let p = Lm_rawfloat.precision x in
         output_rawfloat_params out p;
         match p with
            Lm_rawfloat.Single -> output_float out (Lm_rawfloat.to_float x)
          | Lm_rawfloat.Double -> output_float out (Lm_rawfloat.to_float x)
          | Lm_rawfloat.LongDouble -> output_float out (Lm_rawfloat.to_float x)

   let input_rawint_params inc =
      let p =
         match input_byte inc with
            8 -> Lm_rawint.Int8
          | 16 -> Lm_rawint.Int16
          | 32 -> Lm_rawint.Int32
          | 64 -> Lm_rawint.Int64
          | _ -> raise (Failure "input_rawint_params: bogus precision")
      in
      let s = input_bool inc in
         p, s

   let input_rawfloat_params inc =
      let p =
         match input_byte inc with
            1 -> Lm_rawfloat.Single
          | 2 -> Lm_rawfloat.Double
          | 3 -> Lm_rawfloat.LongDouble
          | _ -> raise (Failure "input_rawfloat_params: bogus precision")
      in
         p

   let input_rawint inc =
      let p, s = input_rawint_params inc in
         match p with
            Lm_rawint.Int8 -> Lm_rawint.of_int p s (input_byte inc)
          | Lm_rawint.Int16 -> Lm_rawint.of_int p s (input_int16 inc)
          | Lm_rawint.Int32 -> Lm_rawint.of_int32 p s (input_int32 inc)
          | Lm_rawint.Int64 -> Lm_rawint.of_int64 p s (input_int64 inc)

   let input_rawfloat inc =
      let p = input_rawfloat_params inc in
         match p with
            Lm_rawfloat.Single -> Lm_rawfloat.of_float p (input_float inc)
          | Lm_rawfloat.Double -> Lm_rawfloat.of_float p (input_float inc)
          | Lm_rawfloat.LongDouble -> Lm_rawfloat.of_float p (input_float inc)

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

   let marshal_int out i =
      (* eprintf "Int: %d%t" i eflush; *)
      output_magic out int_magic;
      output_int out i

   let marshal_magic out x =
      let index = IO.int_of_magic x in
         (* eprintf "Magic: %d%t" index eflush; *)
         output_magic out magic_magic;
         output_int out index

   let marshal_rawint out i =
      (* eprintf "Lm_rawint: %s%t" (Lm_rawint.to_string i) eflush; *)
      output_magic out rawint_magic;
      output_rawint out i

   let marshal_float out x =
      (* eprintf "Float: %g%t" x eflush; *)
      output_magic out float_magic;
      output_float out x

   let marshal_rawfloat out x =
      output_magic out rawfloat_magic;
      output_rawfloat out x

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
       | Int i -> marshal_int out i
       | Magic i -> marshal_magic out i
       | Lm_rawint i -> marshal_rawint out i
       | Float x -> marshal_float out x
       | Lm_rawfloat x -> marshal_rawfloat out x
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

   let unmarshal_int inc =
      let i = input_int inc in
         (* eprintf "Int: %d%t" i eflush; *)
         Int i

   let unmarshal_rawint inc =
      let i = input_rawint inc in
         (* eprintf "Lm_rawint: %s%t" (Lm_rawint.to_string i) eflush; *)
         Lm_rawint i

   let unmarshal_float inc =
      let x = input_float inc in
         (* eprintf "Float: %g%t" x eflush; *)
         Float x

   let unmarshal_rawfloat inc =
      let x = input_rawfloat inc in
         Lm_rawfloat x

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
         else if magic = int_magic then
            unmarshal_int inc
         else if magic = magic_magic then
            unmarshal_magic inc
         else if magic = rawint_magic then
            unmarshal_rawint inc
         else if magic = float_magic then
            unmarshal_float inc
         else if magic = rawfloat_magic then
            unmarshal_rawfloat inc
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
            raise (Failure (sprintf "unmarshal_version: bogus version number: %d" i))

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
