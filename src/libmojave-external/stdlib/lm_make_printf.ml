(*
 * This is a generic printf builder.  We take "simple" printing
 * functions, and turn them into a general printf.
 *
 * Formatted printing.
 * Here are the format strings we handle.
 *    d or i: print an integer in decminal
 *    u: print an unsigned integer in decimal
 *    x: print an integer in unsigned hex in lowercase
 *    X: print an integer in unsigned hex in uppercase
 *    o: print an integer in unsigned octal
 *    s: print a string
 *    c: print a character
 *    f: print a float in decimal
 *    e,E: print a float in exponent notation
 *    g,G: print a float in best notation
 *    b: print a Boolean
 *    a: user-defined printer
 *    t: user-defined printer
 *    %: print the '%' char
 *
 * From the printf man page, each format specifier has
 *    1. 0 or more flags
 *       #: use alternate notation
 *       0: 0-pad the number
 *       '-': left-justify the field
 *       ' ': leave a space before the number
 *       '+': always print the sign of the number
 *    2. An optional field width in decimal
 *    3. An optional precision, specified as a '.' followed
 *       by a decimal number.
 *    4. A format specifier
 *
 * For Format:
 *    @]: close_box
 *    @,: print_cut
 *    @ : print_space
 *    @\n: force_newline
 *    @;: print_break
 *    @?: print_flush
 *    @.: print_newline
 *    @<n>: print_length
 *    @@: plain @ char
 *
 * Note the copious use of Obj.magic
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
 * Argument module.
 *)
module type PrintfArgsSig =
sig
   (* Some buffer type *)
   type t
   type result

   (* The printers *)
   val print_char : t -> char -> unit
   val print_string : t -> string -> unit

   (* Format functions *)
   val open_box : t -> int -> unit
   val open_hbox : t -> unit
   val open_vbox : t -> int -> unit
   val open_hvbox : t -> int -> unit
   val open_hovbox : t -> int -> unit
   val close_box : t -> unit

   val print_cut : t -> unit
   val print_space : t -> unit
   val force_newline : t -> unit
   val print_break : t -> int -> int -> unit
   val print_flush : t -> unit
   val print_newline : t -> unit

   val exit : t -> result
end

(*
 * What this module provides.
 *)
module type PrintfSig =
sig
   (* Some buffer type *)
   type t
   type result

   (* Lm_printf functions *)
   val fprintf : t -> ('a, t, result) format -> 'a
end

(*
 * Here's the actual printf module.
 *)
module MakePrintf (Args : PrintfArgsSig) =
struct
   (************************************************************************
    * TYPES
    ************************************************************************)

   type t = Args.t
   type result = Args.result

   (*
    * Field flags.
    *)
   type format_flag =
      AlternateForm
    | ZeroPad
    | LeftAdjust
    | LeaveBlank
    | AlwaysSign

   (*
    * Types of int printing.
    *)
   type int_spec =
      UnsignedOctal
    | SignedDecimal
    | UnsignedDecimal
    | UnsignedHexLowercase
    | UnsignedHexUppercase

   (*
    * Types of float printing.
    *)
   type float_spec =
      FloatNormal
    | FloatExp
    | FloatBest

   (*
    * Field specifiers.
    *)
   type field_info =
      { field_flags : format_flag list;
        field_width : int option;
        field_precision : int option
      }

   (************************************************************************
    * BASE PRINTERS
    ************************************************************************)

   (*
    * Most of these basic printers just pass out the print
    * request to C functions.  The first string is the format
    * string to be passed to printf.
    *)
   external ext_print_char : string -> char -> string     = "ml_print_char"
   external ext_print_int : string -> int -> string       = "ml_print_int"
   external ext_print_float : string -> float -> string   = "ml_print_float"
   external ext_print_string : string -> string -> string = "ml_print_string"
   external ext_print_string2 : int -> string -> string -> string = "ml_print_string2"

   (*
    * Maximum width specified in the format.
    *)
   let max_width info =
      match info with
         { field_width = Some width; field_precision = Some pre } ->
            max width pre
       | { field_width = Some width; field_precision = None }
       | { field_width = None; field_precision = Some width } ->
            width
       | { field_width = None; field_precision = None } ->
            0

   (*
    * Next arg should be an int.
    *)
   let rec print_bool buf i len s fmt _info =
      let print b =
         let str = ext_print_string fmt (if b then "true" else "false") in
            Args.print_string buf str;
            print_loop buf i len s
      in
         Obj.magic print

   and print_char buf i len s fmt _info =
      let print c =
         let str = ext_print_char fmt c in
            Args.print_string buf str;
            print_loop buf i len s
      in
         Obj.magic print

   and print_int buf i len s fmt _info _spec =
      let print x =
         let str = ext_print_int fmt x in
            Args.print_string buf str;
            print_loop buf i len s
      in
         Obj.magic print

   and print_float buf i len s fmt _info _spec =
      let print x =
         let str = ext_print_float fmt x in
            Args.print_string buf str;
            print_loop buf i len s
      in
         Obj.magic print

   and print_string buf i len s fmt info =
      let print str =
         let str = ext_print_string2 (max_width info) fmt str in
            Args.print_string buf str;
            print_loop buf i len s
      in
         Obj.magic print

   and print_user1 buf i len s _fmt _info =
      let print f =
         f buf;
         print_loop buf i len s
      in
         Obj.magic print

   and print_user2 buf i len s _fmt _info =
      let print f x =
         f buf x;
         print_loop buf i len s
      in
         Obj.magic print

   and print_dummy _buf _i _len _s =
      Obj.magic ()

   and print_percent buf i len s fmt _info =
      let str = ext_print_string fmt "%" in
         Args.print_string buf str;
         print_loop buf i len s

   (*
    * Parse the format specification.
    *)
   and print_format buf index len s =
      (*
       * Read off any flag characters.
       *)
      let rec parse_flags flags i =
         if i = len then
            print_dummy buf i len s
         else
            let c = s.[i] in
               match c with
                  '#' ->
                     parse_flags (AlternateForm :: flags) (succ i)
                | '0' ->
                     parse_flags (ZeroPad :: flags) (succ i)
                | '-' ->
                     parse_flags (LeftAdjust :: flags) (succ i)
                | ' ' ->
                     parse_flags (LeaveBlank :: flags) (succ i)
                | '+' ->
                     parse_flags (AlwaysSign :: flags) (succ i)
                | '1'..'9' ->
                     parse_field_width flags 0 i
                | '.' ->
                     parse_precision flags None 0 (succ i)
                | _ ->
                     parse_spec flags None None i

      (*
       * Read off the field width.
       *)
      and parse_field_width flags width i =
         if i = len then
            print_dummy buf i len s
         else
            let c = s.[i] in
               match c with
                  '0'..'9' ->
                     parse_field_width flags (width * 10 + Char.code c - Char.code '0') (succ i)
                | '.' ->
                     parse_precision flags (Some width) 0 (succ i)
                | _ ->
                     parse_spec flags (Some width) None i

      (*
       * Parse the precision specifier.
       *)
      and parse_precision flags width pre i =
         if i = len then
            print_dummy buf i len s
         else
            let c = s.[i] in
               match c with
                  '0'..'9' ->
                     parse_precision flags width (pre * 10 + Char.code c - Char.code '0') (succ i)
                | _ ->
                     parse_spec flags width (Some pre) i

      (*
       * Finally we have the format specifier.
       *)
      and parse_spec flags width pre i =
         if i = len then
            print_dummy buf i len s
         else
            let info =
               { field_flags = List.rev flags;
                 field_width = width;
                 field_precision = pre
               }
            in
            let c = s.[i] in
            let i = succ i in
            let fmt = String.sub s index (i - index) in
               match c with
                  'd' | 'i' -> print_int     buf i len s fmt info SignedDecimal
                | 'u'       -> print_int     buf i len s fmt info UnsignedDecimal
                | 'x'       -> print_int     buf i len s fmt info UnsignedHexLowercase
                | 'X'       -> print_int     buf i len s fmt info UnsignedHexUppercase
                | 'o'       -> print_int     buf i len s fmt info UnsignedOctal
                | 's'       -> print_string  buf i len s fmt info
                | 'c'       -> print_char    buf i len s fmt info
                | 'f'       -> print_float   buf i len s fmt info FloatNormal
                | 'e' | 'E' -> print_float   buf i len s fmt info FloatExp
                | 'g' | 'G' -> print_float   buf i len s fmt info FloatBest
                | 'b'       -> print_bool    buf i len s fmt info
                | 'a'       -> print_user2   buf i len s fmt info
                | 't'       -> print_user1   buf i len s fmt info
                | '%'       -> print_percent buf i len s fmt info
                | _         -> raise (Invalid_argument "parse_spec")
      in
         parse_flags [] (succ index)

   (************************************************************************
    * FORMAT CONTROL
    ************************************************************************)

   (*
    * Parse the format string.
    *)
   and print_rformat buf index len s =
      (*
       * Look for some options in <options> format,
       * separated by white space.
       *)
      let scratch = Buffer.create 19 in
      let rec parse_options i cont =
         if i = len then
            cont i []
         else
            let c = s.[i] in
               if c = '<' then
                  parse_args [] (succ i) cont
               else
                  cont i []

      and parse_args options i cont =
         if i = len then
            cont i (List.rev options)
         else
            let c = s.[i] in
               match c with
                  ' ' | '\t' | '\n' ->
                     if Buffer.length scratch <> 0 then
                        let s = Buffer.contents scratch in
                           Buffer.clear scratch;
                           parse_args (s :: options) (succ i) cont
                     else
                        parse_args options (succ i) cont
                | '>' ->
                     cont (succ i) (List.rev options)
                | c ->
                     Buffer.add_char scratch c;
                     parse_args options (succ i) cont
      in

      (*
       * Now we should have the spec.
       *)
      let parse_spec i =
         if i = len then
            print_dummy buf i len s
         else
            let c = s.[i] in
            let i = succ i in
               match c with
                  '[' ->
                     parse_options i (fun i options ->
                           let box, indent =
                              match options with
                                 box :: indent :: _ ->
                                    box, int_of_string indent
                               | [box] ->
                                    box, 0
                               | [] ->
                                    "b", 0
                           in
                           let _ =
                              match box with
                                 "h" -> Args.open_hbox buf
                               | "v" -> Args.open_vbox buf indent
                               | "hv" -> Args.open_hvbox buf indent
                               | "hov" -> Args.open_hovbox buf indent
                               | "b" -> Args.open_box buf indent
                               | _ -> raise (Invalid_argument ("print_rformat: bogus box type \"" ^ String.escaped box ^ "\""))
                           in
                              print_loop buf i len s)
                | ']' ->
                     Args.close_box buf;
                     print_loop buf i len s
                | ',' ->
                     Args.print_cut buf;
                     print_loop buf i len s
                | ' ' ->
                     Args.print_space buf;
                     print_loop buf i len s
                | '\n' ->
                     Args.force_newline buf;
                     print_loop buf i len s
                | ';' ->
                     parse_options i (fun i options ->
                           let nspaces, off =
                              match options with
                                 nspaces :: off :: _ -> int_of_string nspaces, int_of_string off
                               | [nspaces] -> int_of_string nspaces, 0
                               | [] -> 0, 0
                           in
                              Args.print_break buf nspaces off;
                              print_loop buf i len s)
                | '?' ->
                     Args.print_flush buf;
                     print_loop buf i len s
                | '.' ->
                     Args.print_newline buf;
                     print_loop buf i len s
                | c ->
                     Args.print_char buf c;
                     print_loop buf i len s
        in
           parse_spec (succ index)

   (*
    * The is the main printf function.
    *)
   and print_loop buf i len s =
      if i = len then
         Obj.magic (Args.exit buf)
      else
         match s.[i] with
            '%' ->
               print_format buf i len s
          | '@' ->
               print_rformat buf i len s
          | c ->
               Args.print_char buf c;
               print_loop buf (succ i) len s

   (*
    * Outermost printf function.
    *)
   let fprintf buf =
      let print s =
         print_loop buf 0 (String.length s) s
      in
         Obj.magic print
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
