(*
 * Lm_flags environment
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002,2001 Justin David Smith, Caltech
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
open Lm_string_set
open Lm_printf


(***  Environment to Store Lm_flags  ***)


(*
 * For flags environment, indicate the type of the value recorded.
 *)
type flag_value =
   FlagBool of bool
 | FlagInt of int


(*
 * Flag help text is indexed on the name of the flag.  The first string is
 * the description for the help section, and the table is help text specific
 * to each individual flag.  The help sections themselves are indexed on a
 * named tag, usually the prefix of all the flags in that section.
 *)
type flag_help_section = string * (string StringTable.t)


(*
 * The flags environment contains the environment of currently set flags, as
 * well as a registry of ``valid names'' and their default values.  You can
 * define new names which do not appear in the defaults table, however you
 * cannot lookup a name that appears neither in the current environment nor
 * the defaults.
 *)
type flags =
   { flag_values     :  flag_value StringTable.t;  (* current environment *)
     flag_defaults   :  flag_value StringTable.t;  (* registered names & defaults *)
     flag_help       :  flag_help_section StringTable.t;
   }


(***  Flag Environment Handlers  ***)


(*
 * Simple (low-level) manipulations on the flags environment.
 *)
let flags_empty =
   { flag_values     =  StringTable.empty;
     flag_defaults   =  StringTable.empty;
     flag_help       =  StringTable.empty;
   }

let flags_set flags name value =
   if StringTable.mem flags.flag_defaults name then
      { flags with flag_values = StringTable.add flags.flag_values name value }
   else
      raise (Failure ("flags_set: flag " ^ name ^ " not defined"))

let flags_is_set flags name =
   StringTable.mem flags.flag_values name

let flags_get flags name =
   try
      StringTable.find flags.flag_values name
   with
      Not_found ->
         StringTable.find flags.flag_defaults name


(*
 * Set the informational text for a help section.
 *)
let flags_help_section_text flags help_tag text =
   let help = flags.flag_help in
   let help_section =
      try
         snd (StringTable.find help help_tag)
      with
         Not_found ->
            StringTable.empty
   in
   let help = StringTable.add help help_tag (text, help_section) in
      { flags with flag_help = help }


(*
 * Add a new binding for a particular flag in the help text.
 *)
let flags_register_help flags name help_tag help_text =
   let help = flags.flag_help in
   let help_section_text, help_section =
      try
         StringTable.find help help_tag
      with
         Not_found ->
            help_tag, StringTable.empty
   in
   let help_section = StringTable.add help_section name help_text in
   let help = StringTable.add help help_tag (help_section_text, help_section) in
      help


(*
 * Register a new optimization flag (or multiple flags).
 *)
let flags_register_help flags name default help_tag help_text =
   { flags with
      flag_defaults  = StringTable.add flags.flag_defaults name default;
      flag_help      = flags_register_help flags name help_tag help_text;
   }

let flags_register flags name default =
   flags_register_help flags name default "other" "no help available"

let flags_register_list flags names =
   List.fold_left (fun flags (name, default) -> flags_register flags name default) flags names

let flags_register_list_help flags help_tag names =
   List.fold_left (fun flags (name, default, help_text) ->
      flags_register_help flags name default help_tag help_text) flags names


(*
 * Display the currently-registered names.
 *)
let print_text_block text =
   let length = String.length text in
   let rec print_chars n =
      if n < length then begin
         (match text.[n] with
            ' ' ->
               printf "@ "
          | _ as c ->
               printf "%c" c);
         print_chars (succ n)
      end
   in
      printf "@[<hov 0>";
      print_chars 0;
      printf "@]"

let print_flag_defaults_section flags help_tag help_section_text help_section =
   printf "@[<v 3>%s:  " help_tag;
   print_text_block help_section_text;
   StringTable.iter (fun flag text ->
      let default =
         match StringTable.find flags.flag_defaults flag with
            FlagBool true ->
               "true"
          | FlagBool false ->
               "false"
          | FlagInt i ->
               string_of_int i
      in
         printf "@ @[<v 3>%-30s = %s@ " flag default;
         print_text_block text;
         printf "@]") help_section;
   printf "@]@.@."

let print_flag_defaults flags =
   printf "@[<v 0>";
   StringTable.iter (fun help_tag (help_section_text, help_section) ->
      print_flag_defaults_section flags help_tag help_section_text help_section) flags.flag_help;
   printf "@]"


(*
 * Read and set a boolean value.  flag is the name of the flag; for
 * set, value is the value to set, and for get, the environment is
 * searched first, then the default environment (if the name is not
 * bound or not boolean).
 *)
let flags_set_bool flags flag value =
   flags_set flags flag (FlagBool value)

let flags_get_bool flags flag =
   try
      match flags_get flags flag with
         FlagBool value ->
            value
       | _ ->
            raise (Failure ("flags_get_bool: invalid value type for flag " ^ flag))
   with
      Not_found ->
         raise (Failure ("flags_get_bool: flag " ^ flag ^ " not defined"))


(*
 * Read and set an integer value.  flag is the name of the flag; for
 * set, value is the value to set, and for get, the environment is
 * searched first, then the default environment (if the name is not
 * bound or not integer).
 *)
let flags_set_int flags flag value =
   flags_set flags flag (FlagInt value)

let flags_get_int flags flag =
   try
      match flags_get flags flag with
         FlagInt value ->
            value
       | _ ->
            raise (Failure ("flags_get_int: invalid value type for flag " ^ flag))
   with
      Not_found ->
         raise (Failure ("flags_get_int: flag " ^ flag ^ " not defined"))


(*
 * Set a flag based on a variable=value expression.  The value may
 * be either a boolean or an integer value; the flag type will be
 * set accordingly.
 *)
let flags_set_expr flags exprs =
   let exprs = Lm_string_util.split "," exprs in
   let process_expr flags expr =
      try
         let equals = String.index expr '=' in
         let length = String.length expr in
         let variable = String.sub expr 0 equals in
         let value = String.sub expr (equals + 1) (length - equals - 1) in
            match String.lowercase value with
               "t" | "true" | "y" | "yes" ->
                  flags_set_bool flags variable true
             | "f" | "false" | "n" | "no" ->
                  flags_set_bool flags variable false
             | _ ->
                  try
                     let value = int_of_string value in
                        flags_set_int flags variable value
                  with
                     Invalid_argument _
                   | Failure _ ->
                        raise (Failure ("flags_set_expr: invalid value specified for flag " ^ variable))
      with
         Not_found ->
            raise (Failure ("flags_set_expr: no equals sign found in expression " ^ expr))
   in
      List.fold_left process_expr flags exprs


(***  Standard Flag Set  ***)


(*
 * Set of standard flags.
 *)
let std_flags = ref flags_empty


(*
 * High-level functions to access and modify standard flags.
 *)
let std_flags_get_bool name =
   flags_get_bool !std_flags name

let std_flags_set_bool name value =
   std_flags := flags_set_bool !std_flags name value

let std_flags_get_int name =
   flags_get_int !std_flags name

let std_flags_set_int name value =
   std_flags := flags_set_int !std_flags name value

let std_flags_set_expr expr =
   std_flags := flags_set_expr !std_flags expr


(*
 * High-level function for displaying and registering names.
 *)
let std_flags_help_section_text help_tag help_text =
   std_flags := flags_help_section_text !std_flags help_tag help_text

let std_flags_register_list names =
   std_flags := flags_register_list !std_flags names

let std_flags_register_list_help help_tag names =
   std_flags := flags_register_list_help !std_flags help_tag names

let print_std_flag_defaults () =
   print_flag_defaults !std_flags


(*
 * Print current state
 *)
let print_bool_flag name value = printf "%-30s = %b\n" name value
let print_int_flag name value  = printf "%-30s = %d\n" name value
let print_flag name value =
   match value with
      FlagBool value ->
         print_bool_flag name value
    | FlagInt value ->
         print_int_flag name value

let print_std_flag_values () =
   printf "===begin current state===\n";
   printf "%-30s   %s\n" "Flag name" "Current value";
   StringTable.iter print_flag !std_flags.flag_values;
   printf "=== end current state ===\n"
