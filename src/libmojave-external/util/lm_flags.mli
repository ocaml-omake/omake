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
 * well as a registry of `valid names'' and their default values.  You can
 * define new names which do not appear in the defaults table, however you
 * cannot lookup a name that appears neither in the current environment nor
 * the defaults.
 *)
type flags =
   { flag_values     :  flag_value StringTable.t;  (* current environment *)
     flag_defaults   :  flag_value StringTable.t;  (* registered names & defaults *)
     flag_help       :  flag_help_section StringTable.t;
   }


(*
 * Simple (low-level) manipulations on the flags environment.
 *)
val flags_empty : flags


(*
 * Read and set a boolean value.  flag is the name of the flag; for
 * set, value is the value to set, and for get, the environment is
 * searched first, then the default environment (if the name is not
 * bound or not boolean).
 *)
val flags_set_bool : flags -> string -> bool -> flags
val flags_get_bool : flags -> string -> bool


(*
 * Read and set an integer value.  flag is the name of the flag; for
 * set, value is the value to set, and for get, the environment is
 * searched first, then the default environment (if the name is not
 * bound or not integer).
 *)
val flags_set_int : flags -> string -> int -> flags
val flags_get_int : flags -> string -> int


(*
 * Set a flag based on a variable=value expression.  The value may
 * be either a boolean or an integer value; the flag type will be
 * set accordingly.
 *)
val flags_set_expr : flags -> string -> flags


(***  Standard Flag Set  ***)


(*
 * Set of standard flags.
 *)
val std_flags : flags ref


(*
 * High-level functions to access and modify standard flags.
 *)
val std_flags_get_bool : string -> bool
val std_flags_set_bool : string -> bool -> unit
val std_flags_get_int : string -> int
val std_flags_set_int : string -> int -> unit
val std_flags_set_expr : string -> unit


(*
 * High-level function for displaying and registering names.
 *)
val std_flags_help_section_text : string -> string -> unit
val std_flags_register_list : (string * flag_value) list -> unit
val std_flags_register_list_help : string -> (string * flag_value * string) list -> unit
val print_std_flag_defaults : unit -> unit


(*
 * Print current state
 *)
val print_std_flag_values : unit -> unit
