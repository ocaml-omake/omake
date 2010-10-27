(*
 * Compile (evaluate) an OMakefile.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2010 Mojave Group, California Institute of Technology, and
 * HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@metaprl.org}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_glob
open Lm_location

open Omake_ir
open Omake_env
open Omake_pos
open Omake_node
open Omake_ir_ast
open Omake_exec_type
open Omake_value_type
open Omake_command_type

val print_ast   : bool ref
val print_ir    : bool ref
val print_rules : bool ref
val print_files : bool ref
val debug_eval  : bool ref

(* Utility *)
val raise_uncaught_exception : pos -> exn -> 'a

(*
 * Evaluate an expression.
 *)
val eval : venv -> exp -> value
val eval_exp : venv -> value -> exp -> venv * value
val eval_sequence : venv -> pos -> value -> exp list -> venv * value
val eval_sequence_export : venv -> pos -> value -> exp list -> export -> venv * value
val eval_sequence_exp : venv -> pos -> exp list -> venv * value
val eval_sequence_export_exp : venv -> pos -> exp list -> export -> venv * value

(*
 * String expression evaluation.
 *)
val eval_string_exp : venv -> pos -> string_exp -> value

(*
 * Include the file literally.
 *)
val find_include_file : venv -> pos -> loc -> string -> Node.t
val eval_open_file : venv -> senv_open_file
val eval_include_file : venv -> include_scope -> pos -> loc -> Node.t -> venv * value
val include_file : venv -> include_scope -> pos -> loc -> Node.t -> venv

(*
 * Evaluate a file as if it were an object.
 *)
val eval_object_file : venv -> pos -> loc -> Node.t -> obj

(*
 * Evaluate the program.
 * This modifies the environment.
 *)
val compile : venv -> unit

(*
 * Passes the IR thru Omake_ir_semant.build_prog, printing it if print_ir is enabled.
 *)
val postprocess_ir : venv -> ir -> ir

(*
 * Evaluate a dependency file.
 *)
val compile_deps : venv -> Node.t -> string -> (string list * string list) list

(*
 * Remove outermost applications.
 *)
val eval_value : venv -> pos -> value -> value

(*
 * Evaluate ValBody expressions.
 *)
val eval_body_value : venv -> pos -> value -> value
val eval_body_exp   : venv -> pos -> value -> value -> venv * value

(*
 * Get the object for the value.
 *)
val eval_object : venv -> pos -> value -> obj
val eval_find_field : venv -> pos -> loc -> var_info -> var list -> path * obj * var
val eval_find_method : venv -> pos -> loc -> var_info -> var list -> venv * value
val eval_defined_field : venv -> pos -> loc -> var_info -> var list -> bool

(*
 * Evaluate a value that should be a function.
 * Be careful with this: don't create a ValPrim using
 * this function, since marshaling will fail.
 *)
val eval_fun : venv -> pos -> value -> bool * prim_fun_data

(*
 * Also, if the value is an array of 1 element,
 * return the element.
 *)
val eval_single_value : venv -> pos -> value -> value

(*
 * Evaluate to a primitive value.
 * That is, if the value is an object, return the
 * primitive handle associated with the object.
 * If the object has no primitive value, the object
 * itself is returned.
 *)
val eval_prim_value : venv -> pos -> value -> value

(*
 * Evaluate a function application.
 *)
val eval_apply : venv -> pos -> loc -> value -> value list -> keyword_value list -> venv * value
val eval_partial_apply : venv -> pos -> loc -> value -> value list -> keyword_value list -> venv * value

(*
 * Conversions.
 * The following two functions should be used with care, since
 * they fail if the value contains an array.
 *)
val string_of_value : venv -> pos -> value -> string
val string_of_quote : venv -> pos -> char option -> value list -> string
val file_of_value   : venv -> pos -> value -> Node.t
val path_of_values  : venv -> pos -> value list -> string -> (bool * Dir.t list) list

(*
 * These conversions are safe to use anywhere.
 *)
val tokens_of_value  : venv -> pos -> lexer -> value -> tok list
val arg_of_values    : venv -> pos -> value list -> arg
val argv_of_values   : venv -> pos -> value list list -> arg list
val values_of_value  : venv -> pos -> value -> value list
val strings_of_value : venv -> pos -> value -> string list
val bool_of_value    : venv -> pos -> value -> bool

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
