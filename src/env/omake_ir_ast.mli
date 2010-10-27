(*
 * Compile the AST to IR.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
open Lm_symbol
open Lm_location

open Omake_ir
open Omake_env
open Omake_pos
open Omake_node

(*
 * Parsing environments.
 *)
type penv

(*
 * Parse a variable declaration.
 *)
val parse_declaration : venv -> pos -> loc -> var list -> method_name

(*
 * Environment for parsing AST files.
 *)
type senv_open_file  = string -> pos -> loc -> Node.t * senv

(*
 * Internal function for converting string expressions.
 *)
val build_string     : penv -> Omake_ast.exp -> pos -> penv * string_exp

(*
 * Create a parsing environment for the given file.
 *    penv_create (file, pervasives_id)
 *)
val penv_create        : senv_open_file -> venv -> Node.t -> penv
val penv_class_names   : penv -> symbol list * senv
val penv_of_vars       : senv_open_file -> venv -> Node.t -> senv -> penv

(*
 * Compile an AST program.
 *)
val compile_string   : penv -> Omake_ast.exp -> pos -> penv * string_exp
val compile_exp      : penv -> Omake_ast.exp        -> penv * ir
val compile_exp_list : penv -> Omake_ast.exp list   -> penv * ir
val compile_prog     : penv -> Omake_ast.prog       -> penv * ir

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
