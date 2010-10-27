(*
 * \begin{doc}
 * \section{Arithmetic}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2006 Mojave Group, Caltech
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

open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_pos
open Omake_eval
open Omake_wild
open Omake_node
open Omake_exec
open Omake_value
open Omake_state
open Omake_builtin
open Omake_cache_type
open Omake_builtin_util
open Omake_builtin_type
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_builtin_arith" end)
open Pos

(*
 * \begin{doc}
 * \fun{int}
 *
 * The \verb+int+ function can be used to create integers.
 * It returns an \verb+Int+ object.
 *
 * \verb+$(int 17)+.
 *
 * \fun{float}
 * The \verb+float+ function can be used to create floating-point numbers.
 * It returns a \verb+Float+ object.
 *
 * \verb+$(float 3.1415926)+.
 * \end{doc}
 *)
let int_fun venv pos loc args =
   let pos = string_pos "int" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
            let values = List.map (fun v -> ValInt (int_of_value venv pos v)) values in
               concat_array values
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let float_fun venv pos loc args =
   let pos = string_pos "int" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
            let values = List.map (fun v -> ValFloat (float_of_value venv pos v)) values in
               concat_array values
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Basic arithmetic.
 *
 * \begin{doc}
 * \subsection{Basic arithmetic}
 * \funref{neg}
 * \funref{add}
 * \funref{sub}
 * \funref{mul}
 * \funref{div}
 * \funref{mod}
 * \funref{lnot}
 * \funref{land}
 * \funref{lor}
 * \funref{lxor}
 * \funref{lsl}
 * \funref{lsr}
 * \funref{asr}
 * \funref{min}
 * \funref{max}
 *
 * The following functions can be used to perform basic arithmetic.
 *
 * \begin{itemize}
 * \item \verb+$(neg <numbers>)+: arithmetic inverse
 * \item \verb+$(add <numbers>)+: addition.
 * \item \verb+$(sub <numbers>)+: subtraction.
 * \item \verb+$(mul <numbers>)+: multiplication.
 * \item \verb+$(div <numbers>)+: division.
 * \item \verb+$(mod <numbers>)+: remainder.
 * \item \verb+$(lnot <numbers>)+: bitwise inverse.
 * \item \verb+$(land <numbers>)+: bitwise and.
 * \item \verb+$(lor <numbers>)+: bitwise or.
 * \item \verb+$(lxor <numbers>)+: bitwise exclusive-or.
 * \item \verb+$(lsl <numbers>)+: logical shift left.
 * \item \verb+$(lsr <numbers>)+: logical shift right.
 * \item \verb+$(asr <numbers>)+: arithmetic shift right.
 * \item \verb+$(min <numbers>)+: smallest element.
 * \item \verb+$(max <numbers>)+: largest element.
 * \end{itemize}
 * \end{doc}
 *)
let unary_int op_int venv pos loc args =
   let pos = string_pos "unary_int" pos in
      match args with
         [arg] ->
            concat_array (List.map (fun v -> ValInt (op_int (int_of_value venv pos arg))) (values_of_value venv pos arg))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let arith_int id_int op_int venv pos loc args =
   let pos = string_pos "arith_int" pos in
   let collect i arg =
      op_int i (int_of_value venv pos arg)
   in
   let args =
      match args with
         [arg] ->
            values_of_value venv pos arg
       | _ ->
            args
   in
      match args with
         arg :: args ->
            ValInt (List.fold_left collect (int_of_value venv pos arg) args)
       | [] ->
            ValInt id_int

let unary op_int op_float venv pos loc args =
   let pos = string_pos "unary" pos in
      match args with
         [arg] ->
            concat_array (List.map (fun v ->
                                match number_of_value venv pos v with
                                   ValInt i ->
                                      ValInt (op_int i)
                                 | ValFloat x ->
                                      ValFloat (op_float x)
                                 | _ ->
                                      raise (OmakeException (loc_pos loc pos, StringError "not a number"))) (**)
                             (values_of_value venv pos arg))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let arith id_int op_int op_float venv pos loc args =
   let pos = string_pos "arith" pos in
   let collect i arg =
      match i, number_of_value venv pos arg with
         ValInt i, ValInt arg ->
            ValInt (op_int i arg)
       | ValInt i, ValFloat arg ->
            ValFloat (op_float (float_of_int i) arg)
       | ValFloat i, ValInt arg ->
            ValFloat (op_float i (float_of_int arg))
       | ValFloat i, ValFloat arg ->
            ValFloat (op_float i arg)
       | _ ->
            raise (OmakeException (loc_pos loc pos, StringError "not a number"))
   in
   let args =
      match args with
         [arg] ->
            values_of_value venv pos arg
       | _ ->
            args
   in
      match args with
         arg :: args ->
            List.fold_left collect (number_of_value venv pos arg) args
       | [] ->
            ValInt id_int

(*
 * Basic arithmetic.
 *
 * \begin{doc}
 * \subsection{Comparisons}
 * \funref{lt}
 * \funref{le}
 * \funref{eq}
 * \funref{ge}
 * \funref{gt}
 * \funref{ult}
 * \funref{ule}
 * \funref{uge}
 * \funref{ugt}
 *
 * The following functions can be used to perform numerical comparisons.
 *
 * \begin{itemize}
 * \item \verb+$(lt <numbers>)+: less then.
 * \item \verb+$(le <numbers>)+: no more than.
 * \item \verb+$(eq <numbers>)+: equal.
 * \item \verb+$(ge <numbers>)+: no less than.
 * \item \verb+$(gt <numbers>)+: greater than.
 * \item \verb+$(ult <numbers>)+: unsigned less than.
 * \item \verb+$(ule <numbers>)+: unsigned greater than.
 * \item \verb+$(uge <numbers>)+: unsigned greater than or equal.
 * \item \verb+$(ugt <numbers>)+: unsigned greater than.
 * \end{itemize}
 * \end{doc}
 *)
let compare op_int op_float venv pos loc args =
   let pos = string_pos "arith" pos in
   let rec collect i args =
      match args with
         arg :: args ->
            let arg = number_of_value venv pos arg in
            let test =
               match i, number_of_value venv pos arg with
                  ValInt i, ValInt arg ->
                     op_int i arg
                | ValInt i, ValFloat arg ->
                     op_float (float_of_int i) arg
                | ValFloat i, ValInt arg ->
                     op_float i (float_of_int arg)
                | ValFloat i, ValFloat arg ->
                     op_float i arg
                | _ ->
                     raise (OmakeException (loc_pos loc pos, StringError "not a number"))
            in
               test && collect arg args
       | [] ->
            true
   in
      match args with
         arg :: args ->
            val_of_bool (collect (number_of_value venv pos arg) args)
       | [] ->
            val_true

(************************************************************************
 * Tables.
 *)
let () =
   let builtin_funs =
      [true, "int",             int_fun,                     ArityExact 1;
       true, "float",           float_fun,                   ArityExact 1;
       true, "neg",             unary (~-) (~-.),            ArityExact 1;
       true, "add",             arith 0 ( + ) ( +. ),        ArityAny;
       true, "sub",             arith 0 ( - ) ( -. ),        ArityAny;
       true, "mul",             arith 1 ( * ) ( *. ),        ArityAny;
       true, "div",             arith 1 ( / ) ( /. ),        ArityAny;
       true, "mod",             arith 1 (mod) (mod_float),   ArityAny;
       true, "min",             arith max_int min min,       ArityAny;
       true, "max",             arith min_int max max,       ArityAny;
       true, "mod",             arith 1 (mod) (mod_float),   ArityAny;
       true, "lnot",            unary_int (lnot),            ArityExact 1;
       true, "land",            arith_int 0 (land),          ArityAny;
       true, "lor",             arith_int 0 (lor) ,          ArityAny;
       true, "lxor",            arith_int 0 (lxor),          ArityAny;
       true, "lsl",             arith_int 0 (lsl),           ArityAny;
       true, "lsr",             arith_int 0 (lsr),           ArityAny;
       true, "asr",             arith_int 0 (asr),           ArityAny;
       true, "lt",              compare (<) (<),             ArityAny;
       true, "le",              compare (<=) (<=),           ArityAny;
       true, "eq",              compare (=) (=),             ArityAny;
       true, "ge",              compare (>=) (>=),           ArityAny;
       true, "gt",              compare (>) (>),             ArityAny]
   in
   let builtin_info = { builtin_empty with builtin_funs = builtin_funs } in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
