(*
 * Builtin objects.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2010 Mojave Group, Caltech and HRL Laboratories, LLC
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}, @email{anogin@hrl.com}
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
open Omake_symbol
open Omake_builtin
open Omake_build_type
open Omake_cache_type
open Omake_builtin_type
open Omake_builtin_util
open Omake_value_type
open Omake_value_print

module Pos = MakePos (struct let name = "Omake_builtin_object" end)
open Pos

(*
 * Extend an object with another.
 * The argument may be a file or an object.
 *)
let extends_fun venv pos loc args kargs =
   let pos = string_pos "extends" pos in
   let extend_arg venv v =
      let obj =
         match eval_value venv pos v with
            ValObject obj ->
               obj
          | v ->
               object_of_file venv pos loc (string_of_value venv pos v)
      in
         venv_include_object venv obj
   in
   let venv = List.fold_left extend_arg venv args in
      venv, ValNone

(*
 * Get the object form of a value.
 *)
let object_fun venv pos loc args =
   let pos = string_pos "object" pos in
   let values =
      match args with
         [arg] ->
            values_of_value venv pos arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let values = List.map (fun v -> ValObject (eval_object venv pos v)) values in
      concat_array values

(************************************************************************
 * Object operations.
 *)

(*
 * Field membership.
 *)
let object_mem venv pos loc args =
   let pos = string_pos "object-mem" pos in
      match args with
         [arg; v] ->
            let obj = eval_object venv pos arg in
            let s = string_of_value venv pos v in
            let v = Lm_symbol.add s in
               val_of_bool (venv_defined_field venv obj v)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let object_find venv pos loc args =
   let pos = string_pos "object-find" pos in
      match args with
         [arg; v] ->
            let obj = eval_object venv pos arg in
            let s = string_of_value venv pos v in
            let v = Lm_symbol.add s in
               venv_find_field venv obj pos v
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add a field to an object.
 *)
let object_add venv pos loc args kargs =
   let pos = string_pos "object-add" pos in
      match args, kargs with
         [arg; v; x], [] ->
            let obj = eval_object venv pos arg in
            let s = string_of_value venv pos v in
            let v = Lm_symbol.add s in
            let venv, obj = venv_add_field venv obj pos v x in
               venv, ValObject obj
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add a field to an object.
 *)
let object_length venv pos loc args =
   let pos = string_pos "object-length" pos in
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
               ValInt (venv_object_length obj)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Iterate over the object.
 *)
let object_map venv pos loc args kargs =
   let pos = string_pos "map" pos in
   let pos = string_pos "map" pos in
   let f, obj =
      match args with
         [arg; fun_val] ->
            let obj = eval_object venv pos arg in
            let _, f = eval_fun venv pos fun_val in
               f, obj
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in

   (* If the body exports the environment, preserve it across calls *)
   let venv, obj =
      venv_object_fold_internal (fun (venv, obj) v x ->
            let venv, x = f venv pos loc [ValString (Lm_symbol.to_string v); x] [] in
            let obj = venv_add_field_internal obj v x in
               venv, obj) (venv, obj) obj
   in
      venv, ValObject obj

(*
 * instanceof predicate.
 *)
let object_instanceof venv pos loc args =
   let pos = string_pos "instanceof" pos in
   let obj, v =
      match args with
         [obj; v] ->
            obj, v
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let obj = eval_object venv pos obj in
   let v = Lm_symbol.add (string_of_value venv pos v) in
      if venv_instanceof obj v then
         val_true
      else
         val_false

(************************************************************************
 * Map operations.
 *)

(*
 * Map manipulation.
 *)
let map_of_object venv pos obj =
   try
      match venv_find_field_internal_exn obj map_sym with
         ValMap map ->
            map
       | _ ->
            raise Not_found
   with
      Not_found ->
         raise (OmakeException (pos, StringError "object is not a Map"))

let map_of_value venv pos arg =
   let obj = eval_object venv pos arg in
      map_of_object venv pos obj

let wrap_map obj map =
   ValObject (venv_add_field_internal obj map_sym (ValMap map))

(*
 * Field membership.
 *)
let map_mem venv pos loc args =
   let pos = string_pos "map-mem" pos in
      match args with
         [arg; v] ->
            let map = map_of_value venv pos arg in
            let v = key_of_value venv pos v in
               val_of_bool (venv_map_mem map pos v)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let map_find venv pos loc args =
   let pos = string_pos "map-find" pos in
      match args with
         [arg; v] ->
            let map = map_of_value venv pos arg in
            let v = key_of_value venv pos v in
               venv_map_find map pos v
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Get the number of elements in the map.
 *)
let map_length venv pos loc args =
   let pos = string_pos "map-length" pos in
      match args with
         [arg] ->
            let map = map_of_value venv pos arg in
               ValInt (venv_map_length map)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Add a field to an object.
 *)
let map_add venv pos loc args =
   let pos = string_pos "map-add" pos in
      match args with
         [arg; v; x] ->
            let obj = eval_object venv pos arg in
            let map = map_of_object venv pos obj in
            let key = key_of_value venv pos v in
               wrap_map obj (venv_map_add map pos key x)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Remove a field from an object.
 *)
let map_remove venv pos loc args =
   let pos = string_pos "map-remove" pos in
      match args with
         [arg; v] ->
            let obj = eval_object venv pos arg in
            let map = map_of_object venv pos obj in
            let key = key_of_value venv pos v in
               wrap_map obj (venv_map_remove map pos key)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Iterate over the object.
 *)
let map_map venv pos loc args kargs =
   let pos = string_pos "map-map" pos in
   let f, obj, map =
      match args, kargs with
         [arg; fun_val], [] ->
            let obj = eval_object venv pos arg in
            let map = map_of_object venv pos obj in
            let _, f = eval_fun venv pos fun_val in
               f, obj, map
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in

   (* If the body exports the environment, preserve it across calls *)
   let venv, map =
      venv_map_fold (fun (venv, map) v x ->
            let venv, x = f venv pos loc [v; x] [] in
            let map = venv_map_add map pos v x in
               venv, map) (venv, map) map
   in
      venv, wrap_map obj map

(*
 * Get an array of keys of the map.
 *)
let map_keys venv pos loc args =
   let pos = string_pos "map-keys" pos in
      match args with
         [arg] ->
            let map = map_of_value venv pos arg in
            let keys = venv_map_fold (fun keys k _ -> k::keys) [] map in
               ValArray (List.rev keys)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Get an array of values of the map.
 *)
let map_values venv pos loc args =
   let pos = string_pos "map-values" pos in
      match args with
         [arg] ->
            let map = map_of_value venv pos arg in
            let vals = venv_map_fold (fun vals _ v -> v::vals) [] map in
               ValArray (List.rev vals)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \twofuns{create-map}{create-lazy-map}
 *
 * The \verb+create-map+ is a simplified form for creating \verb+Map+ objects.
 * The \verb+create-map+ function takes an even number of arguments that specify
 * key/value pairs.  For example, the following values are equivalent.
 *
 * \begin{verbatim}
 *     X = $(create-map name1, xxx, name2, yyy)
 *
 *     X. =
 *         extends $(Map)
 *         $|name1| = xxx
 *         $|name2| = yyy
 * \end{verbatim}
 *
 * The \verb+create-lazy-map+ function is similar, but the values are computed
 * lazily.  The following two definitions are equivalent.
 *
 * \begin{verbatim}
 *     Y = $(create-lazy-map name1, $(xxx), name2, $(yyy))
 *
 *     Y. =
 *         extends $(Map)
 *         $|name1| = $`(xxx)
 *         $|name2| = $`(yyy)
 * \end{verbatim}
 *
 * The \hyperfun{create-lazy-map} is used in rule construction.
 * \end{doc}
 *)
let create_map venv pos loc args =
   let pos = string_pos "create-map" pos in
   let rec collect map args =
      match args with
         key :: value :: args ->
            let key = ValData (string_of_value venv pos key) in
            let map = venv_map_add map pos key value in
               collect map args
       | [_] ->
            raise (OmakeException (loc_pos loc pos, StringError ("create-map requires an even number of arguments")))
       | [] ->
            map
   in
      ValMap (collect venv_map_empty args)

(************************************************************************
 * Generic sequence operations.
 *)

(*
 * Return the number of elements in the sequence.
 *)
let int_of_arity arity =
   match arity with
       ArityExact i
     | ArityRange (i, _) ->
          i
     | ArityNone ->
          0
     | ArityAny ->
          max_int

let sequence_length venv pos loc args =
   let pos = string_pos "length" pos in
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
            let len =
               match arg with
                  ValMap map ->
                     venv_map_length map
                | ValObject obj ->
                     venv_object_length obj
                | ValNone
                | ValWhite _ ->
                     0
                | ValInt _
                | ValFloat _
                | ValNode _
                | ValDir _
                | ValBody _
                | ValChannel _ ->
                     1
                | ValData s ->
                     String.length s
                | ValQuote vl ->
                     String.length (string_of_quote venv pos None vl)
                | ValQuoteString (c, vl) ->
                     String.length (string_of_quote venv pos (Some c) vl)
                | ValSequence _
                | ValString _ ->
                     List.length (values_of_value venv pos arg)
                | ValArray a ->
                     List.length a
                | ValFun (_, keywords, params, _, _) ->
                     int_of_arity (fun_arity keywords params)
                | ValFunCurry (_, curry_args, keywords, params, _, _, curry_kargs) ->
                     int_of_arity (curry_fun_arity curry_args keywords params curry_kargs)
                | ValPrim (arity, _, _, _)
                | ValPrimCurry (arity, _, _, _, _) ->
                     int_of_arity arity
                | ValRules l ->
                     List.length l
                | ValCases cases ->
                     List.length cases
                | ValClass _
                | ValOther _
                | ValVar _ ->
                     0
                | ValStringExp _
                | ValMaybeApply _
                | ValDelayed _ ->
                     raise (Invalid_argument "Omake_builtin_error.sequence_length")
            in
               ValInt len
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Get the nth element of a sequence.
 *)
let sequence_nth venv pos loc args =
   let pos = string_pos "nth" pos in
      match args with
         [arg; i] ->
            let i = int_of_value venv pos i in
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValNone
                 | ValWhite _
                 | ValFun _
                 | ValFunCurry _
                 | ValPrim _
                 | ValPrimCurry _
                 | ValStringExp _
                 | ValMaybeApply _
                 | ValDelayed _
                 | ValMap _
                 | ValObject _ ->
                      raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)))
                 | ValInt _
                 | ValFloat _
                 | ValNode _
                 | ValDir _
                 | ValBody _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _
                 | ValVar _ ->
                      if i = 0 then
                         arg
                      else
                         raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)))
                 | ValData s ->
                      let len = String.length s in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i 1)
                 | ValQuote vl ->
                      let s = string_of_quote venv pos None vl in
                      let len = String.length s in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i 1)
                 | ValQuoteString (c, vl) ->
                      let s = string_of_quote venv pos (Some c) vl in
                      let len = String.length s in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i 1)

                 | ValSequence _
                 | ValString _ ->
                      let values = values_of_value venv pos arg in
                      let len = List.length values in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         List.nth values i
                 | ValArray a ->
                      let len = List.length a in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         List.nth a i
                 | ValRules l ->
                      let len = List.length l in
                         if i < 0 || i >= len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValRules [List.nth l i])
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Test if a sequence is nonempty.
 *)
let sequence_nth_tl venv pos loc args =
   let pos = string_pos "nth-tl" pos in
      match args with
         [arg; i] ->
            let i = int_of_value venv pos i in
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValNone
                 | ValFun _
                 | ValFunCurry _
                 | ValPrim _
                 | ValPrimCurry _
                 | ValMaybeApply _
                 | ValDelayed _
                 | ValMap _
                 | ValObject _
                 | ValInt _
                 | ValFloat _
                 | ValNode _
                 | ValDir _
                 | ValStringExp _
                 | ValBody _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _
                 | ValVar _ ->
                      raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)))
                 | ValData s ->
                      let len = String.length s in
                         if i < 0 || i > len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i (String.length s - i))
                 | ValQuote vl ->
                      let s = string_of_quote venv pos None vl in
                      let len = String.length s in
                         if i < 0 || i > len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i (String.length s - i))
                 | ValQuoteString (c, vl) ->
                      let s = string_of_quote venv pos (Some c) vl in
                      let len = String.length s in
                         if i < 0 || i > len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValData (String.sub s i (String.length s - i))

                 | ValSequence _
                 | ValWhite _
                 | ValString _ ->
                      let values = values_of_value venv pos arg in
                      let len = List.length values in
                         if i < 0 || i > len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValArray (Lm_list_util.nth_tl i values)
                 | ValArray a ->
                      let len = List.length a in
                         if i < 0 || i > len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValArray (Lm_list_util.nth_tl i a)
                 | ValRules l ->
                      let len = List.length l in
                         if i < 0 || i > len then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", i)));
                         ValRules (Lm_list_util.nth_tl i l))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Get the nth-tl of a sequence.
 *)
let sequence_nonempty venv pos loc args =
   let pos = string_pos "is-nonempty" pos in
   let b =
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValNone
                 | ValFun _
                 | ValFunCurry _
                 | ValPrim _
                 | ValPrimCurry _
                 | ValStringExp _
                 | ValMaybeApply _
                 | ValDelayed _
                 | ValMap _
                 | ValObject _ ->
                      true
                 | ValInt _
                 | ValFloat _
                 | ValNode _
                 | ValDir _
                 | ValBody _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _
                 | ValWhite _
                 | ValVar _ ->
                      false
                 | ValData s ->
                      String.length s <> 0
                 | ValQuote vl ->
                      let s = string_of_quote venv pos None vl in
                         String.length s <> 0
                 | ValQuoteString (c, vl) ->
                      let s = string_of_quote venv pos (Some c) vl in
                         String.length s <> 0

                 | ValSequence _
                 | ValString _ ->
                      values_of_value venv pos arg <> []
                 | ValArray a ->
                      a <> []
                 | ValRules l ->
                      l <> [])
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
      if b then
         val_true
      else
         val_false

(*
 * Subrange.
 *)
let sequence_sub venv pos loc args =
   let pos = string_pos "sub" pos in
      match args with
         [arg; off; len] ->
            let off = int_of_value venv pos off in
            let len = int_of_value venv pos len in
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValNone
                 | ValWhite _
                 | ValFun _
                 | ValFunCurry _
                 | ValPrim _
                 | ValPrimCurry _
                 | ValMaybeApply _
                 | ValDelayed _
                 | ValMap _
                 | ValObject _
                 | ValInt _
                 | ValFloat _
                 | ValNode _
                 | ValDir _
                 | ValStringExp _
                 | ValBody _
                 | ValChannel _
                 | ValClass _
                 | ValCases _
                 | ValOther _
                 | ValVar _ ->
                      raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", off)))
                 | ValData s ->
                      let length = String.length s in
                         if off < 0 || len < 0 || off + len >= length then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", off)));
                         ValData (String.sub s off len)
                 | ValQuote vl ->
                      let s = string_of_quote venv pos None vl in
                      let length = String.length s in
                         if off < 0 || len < 0 || off + len >= length then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", off)));
                         ValData (String.sub s off len)
                 | ValQuoteString (c, vl) ->
                      let s = string_of_quote venv pos (Some c) vl in
                      let length = String.length s in
                         if off < 0 || len < 0 || off + len >= length then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", off)));
                         ValData (String.sub s off len)

                 | ValSequence _
                 | ValString _ ->
                      let values = values_of_value venv pos arg in
                      let length = List.length values in
                         if off < 0 || len < 0 || off + len >= length then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", off)));
                         ValArray (Lm_list_util.sub values off len)
                 | ValArray values ->
                      let length = List.length values in
                         if off < 0 || len < 0 || off + len >= length then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", off)));
                         ValArray (Lm_list_util.sub values off len)
                 | ValRules values ->
                      let length = List.length values in
                         if off < 0 || len < 0 || off + len >= length then
                            raise (OmakeException (loc_pos loc pos, StringIntError ("out of bounds", off)));
                         ValRules (Lm_list_util.sub values off len))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))

(*
 * Reverse the elements in the sequence.
 *)
let sequence_rev venv pos loc args =
   let pos = string_pos "rev" pos in
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValNone
                 | ValWhite _
                 | ValFun _
                 | ValFunCurry _
                 | ValPrim _
                 | ValPrimCurry _
                 | ValMaybeApply _
                 | ValDelayed _
                 | ValMap _
                 | ValObject _
                 | ValInt _
                 | ValFloat _
                 | ValNode _
                 | ValDir _
                 | ValStringExp _
                 | ValBody _
                 | ValChannel _
                 | ValClass _
                 | ValOther _
                 | ValVar _ ->
                      arg
                 | ValData s1 ->
                      let len = String.length s1 in
                      let s2 = String.create len in
                         for i = 0 to len - 1 do
                            s2.[i] <- s1.[len - i - 1]
                         done;
                         ValData s2
                 | ValQuote vl ->
                      let s1 = string_of_quote venv pos None vl in
                      let len = String.length s1 in
                      let s2 = String.create len in
                         for i = 0 to len - 1 do
                            s2.[i] <- s1.[len - i - 1]
                         done;
                         ValData s2
                 | ValQuoteString (c, vl) ->
                      let s1 = string_of_quote venv pos (Some c) vl in
                      let len = String.length s1 in
                      let s2 = String.create len in
                         for i = 0 to len - 1 do
                            s2.[i] <- s1.[len - i - 1]
                         done;
                         ValData s2
                 | ValCases cases ->
                      ValCases (List.rev cases)
                 | ValSequence _
                 | ValString _ ->
                      let values = values_of_value venv pos arg in
                         ValArray (List.rev values)
                 | ValArray a ->
                      ValArray (List.rev a)
                 | ValRules l ->
                      ValRules (List.rev l))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(*
 * Map.
 *
 * \begin{doc}
 * \section{Iteration and mapping}
 *
 * \fun{foreach}
 *
 * The \verb+foreach+ function maps a function over a sequence.
 *
 * \begin{verbatim}
 *     $(foreach <fun>, <args>)
 *
 *     foreach(<var> => <args>)
 *        <body>
 * \end{verbatim}
 *
 * For example, the following program defines the variable \verb+X+
 * as an array \verb+a.c b.c c.c+.
 *
 * \begin{verbatim}
 *     X =
 *        foreach(x => a b c)
 *           value $(x).c
 *
 *     # Equivalent expression
 *     X = $(foreach $(fun x => $(x).c), a b c)
 * \end{verbatim}
 *
 * There is also an abbreviated syntax.
 *
 * The \verb+export+ form can also be used in a \verb+foreach+
 * body.  The final value of \verb+X+ is \verb+a.c b.c c.c+.
 *
 * \begin{verbatim}
 *     X =
 *     foreach(x => a b c)
 *        X += $(x).c
 *        export
 * \end{verbatim}
 *
 * The \hyperfun{break} can be used to break out of the loop early.
 * \end{doc}
 *)
let foreach_fun venv pos loc args kargs =
   let pos = string_pos "foreach" pos in
   let f, args =
      match args, kargs with
         [fun_val; arg], [] ->
            let args = values_of_value venv pos arg in
            let _, f = eval_fun venv pos fun_val in
               f, args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in

   (* If the body exports the environment, preserve it across calls *)
   let venv, values =
      List.fold_left (fun (venv, values) v ->
            let venv, x = f venv pos loc [v] [] in
               venv, x :: values) (venv, []) args
   in
      venv, ValArray (List.rev values)

(*
 * \begin{doc}
 * \section{Boolean tests}
 *
 * \fun{sequence-forall}
 *
 * The \verb+forall+ function tests whether a predicate holds for each
 * element of a sequence.
 *
 * \begin{verbatim}
 *     $(sequence-forall <fun>, <args>)
 *
 *     sequence-forall(<var> => ..., <args>)
 *        <body>
 * \end{verbatim}
 * \end{doc}
 *)
let forall_fun venv pos loc args kargs =
   let pos = string_pos "sequence-forall" pos in
   let f, args =
      match args, kargs with
         [fun_val; arg], [] ->
            let args = values_of_value venv pos arg in
            let _, f = eval_fun venv pos fun_val in
               f, args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let rec test venv args =
      match args with
         arg :: args ->
            let venv, result = f venv pos loc [arg] [] in
               if bool_of_value venv pos result then
                  test venv args
               else
                  venv, false
       | [] ->
            venv, true
   in
   let venv, x = test venv args in
   let x =
      if x then
         val_true
      else
         val_false
   in
      venv, x

(*
 * \begin{doc}
 * \subsection{sequence-exists}
 *
 * The \verb+exists+ function tests whether a predicate holds for
 * some element of a sequence.
 *
 * \begin{verbatim}
 *     $(sequence-exists <fun>, <args>)
 *
 *     sequence-exists(<var> => ..., <args>)
 *        <body>
 * \end{verbatim}
 * \end{doc}
 *)
let exists_fun venv pos loc args kargs =
   let pos = string_pos "sequence-exists" pos in
   let f, args =
      match args, kargs with
         [fun_val; arg], [] ->
            let args = values_of_value venv pos arg in
            let _, f = eval_fun venv pos fun_val in
               f, args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in

   (* If the body exports the environment, preserve it across calls *)
   let rec test venv args =
      match args with
         arg :: args ->
            let venv, result = f venv pos loc [arg] [] in
               if bool_of_value venv pos result then
                  venv, true
               else
                  test venv args
       | [] ->
            venv, false
   in
   let venv, x = test venv args in
   let x =
      if x then
         val_true
      else
         val_false
   in
      venv, x

(*
 * \begin{doc}
 * \fun{sequence-sort}
 *
 * The \verb+sort+ function sorts the elements in an array,
 * given a comparison function.  Given two elements (x, y),
 * the comparison should return a negative number if x < y;
 * a positive number if x > y; and 0 if x = y.
 *
 * \begin{verbatim}
 *     $(sequence-sort <fun>, <args>)
 *
 *     sort(<var>, <var> => ..., <args>)
 *        <body>
 * \end{verbatim}
 * \end{doc}
 *)
let sort_fun venv pos loc args kargs =
   let pos = string_pos "sequence-sort" pos in
   let f, args =
      match args, kargs with
         [fun_val; arg], [] ->
            let args = values_of_value venv pos arg in
            let _, f = eval_fun venv pos fun_val in
               f, args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in

   let compare v1 v2 =
      let _, x = f venv pos loc [v1; v2] [] in
         int_of_value venv pos x
   in
   let args = List.sort compare args in
      venv, ValArray args

(*
 * \begin{doc}
 * \fun{compare}
 *
 * The \verb+compare+ function compares two values (x, y) generically
 * returning a negative number if x < y;
 * a positive number if x > y; and 0 if x = y.
 *
 * \begin{verbatim}
 *     $(compare x, y) : Int
 * \end{verbatim}
 * \end{doc}
 *)
let compare_fun venv pos loc args =
   let pos = string_pos "compare" pos in
   let x, y =
      match args with
         [x; y] ->
            x, y
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
      ValInt (ValueCompare.compare x y)

(*
 * Printable location.
 *)
let string_of_location venv pos loc args =
   let pos = string_pos "string-of-location" pos in
      match args with
         [arg] ->
            let obj = eval_object venv pos arg in
            let arg = eval_object_value venv pos obj in
               (match arg with
                   ValOther (ValLocation loc) ->
                      ValData (Lm_location.string_of_location loc)
                 | _ ->
                      raise (OmakeException (pos, StringValueError ("not a location", arg))))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(************************************************************************
 * Define the functions.
 *)

(*
 * Add only the builtin functions.
 * The Pervasives file defines most of the remaining methods.
 *)
let () =
   let builtin_funs =
      [true, "object",               object_fun,          ArityExact 1;
       true, "obj-find",             object_find,         ArityExact 2;
       true, "obj-mem",              object_mem,          ArityExact 2;
       true, "obj-length",           object_length,       ArityExact 1;
       true, "obj-instanceof",       object_instanceof,   ArityExact 2;
       true, "map-add",              map_add,             ArityExact 3;
       true, "map-find",             map_find,            ArityExact 2;
       true, "map-mem",              map_mem,             ArityExact 2;
       true, "map-length",           map_length,          ArityExact 1;
       true, "map-remove",           map_remove,          ArityExact 1;
       true, "map-keys",             map_keys,            ArityExact 1;
       true, "map-values",           map_values,          ArityExact 1;
       true, "sequence-length",      sequence_length,     ArityExact 1;
       true, "sequence-nth",         sequence_nth,        ArityExact 2;
       true, "sequence-nth-tl",      sequence_nth_tl,     ArityExact 2;
       true, "sequence-nonempty",    sequence_nonempty,   ArityExact 1;
       true, "sequence-rev",         sequence_rev,        ArityExact 1;
       true, "sequence-sub",         sequence_sub,        ArityExact 3;
       true,  "create-map",          create_map,          ArityAny;
       false, "create-lazy-map",     create_map,          ArityAny;
       true, "compare",              compare_fun,         ArityExact 2;
       true, "string-of-location",   string_of_location,  ArityExact 1
      ]
   in
   let builtin_kfuns =
      [true, "obj-add",              object_add,          ArityExact 3;
       true, "extends",              extends_fun,         ArityExact 1;
       true, "foreach",              foreach_fun,         ArityExact 2;
       true, "obj-map",              object_map,          ArityRange (3, 4);
       true, "map-map",              map_map,             ArityRange (3, 4);
       true, "sequence-map",         foreach_fun,         ArityRange (2, 3);
       true, "sequence-forall",      forall_fun,          ArityExact 2;
       true, "sequence-exists",      exists_fun,          ArityExact 2;
       true, "sequence-sort",        sort_fun,            ArityExact 2;
      ]
   in
   let builtin_vars =
      ["empty-map",        (fun _ -> ValMap venv_map_empty)]
   in

   let builtin_objects =
      ["Int",              value_sym, ValInt 0;
       "Float",            value_sym, ValFloat 0.0;
       "String",           value_sym, ValNone;
       "Array",            value_sym, ValArray [];
       "Fun",              value_sym, ValFun (venv_empty_env, [], [], [], ExportNone);
       "Rule",             value_sym, ValRules [];
       "File",             value_sym, ValNone;
       "Dir",              value_sym, ValNone;
       "Body",             value_sym, ValNone;
       "InChannel",        value_sym, ValNone;
       "OutChannel",       value_sym, ValNone;
       "InOutChannel",     value_sym, ValNone;
       "Map",              map_sym,   ValMap venv_map_empty]
   in

   let pervasives_objects =
      ["Object";
       "Number";
       "Sequence";
       "Node";
       "Channel";
       "Exception";
       "RuntimeException";
       "UnbuildableException";
       "Select";
       "Pipe";
       "Stat";
       "Passwd";
       "Group";
       "Shell";
       "Lexer";
       "Parser";
       "Location";
       "Position";
      ]
   in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs;
                           builtin_kfuns = builtin_kfuns;
                           builtin_vars = builtin_vars;
                           builtin_objects = builtin_objects;
                           pervasives_objects = pervasives_objects
      }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
