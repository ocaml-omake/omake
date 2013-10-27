(*
 * A trace is like a nested list.
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
open Lm_printf

type 'a trace = 'a trace_node list

and 'a trace_node =
   Elem of 'a
 | Lm_trace of 'a * 'a trace

(*
 * Usual map, fold functions.
 *)
let rec map f t =
   List.map (map_node f) t

and map_node f = function
   Elem x -> Elem (f x)
 | Lm_trace (x, t) -> Lm_trace (f x, map f t)

let rec fold f a t =
   List.fold_left (fold_node f) a t

and fold_node f a = function
   Elem x -> f a x
 | Lm_trace (x, t) -> fold f (f a x) t

let rec iter f t =
   List.iter (iter_node f) t

and iter_node f = function
   Elem x -> f x
 | Lm_trace (x, t) -> f x; iter f t

(*
 * Include the nesting depth.
 *)
let iter_depth f t =
   let rec iter depth t =
      List.iter (iter_node depth) t
   and iter_node depth = function
      Elem x ->
         f depth x
    | Lm_trace (x, t) ->
         let depth = succ depth in
            f depth x;
            iter depth t
   in
      iter 0 t

(*
 * Include the nesting depth.
 *)
let map_depth f t =
   let rec map depth t =
      List.map (map_node depth) t
   and map_node depth = function
      Elem x ->
         Elem (f depth x)
    | Lm_trace (x, t) ->
         let depth = succ depth in
         let x = f depth x in
         let t = map depth t in
            Lm_trace (x, t)
   in
      map 0 t

(*
 * Flatten the trace.
 *)
let of_list l =
   List.map (fun x -> Elem x) l

let to_list t =
   let l = fold (fun l x -> x :: l) [] t in
      List.rev l

(*
 * Lm_trace simultaneous map fold.
 *)
let rec fold_map f x l =
   match l with
      h :: t ->
         let x, h = f x h in
         let x, t = fold_map f x t in
            x, h :: t
    | [] ->
         x, []

let fold_map f x l =
   let rec fold_node x a =
      match a with
         Elem a ->
            let x, a = f x a in
               x, Elem a
       | Lm_trace (a, l) ->
            let x, a = f x a in
            let x, l = fold_map fold_node x l in
               x, Lm_trace (a, l)
   in
      fold_map fold_node x l

(*
 * Extract the Lm_trace headers.
 *)
let rec head_elem head x =
   match x with
      Elem _ ->
         head
    | Lm_trace (x, l) ->
         head_list (x :: head) l

and head_list head l =
   match l with
      h :: l ->
         head_list (head_elem head h) l
    | [] ->
         head

let header_nodes l =
   head_list [] l

(*
 * Extract the Lm_trace "special" nodes.
 * These are the loop entries and exits.
 *)
let rec special_elem special x =
   match x with
      Elem _ ->
         special
    | Lm_trace (x, l) ->
         special_list (x :: special) l

and special_list special l =
   match l with
      (Lm_trace _ as h) :: ((Elem x :: _) as l) ->
         special_list (special_elem (x :: special) h) l
    | h :: l ->
         special_list (special_elem special h) l
    | [] ->
         special

let special_nodes l =
   special_list [] l

(*
 * Print the trace.
 *)
let pp_print buf pp_print_node trace =
   let rec print depth = function
      Elem x ->
         fprintf buf "@ ";
         pp_print_node buf x
    | Lm_trace (x, l) ->
         let depth = succ depth in
            fprintf buf "@ @[<v 3>Lm_trace[%d]:@ " depth;
            pp_print_node buf x;
            List.iter (print depth) l;
            fprintf buf "@]"
   in
      List.iter (print 0) trace

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
