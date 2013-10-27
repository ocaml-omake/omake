(*
 * Interval sets.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Mojave Group, Caltech
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
 * jyh@cs.caltech.edu
 *)

(*
 * Elements.
 *)
module type OrderedType =
sig
   type t
   val compare : t -> t -> int
end

(*
 * Elements in countable sets.
 *)
module type CountableOrderedType =
sig
   type t

   val compare : t -> t -> int
   val succ : t -> t
   val pred : t -> t
   val min : t
   val max : t
end

(*
 * Sets specified as intervals.
 *)
type 'a bound =
   Infinity
 | Open of 'a
 | Closed of 'a

module type IntervalSetSig =
sig
   type elt
   type t

   (*
    * Set constructors.
    *)
   val empty : t
   val max_set : t
   val of_interval : elt bound -> elt bound -> t
   val is_empty : t -> bool
   val is_total : t -> bool
   val is_enum : t -> elt -> bool
   val to_enum : t -> elt

   (*
    * Set operations.
    *)
   val subset : t -> t -> bool
   val equal : t -> t -> bool
   val subtract : t -> t -> t
   val negate : t -> t
   val union : t -> t -> t
   val isect : t -> t -> t

   (*
    * Singletons.
    *)
   val of_point : elt -> t
   val mem_point : elt -> t -> bool
   val add_point : t -> elt -> t
   val is_singleton : t -> bool
   val dest_singleton : t -> elt
   val subtract_point : t -> elt -> t

   (*
    * Mapping.
    *)
   val iter : (elt bound -> elt bound -> unit) -> t -> unit
   val fold : ('a -> elt bound -> elt bound -> 'a) -> 'a -> t -> 'a
end

(*
 * Intervals over a dense set (like strings or rationals).
 * Need both open and closed intervals.
 *)
module DenseIntervalSet (Element : OrderedType) : IntervalSetSig with type elt = Element.t =
struct
   (*
    * Simple implementation is just a sorted list of
    * disjoint closed intervals.
    *)
   type elt = Element.t

   type left_bound =
      LeftInfinity
    | LeftOpen of elt
    | LeftClosed of elt

   type right_bound =
      RightInfinity
    | RightOpen of elt
    | RightClosed of elt

   type t = (left_bound * right_bound) list

   (*
    * Short comparisons.
    *)
   let compare = Element.compare

   let lt x y =
      compare x y < 0

   let le x y =
      compare x y <= 0

   let eq x y =
      compare x y = 0

   (*
    * Empty set.
    *)
   let empty = []

   let max_set =
      [LeftInfinity, RightInfinity]

   let is_empty s =
      s = []

   let is_total s =
      s = [LeftInfinity, RightInfinity]

   let is_enum _ _ =
      false

   let to_enum _ =
      raise (Invalid_argument "to_enum")

   (*
    * Set containing one point.
    *)
   let of_point x =
      [LeftClosed x, RightClosed x]

   let is_singleton = function
      [LeftClosed x, RightClosed y] when eq x y ->
         true
    | _ ->
         false

   let dest_singleton = function
      [LeftClosed x, RightClosed y] when eq x y ->
         x
    | _ ->
         raise (Invalid_argument "Lm_interval_set.dest_singleton")

   (*
    * Set containing an interval.
    *)
   let left_bound = function
      Infinity ->
         LeftInfinity
    | Open x ->
         LeftOpen x
    | Closed x ->
         LeftClosed x

   let right_bound = function
      Infinity ->
         RightInfinity
    | Open x ->
         RightOpen x
    | Closed x ->
         RightClosed x

   let of_interval x y =
      [left_bound x, right_bound y]

   (*
    * Compare limits.
    *)
   let compare_left_left x y =
      match x, y with
         LeftInfinity, LeftInfinity ->
            0
       | LeftInfinity, _ ->
            -1
       | _, LeftInfinity ->
            1
       | LeftOpen x, LeftOpen y ->
            compare x y
       | LeftOpen x, LeftClosed y ->
            let eq = compare x y in
               if eq = 0 then
                  1
               else
                  eq
       | LeftClosed x, LeftOpen y ->
            let eq = compare x y in
               if eq = 0 then
                  -1
               else
                  eq
       | LeftClosed x, LeftClosed y ->
            compare x y

   let compare_right_left x y =
      match x, y with
         RightInfinity, _
       | _, LeftInfinity ->
            1
       | RightOpen x, LeftOpen y ->
            let eq = compare x y in
               if eq = 0 then
                  -1
               else
                  eq
       | RightOpen x, LeftClosed y ->
            let eq = compare x y in
               if eq = 0 then
                  -1
               else
                  eq
       | RightClosed x, LeftOpen y ->
            let eq = compare x y in
               if eq = 0 then
                  -1
               else
                  eq
       | RightClosed x, LeftClosed y ->
            compare x y

   let compare_right_right x y =
      match x, y with
         RightInfinity, RightInfinity ->
            0
       | RightInfinity, _ ->
            1
       | _, RightInfinity ->
            -1
       | RightOpen x, RightOpen y ->
            compare x y
       | RightOpen x, RightClosed y ->
            let eq = compare x y in
               if eq = 0 then
                  -1
               else
                  eq
       | RightClosed x, RightOpen y ->
            let eq = compare x y in
               if eq = 0 then
                  1
               else
                  0
       | RightClosed x, RightClosed y ->
            compare x y

   let ge_left_left x y =
      compare_left_left x y >= 0

   let lt_left_left x y =
      compare_left_left x y < 0

   let lt_right_right x y =
      compare_right_right x y < 0

   let le_right_right x y =
      compare_right_right x y <= 0

   let lt_right_left x y =
      compare_right_left x y < 0

   (*
    * Min-max.
    *)
   let min_left_left x y =
      if lt_left_left x y then
         x
      else
         y

   let max_right_right x y =
      if lt_right_right x y then
         y
      else
         x

   let max_left_left x y =
      if lt_left_left x y then
         y
      else
         x

   let min_right_right x y =
      if lt_right_right x y then
         x
      else
         y

   (*
    * Interval operations.
    *)
   let lt_interval_interval (_, right) (left, _) =
      lt_right_left right left

   let subset_interval_interval (left1, right1) (left2, right2) =
      ge_left_left left1 left2 && le_right_right right1 right2

   (*
    * Union assumes the two intervals overlap.
    *)
   let union_interval_interval (left1, right1) (left2, right2) =
      (min_left_left left1 left2, max_right_right right1 right2)

   (*
    * Isect assumes the two intervals overlap.
    *)
   let isect_interval_interval (left1, right1) (left2, right2) =
      (max_left_left left1 left2, min_right_right right1 right2)

   (*
    * Subtract assume sthe intervals overlap.
    *)
   let invert_left = function
      LeftOpen x ->
         RightClosed x
    | LeftClosed x ->
         RightOpen x
    | LeftInfinity ->
         raise (Invalid_argument "invert_left")

   let invert_right = function
      RightOpen x ->
         LeftClosed x
    | RightClosed x ->
         LeftOpen x
    | RightInfinity ->
         raise (Invalid_argument "invert_right")

   let subtract_interval_interval (left1, right1) (left2, right2) =
      let compare_left = compare_left_left left1 left2 in
      let compare_right = compare_right_right right1 right2 in
         if compare_left < 0 then
            if compare_right < 0 then
               (*
                *     left1    right1
                *       |--------|
                *          |---------|
                *        left2     right2
                *)
               [left1, invert_left left2; invert_right right1, right2]
            else if compare_right = 0 then
               (*
                *     left1    right1
                *       |--------|
                *          |-----|
                *        left2 right2
                *)
               [left1, invert_left left2]
            else (* compare_right > 0 *)
               (*
                *     left1       right1
                *       |------------|
                *          |-----|
                *        left2 right2
                *)
               [left1, invert_left left2; invert_right right2, right1]

         else if compare_left = 0 then
            if compare_right < 0 then
               (*
                *     left1    right1
                *       |--------|
                *       |------------|
                *     left2        right2
                *)
               [invert_right right1, right2]
            else if compare_right = 0 then
               (*
                *     left1    right1
                *       |--------|
                *       |--------|
                *     left2    right2
                *)
               []
            else (* compare_right > 0 *)
               (*
                *     left1       right1
                *       |------------|
                *       |--------|
                *     left2    right2
                *)
               [invert_right right2, right1]

         else if compare_right <= 0 then
            (*
             *     left1    right1
             *       |--------|
             *    |--------------|
             *  left2          right2
             *)
            []

         else (* compare_right > 0 *)
            (*
             *     left1       right1
             *       |-----------|
             *    |-----------|
             *  left2       right2
             *)
            [invert_right right2, right1]

   (*
    * Join intervals that have common endpoints.
    *)
   let rec normalize = function
      (left, RightClosed x) :: (LeftClosed y, right) :: set when eq x y ->
         normalize ((left, right) :: set)
    | (left, RightClosed x) :: (LeftOpen y, right) :: set when eq x y ->
         normalize ((left, right) :: set)
    | (left, RightOpen x) :: (LeftClosed y, right) :: set when eq x y ->
         normalize ((left, right) :: set)
    | h :: set ->
         h :: normalize set
    | [] ->
         []

   (*
    * Test subset.
    *)
   let rec subset set1 set2 =
      match set1, set2 with
         [], _ ->
            true
       | _, [] ->
            false
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval2 interval1 then
               subset set1 set2'
            else if subset_interval_interval interval1 interval2 then
               subset set1' set2
            else
               false

   (*
    * Test equality.
    *)
   let equal s1 s2 = subset s1 s2 && subset s2 s1

   let mem_point x set =
      subset [LeftClosed x, RightClosed x] set

   (*
    * Set subtraction.
    *)
   let rec subtract set1 set2 =
      match set1, set2 with
         [], _ ->
            []
       | _, [] ->
            set1
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval1 interval2 then
               interval1 :: subtract set1' set2
            else if lt_interval_interval interval2 interval1 then
               subtract set1 set2'
            else
               (subtract_interval_interval interval1 interval2) @ subtract set1' set2

   let negate set =
      subtract [LeftInfinity, RightInfinity] set

   let subtract_point set x =
      subtract set [LeftClosed x, RightClosed x]

   (*
    * Set union.
    *)
   let rec union set1 set2 =
      match set1, set2 with
         [], _ ->
            set2
       | _, [] ->
            set1
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval1 interval2 then
               interval1 :: union set1' set2
            else if lt_interval_interval interval2 interval1 then
               interval2 :: union set1 set2'
            else
               let interval = union_interval_interval interval1 interval2 in
               let _, right1 = interval1 in
               let _, right2 = interval2 in
                  if lt_right_right right1 right2 then
                     union set1' (interval :: set2)
                  else
                     union (interval :: set1) set2'

   let add_point set x =
      union set (of_point x)

   (*
    * Intersection.
    *)
   let rec isect set1 set2 =
      match set1, set2 with
         [], _
       | _, [] ->
            []
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval1 interval2 then
               isect set1' set2'
            else if lt_interval_interval interval2 interval1 then
               isect set1' set2'
            else
               let interval = isect_interval_interval interval1 interval2 in
                  isect (interval :: set1') set2'

   (*
    * Mapping.
    *)
   let bound_of_left = function
      LeftInfinity ->
         Infinity
    | LeftOpen x ->
         Open x
    | LeftClosed x ->
         Closed x

   let bound_of_right = function
      RightInfinity ->
         Infinity
    | RightOpen x ->
         Open x
    | RightClosed x ->
         Closed x

   let rec iter f set =
      List.iter (fun (left, right) -> f (bound_of_left left) (bound_of_right right)) set

   let rec fold f x set =
      List.fold_left (fun x (left, right) -> f x (bound_of_left left) (bound_of_right right)) x set
end

(*
 * Countable set has predecessor and successor functions.
 *)
module CountableIntervalSet (Element : CountableOrderedType) : IntervalSetSig with type elt = Element.t =
struct
   (*
    * Simple implementation is just a sorted list of
    * disjoint closed intervals.
    *)
   type elt = Element.t
   type t = (elt * elt) list

   (*
    * Short comparisons.
    *)
   let min = Element.min
   let max = Element.max
   let pred = Element.pred
   let succ = Element.succ

   (*
    * Short comparisons.
    *)
   let compare = Element.compare

   let lt x y =
      compare x y < 0

   let le x y =
      compare x y <= 0

   let eq x y =
      compare x y = 0

   (*
    * Empty set.
    *)
   let empty = []

   let max_set =
      [min, max]

   let is_empty s =
      s = []

   let is_total s =
      s = [min, max]

   let is_enum s zero =
      match s with
         [i1, _] ->
            i1 = zero
       | _ ->
            false

   let to_enum s =
      match s with
         [_, i2] ->
            succ i2
       | _ ->
            raise (Invalid_argument "to_enum")

   (*
    * Set containing one point.
    *)
   let of_point x =
      [x, x]

   let is_singleton = function
      [x, y] when eq x y ->
         true
    | _ ->
         false

   let dest_singleton = function
      [x, y] when eq x y ->
         x
    | _ ->
         raise (Invalid_argument "Lm_interval_set.dest_singleton")

   (*
    * Set containing an interval.
    *)
   let left_bound = function
      Infinity ->
         min
    | Open x ->
         if eq x max then
            raise (Invalid_argument "Lm_interval_set.left_bound");
         succ x
    | Closed x ->
         x

   let right_bound = function
      Infinity ->
         max
    | Open x ->
         if eq x min then
            raise (Invalid_argument "Lm_interval_set.right_bound");
         pred x
    | Closed x ->
         x

   let of_interval x y =
      let x = left_bound x in
      let y = right_bound y in
         if y < x then
            []
         else
            [x, y]

   (*
    * Compare limits.
    *)
   let compare_left_left x y =
      compare x y

   let compare_right_left x y =
      compare x y

   let compare_right_right x y =
      compare x y

   let ge_left_left x y =
      compare_left_left x y >= 0

   let lt_left_left x y =
      compare_left_left x y < 0

   let lt_right_right x y =
      compare_right_right x y < 0

   let le_right_right x y =
      compare_right_right x y <= 0

   let lt_right_left x y =
      compare_right_left x y < 0

   (*
    * Min-max.
    *)
   let min_left_left x y =
      if lt_left_left x y then
         x
      else
         y

   let max_right_right x y =
      if lt_right_right x y then
         y
      else
         x

   let max_left_left x y =
      if lt_left_left x y then
         y
      else
         x

   let min_right_right x y =
      if lt_right_right x y then
         x
      else
         y

   (*
    * Interval operations.
    *)
   let lt_interval_interval (_, right) (left, _) =
      lt_right_left right left

   let subset_interval_interval (left1, right1) (left2, right2) =
      ge_left_left left1 left2 && le_right_right right1 right2

   (*
    * Union assumes the two intervals overlap.
    *)
   let union_interval_interval (left1, right1) (left2, right2) =
      (min_left_left left1 left2, max_right_right right1 right2)

   (*
    * Isect assumes the two intervals overlap.
    *)
   let isect_interval_interval (left1, right1) (left2, right2) =
      (max_left_left left1 left2, min_right_right right1 right2)

   (*
    * Subtract assume sthe intervals overlap.
    *)
   let invert_left x =
      pred x

   let invert_right x =
      succ x

   let subtract_interval_interval (left1, right1) (left2, right2) =
      let compare_left = compare_left_left left1 left2 in
      let compare_right = compare_right_right right1 right2 in
         if compare_left < 0 then
            if compare_right < 0 then
               (*
                *     left1    right1
                *       |--------|
                *          |---------|
                *        left2     right2
                *)
               [left1, invert_left left2; invert_right right1, right2]
            else if compare_right = 0 then
               (*
                *     left1    right1
                *       |--------|
                *          |-----|
                *        left2 right2
                *)
               [left1, invert_left left2]
            else (* compare_right > 0 *)
               (*
                *     left1       right1
                *       |------------|
                *          |-----|
                *        left2 right2
                *)
               [left1, invert_left left2; invert_right right2, right1]

         else if compare_left = 0 then
            if compare_right < 0 then
               (*
                *     left1    right1
                *       |--------|
                *       |------------|
                *     left2        right2
                *)
               [invert_right right1, right2]
            else if compare_right = 0 then
               (*
                *     left1    right1
                *       |--------|
                *       |--------|
                *     left2    right2
                *)
               []
            else (* compare_right > 0 *)
               (*
                *     left1       right1
                *       |------------|
                *       |--------|
                *     left2    right2
                *)
               [invert_right right2, right1]

         else if compare_right <= 0 then
            (*
             *     left1    right1
             *       |--------|
             *    |--------------|
             *  left2          right2
             *)
            []

         else (* compare_right > 0 *)
            (*
             *     left1       right1
             *       |-----------|
             *    |-----------|
             *  left2       right2
             *)
            [invert_right right2, right1]

   (*
    * Join intervals that have common endpoints.
    *)
   let rec normalize = function
      (left, x) :: (y, right) :: set when eq (succ x) y ->
         normalize ((left, right) :: set)
    | h :: set ->
         h :: normalize set
    | [] ->
         []

   (*
    * Test subset.
    *)
   let rec subset set1 set2 =
      match set1, set2 with
         [], _ ->
            true
       | _, [] ->
            false
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval2 interval1 then
               subset set1 set2'
            else if subset_interval_interval interval1 interval2 then
               subset set1' set2
            else
               false

   (*
    * Test equality.
    *)
   let equal s1 s2 = subset s1 s2 && subset s2 s1

   let mem_point x set =
      subset [x, x] set

   (*
    * Set subtraction.
    *)
   let rec subtract set1 set2 =
      match set1, set2 with
         [], _ ->
            []
       | _, [] ->
            set1
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval1 interval2 then
               interval1 :: subtract set1' set2
            else if lt_interval_interval interval2 interval1 then
               subtract set1 set2'
            else
               subtract ((subtract_interval_interval interval1 interval2) @ set1') set2

   let negate set =
      subtract [min, max] set

   let subtract_point set x =
      subtract set [x, x]

   (*
    * Set union.
    *)
   let rec union_aux set1 set2 =
      match set1, set2 with
         [], _ ->
            set2
       | _, [] ->
            set1
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval1 interval2 then
               interval1 :: union_aux set1' set2
            else if lt_interval_interval interval2 interval1 then
               interval2 :: union_aux set1 set2'
            else
               let interval = union_interval_interval interval1 interval2 in
               let _, right1 = interval1 in
               let _, right2 = interval2 in
                  if lt_right_right right1 right2 then
                     union_aux set1' (interval :: set2')
                  else
                     union_aux (interval :: set1') set2'

   let union s1 s2 =
      normalize (union_aux s1 s2)

   let add_point set x =
      union set (of_point x)

   (*
    * Intersection.
    *)
   let rec isect_aux set1 set2 =
      match set1, set2 with
         [], _
       | _, [] ->
            []
       | interval1 :: set1', interval2 :: set2' ->
            if lt_interval_interval interval1 interval2 then
               isect_aux set1' set2
            else if lt_interval_interval interval2 interval1 then
               isect_aux set1 set2'
            else
               let interval = isect_interval_interval interval1 interval2 in
               let _, right1 = interval1 in
               let _, right2 = interval2 in
                  if lt_right_right right1 right2 then
                     interval :: isect_aux set1' set2
                  else
                     interval :: isect_aux set1 set2'

   let isect s1 s2 =
      normalize (isect_aux s1 s2)

   (*
    * Mapping.
    *)
   let bound_of_left x =
      Closed x

   let bound_of_right x =
      Closed x

   let rec iter f set =
      List.iter (fun (left, right) -> f (bound_of_left left) (bound_of_right right)) set

   let rec fold f x set =
      List.fold_left (fun x (left, right) -> f x (bound_of_left left) (bound_of_right right)) x set
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
