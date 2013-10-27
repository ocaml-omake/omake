(*
 * Extra utilities for lists.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999-2005 Mojave Group, Caltech
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
 * Array-style operations.
 *)
val sub : 'a list -> int -> int -> 'a list

(*
 * Iterate over two lists, but stop at the
 * end of the shorter one.
 *)
val short_iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(*
 * Iteration over three lists.
 *)
val iter3 : ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list -> 'c list -> unit

(*
 * Generalize fold_left over three lists.
 *)
val fold_left3 : ('a -> 'b -> 'c -> 'd -> 'a) -> 'a -> 'b list -> 'c list -> 'd list -> 'a

(*
 * Reverse iteration
 *)
val rev_iter : ('a -> 'b) -> 'a list -> unit
val rev_iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(*
 * Mapping functions.
 *)
val flat_map : ('a -> 'b list) -> 'a list -> 'b list
val fail_map : ('a -> 'b) -> 'a list -> 'b list
val some_map : ('a -> 'b option) -> 'a list -> 'b list
val some_map_safe : ('a -> 'a option) -> 'a list -> 'a list
val fold_left : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

(*
 * Find the element that satisfies a predicate
 *)
val find : ('a -> bool) -> 'a list -> 'a
(* Now find its index *)
val find_item : ('a -> bool) -> 'a list -> int
(* By equality *)
val find_index : 'a -> 'a list -> int
val find_rindex : 'a -> 'a list -> int
(* By pointer-equality *)
val find_indexq : 'a -> 'a list -> int
(* Use a provided equality *)
val find_index_eq : ('a -> 'a -> bool) -> 'a -> 'a list -> int

(*
 * Split the first elements from the last.
 * raises Failure if the list is empty.
 *)
val split_list : int -> 'a list -> 'a list * 'a list
val split_last : 'a list -> 'a list * 'a
val last : 'a list -> 'a

(*
 * Split the list into two parts.
 * raises Invalid_argument if the list is empty.
 *)
val split : int -> 'a list -> 'a list * 'a list

(*
 * Split up into smaller lists of size no more than n.
 *)
val splitup : int -> 'a list -> 'a list list

(*
 * Return the first n elements of the list.
 *)
val firstn : int -> 'a list -> 'a list

(*
 * Remove the first n elements of the list.
 *)
val nth_tl : int -> 'a list -> 'a list

(*
 * Replace the nth element of the list.
 *)
val replace_nth : int -> 'a -> 'a list -> 'a list

(*
 * Subtract two lists as if they were sets.
 *)
val subtract_list : 'a list -> 'a list -> 'a list

val map2to1 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_filter : ('a -> 'b -> 'a * bool) -> 'a -> 'b list -> 'a * 'b list

(*
 * fold_map
 *)
val fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val fold_map2to1 : ('a -> 'b -> 'c -> 'a * 'd) -> 'a -> 'b list -> 'c list -> 'a * 'd list
val fold_map1to2 : ('a -> 'b -> 'a * 'c * 'd) -> 'a -> 'b list -> 'a * 'c list * 'd list

(*
 * Do map and try preserving sharing
 *)
val smap : ('a -> 'a) -> 'a list -> 'a list

(*
 * check lengths equal
 *)
val length_eq : 'a list -> 'b list -> bool

(*
 * Functional replacement.
 *)
val replacef_nth     : int -> ('a -> 'a) -> 'a list -> 'a list
val replacef_arg_nth : int -> ('a -> 'a * 'b) -> 'a list -> 'a list * 'b
val replace_nth      : int -> 'a -> 'a list -> 'a list
val replaceq         : 'a -> 'a -> 'a list -> 'a list
val replace_first    : ('a -> bool) -> 'a -> 'a list -> 'a list
val replace_all      : ('a -> bool) -> 'a -> 'a list -> 'a list

(*
 * Removing elements.
 *)
val remove          : 'a -> 'a list -> 'a list
(* tryremove does not raise any exception when the element is not in the list *)
val tryremove       : 'a -> 'a list -> 'a list
val removeq         : 'a -> 'a list -> 'a list
val remove_elements : bool list -> 'a list -> 'a list
val remove_suffix   : 'a list -> 'a list -> 'a list
val insert_nth      : int -> 'a -> 'a list -> 'a list
val remove_nth      : int -> 'a list -> 'a list

(* Filter items out of a list *)
val filter : ('a -> bool) -> 'a list -> 'a list

(*
 * Set operations.
 *)
val addq       : 'a -> 'a list -> 'a list
val intersect  : 'a list -> 'a list -> 'a list
val intersectq : 'a list -> 'a list -> 'a list
val intersects : 'a list -> 'a list -> bool
val subtract   : 'a list -> 'a list -> 'a list
val subtractq  : 'a list -> 'a list -> 'a list
val subtract_multiset : 'a list -> 'a list -> 'a list
val union      : 'a list -> 'a list -> 'a list
val unionq     : 'a list -> 'a list -> 'a list
val subset     : 'a list -> 'a list -> bool

(* Lexicographic comparison of two lists *)
val compare_lists : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(* Elements must by physically equal *)
val compare_eq : 'a list -> 'a list -> bool

(* Elements must be equal, but lists may be different lengths *)
val compare_cmp : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(*
 * These functions are just like the List functions
 * but they raise Failure, not Invalid_argument.
 *)
val nth : 'a list -> int -> 'a
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(* Returns false if the list lengths mismatch *)
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(* Ignores the tail of the longer list *)
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(*
 * Association lists.
 *)
(* zip_list a b c zips b and c and puts the result in front of a in reverce order *)
val zip_list : ('a * 'b) list -> 'a list -> 'b list -> ('a * 'b) list
val zip : 'a list -> 'b list -> ('a * 'b) list
val fst_split : ('a * 'b) list -> 'a list
val assoc_index : ('a * 'b) list -> 'a -> int
val assoc_replace : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
val add_assoc : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
val assoc_in_dom : ('b -> 'a -> bool) -> 'b -> ('a * 'c) list -> bool
val assoc_in_range : ('b -> 'c -> bool) -> 'b -> ('a * 'c) list -> bool
(*
 * assoc_append_replace_snd l1 b l2 replaces the second element of all pairs in l2 with b
 * and appends l1 at the end of the result
 *)
val assoc_append_replace_snd : ('a * 'b) list -> 'b -> ('a * 'c) list -> ('a * 'b) list

(*
 * Apply a function to the value in
 * an association list.
 *)
val apply_assoc : 'a -> ('a * 'b) list -> ('b -> 'b) -> ('a * 'b) list

(*
 * Association list with an equality.
 *)
val assoc_eq : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b

(*
 * if either of the assoc list sides has duplicate entries, only the first entry is used
 * and the duplicate entry forces all second component matches to return false
 *
 * i.e. check_assoc v1 v2 [1,2; 3,2; 3,4] = (v1<>3) && (v2<>4) && check_assoc v1 v2 [1,2]
 *
 * try_check_assoc is the same as check_assoc, but raises an exception if an entry is not found
 *)
val check_assoc : 'a -> 'a -> ('a * 'a) list -> bool
val try_check_assoc : 'a -> 'b -> ('a * 'b) list -> bool

(* if left side has duplicate entries, only the first entry is used *)
val try_assoc: 'a -> ('a * 'a) list -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
