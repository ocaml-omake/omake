(*
 * This module provides a linearly ordered numbered set implementation
 * with lazy functions based on splay trees
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_array_util
open Lm_linear_set_sig

type ind = int
type 'a tree =
   Leaf
 | Node of ind * 'a * 'a linear_set * 'a linear_set * int
 | Lazy of ('a -> 'a) * 'a linear_set
 | Offset of int * 'a linear_set
and 'a linear_set = { mutable tree : 'a tree }

module Make (Type : TypeSig) =
struct
   type elt = Type.t
   type t = Type.t linear_set
   type index = ind

   type direction =
      Left of t
    | Right of t

   let empty = { tree = Leaf }

   let singleton x = { tree = Node (0,x,empty,empty,1) }

   open Lm_printf

   let rec print_aux s = function
      { tree = Leaf } -> ()
    | { tree = Node (i,_,t1,t2,_) } ->
      print_aux (s^"/") t1;
      printf "%s%d\n" s i;
      print_aux (s^"\\") t2;
    | { tree = Lazy (_,t) } ->
         printf "%s Lazy\n" s;
         print_aux (s^"|") t
    | { tree = Offset (i,t) } ->
         printf "%s Offset %d" s i;
         print_aux (s^"|") t

   let print s t = print_aux s t; printf "%t" flush

   let rec length t = match t.tree with
      Leaf  -> 0
    | Node (_,_,_,_,i) -> i
    | Lazy (_,t) | Offset (_,t) -> length t

   let rec create_aux start num elt =
      if num=0 then Leaf else
      let numl = num/2 in
      let offr = succ numl in
      let numr = num - offr in
      let numi = start + numl in
      let startr = succ numi in
         Node ( numi,elt,
            { tree = create_aux start numl elt },
            { tree = create_aux startr numr elt }, num )

   let create i e = { tree = create_aux 0 i e }
   let make = create

   let rec go_down f = function
      { tree = Leaf } -> Leaf
    | { tree = Node (i,e,t1,t2,n) } ->
         Node (i,f e, { tree = go_down f t1 }, { tree = go_down f t2 }, n)
    | { tree = Lazy (f2,tree) } as t ->
         t.tree <- go_down f2 tree;
         go_down f t
    | { tree = Offset(i,t) } -> Offset(i,{ tree = go_down f t })

   let rec to_list_aux collect = function
      { tree = Leaf } -> collect
    | { tree = Node (_,e,t1,t2,_) } ->
         to_list_aux (e::(to_list_aux collect t2)) t1
    | { tree = Lazy (f,tree) } as t ->
         t.tree <- go_down f tree;
         to_list_aux collect t
    | { tree = Offset (_,t) } ->
         to_list_aux collect t

   let to_list t = to_list_aux [] t

   let rec iter ( f : elt -> unit ) = function
      { tree = Leaf } -> ()
    | { tree = Node (_,e,t1,t2,_) } ->
      iter f t1;
      f e;
      iter f t2
    | { tree = Lazy (f',tree) } as t ->
         t.tree <- go_down f' tree;
         iter f t
    | { tree = Offset (_,t) } ->
         iter f t

   let rec map ( f : elt -> elt ) = function
      { tree = Leaf } -> { tree = Leaf }
    | { tree = Node (ind, e, t1, t2, i) } ->
         { tree = Node (ind, f e, map f t1, map f t2, i) }
    | { tree = Lazy (f', tree) } as t ->
         t.tree <- go_down f' tree;
         map f t
    | { tree = Offset (i, t) } ->
         { tree = Offset (i, map f t) }

   let rec fold (f : 'a -> index -> elt -> 'a) x = function
      { tree = Leaf } ->
         x
    | { tree = Node (ind, e, t1, t2, _) } ->
        fold f (f (fold f x t1) ind e) t2
    | { tree = Lazy (f', tree) } as t ->
         t.tree <- go_down f' tree;
         fold f x t
    | { tree = Offset (_, t) } ->
         fold f x t

   let rec of_list_aux max start lst =
      match max, lst with
         _,[]
       | 0,_ ->
            empty, start, lst
       | _ -> begin
            match of_list_aux (max/2) start lst with
               _, _, [] as c -> c
             | left, lend, h::t ->
                  let right, rend, rlst = of_list_aux (max - max/2 - 1) (succ lend) t in
                  { tree = Node (lend,h,left,right,rend-start) },rend,rlst
         end

   let of_list l =
      match of_list_aux (List.length l) 0 l with
         t,_,[] -> (* print "of_list " t; *) t
       | _ -> raise (Invalid_argument "Linear_set.of_list")

   let lazy_apply f t = { tree = Lazy (f,t) }

   let append t1 e t2 =
      let s1 = length t1 in
      let ss1 = succ s1 in
      { tree = Node (s1,e,t1,{ tree = Offset (ss1,t2) }, ss1+(length t2)) }

   (* XXX: this is very slow of course *)
   let concat t1 t2 =
      of_list (to_list t1 @ to_list t2)

   let compose g f x = g(f(x))

   let rec offset_down off = function
      { tree = Offset (off2, t) } ->
         offset_down (off+off2) t
    | { tree = (Node (i,e,t1,t2,n) as node) } ->
         if off=0 then node else
         Node (i+off,e,{tree = Offset(off,t1)}, {tree=Offset(off,t2)},n)
    | { tree = Leaf } -> Leaf
    | t -> push_down t; offset_down off t

   and func_down f = function
      { tree = Lazy(g,t) } ->
         func_down (compose f g) t
    | { tree = Node (i,e,t1,t2,n) } ->
         Node (i, f e, {tree = Lazy (f,t1)}, {tree=Lazy(f,t2)},n)
    | { tree = Leaf } -> Leaf
    | t -> push_down t; func_down f t

   and push_down = function
      { tree = Offset (off, t') } as t ->
         t.tree <- offset_down off t'
    | { tree = Lazy (f,t') } as t ->
         t.tree <- func_down f t'
    | _ -> ()

   let new_node i e l r =
      { tree = Node(i,e,l,r,succ (length l) + (length r)) }

   let rec rotate_left = function
      { tree = Node (i,e,{tree=Node(il,el,left_left,left_right,_)},right,size) } as t ->
         t.tree <- Node(il,el,left_left,new_node i e left_right right,size)
    | _ -> raise (Invalid_argument "Linear_set.rotate_left")

   (* This should not happen ?
    | { tree = Node (_,_,tl,_,_) } as t ->
         push_down tl;
         rotate_left t
    | t ->
         push_down t;
         rotate_left t
   *)

   let rec rotate_right = function
      { tree = Node (i,e,left,{tree=Node(ir,er,right_left,right_right,_)},size) } as t ->
         t.tree <- Node(ir,er,new_node i e left right_left,right_right,size)
    | _ -> raise (Invalid_argument "Linear_set.rotate_right")

   (* This should not happen ?
    | { tree = Node (_,_,_,tr,_) } as t ->
         push_down tr;
         rotate_right t
    | t ->
         push_down t;
         rotate_right t
   *)

   let rec lift = function
      [] -> ()
    | [Left parent] ->
         rotate_left parent
    | [Right parent] ->
         rotate_right parent
    | Left _parent :: Left grandparent :: ancestors ->
         (
            rotate_left grandparent;
            rotate_left grandparent;  (* parent has moved into grandparent's pos
ition *)
            lift ancestors
         )
    | Right _parent :: Right grandparent :: ancestors ->
         (
            rotate_right grandparent;
            rotate_right grandparent;  (* parent has moved into grandparent's po
sition *)
            lift ancestors
         )
    | Left parent :: Right grandparent :: ancestors ->
         (
            rotate_left parent;
            rotate_right grandparent;
            lift ancestors
         )
    | Right parent :: Left grandparent :: ancestors ->
         (
            rotate_right parent;
            rotate_left grandparent;
            lift ancestors
         )

  let rec splay ind path = function
      { tree = Node (i, _, left, right, _) } as t ->
         if i = ind then
            lift path
         else if ind < i then
            splay ind (Left t :: path) left
         else
            splay ind (Right t :: path) right
    | { tree = Leaf } ->
         raise (Invalid_argument "Linear_set.splay - index out of bounds")
    | t ->
         push_down t;
         splay ind path t

   let get t ind =
      (* printf "\n\nGET %d\n" ind;
      print "get " t; *)
      splay ind [] t;
      match t.tree with
         Node (_,e,_,_,_) -> e
       | _ -> raise (Invalid_argument "Linear_set.get")

   let split t ind =
      (* print "split " t; *)
      splay ind [] t;
      match t.tree with
         Node (i,e,l,r,_) -> l,e,{ tree = Offset(succ i,r) }
       | _ -> raise (Invalid_argument "Linear_set.split")

   let lazy_sub_map f t i len =
      if len=0 then empty else
      let r,lt = if i=0 then t,length t else begin
         (* printf "\n\nSUB_MAP %d %d\n" i len;
         print "sub_map " t; *)
         splay i [] t;
         match t.tree with
            Node (_,_,_,r,n) -> r,n
          | _ -> raise (Invalid_argument "Linear_set.lazy_sub_map")
      end in
      let seg_end = i+len in
      let c = if seg_end = lt then r else begin
         (* print "sub_map_ii " r; *)
         splay seg_end [] r;
         match r.tree with
            Node (_,_,c,_,_) -> c
          | _ -> raise (Invalid_argument "Linear_set.lazy_sub_map")
      end in
      let oc = if i=0 then c else { tree = Offset (i,c) } in
         { tree = Lazy (f,oc) }

   let rec join t1 t2 = match t1,t2 with
      { tree = Leaf }, _ -> t2
    | _, { tree = Leaf } -> t1
    | { tree = Offset _ }, _
    | { tree = Lazy _ }, _ ->
         push_down t1;
         join t1 t2
    | _, { tree = Offset _ }
    | _, { tree = Lazy _ } ->
         push_down t2;
         join t1 t2
    | { tree = Node (_,_,_,_,n1) }, _ -> begin
         (* print "join " t2; *)
         splay 0 [] t2;
         match t2 with
            { tree = Node (0,e,_,r,n2) } ->
               { tree = Node (n1,e,t1,{ tree = Offset (succ n1,r) },n1+n2) }
          | _ -> raise (Invalid_argument "Linear_set.join")
      end

   let append_list t1 l t2 = match l with
      [] -> join t1 t2
    | [e] -> append t1 e t2
    | e::tl -> begin
         let l1 = length t1 in
         let ll = List.length tl in
         match of_list_aux (pred ll) l1 tl with
            tlt,off,[tle] ->
               let soff = succ off in
               { tree = Node (off,tle,
                  { tree = Node (succ l1, e, t2, tlt, off) },
                  { tree = Offset (soff,t2) },
                  (soff+(length t2))) }
          | _ -> raise (Invalid_argument "Linear_set.append_list")
      end

   let rec mapi f = function
      { tree = Leaf } as t -> t
    | { tree = Node(i,e,l,r,n) } as t ->
         let l' = mapi f l in
         let e' = f i e in
         let r' = mapi f r in
         if (l'==l) && (e'==e) && (r'==r) then t
         else { tree = Node (i,e',l',r',n) }
    | t ->
         push_down t;
         mapi f t

   let rec init_aux f max start =
      if max = 0 then empty else
      let m2=max/2 in
      let lend = start+m2 in
      let left = init_aux f m2 start in
      let right = init_aux f (max-m2-1) (succ lend) in
      { tree = Node (lend,f lend,left,right,max) }

   let init i f = init_aux f i 0

   let rec parts_length len = function
      [] ->
         len
    | part :: parts ->
         match part with
            ArrayElement _ ->
               parts_length (succ len) parts
          | ArrayArray (a, i, len') ->
               if i < 0 || len' < 0 || i + len' > length a then
                  raise (Invalid_argument "Linear_set.collect")
               else
                  parts_length (len + len') parts

   let rec strip = function
      ArrayArray (_,_,0)::tl -> strip tl
    | l -> l

   let rec collect_aux min start lst =
      match min, lst with
         _, ArrayArray (_,_,0)::tl ->
            collect_aux min start tl
       | _,[]
       | 0,_ ->
            empty, start, lst
       | 1, (ArrayElement x)::tl ->
            { tree = Node (start,x,empty,empty,1) },(succ start),(strip tl)
       | _, ArrayArray (t,0,l)::tl when l>=min ->
            let t' = if l = (length t) then t else begin
               (* print "collect_aux_iv " t; *)
               splay l [] t;
               match t with
                  { tree = Node (_,_,l,_,_) } ->
                     if start=0 then l else { tree = Offset (start,l) }
                | _ -> raise (Invalid_argument "Linear_set.collect")
            end in
               t',start+(length t'),(strip tl)
       | _, ArrayArray (t,k,l)::tl when l>=min ->
            (* print "collect_aux " t; *)
            splay (pred k) [] t;
            begin match t with
               { tree = Node (_,_,_,r,_) } ->
                  let r' = if k+l = (length t) then r else begin
                     (* print "collect_aux_ii " r; *)
                     splay (k+l) [] r;
                     match r with
                        { tree = Node (_,_,l,_,_) } ->
                           if k=start then l else { tree = Offset (start-k,l) }
                      | _ -> raise (Invalid_argument "Linear_set.collect")
                  end in
                     r',start+(length r'),(strip tl)
             | _ -> raise (Invalid_argument "Linear_set.collect")
            end
       | _ ->
            begin match collect_aux (min/2) start lst with
               (_left,_lend,[]) as c -> c
             | left, lend, (ArrayElement h)::t ->
                  let right, rend, rlst = collect_aux (min - min/2 - 1) (succ lend) t in
                  { tree = Node (lend,h,left,right,rend-start) },rend,rlst
             | left, lend, (ArrayArray (a,i,len))::t ->
                  (* print "collect_aux_iii " a; *)
                  splay i [] a;
                  begin match a with
                     { tree = Node (_,h,_,_,_) } ->
                        let right, rend, rlst = collect_aux (min - min/2 - 1) (succ lend) (ArrayArray (a,succ i,pred len)::t) in
                        { tree = Node (lend,h,left,right,rend-start) },rend,rlst
                   | _ -> raise (Invalid_argument "Linear_set.collect")
                  end
            end

   let collect l =
      match collect_aux (parts_length 0 l) 0 l with
         t,_,[] -> t
       | _ -> raise (Invalid_argument "Linear_set.collect")

   let rec for_all (f : elt -> bool) = function
      { tree = Leaf } ->
         true
    | { tree = Node (_ind, e, t1, t2, _i) } ->
         f e && for_all f t1 && for_all f t2
    | { tree = Lazy (f', tree) } as t ->
         t.tree <- go_down f' tree;
         for_all f t
    | { tree = Offset (_i, t) } ->
         for_all f t

   let rec exists (f : elt -> bool) = function
      { tree = Leaf } ->
         false
    | { tree = Node (_ind, e, t1, t2, _i) } ->
         f e || exists f t1 || exists f t2
    | { tree = Lazy (f', tree) } as t ->
         t.tree <- go_down f' tree;
         exists f t
    | { tree = Offset (_i, t) } ->
         exists f t
end
