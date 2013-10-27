(* General splaying operations on ordered trees
 * Geoffrey Irving
 * $Id: lm_splay.ml 7878 2005-10-09 00:13:24Z jyh $ *)

(************************************** types *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

(* cut x is <, =, or > 0 depending on whether a <, =, or > x, for some a *)
type 'a cut = 'a -> int

(************************************** general top down splaying *)

(* Top-down splay routine.  Reference:
 *   "Self-adjusting Binary Search Trees", Sleator and Tarjan,
 *   JACM Volume 32, No 3, July 1985, pp 652-686.
 * See page 668 for specific splay transformations *)

(* This code takes a cut and an expanded node and returns the splayed
 * tree as an expanded node. *)

let rec splay cut l x r =
  let c = cut x in
    if c == 0 then
      l, x, r                                                          (*** success ***)
    else if c < 0 then
      match l with
          Leaf ->
            Leaf, x, r                                                 (*** trivial ***)
        | Node (ll, y, lr) ->
            let cc = cut y in
              if cc == 0 then
                ll, y, Node (lr, x, r)                                 (*** zig ***)
              else if cc < 0 then
                match ll with
                    Leaf ->
                      Leaf, y, Node (lr, x, r)                         (*** zig ***)
                  | Node (lll, z, llr) ->
                      let left, mid, right = splay cut lll z llr in
                        left, mid, Node (right, y, Node (lr, x, r))    (*** zig-zig ***)
              else (* cc > 0 *)
                match lr with
                    Leaf ->
                      ll, y, Node (Leaf, x, r)                         (*** zig ***)
                  | Node (lrl, z, lrr) ->
                      let left, mid, right = splay cut lrl z lrr in
                        Node (ll, y, left), mid, Node (right, x, r)    (*** zig-zag ***)
    else (* c > 0 *)
      match r with
          Leaf ->
            l, x, Leaf                                                 (*** trivial ***)
        | Node (rl, y, rr) ->
            let cc = cut y in
              if cc == 0 then
                Node (l, x, rl), y, rr                                 (*** zig ***)
              else if cc > 0 then
                match rr with
                    Leaf ->
                      Node (l, x, rl), y, Leaf                         (*** zig ***)
                  | Node (rrl, z, rrr) ->
                      let left, mid, right = splay cut rrl z rrr in
                        Node (Node (l, x, rl), y, left), mid, right    (*** zig-zig ***)
              else (* cc < 0 *)
                match rl with
                    Leaf ->
                      Node (l, x, Leaf), y, rr                         (*** zig ***)
                  | Node (rll, z, rlr) ->
                      let left, mid, right = splay cut rll z rlr in
                        Node (l, x, left), mid, Node (right, y, rr)    (*** zig-zag ***)

(************************************** special cases of splaying *)

(* intuitively, splay_max = splay (fun _ -> 1) *)

(* splays maximum element to the top, returns left, max (right is Leaf) *)
let rec splay_max l x r =
  match r with
      Leaf ->
        l, x                                      (*** trivial ***)
    | Node (rl, y, Leaf) ->
        Node (l, x, rl), y                        (*** zig ***)
    | Node (rl, y, Node (rrl, z, rrr)) ->
        let left, mid = splay_max rrl z rrr in
          Node (Node (l, x, rl), y, left), mid    (*** zig-zig ***)

(* splays minimum element to the top, returns min, right (left is Leaf) *)
let rec splay_min l x r =
  match l with
      Leaf ->
        x, r                                      (*** trivial ***)
    | Node (Leaf, y, lr) ->
        y, Node (lr, x, r)                        (*** zig ***)
    | Node (Node (lll, z, llr), y, lr) ->
        let mid, right = splay_min lll z llr in
          mid, Node (right, y, Node (lr, x, r))   (*** zig-zig ***)
