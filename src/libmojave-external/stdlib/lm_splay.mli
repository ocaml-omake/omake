(* General splaying operations on ordered trees
 * Geoffrey Irving
 * $Id: lm_splay.mli 7878 2005-10-09 00:13:24Z jyh $ *)

(* Notes:
 *   1. All functions assume that a total order of 'a exists, and that all cuts respect it.
 *   2. A cut is basically a generalized element.  If you have a normal element y, the cut
 *      corresponding to it is compare y (partially applied).  However, cuts can also be in 
 *      between all elements (y + epsilon), or infinite (fun _ -> +-1).
 *   3. The semantics of the splay operation can be rigorously defined as follows:
 *      If splay c t = l, x, r, then union(l,x,r) = t, l < c < r, l < x < r.
 *   4. The splay functions take and return expanded nodes for efficiency reasons.  They
 *      could have been defined by
 *        val splay : 'a cut -> 'a tree -> 'a tree,
 *      but that would require duplicated matchings.  Although this make the definitions 
 *      slightly uglier, it will probably make the use of this module much cleaner.
 *   5. splay_max is equivalent to splay (fun _ -> 1), and is included for convenience and
 *      performance reasons.  The same goes for splay_min.
 *   6. This code is purely functional.
 *)

(**** types *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

(* cut x is <, =, or > 0 depending on whether a <, =, or > x, for some a *)
type 'a cut = 'a -> int

(**** splaying *)

val splay : 'a cut -> 'a tree -> 'a -> 'a tree -> 'a tree * 'a * 'a tree

val splay_max : 'a tree -> 'a -> 'a tree -> 'a tree * 'a
val splay_min : 'a tree -> 'a -> 'a tree -> 'a * 'a tree
