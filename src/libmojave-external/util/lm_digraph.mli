(* Directed imperative graph implementation
 * Geoffrey Irving
 * $Id: lm_digraph.mli 7878 2005-10-09 00:13:24Z jyh $ *)

(* I will add features to this exactly when someone
 * needs to use them. *)

(************************************** types *)

type 'a t
type 'a node

(************************************** simple routines *)

(* empty graph *)
val create : unit -> 'a t

(* graph modification *)
val add_node : 'a t -> 'a -> 'a node
val add_edge : 'a node -> 'a node -> unit
val add_multi_edge : 'a node -> 'a node -> unit   (* as above but can create multiple edges *)
val delete_node : 'a node -> unit
val delete_edge : 'a node -> 'a node -> unit

(* merge two nodes into one and return it *)
val coalesce_nodes : 'a node -> 'a node -> 'a -> 'a node

(* node information *)
val get : 'a node -> 'a
val in_degree : 'a node -> int
val out_degree : 'a node -> int
val succ : 'a node -> 'a node list
val pred : 'a node -> 'a node list
val query : 'a node -> 'a node -> bool

(************************************** structure independent operations *)

(* extracting single nodes *)
val choose : 'a t -> 'a node                     (* unspecified node, or Failure "choose" *)
val find : ('a node -> bool) -> 'a t -> 'a node  (* find node satisfying predicate, or Not_found *)

(* iteration over all nodes (unspecified order) *)
val list : 'a t -> 'a node list
val iter : ('a node -> unit) -> 'a t -> unit
val fold : ('a -> 'b node -> 'a) -> 'a -> 'b t -> 'a
val filter : ('a node -> bool) -> 'a t -> 'a node list

(************************************** depth first search and related functions *)

(* is there a (directed) path from n to m? *)
val is_path : 'a t -> 'a node -> 'a node -> bool

(* ordered node lists - only nodes reachable from the start node included in the list *)
val postorder_list : 'a t -> 'a node -> 'a node list
val rev_postorder_list : 'a t -> 'a node -> 'a node list

(* find all nodes not touched by the last search *)
val untouched : 'a t -> 'a node list

(* full graph ordered node lists (approximate) *)
val full_rev_postorder_list: 'a t -> 'a node list
