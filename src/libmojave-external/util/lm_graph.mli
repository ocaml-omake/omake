(* Undirected imperative graph implementation
 * Geoffrey Irving
 * $Id: lm_graph.mli 7878 2005-10-09 00:13:24Z jyh $ *)

(* type of undirected graphs and nodes containing type 'a *)
type 'a t
type 'a node

(* empty graph *)
val create : unit -> 'a t

(* add nodes and edges *)
val add_node : 'a t -> 'a -> 'a node
val add_edge : 'a t -> 'a node -> 'a node -> unit
val delete_node : 'a node -> unit
val delete_edge : 'a node -> 'a node -> unit

(* extract node information *)
val get : 'a node -> 'a
val degree : 'a node -> int
val neighbors : 'a node -> 'a node list
val query : 'a node -> 'a node -> bool

(* extracting single nodes *)
val choose : 'a t -> 'a node                     (* unspecified node, or Failure "choose" *)
val find : ('a node -> bool) -> 'a t -> 'a node  (* find node satisfying predicate, or Not_found *)

(* iteration over all nodes (unspecified order) *)
val list : 'a t -> 'a node list
val iter : ('a node -> unit) -> 'a t -> unit
val fold : ('a -> 'b node -> 'a) -> 'a -> 'b t -> 'a
val filter : ('a node -> bool) -> 'a t -> 'a node list

(* iteration over components (depth first search) 
 * dfs_init must be called first, but multiple components can
 * be searched after one dfs_init call *)

val dfs_init : 'a t -> unit 

val dfs_list : 'a node -> 'a node list
val dfs_list_rev : 'a node -> 'a node list  (* faster than dfs_list *)
val dfs_iter : ('a node -> unit) -> 'a node -> unit
val dfs_fold : ('a -> 'b node -> 'a) -> 'a -> 'b node -> 'a
