(* Undirected imperative graph implementation
 * Geoffrey Irving
 * $Id: lm_graph.ml 13172 2008-08-14 17:34:16Z jyh $ *)

let rec list_remove n = function
    [] -> []
  | m :: l ->
      if n == m then
        l
      else
        m :: list_remove n l

type color = White | Black

(* nodes are stored in a semi-doubly linked list *)
type 'a node =
  { node_info : 'a;
    mutable node_degree : int;
    mutable node_edges : 'a node list;
    mutable node_color : color;

    node_next : 'a node option ref;
    mutable node_prev : 'a node option ref
  }

type 'a t = 'a node option ref

(* empty graph *)
let create () = ref None

(* node information *)
let get n = n.node_info
let degree n = n.node_degree
let neighbors n = n.node_edges
let query n m = List.memq m n.node_edges

(* add node to g with information i *)
let add_node g i =
  let l = !g in
  let next = ref l in
  let n =
    { node_info = i;
      node_degree = 0;
      node_edges = [];
      node_color = White;
      node_next = next;
      node_prev = g }
  in
    g := Some n;
    match l with
        None -> n
      | Some m -> m.node_prev <- next; n

(* add edge to g from node n to node m *)
let add_edge _g n m =
  if not (query n m) then
    (n.node_edges <- m :: n.node_edges;
     m.node_edges <- n :: m.node_edges;
     n.node_degree <- n.node_degree + 1;
     m.node_degree <- m.node_degree + 1)

let delete_half_edge n m =
  m.node_edges <- list_remove n m.node_edges;
  m.node_degree <- m.node_degree - 1

(* delete node n from graph g *)
let delete_node n =
  List.iter (delete_half_edge n) n.node_edges;
  let prev = n.node_prev in
  let next = !(n.node_next) in
    prev := next;
    match next with
        None -> ()
      | Some m -> m.node_prev <- prev

(* delete edge between nodes n and m, if it exists *)
let delete_edge n m =
  if query n m then
    (delete_half_edge n m;
     delete_half_edge m n)

(* extracting single nodes *)
let choose g =
  match !g with
      Some n -> n
    | None -> raise (Failure "choose")

let rec find p g =
  match !g with
      None -> raise Not_found
    | Some n ->
        if p n then
          n
        else
          find p n.node_next

(* iteration over all nodes (unspecified order) *)
let rec list_aux l g =
  match !g with
      None -> l
    | Some n -> list_aux (n :: l) n.node_next
let list g = list_aux [] g

let rec iter f g =
  match !g with
      None -> ()
    | Some n -> f n; iter f n.node_next

let rec fold f i g =
  match !g with
      None -> i
    | Some n -> fold f (f i n) n.node_next

let rec filter_aux l p g =
  match !g with
      None -> l
    | Some n -> filter_aux (if p n then n :: l else l) p n.node_next
let filter p g = filter_aux [] p g

(* depth first searching *)

let dfs_init g = iter (fun n -> n.node_color <- White) g

let rec dfs_list_rev_aux l n =
  if n.node_color == White then (
    n.node_color <- Black;
    List.fold_left dfs_list_rev_aux (n :: l) n.node_edges )
  else
    l

let dfs_list_rev n = dfs_list_rev_aux [] n
let dfs_list n = List.rev (dfs_list_rev n)

let rec dfs_iter f n =
  if n.node_color == White then (
    n.node_color <- Black;
    f n;
    List.iter (dfs_iter f) n.node_edges)

let rec dfs_fold f i n =
  if n.node_color == White then (
    n.node_color <- Black;
    List.fold_left (dfs_fold f) (f i n) n.node_edges)
  else
    i
