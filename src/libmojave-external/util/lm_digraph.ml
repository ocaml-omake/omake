(* Directed imperative graph implementation
 * Geoffrey Irving
 * $Id: lm_digraph.ml 7878 2005-10-09 00:13:24Z jyh $ *)

(************************************** types *)

type color = White | Black

(* nodes are stored in a semi-doubly linked list *)
type 'a node =
  { mutable n_info : 'a;
    mutable n_in_degree : int;
    mutable n_out_degree : int;
    mutable n_pred : 'a node list;
    mutable n_succ : 'a node list;
    mutable n_color : color;

    n_next : 'a node option ref;
    mutable n_prev : 'a node option ref
  }

type 'a t = 'a node option ref

(************************************** simple routines *)

(* empty graph *)
let create () = ref None

(* node information *)
let get n = n.n_info
let in_degree n = n.n_in_degree
let out_degree n = n.n_out_degree
let pred n = n.n_pred
let succ n = n.n_succ
let query n m = List.memq m n.n_succ

(* add node to g with information i *)
let add_node g i =
  let l = !g in
  let next = ref l in
  let n =
    { n_info = i;
      n_in_degree = 0;
      n_out_degree = 0;
      n_pred = [];
      n_succ = [];
      n_color = White;
      n_next = next;
      n_prev = g } 
  in
    g := Some n;
    match l with
        None -> n
      | Some m -> m.n_prev <- next; n

(* add edge to g from node n to node m, possibly creating parallel edges *)
let add_multi_edge n m =
  n.n_succ <- m :: n.n_succ;
  m.n_pred <- n :: m.n_pred;
  n.n_out_degree <- n.n_out_degree + 1;
  m.n_in_degree <- m.n_in_degree + 1

(* add edge to g from node n to node m *)
let add_edge n m =
  if not (query n m) then
    add_multi_edge n m

(************************************** deletion is complicated *)

let rec list_remove n l = 
  match l with
      [] -> raise (Invalid_argument "list_remove in digraph.ml")
    | m :: l ->
        if n == m then
          l
        else 
          m :: list_remove n l

(* delete from m the edge to n from m *)
let delete_half_edge_to n m =
  m.n_succ <- list_remove n m.n_succ;
  m.n_out_degree <- m.n_out_degree - 1

(* delete from m the edge from n to m *)
let delete_half_edge_from n m =
  m.n_pred <- list_remove n m.n_pred;
  m.n_in_degree <- m.n_in_degree - 1

(* delete node n from graph g *)
let delete_node n =
  List.iter (delete_half_edge_to n) n.n_pred;
  List.iter (delete_half_edge_from n) n.n_succ;
  let prev = n.n_prev in
  let next = !(n.n_next) in
    prev := next;
    match next with
        None -> ()
      | Some m -> m.n_prev <- prev

(* delete edge between nodes n and m, if it exists *)
let delete_edge n m =
  if query n m then
    (delete_half_edge_from n m;
     delete_half_edge_to m n)

(************************************** weird graph modification *)

(* second node merged into first *)
let coalesce_nodes n m i =
  n.n_info <- i;
  delete_node m;
  List.iter (add_edge n) m.n_succ;
  List.iter (fun k -> add_edge k n) m.n_pred;
  n

(************************************** structure independent operations *)

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
          find p n.n_next

(* iteration over all nodes (unspecified order) *)
let rec list_aux l g =
  match !g with
      None -> l  
    | Some n -> list_aux (n :: l) n.n_next
let list g = list_aux [] g

let rec iter f g =
  match !g with
      None -> ()
    | Some n -> f n; iter f n.n_next

let rec fold f i g =
  match !g with
      None -> i
    | Some n -> fold f (f i n) n.n_next

let rec filter_aux l p g =
  match !g with
      None -> l
    | Some n -> filter_aux (if p n then n :: l else l) p n.n_next
let filter p g = filter_aux [] p g

let rec untouched_aux l g =
  match !g with
      None -> l
    | Some n -> untouched_aux (if n.n_color == White then n :: l else l) n.n_next
let untouched g = untouched_aux [] g

(************************************** depth first search and related functions *)

let rec whitewash g =
  match !g with
      None -> ()
    | Some n -> n.n_color <- White; whitewash n.n_next

let rec is_path_aux m n =
  if n.n_color == White then (
    n.n_color <- Black;
    n == m || List.exists (is_path_aux m) n.n_succ)
  else
    false

let is_path g n m =
  whitewash g;
  is_path_aux m n

let rec rev_postorder_list_aux l n =
  if n.n_color == White then (
    n.n_color <- Black;
    n :: List.fold_left rev_postorder_list_aux l n.n_succ)
  else
    l

let rev_postorder_list g n =
  whitewash g;
  rev_postorder_list_aux [] n

let postorder_list g n =
  List.rev (rev_postorder_list g n)

(************************************** approximate full graph orderings *)

let rec full_rev_postorder_list_aux l g =
  match !g with
      None -> l
    | Some n ->
        let l = 
          if n.n_color == White then 
            rev_postorder_list_aux l n 
          else 
            l in
        full_rev_postorder_list_aux l n.n_next

let full_rev_postorder_list g =
  whitewash g;
  full_rev_postorder_list_aux [] g
        





