(*
 * This is mainly for debugging.  It will rarely used, if ever,
 * but it may be some help in tracking down GC problems.

 *)
(* open Lm_printf *)

(* external lm_heap_check : string -> unit = "lm_heap_check" *)

(* let heap_check debug = *)
(*    eprintf "%s: start@." debug; *)
(*    lm_heap_check debug; *)
(*    eprintf "%s: heap checked@." debug *)

(*
 * Normally disabled.
 *)
let heap_check _ =
   ()
