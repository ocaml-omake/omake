(*
 * Miscellanous printing utilities
 * Taken from code by Jason Hickey
 * 12apr01
 *)

open Lm_symbol

val tabstop : int

val print_sep_list : string -> ('a -> unit) -> 'a list -> unit
val print_sep_list_no_space : string -> ('a -> unit) -> 'a list -> unit
val print_sep_list_box : string -> ('a -> unit) -> 'a list -> unit
val print_fst_symbol : symbol * 'a -> unit
