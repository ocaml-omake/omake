(*
 * Miscellanous printing utilities
 * Taken from code by Jason Hickey
 * 12apr01
 *)
open Lm_symbol
open Lm_printf

(*
 * Blocks indents by this many spaces.
 *)
let tabstop = 2

(*
 * Operators.
 *)

(*
 * Print a list of items with a separator
 *)
let rec print_sep_list sep printer = function
   [] ->
      ()
 | [h] ->
      printer h
 | h :: t ->
      printer h;
      print_string sep;
      print_space ();
      print_sep_list sep printer t

let rec print_sep_list_no_space sep printer = function
   [] ->
      ()
 | [h] ->
      printer h
 | h :: t ->
      printer h;
      print_string sep;
      print_sep_list_no_space sep printer t

let rec print_sep_list_box sep printer = function
   [] ->
      ()
 | [h] ->
      open_box tabstop;
      printer h;
      close_box ()
 | h :: t ->
      open_box tabstop;
      printer h;
      print_string sep;
      close_box ();
      print_space ();
      print_sep_list_box sep printer t

let print_fst_symbol pair =
    pp_print_symbol std_formatter (fst pair)
