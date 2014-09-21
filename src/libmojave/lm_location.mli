(*
 * Source file locations.
 *)



type loc

(*
 * Comparison.
 *)
val compare : loc -> loc -> int

(*
 * Don't use this if you can avoid it.
 *)
val bogus_loc : string -> loc

(*
 * This is the normal way to make a location.
 *    filename, start_line, start_char, end_line, end_char
 *)
val create_loc : Lm_symbol.t -> int -> int -> int -> int -> loc

(*
 * For marshaling.
 *)
val dest_loc : loc -> Lm_symbol.t * int * int * int * int

(*
 * Combine two locations.
 * The resulting span covers both.
 *)
val union_loc : loc -> loc -> loc

(*
 * Print a file location.
 *)
val pp_print_location : loc Lm_printf.t 
val string_of_location : loc -> string
