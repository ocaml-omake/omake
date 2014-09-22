(*
 * Source file tations.
 *)



type t

(*
 * Comparison.
 *)
val compare : t -> t -> int

(*
 * Don't use this if you can avoid it.
 *)
val bogus_loc : string -> t

(*
 * This is the normal way to make a tation.
 *    filename, start_line, start_char, end_line, end_char
 *)
val create_loc : Lm_symbol.t -> int -> int -> int -> int -> t

(*
 * For marshaling.
 *)
val dest_loc : t -> Lm_symbol.t * int * int * int * int

(*
 * Combine two tations.
 * The resulting span covers both.
 *)
val union_loc : t -> t -> t

(*
 * Print a file tation.
 *)
val pp_print_location : t Lm_printf.t 
val string_of_location : t -> string
