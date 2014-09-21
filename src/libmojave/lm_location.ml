(*
 * Source locations.
 *)

(* open Lm_printf. *)

(* XXX: TODO: we should switch to using MLast.loc instead *)

(*
 * A location is a character range
 *    filename, start_line, start_char, end_line, end_char
 *)
(* %%MAGICBEGIN%% *)
type loc = Lm_symbol.t * int * int * int * int
(* %%MAGICEND%% *)

(*
 * Comparison.
 *)
let compare (**)
       (v1, start_line1, start_char1, end_line1, end_char1)
       (v2, start_line2, start_char2, end_line2, end_char2) =
   let cmp = Lm_symbol.compare v1 v2 in
      if cmp = 0 then
         let cmp = start_line1 - start_line2 in
            if cmp = 0 then
               let cmp = start_char1 - start_char2 in
                  if cmp = 0 then
                     let cmp = end_line1 - end_line2 in
                        if cmp = 0 then
                           end_char1 - end_char2
                        else
                           cmp
                  else
                     cmp
            else
               cmp
      else
         cmp

(*
 * Source location if all else fails.
 *)
let bogus_loc name =
   Lm_symbol.add name, 0, 0, 0, 0

(*
 * Normal location.
 *)
let create_loc name start_line start_char end_line end_char =
   name, start_line, start_char, end_line, end_char

(*
 * For marshaling.
 *)
let dest_loc (name, start_line, start_char, end_line, end_char) =
   name, start_line, start_char, end_line, end_char

(*
 * Union of locations.
 *)
let union_loc loc1 loc2 =
   let file1, start_line1, start_char1, _, _ = loc1 in
   let file2, _, _, end_line2, end_char2 = loc2 in
      if file1 = file2 then
         (file1, start_line1, start_char1, end_line2, end_char2)
      else
         loc1

(*
 * Print a file location.
 *)
let pp_print_location buf (file, start_line, start_char, end_line, end_char) =
   Format.fprintf buf "File %a: " Lm_symbol.output_symbol file;
   if start_line = end_line then
      Format.fprintf buf "line %d, characters %d-%d" start_line start_char end_char
   else
      Format.fprintf buf "lines %d:%d-%d:%d" start_line start_char end_line end_char

let string_of_location loc =
   pp_print_location Format.str_formatter loc;
   Format.flush_str_formatter ()
