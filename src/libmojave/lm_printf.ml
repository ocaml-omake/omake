
include Format
type 'a t = Format.formatter -> 'a  -> unit
(**
 * Redirect formatter's output to both a channel and a log file.
 * The log file will be appended (not truncated), lockf-mutexed,
 * and all the entries will be annotated with the PID of the logger:
 * [|PID1: string1|][|PID2: string2|]...
 * (with consequetive non-NL strings from the same PID merged together).
*)
 
let open_out name =
  formatter_of_out_channel (open_out name)

let open_out_bin name =
  formatter_of_out_channel (open_out_bin name)
 
let output_char       = pp_print_char
let output_string     = pp_print_string
 

(** Normal printing.*)
let print_char    = pp_print_char std_formatter
let print_int     = pp_print_int std_formatter
let print_string  = pp_print_string std_formatter

let prerr_char    = pp_print_char err_formatter
let prerr_int     = pp_print_int err_formatter
let prerr_string  = pp_print_string err_formatter
 
(** Print a newline and flush. *)
let flush buf  = pp_print_flush buf ()
let eflush buf = pp_print_newline buf ()
 
 
 
(*
* List separated by semicolons.
*)
let rec print_any_list print out l =
    match l with
    | [h] ->
      print out h
    | h::t ->
      print out h;
      output_string out "; ";
      print_any_list print out t
    | [] -> ()

let print_string_list = print_any_list pp_print_string

let print_int_list = print_any_list pp_print_int
 
(* Get a formatter. *)
let out_channel_of_formatter out = out

let rec pp_print_any_list print buf = function
  | [] -> ()
  | [a] ->
    print buf a
  | a::rest ->
    print buf a;
    pp_print_string buf ";";
    pp_print_space buf ();
    pp_print_any_list print buf rest
  
