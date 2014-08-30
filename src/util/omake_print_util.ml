

(*
 * Method name.
 *)
let rec pp_print_method_name buf vl =
  match vl with
  |[v] ->
    Lm_symbol.pp_print_symbol buf v
  | v :: vl ->
    Lm_symbol.pp_print_symbol buf v;
    Lm_printf.pp_print_char buf '.';
    pp_print_method_name buf vl
  | [] ->
    ()

