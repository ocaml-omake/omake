(* Standard exceptions. *)

(* open Omake_value_print. *)



let string_loc = Lm_location.bogus_loc "<Omake_env>"

let loc_of_item (x : Omake_value_type.item ) =
  match x with
  | AstExp e -> Omake_ast_util.loc_of_exp e
  | IrExp e ->
    Omake_ir_util.loc_of_exp e
  | Location loc -> loc
  | Value _
  | Symbol _
  | String _
  | Error _ -> string_loc

let rec pp_print_item buf (x  : Omake_value_type.item ) =
  match x  with
  | AstExp e ->
    Omake_ast_print.pp_print_exp buf e
  | IrExp e -> Omake_ir_print.pp_print_exp buf e
  | Location _ -> ()

  | Symbol v ->
    Lm_symbol.pp_print_symbol buf v
  | String s ->
    Lm_printf.pp_print_string buf s
  | Value v ->
    Omake_value_print.pp_print_value buf v
  | Error e ->
    pp_print_exn buf e

and pp_print_exn buf (x : Omake_value_type.omake_error )= 
  match x with 
  | SyntaxError s ->
    Format.fprintf buf "syntax error: %s" s
  | StringAstError (s, e) ->
    Format.fprintf buf "@[<hv 3>%s:@ %a@]" s Omake_ast_print.pp_print_simple_exp e
  | StringError s ->
    Lm_printf.pp_print_string buf s
  | StringIntError (s, i) ->
    Format.fprintf buf "%s: %d" s i
  | StringStringError (s1, s2) ->
    Format.fprintf buf "%s: %s" s1 s2
  | StringVarError (s, v) ->
    Format.fprintf buf "%s: %a" s Lm_symbol.pp_print_symbol v
  | StringMethodError (s, v) ->
    Format.fprintf buf "%s: %a" s Omake_print_util.pp_print_method_name v
  | StringDirError (s, n)->
    Format.fprintf buf "%s: %a" s Omake_node.pp_print_dir n
  | StringNodeError (s, n)->
    Format.fprintf buf "%s: %a" s Omake_node.pp_print_node n
  | StringValueError (s, v) ->
    Format.fprintf buf "@[<hv 3>%s:@ %a@]" s Omake_value_print.pp_print_value v
  | StringTargetError (s, t) ->
    Format.fprintf buf "%s: %a" s Omake_value_print.pp_print_target t
  | LazyError printer ->
    printer buf
  | UnboundVar v ->
    Format.fprintf buf "unbound variable: %a" Lm_symbol.pp_print_symbol v
  | UnboundVarInfo v ->
    Format.fprintf buf "unbound variable: %a" Omake_ir_print.pp_print_var_info v
  | UnboundKey v ->
    Format.fprintf buf "unbound key: %s" v
  | UnboundValue v ->
    Format.fprintf buf "unbound value: %a" Omake_value_print.pp_print_value v
  | UnboundFun v ->
    Format.fprintf buf "unbound function: %a" Lm_symbol.pp_print_symbol v
  | UnboundMethod vl ->
    Format.fprintf buf "unbound method: %a" Omake_print_util.pp_print_method_name vl
  | UnboundFieldVar (obj, v) ->
    Format.fprintf buf "@[<v 3>unbound method '%a', object classes:@ @[<b 3>" Lm_symbol.pp_print_symbol v;
    Lm_symbol.SymbolTable.iter (fun v _ ->
      Format.fprintf buf "@ %a" Lm_symbol.pp_print_symbol v) (Omake_value_type.venv_get_class obj);
    Format.fprintf buf "@]@]"
  | ArityMismatch (len1, len2) ->
    Format.fprintf buf "arity mismatch: expected %a args, got %d" 
      Omake_ir_print.pp_print_arity len1 len2
  | NotImplemented s ->
    Format.fprintf buf "not implemented: %s" s
  | NullCommand ->
    Lm_printf.pp_print_string buf "invalid null command"





module MakePos (Name : sig val name : string end) : Omake_value_type.PosSig =
struct
  include Lm_position.MakePos 
      (struct
        type t = Omake_value_type.item
        let name = Name.name
        let loc_of_value = loc_of_item
        let pp_print_value = pp_print_item
      end
      )
  (* let loc_pos_pos loc pos = *)
  (*   cons_pos (Location loc) pos *)

  let ast_exp_pos e    = base_pos (AstExp e)
  let ir_exp_pos e     = base_pos (IrExp e)
  let var_exp_pos v    = base_pos (Symbol v)
  let string_exp_pos s = base_pos (String s)
  let value_exp_pos v  = base_pos (Value v)
  let var_pos          = symbol_pos
  (* let value_pos v pos  = cons_pos (Value v) pos *)
  let error_pos e pos  = cons_pos (Error e) pos
end




