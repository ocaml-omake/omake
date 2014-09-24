(* Standard exceptions. *)



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







module Make (Name : sig val name : string end) =
struct
  include Lm_position.MakePos 
      (struct
        type t = Omake_value_type.item
        let name = Name.name
        let loc_of_t = loc_of_item
        let pp_print_t = Omake_value_print.pp_print_item
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




