







open Omake_value_type



module Make (Name : sig val name : string end) : sig
   val loc_exp_pos    : Lm_location.t -> pos
   val loc_pos        : Lm_location.t -> pos -> pos

   val ast_exp_pos    : Omake_ast.exp -> pos
   val ir_exp_pos     : Omake_ir.exp -> pos
   val var_exp_pos    : Omake_ir.var -> pos
   val string_exp_pos : string -> pos
   val value_exp_pos  :  Omake_value_type.t -> pos

   val string_pos     : string -> pos -> pos
   val pos_pos        : pos -> pos -> pos
   val int_pos        : int -> pos -> pos
   val var_pos        : Omake_ir.var -> pos -> pos
   val error_pos      : omake_error -> pos -> pos

   val del_pos        : (Format.formatter -> unit) -> Lm_location.t -> pos
   val del_exp_pos    : (Format.formatter -> unit) -> pos -> pos

   (* Utilities *)
   val loc_of_pos     : pos -> Lm_location.t
   val pp_print_pos   : pos Lm_printf.t
end


