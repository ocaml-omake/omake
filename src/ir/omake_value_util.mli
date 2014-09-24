(* module type PosSig = *)
(*   sig *)
(*     val loc_exp_pos : Lm_location.t -> Omake_value_type.pos *)
(*     val loc_pos : *)
(*       Lm_location.t -> Omake_value_type.pos -> Omake_value_type.pos *)
(*     val ast_exp_pos : Omake_ast.exp -> Omake_value_type.pos *)
(*     val ir_exp_pos : Omake_ir.exp -> Omake_value_type.pos *)
(*     val var_exp_pos : Omake_ir.var -> Omake_value_type.pos *)
(*     val string_exp_pos : string -> Omake_value_type.pos *)
(*     val value_exp_pos : Omake_value_type.value -> Omake_value_type.pos *)
(*     val string_pos : string -> Omake_value_type.pos -> Omake_value_type.pos *)
(*     val pos_pos : *)
(*       Omake_value_type.pos -> Omake_value_type.pos -> Omake_value_type.pos *)
(*     val int_pos : int -> Omake_value_type.pos -> Omake_value_type.pos *)
(*     val var_pos : *)
(*       Omake_ir.var -> Omake_value_type.pos -> Omake_value_type.pos *)
(*     val error_pos : *)
(*       Omake_value_type.omake_error -> *)
(*       Omake_value_type.pos -> Omake_value_type.pos *)
(*     val del_pos : *)
(*       (Format.formatter -> unit) -> Lm_location.t -> Omake_value_type.pos *)
(*     val del_exp_pos : *)
(*       (Format.formatter -> unit) -> *)
(*       Omake_value_type.pos -> Omake_value_type.pos *)
(*     val loc_of_pos : Omake_value_type.pos -> Lm_location.t *)
(*     val pp_print_pos : Format.formatter -> Omake_value_type.pos -> unit *)
(*   end *)


val empty_obj : 'a Lm_symbol.SymbolTable.t
val class_sym : Lm_symbol.t
val venv_get_class :
  Omake_value_type.t Lm_symbol.SymbolTable.t ->
  Omake_value_type.obj Lm_symbol.SymbolTable.t


module ValueCompare :
  sig
    type t = Omake_value_type.t
    val check_simple : Omake_value_type.pos -> Omake_value_type.t -> unit
    val check :
      Omake_value_type.pos ->
      Omake_value_type.t -> Omake_value_type.t
    val tag : Omake_value_type.t -> int
    val compare : Omake_value_type.t -> Omake_value_type.t -> int
    val compare_list :
      Omake_value_type.t list -> Omake_value_type.t list -> int
  end

module ValueTable :
  sig
    type key = ValueCompare.t
    type 'a t = (ValueCompare.t, 'a) Lm_map.tree
    val empty : 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val find_key : 'a t -> key -> key option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
    val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val forall : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val find_iter : (key -> 'a -> 'b option) -> 'a t -> 'b option
    val isect_mem : 'a t -> (key -> bool) -> 'a t
    val choose : 'a t -> key * 'a
    val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
    val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
    val replace : 'a t -> key -> ('a -> 'a) -> 'a t
    val keys : 'a t -> key list
    val data : 'a t -> 'a list
    val add_list : 'a t -> (key * 'a) list -> 'a t
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  end
