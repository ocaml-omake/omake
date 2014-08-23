
val debug_pos : bool ref
val trace_pos : bool ref

(*
 * Lm_position information.
 *)
type 'a pos

(*
 * Module for creating positions.
 * You have to specify the name of the module
 * where the exception are being created: use
 * MakePos in each file where Name.name is set
 * to the name of the module.
 *)
module type PosSig =
sig
  type t

  (* Creating positions *)
  val loc_exp_pos : Lm_location.loc -> t pos
  val loc_pos     : Lm_location.loc -> t pos -> t pos
  val base_pos    : t -> t pos
  val cons_pos    : t -> t pos -> t pos
  val pos_pos     : t pos -> t pos -> t pos
  val int_pos     : int -> t pos -> t pos
  val string_pos  : string -> t pos -> t pos
  val symbol_pos  : Lm_symbol.symbol -> t pos -> t pos
  val del_pos     : (Format.formatter -> unit) -> Lm_location.loc -> t pos
  val del_exp_pos : (Format.formatter -> unit) -> t pos -> t pos

  (* Utilities *)
  val loc_of_pos  : t pos -> Lm_location.loc
  val pp_print_pos  : t pos Lm_printf.t 
end

module type NameSig =
sig
  type t

  (* This is the name of the module where the position info is created *)
  val name : string

  (* Utilities for managing values *)
  val loc_of_value : t -> Lm_location.loc
  val pp_print_value : t Lm_printf.t 
end

module MakePos (Name : NameSig) : PosSig with type t = Name.t

