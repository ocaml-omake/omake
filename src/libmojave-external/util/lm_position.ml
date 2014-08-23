open! Lm_printf

let debug_pos =
  Lm_debug.create_debug (**)
    { debug_name = "pos";
      debug_description = "print verbose position information for debugging";
      debug_value = false
    }

let trace_pos =
  Lm_debug.create_debug (**)
    { debug_name = "trace_pos";
      debug_description = "print position trace for debugging";
      debug_value = false
    }

(************************************************************************
 * TYPES
 ************************************************************************)

(*
 * We include the name of the module where
 * the position is created.  The value is a location,
 * a raw value, or a value with another position.
 *)
(* %%MAGICBEGIN%% *)
type 'a pos = string * 'a exn_loc

and 'a exn_loc =
  | DebugLoc     of Lm_location.loc
  | DebugBase    of 'a
  | DebugCons    of 'a * 'a pos
  | DebugConsLoc of Lm_location.loc * 'a pos
  | DebugPos     of 'a pos * 'a pos
  | DebugInt     of int * 'a pos
  | DebugString  of string * 'a pos
  | DebugSymbol  of Lm_symbol.symbol * 'a pos
  | DebugDel     of (out_channel -> unit) * Lm_location.loc
  | DebugDelExp  of (out_channel -> unit) * 'a pos
    (* %%MAGICEND%% *)

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
  val del_pos     : (out_channel -> unit) -> Lm_location.loc -> t pos
  val del_exp_pos : (out_channel -> unit) -> t pos -> t pos

  (* Utilities *)
  val loc_of_pos : t pos -> Lm_location.loc
  val pp_print_pos : t pos Lm_printf.t 
end

module type NameSig =
sig
  type t

  (* This is the name of the module where the position info is created *)
  val name : string

  (* Utilities for managing values *)
  val loc_of_value : t -> Lm_location.loc
  val pp_print_value  : t Lm_printf.t 
end

(************************************************************************
 * IMPLEMENTATION
 ************************************************************************)

module MakePos (Name : NameSig) =
struct
  open Name

  type t = Name.t

  (*
    * Get the source location for an exception.
    *)
  let rec loc_of_pos (_, pos) =
    match pos with
      DebugLoc loc
    | DebugDel (_, loc)
    | DebugConsLoc (loc, _) ->
      loc
    | DebugBase x ->
      loc_of_value x
    | DebugCons (_, pos)
    | DebugPos (_, pos)
    | DebugInt (_, pos)
    | DebugString (_, pos)
    | DebugSymbol (_, pos)
    | DebugDelExp (_, pos) ->
      loc_of_pos pos

  (*
    * Print debugging info.
    *)
  let rec pp_print_pos buf (name, e) =
    match e with
    | DebugLoc _ ->
      ()

    | DebugBase x ->
      Lm_printf.fprintf buf "@ %s.%a" name pp_print_value x

    | DebugCons (x, pos) ->
      pp_print_pos buf pos;
      Lm_printf.fprintf buf "@ /%s.%a" name pp_print_value x

    | DebugConsLoc (_, pos) ->
      pp_print_pos buf pos

    | DebugPos (pos1, pos2) ->
      Lm_printf.fprintf buf "@ @[<v 3>Called from: %s%a@]%a" (**)
        name
        pp_print_pos pos1
        pp_print_pos pos2

    | DebugString (s, pos) ->
      pp_print_pos buf pos;
      Lm_printf.fprintf buf "@ /%s.%s" name s

    | DebugInt (i, pos) ->
      pp_print_pos buf pos;
      Lm_printf.fprintf buf "@ %s.%d" name i

    | DebugSymbol (v, pos) ->
      pp_print_pos buf pos;
      Lm_printf.fprintf buf "@ %s.%a" name Lm_symbol.output_symbol v

    | DebugDel (f, _) ->
      Lm_printf.fprintf buf "@ %t" f

    | DebugDelExp (f, pos) ->
      pp_print_pos buf pos;
      Lm_printf.fprintf buf "@ %t" f

  (*
    * Real error printer.
    *)
  let pp_print_pos buf pos =
    Lm_printf.fprintf buf "@[<v 3>%a" Lm_location.pp_print_location (loc_of_pos pos);
    if !debug_pos then
      pp_print_pos buf pos;
    Lm_printf.fprintf buf "@]"

  (*
    * Base values.
    *)
  let loc_exp_pos loc =
    if !trace_pos then
      eprintf "Lm_trace: %s.%a@." name Lm_location.pp_print_location loc;
    name, DebugLoc loc

  let loc_pos loc pos =
    if !trace_pos then
      eprintf "Lm_trace: %s.loc@." name;
    name, DebugConsLoc (loc, pos)

  let base_pos x =
    if !trace_pos then
      eprintf "Lm_trace: %s.base@." name;
    name, DebugBase x

  let pos_pos pos1 pos2 =
    if !trace_pos then
      eprintf "Lm_trace: %s.pos@." name;
    if !debug_pos then
      name, DebugPos (pos1, pos2)
    else
      pos2

  let cons_pos x pos =
    if !trace_pos then
      eprintf "Lm_trace: %s.cons@." name;
    if !debug_pos then
      name, DebugCons (x, pos)
    else
      pos

  let int_pos i pos =
    if !trace_pos then
      eprintf "Lm_trace: %s.int: %d@." name i;
    if !debug_pos then
      name, DebugInt (i, pos)
    else
      pos

  let string_pos s pos =
    if !trace_pos then
      eprintf "Lm_trace: %s.string: %s@." name s;
    if !debug_pos then
      name, DebugString (s, pos)
    else
      pos

  let symbol_pos v pos =
    if !trace_pos then
      eprintf "Lm_trace: %s.symbol: %a@." name Lm_symbol.output_symbol v;
    if !debug_pos then
      name, DebugSymbol (v, pos)
    else
      pos

  let del_pos f loc =
    if !trace_pos then
      eprintf "Lm_trace: %s.delayed@." name;
    name, DebugDel (f, loc)

  let del_exp_pos f pos =
    if !trace_pos then
      eprintf "Lm_trace: %s.delayed@." name;
    if !debug_pos then
      name, DebugDelExp (f, pos)
    else
      pos
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
*)
