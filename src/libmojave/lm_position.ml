
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


(*
 * We include the name of the module where
 * the position is created.  The value is a location,
 * a raw value, or a value with another position.
 *)
(* %%MAGICBEGIN%% *)
type 'a pos = string * 'a exn_loc

and 'a exn_loc =
  | Loc     of Lm_location.t
  | Base    of 'a
  | Cons    of 'a * 'a pos
  | ConsLoc of Lm_location.t * 'a pos
  | Pos     of 'a pos * 'a pos
  | Int     of int * 'a pos
  | String  of string * 'a pos
  | Symbol  of Lm_symbol.t * 'a pos
  | Del     of (Format.formatter -> unit) * Lm_location.t
  | DelExp  of (Format.formatter -> unit) * 'a pos
    (* %%MAGICEND%% *)

module MakePos (Name : sig
  type t

  (* This is the name of the module where the position info is created *)
  val name : string

  (* Utilities for managing values *)
  val loc_of_t : t -> Lm_location.t
  val pp_print_t  : t Lm_printf.t 
end
) =
struct

  type t = Name.t

  (*
    * Get the source location for an exception.
    *)
  let rec loc_of_pos (_, pos) =
    match pos with
    | Loc loc
    | Del (_, loc)
    | ConsLoc (loc, _) ->
      loc
    | Base x ->
      Name.loc_of_t x
    | Cons (_, pos)
    | Pos (_, pos)
    | Int (_, pos)
    | String (_, pos)
    | Symbol (_, pos)
    | DelExp (_, pos) ->
      loc_of_pos pos

  (*
    * Print debugging info.
    *)
  let rec pp_print_pos buf (name, e) =
    match e with
    | Loc _ ->
      ()

    | Base x ->
      Format.fprintf buf "@ %s.%a" name Name.pp_print_t x

    | Cons (x, pos) ->
      pp_print_pos buf pos;
      Format.fprintf buf "@ /%s.%a" name Name.pp_print_t x

    | ConsLoc (_, pos) ->
      pp_print_pos buf pos

    | Pos (pos1, pos2) ->
      Format.fprintf buf "@ @[<v 3>Called from: %s%a@]%a" (**)
        name
        pp_print_pos pos1
        pp_print_pos pos2

    | String (s, pos) ->
      Format.fprintf buf "%a@ /%s.%s" pp_print_pos pos name s

    | Int (i, pos) ->
      Format.fprintf buf "%a@ %s.%d" pp_print_pos pos name i

    | Symbol (v, pos) ->

      Format.fprintf buf "%a@ %s.%a" pp_print_pos pos name Lm_symbol.output_symbol v

    | Del (f, _) ->
      Format.fprintf buf "@ %t" f

    | DelExp (f, pos) ->

      Format.fprintf buf "%a@ %t" pp_print_pos pos f

  (*
    * Real error printer.
    *)
  let pp_print_pos buf pos =
    Format.fprintf buf "@[<v 3>%a" Lm_location.pp_print_location (loc_of_pos pos);
    if !debug_pos then
      pp_print_pos buf pos;
    Format.fprintf buf "@]"

  (*
    * Base values.
    *)
  let loc_exp_pos loc =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.%a@." Name.name Lm_location.pp_print_location loc;
    Name.name, Loc loc

  let loc_pos loc pos =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.loc@." Name.name;
    Name.name, ConsLoc (loc, pos)

  let base_pos x =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.base@." Name.name;
    Name.name, Base x

  let pos_pos pos1 pos2 =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.pos@." Name.name;
    if !debug_pos then
      Name.name, Pos (pos1, pos2)
    else
      pos2

  let cons_pos x pos =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.cons@." Name.name;
    if !debug_pos then
      Name.name, Cons (x, pos)
    else
      pos

  let int_pos i pos =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.int: %d@." Name.name i;
    if !debug_pos then
      Name.name, Int (i, pos)
    else
      pos

  let string_pos s pos =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.string: %s@." Name.name s;
    if !debug_pos then
      Name.name, String (s, pos)
    else
      pos

  let symbol_pos v pos =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.symbol: %a@." Name.name Lm_symbol.output_symbol v;
    if !debug_pos then
      Name.name, Symbol (v, pos)
    else
      pos

  let del_pos f loc =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.delayed@." Name.name;
    Name.name, Del (f, loc)

  let del_exp_pos f pos =
    if !trace_pos then
      Format.eprintf "Lm_trace: %s.delayed@." Name.name;
    if !debug_pos then
      Name.name, DelExp (f, pos)
    else
      pos
end
