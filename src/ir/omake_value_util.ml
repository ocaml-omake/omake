open Omake_value_type
(*
 * Position printer.
 *)
module type PosSig =
sig
   val loc_exp_pos    : Lm_location.t -> pos
   val loc_pos        : Lm_location.t -> pos -> pos

   val ast_exp_pos    : Omake_ast.exp -> pos
   val ir_exp_pos     : Omake_ir.exp -> pos
   val var_exp_pos    : Omake_ir.var -> pos
   val string_exp_pos : string -> pos
   val value_exp_pos  : t -> pos

   val string_pos     : string -> pos -> pos
   val pos_pos        : pos -> pos -> pos
   val int_pos        : int -> pos -> pos
   val var_pos        : Omake_ir.var -> pos -> pos
   val error_pos      : omake_error -> pos -> pos

   val del_pos        : (Format.formatter -> unit) -> Lm_location.t -> pos
   val del_exp_pos    : (Format.formatter -> unit) -> pos -> pos

   (* Utilities *)
   val loc_of_pos     : pos -> Lm_location.t
   val pp_print_pos   : Format.formatter -> pos -> unit
end

(************************************************************************
 * Basic values and functions.
 *)

(*
 * Empty object.
 *)
let empty_obj = Lm_symbol.SymbolTable.empty

(*
 * Get the class identifiers from the object.
 *)
let class_sym = Lm_symbol.add "$class"

let venv_get_class obj =
   match Lm_symbol.SymbolTable.find obj class_sym with
   |   ValClass table -> table
   | _ -> Lm_symbol.SymbolTable.empty 
   | exception Not_found -> Lm_symbol.SymbolTable.empty 

(************************************************************************
 * Value table.
 *)
module ValueCompare =
struct
   type t = Omake_value_type.t

   (*
    * Check for simple values.
    * Arrays cannot be nested.
    *)
   let check_simple pos v =
      match v with
         ValNone
       | ValInt _
       | ValFloat _
       | ValData _
       | ValNode _
       | ValDir _
       | ValOther (ValLocation _)
       | ValOther (ValExitCode _)
       | ValVar _ ->
            ()
       | _ ->
            raise (OmakeException (pos, StringValueError ("illegal Map key", v)))

   let check pos v =
      (match v with
          ValArray vl ->
             List.iter (check_simple pos) vl
        | _ ->
             check_simple pos v);
      v

   (*
    * Compare two simple values.
    *)
   let tag = function
      ValNone                  -> 0
    | ValInt _                 -> 1
    | ValFloat _               -> 2
    | ValArray _               -> 3
    | ValData _                -> 4
    | ValNode _                -> 5
    | ValDir _                 -> 6
    | ValOther (ValExitCode _) -> 7
    | ValOther (ValLocation _) -> 8
    | ValVar _                 -> 9
    | _ ->
         raise (Invalid_argument "ValueCompare: value not supported")

   let compare_values = (* Stdlib.*) compare

   let rec compare v1 v2 =
      match v1, v2 with
         ValNone, ValNone ->
            0
       | ValInt i1, ValInt i2
       | ValOther (ValExitCode i1), ValOther (ValExitCode i2) ->
            if i1 < i2 then
               -1
            else if i1 > i2 then
               1
            else
               0
       | ValFloat x1, ValFloat x2 ->
            if x1 < x2 then
               -1
            else if x1 > x2 then
               1
            else
               0
       | ValArray a1, ValArray a2 ->
            compare_list a1 a2
       | ValData s1, ValData s2 ->
            compare_values s1 s2
       | ValNode node1, ValNode node2 ->
            Omake_node.Node.compare node1 node2
       | ValDir dir1, ValDir dir2 ->
            Omake_node.Dir.compare dir1 dir2
       | ValOther (ValLocation loc1), ValOther (ValLocation loc2) ->
            Lm_location.compare loc1 loc2
       | ValVar (_, v1), ValVar (_, v2) ->
            Omake_ir_util.VarInfoCompare.compare v1 v2
       | _ ->
            tag v1 - tag v2

   and compare_list l1 l2 =
      match l1, l2 with
         v1 :: l1, v2 :: l2 ->
            let cmp = compare v1 v2 in
               if cmp = 0 then
                  compare_list l1 l2
               else
                  cmp
       | [], [] ->
            0
       | [], _ :: _ ->
            -1
       | _ :: _, [] ->
            1
end;;

module ValueTable = Lm_map.LmMakeRec (ValueCompare);;

