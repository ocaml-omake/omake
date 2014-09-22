(** Right now the symbol table is just a representation of strings.
    Representation of symbols. *)
type t 


(*
 * Debugging adds extra qualifiers to new symbols.
 *)
val debug_symbol : bool ref

(*
 * An "empty" variable name
 *)
val empty_var : t

(**
  Add a symbol to the table.
   {[
     Lm_symbol.add "_xyy32";;
     - : Lm_symbol.t = (32,_xyy)
   ]}
 *)
val add : string -> t
val make : string -> int -> t
val is_interned : t -> bool

val is_numeric_symbol : t -> bool

val new_symbol : t -> t
val new_symbol_pre : string -> t -> t
val new_symbol_string : string -> t
val to_int : t -> int
val to_string : t -> string

(*
 * Find a symbol for which the predicate is false.
 *)
val new_name : t -> (t -> bool) -> t
(* val new_name_gen : t -> (t -> 'a option) -> 'a *)

(*
 * Get back the string.
 *)
val string_of_symbol : t -> string
val string_of_ext_symbol : t -> string

val hash : t -> int
val eq : t -> t -> bool
val compare : t -> t -> int

(*
 * This table provides associations between symbols
 * and values.
 *)
module SymbolSet : Lm_set_sig.LmSet with type elt = t
module SymbolTable : Lm_map_sig.LmMap with type key = t
module SymbolMTable : Lm_map_sig.LmMapList with type key = t
module SymbolIndex : Lm_index.LmIndex with type key = t


val output_symbol        : t Lm_printf.t
val pp_print_symbol      : t Lm_printf.t
val dump_symbol          : t Lm_printf.t 
val pp_print_ext_symbol  : t Lm_printf.t 
val output_symbol_list   : t list Lm_printf.t
val output_symbol_set    : SymbolSet.t Lm_printf.t 
val pp_print_method_name : t list Lm_printf.t


