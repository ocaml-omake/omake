(*
 * Right now the symbol table is just a representation of strings.
 *)
(* open! Lm_printf. *)

(*
 * Representation of symbols.
 *)
type symbol


(*
 * Debugging adds extra qualifiers to new symbols.
 *)
val debug_symbol : bool ref

(*
 * An "empty" variable name
 *)
val empty_var : symbol

(*
 * Add a symbol to the table.
 *)
val add : string -> symbol
val make : string -> int -> symbol
val is_interned : symbol -> bool
val reintern : symbol -> symbol
val is_numeric_symbol : symbol -> bool

(*
 * Make a new symbol.
 *)
val new_symbol : symbol -> symbol
val new_symbol_pre : string -> symbol -> symbol
val new_symbol_string : string -> symbol
val to_int : symbol -> int
val to_string : symbol -> string

(*
 * Find a symbol for which the predicate is false.
 *)
val new_name : symbol -> (symbol -> bool) -> symbol
val new_name_gen : symbol -> (symbol -> 'a option) -> 'a

(*
 * Get back the string.
 *)
val string_of_symbol : symbol -> string
val string_of_ext_symbol : symbol -> string

(*
 * Hash a symbol.
 *)
val hash : symbol -> int

(*
 * Compare two symbols for equality.
 *)
val eq : symbol -> symbol -> bool

(*
 * Ordered comparisons of symbols.
 *)
val compare : symbol -> symbol -> int

(*
 * Ordered comparisons of symbol pairs.
 *)
val compare_pair : symbol * symbol -> symbol * symbol -> int

(*
 * Ordered comparisons of symbol triples.
 *)
val compare_triple : symbol * symbol * symbol -> symbol * symbol * symbol -> int

(*
 * Ordered comparison of symbol lists.
 *)
val compare_lists : symbol list -> symbol list -> int

(*
 * We also provide a function to produce a unique integer.
 *)
val new_number : unit -> int

(*
 * This table provides associations between symbols
 * and values.
 *)
module SymbolSet : Lm_set_sig.LmSet with type elt = symbol
module SymbolTable : Lm_map_sig.LmMap with type key = symbol
module SymbolMTable : Lm_map_sig.LmMapList with type key = symbol
module SymbolIndex : Lm_index.LmIndex with type key = symbol

module SymbolPairSet : Lm_set_sig.LmSet with type elt = symbol * symbol
module SymbolPairTable : Lm_map_sig.LmMap with type key = symbol * symbol
module SymbolPairMTable : Lm_map_sig.LmMapList with type key = symbol * symbol
module SymbolPairIndex : Lm_index.LmIndex with type key = symbol * symbol

module SymbolTripleSet : Lm_set_sig.LmSet with type elt = symbol * symbol * symbol
module SymbolTripleTable : Lm_map_sig.LmMap with type key = symbol * symbol * symbol
module SymbolTripleMTable : Lm_map_sig.LmMapList with type key = symbol * symbol * symbol
module SymbolTripleIndex : Lm_index.LmIndex with type key = symbol * symbol * symbol

(*
 * Symbol lists.
 *)
module SymbolListSet   : Lm_set_sig.LmSet with type elt = symbol list
module SymbolListTable : Lm_map_sig.LmMap with type key = symbol list

(*
 * This printer uses printf.
 *)
val output_symbol        : symbol Lm_printf.t
val output_symbol_list   : symbol list Lm_printf.t
val output_symbol_set    : SymbolSet.t Lm_printf.t 

(*
 * Format versions.
 *)
val pp_print_symbol      : symbol Lm_printf.t
val pp_print_symbol_list : symbol list Lm_printf.t 
val pp_print_method_name : symbol list Lm_printf.t

val pp_print_symbol_set  : SymbolSet.t Lm_printf.t 
val pp_print_ext_symbol  : symbol Lm_printf.t 

