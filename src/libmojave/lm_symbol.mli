(*
 * Right now the symbol table is just a representation of strings.
 *)

(*
 * Representation of symbols.
 *)
type t


(*
 * Debugging adds extra qualifiers to new symbols.
 *)
val debug_symbol : bool ref

(*
 * An "empty" variable name
 *)
val empty_var : t

(*
 * Add a symbol to the table.
 *)
val add : string -> t
val make : string -> int -> t
val is_interned : t -> bool
val reintern : t -> t
val is_numeric_symbol : t -> bool

(*
 * Make a new symbol.
 *)
val new_symbol : t -> t
val new_symbol_pre : string -> t -> t
val new_symbol_string : string -> t
val to_int : t -> int
val to_string : t -> string

(*
 * Find a symbol for which the predicate is false.
 *)
val new_name : t -> (t -> bool) -> t
val new_name_gen : t -> (t -> 'a option) -> 'a

(*
 * Get back the string.
 *)
val string_of_symbol : t -> string
val string_of_ext_symbol : t -> string

(*
 * Hash a symbol.
 *)
val hash : t -> int

(*
 * Compare two symbols for equality.
 *)
val eq : t -> t -> bool

(*
 * Ordered comparisons of symbols.
 *)
val compare : t -> t -> int

(*
 * Ordered comparisons of symbol pairs.
 *)
val compare_pair : t * t -> t * t -> int

(*
 * Ordered comparisons of symbol triples.
 *)
val compare_triple : t * t * t -> t * t * t -> int

(*
 * Ordered comparison of symbol lists.
 *)
val compare_lists : t list -> t list -> int

(*
 * We also provide a function to produce a unique integer.
 *)
val new_number : unit -> int

(*
 * This table provides associations between symbols
 * and values.
 *)
module SymbolSet : Lm_set_sig.LmSet with type elt = t
module SymbolTable : Lm_map_sig.LmMap with type key = t
module SymbolMTable : Lm_map_sig.LmMapList with type key = t
module SymbolIndex : Lm_index.LmIndex with type key = t

module SymbolPairSet : Lm_set_sig.LmSet with type elt = t * t
module SymbolPairTable : Lm_map_sig.LmMap with type key = t * t
module SymbolPairMTable : Lm_map_sig.LmMapList with type key = t * t
module SymbolPairIndex : Lm_index.LmIndex with type key = t * t

module SymbolTripleSet : Lm_set_sig.LmSet with type elt = t * t * t
module SymbolTripleTable : Lm_map_sig.LmMap with type key = t * t * t
module SymbolTripleMTable : Lm_map_sig.LmMapList with type key = t * t * t
module SymbolTripleIndex : Lm_index.LmIndex with type key = t * t * t

(*
 * Symbol lists.
 *)
module SymbolListSet   : Lm_set_sig.LmSet with type elt = t list
module SymbolListTable : Lm_map_sig.LmMap with type key = t list

(*
 * This printer uses printf.
 *)
val output_symbol        : t Lm_printf.t
val output_symbol_list   : t list Lm_printf.t
val output_symbol_set    : SymbolSet.t Lm_printf.t 

(*
 * Format versions.
 *)
val pp_print_symbol      : t Lm_printf.t
val pp_print_symbol_list : t list Lm_printf.t 
val pp_print_method_name : t list Lm_printf.t

val pp_print_symbol_set  : SymbolSet.t Lm_printf.t 
val pp_print_ext_symbol  : t Lm_printf.t 

