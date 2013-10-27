(*
 * Right now the symbol table is just a representation of strings.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999-2002-2005 Mojave Group, Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *
 * ----------------------------------------------------------------
 * Revision History
 *
 *  2002  Dec  4  Michael Maire  Added SymbolIndex
 *                               Added sets, tables, indices for
 *                               symbol pairs and triples
 *)
open Lm_printf

(*
 * Representation of symbols.
 *)
type symbol
type var = symbol

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
val output_symbol        : out_channel -> symbol -> unit
val output_symbol_list   : out_channel -> symbol list -> unit
val output_symbol_set    : out_channel -> SymbolSet.t -> unit

(*
 * Format versions.
 *)
val pp_print_symbol      : formatter -> symbol -> unit
val pp_print_symbol_list : formatter -> symbol list -> unit
val pp_print_symbol_set  : formatter -> SymbolSet.t -> unit
val pp_print_ext_symbol  : formatter -> symbol -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
