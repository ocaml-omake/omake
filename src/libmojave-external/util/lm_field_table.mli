(*
 * The FieldMTable is used for information about
 * methods and fields.  Each field in the table
 * has both an external and internal name.  Fields
 * can share external names, but internal names
 * have to be distinct.
 *
 * We use a special representation so we can get
 * all of the following:
 *    1. Strict ordering of the fields
 *    2. Quick access based on external name
 *    3. Quick access based on internal name
 *
 * The internal name table is computed lazily: we generate
 * it when we need it and save the generated table.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002-2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

open Lm_symbol

(*
 * FieldTable is a simple table, like
 * a SymbolTable that defines an ordering on
 * the fields.
 *)
module type FieldTableSig =
sig
   (*
    * The table type.
    *)
   type 'a t

   (*
    * Empty table.
    *)
   val empty : 'a t

   (*
    * Number of entries.
    *)
   val cardinal : 'a t -> int

   (*
    * Add an entry.  The first var is the external name,
    * the second var is the internal name.
    *)
   val add : 'a t -> symbol -> 'a -> 'a t

   (*
    * Add an entry with a specific index.
    * WARNING: use this only if you know what you are doing.
    * You should never add multiple entries with the same index.
    * One case where this is ok is when you construct one table from
    * another using fold_index.
    *)
   val add_index : 'a t -> symbol -> 'a -> int -> 'a t

   (*
    * Membership in the table.
    *)
   val mem : 'a t -> symbol -> bool

   (*
    * Get the entries for the name.
    *)
   val find : 'a t -> symbol -> 'a
   val find_index : 'a t -> symbol -> int
   val find_with_index : 'a t -> symbol -> 'a * int

   (*
    * Return an ordered list of fields.
    *)
   val to_list : 'a t -> (symbol * 'a) list

   (*
    * Iterators.
    *)
   val iter : (symbol -> 'a -> unit) -> 'a t -> unit
   val map  : ('a -> 'b) -> 'a t -> 'b t
   val mapi : (symbol -> 'a -> 'b) -> 'a t -> 'b t
   val fold : ('a -> symbol -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val fold_map : ('a -> symbol -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
   val fold_index : ('a -> symbol -> 'b -> int -> 'a) -> 'a -> 'b t -> 'a
end

(*
 * FieldMTable is a table that allows multiple
 * definitions for each external field name.
 *)
type ext_symbol = symbol
type int_symbol = symbol
module type FieldMTableSig =
sig
   (*
    * The table type.
    *)
   type 'a t

   (*
    * Create an empty table.
    *)
   val create : unit -> 'a t

   (*
    * Add an entry.  The first var is the external name,
    * the second var is the internal name.
    *)
   val add : 'a t -> ext_symbol -> int_symbol -> 'a -> 'a t

   (*
    * Get the entries for the external name.
    *)
   val find_ext : 'a t -> ext_symbol -> (int_symbol * 'a) list
   val find_int : 'a t -> int_symbol -> ext_symbol * 'a

   (*
    * Allow replacement of ext entries.
    * The replacement function is called on each of the entries, until
    * a valid replacement is found.  The replacement function should
    * raise Not_found if the replacement is not desired.  The first
    * replacement is accepted, and if no replacement is found, this
    * function raises Not_found.
    *)
   val replace_ext : 'a t -> ext_symbol -> (int_symbol -> 'a -> int_symbol * 'a) -> 'a t

   (*
    * Return the list of fields.
    *)
   val to_list : 'a t -> (ext_symbol * int_symbol * 'a) list

   (*
    * Iterators.
    *)
   val debug_iter : (ext_symbol -> int_symbol -> 'a -> int -> unit) -> 'a t -> unit
   val map : (ext_symbol -> int_symbol -> 'a -> int_symbol * 'b) -> 'a t -> 'b t
   val fold : ('a -> ext_symbol -> int_symbol -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

(*
 * A simple field table.
 *)
module FieldTable : FieldTableSig
module FieldMTable : FieldMTableSig

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
