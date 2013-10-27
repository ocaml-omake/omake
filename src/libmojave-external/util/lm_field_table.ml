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
module FieldTable : FieldTableSig =
struct
   (*
    * The table keeps tracks of the maximum number of entries,
    * and the SymbolTable.
    *)
   type 'a t =
      { table_count : int;
        table_int : ('a * int) SymbolTable.t
      }

   (*
    * Empty table.
    *)
   let empty =
      { table_count = 0;
        table_int = SymbolTable.empty
      }

   (*
    * Number of entries in the table.
    *)
   let cardinal { table_int = table } =
      SymbolTable.cardinal table

   (*
    * Add an element.
    *)
   let add table v x =
      let { table_count = index;
            table_int = itable
          } = table
      in
         { table_count = succ index;
           table_int = SymbolTable.add itable v (x, index)
         }

   (*
    * Add with a particular index.
    *)
   let add_index table v x i =
      let { table_count = index;
            table_int = itable
          } = table
      in
         { table_count = max (succ i) index;
           table_int = SymbolTable.add itable v (x, i)
         }

   (*
    * Test for membership.
    *)
   let mem table v =
      SymbolTable.mem table.table_int v

   (*
    * Find an entry in the table.
    *)
   let find table v =
      fst (SymbolTable.find table.table_int v)

   let find_index table v =
      snd (SymbolTable.find table.table_int v)

   let find_with_index table v =
      SymbolTable.find table.table_int v

   (*
    * Sort the entries into a list.
    *)
   let to_list table =
      let l =
         SymbolTable.fold (fun l v (x, i) ->
               (v, x, i) :: l) [] table.table_int
      in
      let l = List.sort (fun (_, _, i1) (_, _, i2) -> i1 - i2) l in
         List.map (fun (v, x, _) -> v, x) l

   (*
    * Iteration.
    *)
   let iter f table =
      SymbolTable.iter (fun v (x, _) -> f v x) table.table_int

   (*
    * Map a function over the table.
    *)
   let map f table =
      { table with table_int = SymbolTable.map (fun (x, i) -> f x, i) table.table_int }

   let mapi f table =
      { table with table_int = SymbolTable.mapi (fun v (x, i) -> f v x, i) table.table_int }

   (*
    * Folding.
    *)
   let fold f x table =
      SymbolTable.fold (fun x v (y, _) ->
            f x v y) x table.table_int

   (*
    * Pass the index.
    *)
   let fold_index f x table =
      SymbolTable.fold (fun x v (y, i) ->
            f x v y i) x table.table_int

   (*
    * Fold with map.
    *)
   let fold_map f x table =
      let x, table_int =
         SymbolTable.fold_map (fun x v (y, i) ->
               let x, y = f x v y in
                  x, (y, i)) x table.table_int
      in
         x, { table with table_int = table_int }
end

module FieldMTable : FieldMTableSig =
struct
   (*
    * The table keeps track of the number of fields.
    * The table is constructed based on the external table,
    * and the internal table is computed lazily.
    *)
   type 'a t =
      { table_count : int;
        table_ext : (symbol * 'a * int) SymbolMTable.t;
        mutable table_int : (symbol * 'a * int) SymbolTable.t option
      }

   (*
    * Empty table, note that we can't define an "empty" table
    * because of the lazy computation.
    *)
   let create () =
      { table_count = 0;
        table_ext = SymbolMTable.empty;
        table_int = None
      }

   (*
    * Add an entry with an external and internal name.
    *)
   let add table v_ext v_int x =
      let { table_count = index;
            table_ext = etable
          } = table
      in
         { table_count = succ index;
           table_ext = SymbolMTable.add etable v_ext (v_int, x, index);
           table_int = None
         }

   (*
    * Filter the entries.
    *)
   let replace_ext table v_ext replace =
      (* Search takes one def. and calls our function with its v_int and data *)
      let rec search = function
         (v_int, x, index) as def :: defs ->
             (try
                let v_int', x' = replace v_int x in
                   (v_int', x', index) :: defs
             with
                Not_found ->
                   def :: search defs)
       | [] ->
            raise Not_found
      in
         { table with
            table_ext = SymbolMTable.filter table.table_ext v_ext search;
            table_int = None
         }

   (*
    * Get the internal table.
    * Compute it if is has not been defined yet.
    *)
   let get_int_table table =
      match table.table_int with
         Some int_table ->
            int_table
       | None ->
            let int_table =
               SymbolMTable.fold_all (fun int_table v_ext defs ->
                     List.fold_left (fun int_table (v_int, x, index) ->
                           SymbolTable.add int_table v_int (v_ext, x, index)) int_table defs) SymbolTable.empty table.table_ext
            in
               table.table_int <- Some int_table;
               int_table

   (*
    * Get a sorted list of fields.
    * Raises Failure if the field indexes
    * are not valid.
    *)
   let to_list table =
      (* Collect the fields and check their indexes *)
      let rec collect index fields' fields =
         match fields with
            (v_ext, v_int, ty, index') :: fields ->
               let index = pred index in
                  if index' <> index then
                     raise (Failure
                           "FieldMTable.to_list: fields are out of order");
                  collect index ((v_ext, v_int, ty) :: fields') fields
          | [] ->
               if index <> 0 then
                  raise (Failure "FieldMTable.to_list: some fields are missing");
               fields'
      in

      (* Sort the methods in reverse order *)
      let fields =
         SymbolTable.fold (fun fields v_int (v_ext, x, index) ->
               (v_ext, v_int, x, index) :: fields) [] (get_int_table table)
      in
      let fields = List.sort (fun (_, _, _, index1) (_, _, _, index2) ->
                                    index2 - index1) fields in
         collect table.table_count [] fields

   (*
    * Find all the entries based on the external name.
    *)
   let find_ext table v_ext =
      let fields = SymbolMTable.find_all_partial table.table_ext v_ext in
         List.map (fun (v', x, _) -> v', x) fields

   (*
    * Find an entry based on internal name.
    *)
   let find_int table v_int =
      let v, x, _ = SymbolTable.find (get_int_table table) v_int in
         v, x

   (*
    * Iterate over the table.
    * This is the debug version: it passes the indexes to the function.
    *)
   let debug_iter f table =
      SymbolMTable.iter_all (fun v_ext defs ->
            List.iter (fun (v_int, x, index) ->
                  f v_ext v_int x index) defs) table.table_ext

   (*
    * Map a function over the elements.
    *)
   let map f table =
      let etable =
         SymbolMTable.mapi_all (fun v_ext defs ->
               List.map (fun (v_int, x, index) ->
                     let v_int, x = f v_ext v_int x in
                        v_int, x, index) defs) table.table_ext
      in
         { table with table_ext = etable; table_int = None }

   (*
    * Fold a function over the elements.
    *)
   let fold f arg table =
      SymbolMTable.fold_all (fun arg v_ext defs ->
            List.fold_left (fun arg (v_int, x, _) ->
                  f arg v_ext v_int x) arg defs) arg table.table_ext
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
