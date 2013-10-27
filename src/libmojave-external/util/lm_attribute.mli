(*
 * This is a lazy attribute-grammar.
 * A term has:
 *
 *    0. A "core" value of the term (called 'core)
 *
 *    1. A label of some arbitrary type (called 'label)
 *
 *    2. A synthesized attribute (called 'up).  The attribute
 *       is computed lazily.  The value is arbitrary, but the
 *       calling module has to provide a function to compute the
 *       attribute for the term.
 *
 *    3. An inherited attribute (called 'down).  Inherited attributes
 *       transform the core value, and are mapped from the top
 *       down.  The calling module has to supply a function to
 *       apply the attribute to the core value.
 *
 *
 * The common example is:
 *
 *    1. The label is a source position in the program.
 *    2. The synthesized attribute is the free variables of the term.
 *    3. The inherited attribute is a substitution.
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
open Lm_printf

(*
 * An annotated term.
 *)
type ('label, 'up, 'down, 'core) term

(*
 * An unannotated term.
 *)
type ('label, 'core) simple_term

(*
 * Create a term.
 *)
val wrap_core : 'label -> 'core -> ('label, 'up, 'down, 'core) term
val wrap_simple_core : 'label -> 'core -> ('label, 'core) simple_term

(*
 * Get the label.
 *)
val label_of_term : ('label, 'up, 'down, 'core) term -> 'label
val label_of_simple_term : ('label, 'core) simple_term -> 'label

(*
 * Apply an inherited attribute.
 * This expects a function to take the union
 * of two inherited attributes.
 *)
val apply : ('down -> 'down -> 'down) -> 'down -> ('label, 'up, 'down, 'core) term -> ('label, 'up, 'down, 'core) term

(*
 * Get the core value.
 *)
val dest_core : ('down -> 'core -> 'core) -> ('label, 'up, 'down, 'core) term -> 'core
val dest_simple_core : ('label, 'core) simple_term -> 'core

(*
 * Get the synthesized attribute.
 * It expects a function to compute the attribute from the
 * core and any delayed inherited attributes.
 *)
val dest_up : ('core -> 'up) -> ('down -> 'up -> 'up) -> ('label, 'up, 'down, 'core) term -> 'up

(*
 * For debugging.
 *)
val pp_print_term :
   (formatter -> 'up -> unit) ->
   (formatter -> 'down -> unit) ->
   (formatter -> 'core -> unit) ->
   formatter -> ('label, 'up, 'down, 'core) term -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
