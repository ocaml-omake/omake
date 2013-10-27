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

(************************************************************************
 * TYPES
 ************************************************************************)

(*
 * A delayed substitution.
 *)
type ('down, 'core) core =
   Core of 'core
 | Apply of 'down * 'core

(*
 * An annotated term.
 *)
type ('label, 'up, 'down, 'core) term =
   { term_label : 'label;
     mutable term_up : 'up option;
     mutable term_core : ('down, 'core) core
   }

(*
 * A simple term does not have attributes.
 *)
type ('label, 'core) simple_term =
   { simple_label : 'label;
     simple_core : 'core
   }

(************************************************************************
 * IMPLEMENTATION
 ************************************************************************)

(*
 * Wrap a core.
 *)
let wrap_core note core =
   { term_label = note;
     term_up = None;
     term_core = Core core
   }

let wrap_simple_core note core =
   { simple_label = note;
     simple_core = core
   }

let label_of_term term =
   term.term_label

let label_of_simple_term term =
   term.simple_label

(*
 * Inherited attribute application.
 *)
let apply down_union down term =
   let { term_label = loc;
         term_core = core
       } = term
   in
   let core =
      match core with
         Core core ->
            Apply (down, core)
       | Apply (down', core) ->
            Apply (down_union down down', core)
   in
      { term_label = loc;
        term_core = core;
        term_up = None
      }

(*
 * Destruction.
 *)
let dest_core apply term =
   match term.term_core with
      Core core ->
         core
    | Apply (down, core) ->
         let core' = apply down core in
            term.term_core <- Core core';
            core'

let dest_simple_core term =
   term.simple_core

(*
 * Get the synthesized attribute.
 * This doesn't actually return the attribute directly.
 * It expects a function to compute the attribute from the
 * core and any delayed inherited attributes.
 *)
let rec dest_up synthesize1 synthesize2 term =
   match term.term_up with
      Some up ->
         up
    | None ->
         let up =
            match term.term_core with
             Core core ->
                synthesize1 core
           | Apply (down, core) ->
                synthesize2 down (synthesize1 core)
         in
            term.term_up <- Some up;
            up

(*
 * For debugging, we allow printing.
 *)
let pp_print_term pp_print_up pp_print_down pp_print_core buf t =
   let { term_up = up;
         term_core = core
       } = t
   in
      fprintf buf "@[<hv 0>@[<hv 2>{ ";
      let break_flag =
         match up with
            Some up ->
               fprintf buf "@[<hv 3>up =@ %a@]" pp_print_up up;
               true
          | None ->
               false
      in
      let break_flag, core =
         match core with
            Apply (down, core) ->
               if break_flag then
                  pp_print_space buf ();
               fprintf buf "@[<hv 3>down =@ %a@]" pp_print_down down;
               true, core
          | Core core ->
               break_flag, core
      in
         if break_flag then
            pp_print_space buf ();
         fprintf buf "@[<hv 3>core =@ %a@]@]@ }@]" pp_print_core core

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
