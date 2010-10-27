(*
 * The OMake version of the parser uses symbols.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * 
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol

module Parser =
struct
   module ParserArg =
   struct
      type symbol = Lm_symbol.symbol

      let to_string = to_string
      let pp_print_symbol = pp_print_symbol
      let hash_symbol = Hashtbl.hash
      let compare_symbol = Lm_symbol.compare

      let eof = Lm_symbol.add "<eof>"

      module Action = Omake_lexer.LexerAction;;

      type action = Action.action
      let hash_action = Action.hash
      let compare_action = Action.compare
      let pp_print_action = Action.pp_print_action
   end

   include Lm_parser.MakeParser (ParserArg) (Lm_parser.ParserPrecedence)

   let empty = add_prec empty prec_min (Lm_symbol.add ".min")
   let empty = add_prec empty prec_max (Lm_symbol.add ".max")
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
