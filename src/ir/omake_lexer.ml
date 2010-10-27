(*
 * We build lexers from channels.
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

open Omake_symbol

(*
 * An action is a symbol.
 *)
module LexerAction =
struct
   type action = symbol

   let choose = max
   let pp_print_action = pp_print_symbol

   let hash = Hashtbl.hash
   let compare = Lm_symbol.compare
end

module Lexer = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (LexerAction);;

(*
 * Some extra functions.
 *)
let lexer_of_string s =
   snd (Lexer.add_clause Lexer.empty lex_sym s)

let lexer_matches info s =
   Lexer.matches info (Lm_channel.of_string s)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
