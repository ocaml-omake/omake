(*
 * We build lexers from channels.
 *)

(*
 * An action is a symbol.
 *)
module LexerAction =
struct
  type action = Lm_symbol.t
  let choose = max
  let pp_print_action = Lm_symbol.pp_print_symbol

  let hash : action -> int = Hashtbl.hash
  let compare = Lm_symbol.compare
end

module Lexer = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (LexerAction);;

(*
 * Some extra functions.
 *)
let lexer_of_string s =
   snd (Lexer.add_clause Lexer.empty  Omake_symbol.lex_sym s)

let lexer_matches info s =
   Lexer.matches info (Lm_channel.of_string s)
