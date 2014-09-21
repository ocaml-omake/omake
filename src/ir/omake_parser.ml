(*
 * The OMake version of the parser uses symbols.
 *)


module Parser =
struct
   module ParserArg =
   struct
      type symbol = Lm_symbol.t

      let to_string = Lm_symbol.to_string
      let pp_print_symbol = Lm_symbol.pp_print_symbol
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
