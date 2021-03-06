(*
 * Lexer for the omake language.
 * This is a little difficult because indentation is
 * significant, and we want it to work in interactive mode
 * too.
 *
 * GS. Also includes the entry points for parsing:
 * - parse_ast:     parse a file
 * - parse_string:  parse a string
 *)

{

include Omake_pos.Make (struct let name = "Omake_ast_lex" end)


let debug_lex =
   Lm_debug.create_debug (**)
      { debug_name = "debug-ast-lex";
        debug_description = "Print tokens as they are scanned";
        debug_value = false
      }

(*
 * Current mode:
 *    ModeNormal: normal lexing mode
 *    ModeString s: parsing a literal string, dollar sequences are still expanded,
 *       s is the quotation delimiter
 *    ModeSkipString s :parsing a literal string, dollar sequences are still expanded,
 *       s is the quotation delimiter, skip the token if it is a quote that is not s
 *    ModeQuote s: parsing a literal string, dollar sequences are still expanded,
 *       escape sequences are allowed, s is the quotation delimiter.
 *
 * GS. The main entry is lex_line (below). Depending on the current mode,
 * a different lexer function is invoked:
 *
 * ModeNormal:      calls lex_main
 * ModeString:      calls lex_string, for text in $+dquote (e.g. $"")
 * ModeSkipString:  calls lex_skip_string. This is used after newlines inside
 *                  $-dquoted-text for checking whether the matching end
 *                  quote is following. Fairly technical.
 * ModeQuote:       calls lex_quote, for text after dquote
 *)
type mode =
   ModeNormal
 | ModeSkipString of string
 | ModeString of string
 | ModeQuote of string

(*
 * The lexing mode.
 *    ModeInitial: lexbuf is ready to be used
 *    ModeIndent i: initial indentation has been scanned
 *    ModeNormal: normal processing
 *
 * GS. LexModeInitial means we are at the beginning of the line. LexModeNormal
 * means that we've just lexed the left indentation.
 *)
type lexmode =
   LexModeInitial
 | LexModeNormal of int

(*
 * Parsing results.
 *)
type parse_item =
   ParseExp of Omake_ast.exp list
 | ParseError
 | ParseEOF

(*
 * This is the info for each indentation level.
 *)
type info =
   { info_mode     : mode;
     info_indent   : int;
     info_parens   : int option
   }

(*
 * State of the lexer.
 *)
type session =
   { (* The current location *)
     current_file            : Lm_symbol.t;
     mutable current_line    : int;
     mutable current_off     : int;
     mutable current_loc     : Lm_location.t;

     (* GS TODO: line/off/loc is now tracked by lexbuf (it wasn't in ancient
        versions of OCaml). Remove this here, and rely on lexbuf only.
      *)

     (* The current input buffer *)
     mutable current_buffer  : string;
     mutable current_index   : int;
     mutable current_prompt  : string;
     mutable current_fill_ok : bool;
     mutable current_eof     : bool;
     readline                : (string -> string);
     mutable is_interactive  : bool;

     (* The current lexbuf *)
     mutable current_lexbuf  : Lexing.lexbuf;
     mutable current_lexmode : lexmode;
     mutable current_token   : Omake_ast_parse.token;

     (* The current mode *)
     mutable current_mode    : mode;
     mutable current_parens  : int option;
     mutable current_indent  : int;
     mutable current_stack   : info list
   }

(************************************************************************
 * Printing.  NOTICE: if new tokens are added, please update
 * the token list in omake_gen_parse.ml!!!
 *)
let pp_print_token buf = function
    Omake_ast_parse.TokEof _ ->
       Lm_printf.pp_print_string buf "<eof>"
  | TokEol _ ->
       Lm_printf.pp_print_string buf "<eol>"
  | TokWhite (s, _) ->
       Format.fprintf buf "whitespace: \"%s\"" s
  | TokLeftParen (s, _) ->
       Format.fprintf buf "left parenthesis: %s" s
  | TokRightParen (s, _) ->
       Format.fprintf buf "right parenthesis: %s" s
  | TokArrow (s, _) ->
       Format.fprintf buf "arrow: %s" s
  | TokComma (s, _) ->
       Format.fprintf buf "comma: %s" s
  | TokColon (s, _) ->
       Format.fprintf buf "colon: %s" s
  | TokDoubleColon (s, _) ->
       Format.fprintf buf "doublecolon: %s" s
  | TokNamedColon (s, _) ->
       Format.fprintf buf "named colon: %s" s
  | TokDollar (s, strategy, _) ->
       Format.fprintf buf "dollar: %s%a" s Omake_ast_print.pp_print_strategy strategy
  | TokEq (s, _) ->
       Format.fprintf buf "equals: %s" s
  | TokArray (s, _) ->
       Format.fprintf buf "array: %s" s
  | TokDot (s, _) ->
       Format.fprintf buf "dot: %s" s
  | TokId (s, _) ->
       Format.fprintf buf "id: %s" s
  | TokInt (s, _) ->
       Format.fprintf buf "int: %s" s
  | TokFloat (s, _) ->
       Format.fprintf buf "float: %s" s
  | TokKey (s, _) ->
       Format.fprintf buf "key: %s" s
  | TokKeyword (s, _) ->
       Format.fprintf buf "keyword: %s" s
  | TokCatch (s, _) ->
       Format.fprintf buf "catch: %s" s
  | TokClass (s, _) ->
       Format.fprintf buf "class: %s" s
  | TokVar (_, s, _) ->
       Format.fprintf buf "var: %s" s
  | TokOp (s, _) ->
       Format.fprintf buf "op: %s" s
  | TokString (s, _) ->
       Format.fprintf buf "string: \"%s\"" (String.escaped s)
  | TokBeginQuote (s, _) ->
       Format.fprintf buf "begin-quote: %s" s
  | TokEndQuote (s, _) ->
       Format.fprintf buf "end-quote: %s" s
  | TokBeginQuoteString (s, _) ->
       Format.fprintf buf "begin-quote-string: %s" s
  | TokEndQuoteString (s, _) ->
       Format.fprintf buf "end-quote-string: %s" s
  | TokStringQuote (s, _) ->
       Format.fprintf buf "quote: %s" s
  | TokVarQuote (_, s, _) ->
       Format.fprintf buf "key: %s" s

(*
 * Set state.
 *)
let create name readline =
   let loc = Lm_location.bogus_loc name in
      { current_file    = Lm_symbol.add name;
        current_line    = 1;
        current_off     = 0;
        current_loc     = loc;
        current_buffer  = "";
        current_index   = 0;
        current_prompt  = ">";
        current_fill_ok = true;
        current_eof     = true;
        readline        = readline;
        is_interactive  = false;
        current_lexbuf  = Lexing.from_string "";
        current_lexmode = LexModeInitial;
        current_token   = TokEof loc;
        current_mode    = ModeNormal;
        current_parens  = None;
        current_indent  = 0;
        current_stack   = []
      }

(* let set_current_loc state loc = *)
(*    state.current_loc <- loc *)

let current_location state =
   state.current_loc

(*
 * Advance a line.
 *)
let set_next_line state lexbuf =
   let { current_line = line;
         current_file = file;
         _
       } = state
   in
   let line = succ line in
      state.current_line  <- line;
      state.current_off   <- Lexing.lexeme_start lexbuf;
      state.current_loc   <- Lm_location.create_loc file line 0 line 0

(*
 * Save the state.
 *)
let save_mode state =
   let { current_mode   = mode';
         current_parens = parens;
         current_indent = indent;
         current_stack  = stack;
         _
       } = state
   in
   let info =
      { info_mode   = mode';
        info_parens = parens;
        info_indent = indent
      }
   in
      info :: stack

(*
 * Restore the state.
 *)
let restore_mode state stack =
   match stack with
      info :: stack ->
         state.current_mode    <- info.info_mode;
         state.current_parens  <- info.info_parens;
         state.current_indent  <- info.info_indent;
         state.current_stack   <- stack
    | [] ->
         ()

(*
 * Push the new mode.
 *)
let push_mode state mode =
   let stack = save_mode state in
      state.current_mode   <- mode;
      state.current_parens <- None;
      state.current_stack  <- stack

(*
 * Pop the mode.
 *)
let pop_mode state =
   restore_mode state state.current_stack

(*
 * We are moving from a quotation to normal mode.
 * Start collecting parentheses.
 *)
let push_dollar state mode =
   push_mode state mode;
   state.current_parens <- Some 0

(* GS. The reason for counting open parentheses (in current_parens) is that
   a line feed is interpreted differently while there is an open parenthesis.
 *)

(*
 * Push a paren.
 *)
let push_paren state =
   let { current_parens = parens ; _} = state in
      match parens with
         Some i ->
            state.current_parens <- Some (succ i)
       | None ->
            ()

(*
 * When a paren is popped, if the level becomes zero,
 * then return to the previous mode.
 *)
let pop_paren state =
   let { current_parens = parens ; _} = state in
      match parens with
         Some i ->
            let i = pred i in
               if i = 0 then
                  pop_mode state
               else
                  state.current_parens <- Some i
       | None ->
            ()

(*
 * Get the location of the current lexeme.
 * We assume it is all on one line.
 *)
let lexeme_loc state lexbuf =
   let { current_line = line;
         current_off  = off;
         current_file = file;
         _
       } = state
   in
   let schar = Lexing.lexeme_start lexbuf - off in
   let echar = Lexing.lexeme_end lexbuf - off in
   let loc = Lm_location.create_loc file line schar line echar in
      state.current_loc <- loc;
      loc
(* GS TODO: use Lexing.lexeme_start_p and Lexing.lexeme_end_p instead *)


(*
 * Raise a syntax error exception.
 *)
let parse_error state =
   let lexbuf = state.current_lexbuf in
   let loc = lexeme_loc state lexbuf in
   let print_error buf =
      Format.fprintf buf "unexpected token: %a" pp_print_token state.current_token
   in
      raise (Omake_value_type.OmakeException (loc_exp_pos loc, LazyError print_error))

let syntax_error state s lexbuf =
   let loc = lexeme_loc state lexbuf in
      raise (Omake_value_type.OmakeException (loc_exp_pos loc, SyntaxError s))

(*
 * Get the string in the lexbuf.
 *)
let lexeme_string state lexbuf =
   let loc = lexeme_loc state lexbuf in
   let s = Lexing.lexeme lexbuf in
      s, loc

(*
 * Remove any trailing dots from the string.
 *)
(* let split_nl_string s = *)
(*    let len = String.length s in *)
(*    let rec search i = *)
(*       if i = len then *)
(*          s, "" *)
(*       else *)
(*          match s.[i] with *)
(*              '\n' *)
(*            | '\r' -> *)
(*                  search (succ i) *)
(*            | _ -> *)
(*                  String.sub s 0 i, String.sub s i (len - i) *)
(*    in *)
(*       search 0 *)

(*
 * Process a name.
 *)
let lexeme_name state lexbuf =
   let id, loc = lexeme_string state lexbuf in
      match id with
         "if"
       | "elseif"
       | "else"
       | "switch"
       | "match"
       | "select"
       | "case"
       | "default"
       | "section"
       | "include"
       | "extends"
       | "import"
       | "try"
       | "when"
       | "finally"
       | "raise"
       | "return"
       | "export"
       | "open"
       | "autoload"
       | "declare"
       | "value"
       | "with"
       | "as"
       | "while"
       | "do"
       | "set"
       | "program-syntax" ->
             Omake_ast_parse.TokKeyword (id, loc)
       | "catch" ->
             TokCatch (id, loc)
       | "class" ->
             TokClass (id, loc)
       | _ ->
             TokId (id, loc)

let lexeme_key state lexbuf =
    let id, loc = lexeme_string state lexbuf in
    Omake_ast_parse.TokKey (id, loc)

(*
 * Get the escaped char.
 * GS. e.g. "\X" -> "X"
 *)
let lexeme_esc state lexbuf =
   let s, loc = lexeme_string state lexbuf in
      String.make 1 s.[1], loc

(*
 * Single character variable.
 * GS. $x (not $(...)). Also $`x and $,x.
 *)
let lexeme_var state lexbuf =
  let s, loc = lexeme_string state lexbuf in
  let strategy, s =
    match s.[1] with
    | '`' -> Omake_ast.LazyApply, String.sub s 2 1
    | ',' -> EagerApply, String.sub s 2 1
    | _ -> NormalApply, String.sub s 1 1
  in
  Omake_ast_parse.TokVar (strategy, s, loc)

(*
 * Dollar sequence.
 *)
let lexeme_dollar_pipe state lexbuf =
   let s, loc = lexeme_string state lexbuf in
   let len = String.length s in
   let strategy, off =
      if len >= 2 then
         match s.[1] with
            '`'  -> Omake_ast.LazyApply, 2
          | ','  -> EagerApply, 2
          | '|'  -> NormalApply, 1
          | _    -> syntax_error state ("illegal character: " ^ s) lexbuf
      else
         NormalApply, 1
   in
   let s = String.sub s off (String.length s - off) in
      strategy, s, loc

(* GS. Unclear why there are two versions of this function. lexeme_dollar
   seems to be the usual function, for all of $` $, $$
 *)

let lexeme_dollar state lexbuf =
   let s, loc = lexeme_string state lexbuf in
   let len = String.length s in
      if len >= 2 then
         match s.[1] with
            '`'  -> Omake_ast_parse.TokDollar (s, LazyApply, loc)
          | ','  -> TokDollar (s, EagerApply, loc)
          | '$'  -> TokString ("$", loc)
          | _    -> syntax_error state ("illegal character: " ^ s) lexbuf
      else
         TokDollar (s, NormalApply, loc)

(*
 * Special character.
 * Keep track of paren nesting.
 *)
let lexeme_char state lexbuf =
  let s, loc = lexeme_string state lexbuf in
  match s.[0] with
    '$' ->
    Omake_ast_parse.TokDollar (s, NormalApply, loc)
  | ':' ->
    TokColon (s, loc)
  | ',' ->
    TokComma (s, loc)
  | '=' ->
    TokEq (s, loc)
  | '.' ->
    TokDot (s, loc)
  | '%' ->
    TokVar (NormalApply, s, loc)
  | '(' ->
    push_paren state;
    TokLeftParen (s, loc)
  | ')' ->
    pop_paren state;
    TokRightParen (s, loc)
  | _   ->
    TokOp (s, loc)

(*
 * Special string.
 *)
let lexeme_special_string state lexbuf =
  let s, loc = lexeme_string state lexbuf in
  match s with
    "=>" ->
    Omake_ast_parse.TokArrow (s, loc)
  | "::" ->
    TokDoubleColon (s, loc)
  | "+=" ->
    TokEq (s, loc)
  | "[]" ->
    TokArray (s, loc)
  | _ ->
    TokOp (s, loc)

(*
 * Count the indentation in a string of characters.
 *)
let indent_of_string s =
   let len = String.length s in
   let rec loop col i =
      if i = len then
         col
      else
         match s.[i] with
            '\r'
          | '\n' ->
             loop 0 (succ i)
          | '\t' ->
             loop ((col + 8) land (lnot 7)) (succ i)
          | _ ->
             loop (succ col) (succ i)
   in
      loop 0 0

(*
 * Use lexer positions.
 *)
let lexeme_pos lexbuf =
   let s = Lexing.lexeme lexbuf in
   let pos1 = Lexing.lexeme_start_p lexbuf in
   let pos2 = Lexing.lexeme_end_p lexbuf in
   let { Lexing.pos_fname = file;
         Lexing.pos_lnum = line1;
         Lexing.pos_bol = bol1;
         Lexing.pos_cnum = cnum1
       } = pos1
   in
   let { Lexing.pos_lnum = line2;
         Lexing.pos_bol = bol2;
         Lexing.pos_cnum = cnum2;
         _
       } = pos2
   in
   let loc = Lm_location.create_loc (Lm_symbol.add file) line1 (cnum1 - bol1) line2 (cnum2 - bol2) in
      s, loc
}

(*
 * White space.
 * Line is terminated by '\n' or eof,
 * but be nice to DOS.
 *)
let whitec          = [' ' '\t' '\012']
let white           = whitec +
let opt_white       = whitec *

let strict_nl       = "\r\n" | ['\n' '\r']
let white_nl        = opt_white strict_nl
let strict_eol      = strict_nl | eof

(*
 * Identifiers and keywords.
 *)
let name_prefix     = ['_' 'A'-'Z' 'a'-'z' '0'-'9' '@']
let name_suffix     = ['_' 'A'-'Z' 'a'-'z' '0'-'9' '-' '~' '@']
let name            = name_prefix name_suffix* | '[' | ']'
let key             = ['~' '?'] name_suffix+
  (* GS. Named function arguments, as in OCaml *)

(*
 * Numbers.
 *)
let binary          = "0b" ['0'-'1']*
let octal           = "0o" ['0'-'7']*
let decimal         = ['0'-'9']+
let hex             = "0x" ['0'-'9' 'a'-'f' 'A'-'F']*
let integer         = binary | octal | decimal | hex

let float_exp       = ['e' 'E'] ['-' '+']? ['0'-'9']+
let float1          = ['0'-'9']* '.' ['0'-'9'] float_exp?
let float2          = ['0'-'9']+ float_exp
let float           = float1 | float2

(*
 * Comments begin with a # symbol and continue to end-of-line.
 * Comments are relaxed w.r.t. leading whitespace.
 *)
let comment         = opt_white '#' [^ '\n']*
let comment_nl      = comment strict_nl
let comment_eol     = comment strict_eol

(*
 * Quotes.
 *)
let squote         = ['\'']+
let dquote         = ['"']+
let pipe           = ['|']+
let quote          = squote | dquote | pipe
let quote_opt      = quote?

(*
 * Special variables.
 * GS. This refers to one-character dollar refs, without parentheses.
 *)
let dollar         = '$' ['`' ',' '$']
let paren_dollar   = '$' ['`' ',']?
let special_sym    = ['@' '&' '*' '<' '^' '+' '?' 'A'-'Z' 'a'-'z' '_' '0'-'9' '~' '[' ']']
let special_var    = paren_dollar special_sym

(*
 * Named colon separators.
 *)
let special_colon   = ':' name ':'

(*
 * Escape sequences.
 *)
let esc_char            = '\\' ['$' '(' ')' ':' ',' '=' '#' '\\' '\'' '"' ' ' '\t']
let esc_quote           = '\\' ['\\' '\'' '"']
let esc_line            = '\\' strict_eol

(*
 * Special sequences.
 *)
let special_char        = ['$' '(' ')' ':' ',' ';' '=' '.' '%' '+' '-' '*' '/' '<' '>' '^' '&' '|']
let special_string      = "=>" | "::" | "+=" | "[]" | "<<" | ">>" | ">>>" | "&&" | "||" | "..." | "[...]"

(*
 * Other stuff that is not names or special characters.
 *)
let other_char          = [^ ' ' '\t' '\012' '\n' '\r'
                           '_' 'A'-'Z' 'a'-'z' '0'-'9' '-' '?' '@' '~'
                           '$' '(' ')' ':' ',' ';' '=' '\\' '#' '%' '[' ']' '.' '"' '\''
			   '<' '>' '^' '|' '&' '*' '/' '+']
let other_drive         = ['A'-'Z' 'a'-'z'] ':' ['\\' '/']
let other_prefix        = other_char | other_drive
let other_special       = ['~' '?']
let other_suffix1       = name_suffix | other_prefix | other_special
let other_suffix2       = other_prefix | other_special
let other               = other_prefix other_suffix1 * | other_special other_suffix2 *

(*
 * A string is anything but a quote, dollar, or backslash.
 *)
let string_text = [^ '\'' '"' '$' '\\' '\r' '\n']+
let literal_text  = [^ '\'' '"' '|' '\r' '\n']+

(*
 * Main lexer.
 *)
rule lex_main state = parse
   white_nl
 | comment_nl
   { let loc = state.current_loc in
     let _ = lexeme_loc state lexbuf in
        set_next_line state lexbuf;
        Omake_ast_parse.TokEol loc
   }
 | white
   { let s, loc = lexeme_string state lexbuf in
        TokWhite (s, loc)
   }
   (* Note: many numbers are also identifiers,
    * like the decimal numbers, etc.  We can define the
    * regular expressions normally, but give precedence
    * to identifiers. *)
   (* GS TODO. This doesn't seem to be correct. The regexp for [name] also
      parses [integer] and some forms of [float]. Because ocamllex picks
      the first regexp when two regexps match the same string, [name] is
      always preferred.
    *)
 | name
   { lexeme_name state lexbuf }
 | integer
   { let s, loc = lexeme_string state lexbuf in
        TokInt (s, loc)
   }
 | float
   { let s, loc = lexeme_string state lexbuf in
        TokFloat (s, loc)
   }
 | key        (* GS: name prefixed with '?' or '~' (named arguments) *)
   { lexeme_key state lexbuf }
 | ['\'' '"']
   { let id, loc = lexeme_string state lexbuf in
     let mode = ModeQuote id in
        push_mode state mode;
        TokBeginQuoteString (id, loc)
   }
 (* GS. Remember dquote and squote can be several quotes *)
 | '$' dquote
   { let id, loc = lexeme_string state lexbuf in
     let id = String.sub id 1 (pred (String.length id)) in
              (* GS TODO: use "as" *)
     let mode = ModeString id in
        push_mode state mode;
        TokBeginQuote ("", loc)
   }
 | '$' squote
   { let id, _ = lexeme_string state lexbuf in
     let id = String.sub id 1 (pred (String.length id)) in
              (* GS TODO: use "as" *)
     let s, loc = lex_literal state (Buffer.create 32) id lexbuf in
     (* GS: lex_literal is a sublexer. Returns the quoted string *)
        TokStringQuote (s, loc)
   }
 (* GS: e.g. $||text||, this is used for map keys *)
 | paren_dollar pipe
   { let strategy, id, _ = lexeme_dollar_pipe state lexbuf in
     let s, loc = lex_literal state (Buffer.create 32) id lexbuf in
        TokVarQuote (strategy, s, loc)
   }
 (* $ plus a single character *)
 | special_var
   { lexeme_var state lexbuf }
 (* other $ *)
 | dollar
   { lexeme_dollar state lexbuf }
 (* $ ( ) : , ; = . % + - * / < > ^ & | *)
 | special_char
   { lexeme_char state lexbuf }
 (* => :: += [] << >> >>> && || ... [...] *)
 | special_string
   { lexeme_special_string state lexbuf }
 | special_colon
   { let s, loc = lexeme_string state lexbuf in
        TokNamedColon (s, loc)
   }
 | other
   { let s, loc = lexeme_string state lexbuf in
        TokString (s, loc)
   }
 | '\\'
   { let s, loc = lexeme_string state lexbuf in
        TokString (s, loc)
   }
 | esc_char
   { let s, loc = lexeme_esc state lexbuf in
        TokStringQuote (s, loc)
   }
 | esc_line
   { let loc = lexeme_loc state lexbuf in
        set_next_line state lexbuf;
        state.current_prompt <- "\\";
        state.current_fill_ok <- true;
        TokString (" ", loc)
   }
 | eof
   { let loc = lexeme_loc state lexbuf in
        match state.current_token with
           TokEol _
         | TokEof _ ->
              TokEof loc
         | _ ->
              TokEol loc
   }
 | _
   { let s, _ = lexeme_string state lexbuf in
        syntax_error state ("illegal character: " ^ String.escaped s) lexbuf
   }

(*
 * Inline text.  We allow any text, but dollars are expanded,
 * escape sequences are allowed, and unescaped newlines are
 * not allowed (this is the normal shell definition of
 * a quoted string).
 *
 * GS: text after double quotes
 *)
and lex_quote state = parse
   strict_nl
   { set_next_line state lexbuf;
     syntax_error state "unterminated string" lexbuf
   }
 | '\\'
 | string_text
   { let s, loc = lexeme_string state lexbuf in
   Omake_ast_parse.TokString (s, loc)
   }
 | ['\'' '"']
   { let s, loc = lexeme_string state lexbuf in
        match state.current_mode with
           ModeQuote s' when s' = s ->
              pop_mode state;
              TokEndQuoteString (s, loc)
         | _ ->
              TokString (s, loc)
   }
 | "$$"
   { let loc = lexeme_loc state lexbuf in
        TokString ("$", loc)
   }
 | special_var
   { lexeme_var state lexbuf }
 | paren_dollar
   { push_dollar state ModeNormal;
     lexeme_dollar state lexbuf
   }
 | esc_quote
   { let s, loc = lexeme_esc state lexbuf in
        TokString (s, loc)
   }
 | esc_line
   { let loc = lexeme_loc state lexbuf in
        set_next_line state lexbuf;
        state.current_fill_ok <- true;
        TokString ("", loc)
   }
 | eof
   { syntax_error state "unterminated string" lexbuf }
 | _
   { let s, _ = lexeme_string state lexbuf in
        syntax_error state ("illegal character in string constant: " ^ String.escaped s) lexbuf
   }

(*
 * Inline text.  We allow any text, but dollars are expanded.
 * Escape sequence other than an escaped newline are not
 * processed.
 *
 * GS: text after $ + double quotes
 *)
and lex_string state = parse
   '\\'
 | string_text
   { let s, loc = lexeme_string state lexbuf in
        Omake_ast_parse.TokString (s, loc)
   }
 | quote
   { let s, loc = lexeme_string state lexbuf in
        match state.current_mode with
           ModeString s' when s' = s ->
              pop_mode state;
              TokEndQuote ("", loc)
         | _ ->
              TokString (s, loc)
   }
 | "$$"
   { let loc = lexeme_loc state lexbuf in
        TokString ("$", loc)
   }
 | special_var
   { lexeme_var state lexbuf }
 | paren_dollar
   { push_dollar state ModeNormal;
     lexeme_dollar state lexbuf
   }
 | strict_nl
   { let s, loc = lexeme_string state lexbuf in
     let () =
        match state.current_mode with
           ModeString s ->
              push_mode state (ModeSkipString s)
         | _ ->
             (* GS CHECK: When is this possible? *)
              ()
     in
        set_next_line state lexbuf;
        state.current_fill_ok <- true;
        TokString (s, loc)
   }
 | esc_line              (* GS: Backslash before newline *)
   { let loc = lexeme_loc state lexbuf in
     let () =
        match state.current_mode with
           ModeString s ->
              push_mode state (ModeSkipString s)
         | _ ->
              ()
     in
        set_next_line state lexbuf;
        state.current_fill_ok <- true;
        TokString ("", loc)
   }
 | eof
   { syntax_error state "unterminated string" lexbuf }
 | _
   { let s, _ = lexeme_string state lexbuf in
        syntax_error state ("illegal character: " ^ String.escaped s) lexbuf
   }

and lex_skip_string state = parse
  (* GS. This also matches the empty string, so this rule is always matched.
     This is a technical sub-lexer of lex_string
   *)
   quote_opt
   { let s, loc = lexeme_string state lexbuf in
        pop_mode state;
        match state.current_mode with
           ModeString s' when s' = s ->
              pop_mode state;
              Omake_ast_parse.TokEndQuote ("", loc)
         | _ ->
              TokString ("", loc)
   }

(*
 * Text, but we don't expand variables.
 * GS. E.g. after $''. Parses until matching ''
 *)
and lex_literal state buf equote = parse
   strict_nl
   { let s, _ = lexeme_string state lexbuf in
        set_next_line state lexbuf;
        state.current_fill_ok <- true;
        Buffer.add_string buf s;
        lex_literal_skip state buf equote lexbuf
   }
 | literal_text
   { let s, _ = lexeme_string state lexbuf in
        Buffer.add_string buf s;
        lex_literal state buf equote lexbuf
   }
 | quote
   { let s, loc = lexeme_string state lexbuf in
        if s = equote then
           let s = Buffer.contents buf in
              s, loc
        else
           begin
              Buffer.add_string buf s;
              lex_literal state buf equote lexbuf
           end
   }
 | eof
   { syntax_error state "unterminated string" lexbuf }
 | _
   { let s, _ = lexeme_string state lexbuf in
        syntax_error state ("illegal character: " ^ String.escaped s) lexbuf
   }

and lex_literal_skip state buf equote = parse
   quote_opt
   { let s, loc = lexeme_string state lexbuf in
        if s = equote then
           let s = Buffer.contents buf in
              s, loc
        else
           lex_literal state buf equote lexbuf
   }

(*
 * Parse the whitespace at the beginning of the line.
 *)
and lex_indent state = parse
   comment_eol
 | white_nl
   { set_next_line state lexbuf;
     state.current_fill_ok <- true;
     lex_indent state lexbuf
   }
 | opt_white
   { let s, _ = lexeme_string state lexbuf in
     let indent = indent_of_string s in
        indent
   }

(*
 * For speed, define a scanner just for dependency files.
 *)
and lex_deps = parse
   name
 | white
 | other_drive
 | '\\'
   { let s, loc = lexeme_pos lexbuf in
   Omake_ast_parse.TokString (s, loc)
   }
 | "\\:"
   { let _, loc = lexeme_pos lexbuf in
        TokString (":", loc)
   }
 | ':'
   { let s, loc = lexeme_pos lexbuf in
        TokColon (s, loc)
   }
 | ['"' '\'']
   { let s, loc = lexeme_pos lexbuf in
     let buf = Buffer.create 64 in
        Buffer.add_string buf s;
        lex_deps_quote s buf lexbuf;
        TokString (Buffer.contents buf, loc)
   }
 | white_nl
 | comment_nl
   { let _, loc = lexeme_pos lexbuf in
        TokEol loc
   }
 | esc_char
   { let s, loc = lexeme_pos lexbuf in
     let s = String.make 1 s.[1] in
        TokStringQuote (s, loc)
   }
 | esc_line
   { let _, loc = lexeme_pos lexbuf in
        TokWhite (" ", loc)
   }
 | _
   { let s, loc = lexeme_pos lexbuf in
        TokString (s, loc)
   }
 | eof
   { let _, loc = lexeme_pos lexbuf in
        TokEof loc
   }

and lex_deps_quote term buf = parse
   '\\'
 | '\\' ['"' '\'']
 | [^ '\\' '"' '\'']+
   { let s, _ = lexeme_pos lexbuf in
        Buffer.add_string buf s;
        lex_deps_quote term buf lexbuf
   }
 | ['\'' '"']
   { let s, _ = lexeme_pos lexbuf in
        Buffer.add_string buf s;
        if s <> term then
           lex_deps_quote term buf lexbuf
   }
 | _
 | eof
   { raise Parsing.Parse_error }

{
(************************************************************************
 * Prompts.
 *)

(*
 * Lex and parse a line for the shell.
 *)
let tabstop = 3

let prompt_ext s =
   s ^ "> "

(* Prune the prompt to a reasonable length *)
let prompt_prune prompt indent =
   let max_len = 8 in
   let s = Bytes.make (indent * tabstop + max_len + 2) ' ' in
   let length = String.length prompt in
      if length > max_len then
         begin
            Bytes.blit_string prompt 0 s 0 max_len;
            Bytes.set s max_len '>'
         end
      else
         Bytes.blit_string prompt 0 s 0 length;
      Bytes.to_string s

let prompt_indent prompt root indent =
   if root then
      prompt
   else
      prompt_prune prompt indent

let prompt_string state root nest e =
   let prompt = prompt_ext (Omake_ast_util.key_of_exp e) in
      if state.is_interactive && root then
         Lm_printf.printf "%s%s@?" (prompt_prune prompt nest) state.current_buffer;
      prompt

(*
 * Parser for the body of an expression.
 *)
let body_parser state body =
   match body with
      Omake_ast.NoBody ->
         None
    | OptBody ->
         if state.is_interactive then
            None
         else
            Some Omake_ast_parse.shell
    | ColonBody ->
         Some Omake_ast_parse.shell
    | ArrayBody ->
         Some Omake_ast_parse.string

(************************************************************************
 * Lexing input.
 *)

(*
 * Copy into the lexbuf.
 *)
let lex_fill state buf len =
   let { 
         current_buffer = buffer;
         current_index = index;
         _
       } = state
   in
   let length = String.length buffer in
   let amount = min (length - index) len in
      if amount = 0 then
         state.current_eof <- true
      else
         begin
            String.blit buffer index buf 0 amount;
            state.current_index <- index + amount
         end;
      amount

(*
 * Refill the buffer using the readline function.
 *)
let state_refill state =
   let { current_fill_ok = fill_ok;
         current_prompt = prompt;
         readline = readline;
         _
       } = state
   in
      if fill_ok then
         let line = readline prompt in
         let line =
            if state.is_interactive && line = ".\n" then
               ""
            else
               line
         in
            state.current_buffer <- line;
            state.current_index <- 0;
            state.current_fill_ok <- false

(*
 * Lexer function to refill the buffer.
 * GS. This is for Lexing.from_function.
 *)
let lex_refill state buf len =
   let { current_buffer = buffer;
         current_index = index;
         _
       } = state
   in
   let length = String.length buffer in
   let amount = length - index in
      if amount = 0 then
         state_refill state;
      lex_fill state buf len

(************************************************************************
 * Main lexer.
 *)

(*
 * Get the input.
 *)
let lex_line state lexbuf =
   let tok =
      match state.current_mode with
         ModeNormal ->
            lex_main state lexbuf
       | ModeString _ ->
            lex_string state lexbuf
       | ModeSkipString _ ->
            lex_skip_string state lexbuf
       | ModeQuote _ ->
            lex_quote state lexbuf
   in
      if !debug_lex then
         Lm_printf.eprintf "Token: %a@." pp_print_token tok;
      state.current_token <- tok;
      tok

(************************************************************************
 * Parse main loop.
 *)

(*
 * Make sure the lexbuf is valid.
 *)
let parse_refill state prompt root nest =
   if state.current_eof then
      begin
         let lexbuf = Lexing.from_function (lex_refill state) in
            state.current_eof     <- false;
            state.current_fill_ok <- true;
            state.current_prompt  <- prompt_indent prompt root nest;
            state.current_lexbuf  <- lexbuf;
            state.current_lexmode <- LexModeInitial;
            state.current_off     <- 0
      end

(*
 * Get the current indentation level.
 *)
let parse_indent state prompt root nest =
   parse_refill state prompt root nest;
   match state.current_lexmode with
      LexModeInitial ->
         let indent =
            (* Interactive shell ignores indentation *)
            if state.is_interactive then
               nest
            else
               lex_indent state state.current_lexbuf
         in
            if !debug_lex then
               Lm_printf.eprintf "indent: %d@." indent;
            state.current_lexmode <- LexModeNormal indent;
            indent
    | LexModeNormal indent ->
         indent

(* GS. In the following, parse = Omake_ast_parse.shell, i.e. the ocamlyacc
   generated parser
 *)

(*
 * Parse a single expression.
 * GS. an "expression" is not just a $-expression, but any code block, which
 * may span several lines.
 *)
let rec parse_exp state parse prompt root nest =
  let indent = parse_indent state prompt root nest in
  if indent > state.current_indent then
    syntax_error state "illegal indentation" state.current_lexbuf
  else if indent < state.current_indent then
    raise End_of_file
  else
    parse_exp_indent state parse prompt root nest

and parse_exp_indent state parse _ root nest =
  (* GS: after the indentation... *)
  let code, e =
    try parse (lex_line state) state.current_lexbuf with
      Parsing.Parse_error ->
      parse_error state
  in  (* GS: e is the parsed expression *)
  let code = Omake_ast_util.scan_body_flag code e in
  let parse = body_parser state code in
  (* GS. parse is now None, or Some Omake_ast_parse.shell or .string *)
  match parse with
    Some parse ->
    let prompt = prompt_string state root nest e in
    let body = parse_body state parse prompt nest in
    let e = Omake_ast_util.update_body e code body in
    (match Omake_ast_util.can_continue e with
      Some prompt ->
      (try e :: parse_exp state parse (prompt_ext prompt) false nest with
        End_of_file ->
        [e])
    | None ->
      [e])
  | None ->
    [e]

and parse_body state parse prompt nest =
  let nest = succ nest in
  let indent = parse_indent state prompt false nest in
  (* GS. The body must be further indented, otherwise it is not a body
     of the preceding expr
   *)
  if indent > state.current_indent then
    begin
      push_mode state ModeNormal;
      state.current_indent <- indent;
      parse_body_indent state parse prompt nest []
    end
  else
    []

and parse_body_indent state parse prompt nest el =
  (* GS TODO: reformulate with "match ... with exception" *)
  let e =
    try ParseExp (parse_exp state parse prompt false nest) with
      End_of_file ->
      if state.is_interactive then
        Lm_printf.printf ".@.";
      pop_mode state;
      ParseEOF
    | Omake_value_type.OmakeException _ as exn when state.is_interactive ->
      Lm_printf.eprintf "%a@." Omake_exn_print.pp_print_exn exn;
      ParseError
  in
  match e with
    ParseExp e ->
    parse_body_indent state parse prompt nest (List.rev_append e el)
  | ParseError ->
    parse_body_indent state parse prompt nest el
  | ParseEOF ->
    List.rev el

(*
 * Parse a file.
 * GS: Entry point
 *)
let parse_ast name =
   let inx = open_in name in
   let readline _ =
      try input_line inx ^ "\n" with
         End_of_file ->
            ""
   in
   let state = create name readline in
   let el = parse_body_indent state Omake_ast_parse.shell "<prompt>" 0 [] in
      close_in inx;
      el

(*
 * Parse a string.
 * GS: Entry point
 *)
let parse_string s =
   let len = String.length s in
   let index = ref 0 in
   let readline _ =
      let start = !index in
      let rec search i =
         if i = len then
            if start < i then
               begin
                  index := i;
                  String.sub s start (i - start) ^ "\n"
               end
            else
               raise End_of_file
         else if s.[i] = '\n' then
            begin
               index := i + 1;
               String.sub s start (i - start + 1)
            end
         else
            search (succ i)
      in
         search start
   in
   let state = create "-" readline in
        parse_body_indent state Omake_ast_parse.shell "<prompt>" 0 []

(*
 * Parse an expression.
 *)
let create_shell () =
   let state = create "-" Lm_readline.readline in
      state.is_interactive <- Lm_readline.is_interactive ();
      state

(*
 * Copy the state, if an exception happens, then
 * restore the initial state.
 *)
let parse_shell state prompt =
   let stack = save_mode state in
      state.current_fill_ok <- true;
      try parse_exp state Omake_ast_parse.shell prompt true 0 with
         exn ->
            Lm_readline.flush ();
            restore_mode state stack;
            state.current_buffer <- "";
            state.current_index <- 0;
            raise exn

(*
 * Just dependency analysis.
 *)
let parse_deps name =
   let inx = open_in name in
   let lexbuf = Lexing.from_channel inx in
   let deps =
      try Omake_ast_parse.deps lex_deps lexbuf with
         exn ->
            close_in inx;
            Lm_printf.eprintf "%s: char %d: scanner dependency syntax error@." name (Lexing.lexeme_end lexbuf);
            raise exn
   in
      close_in inx;
      deps
}

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
