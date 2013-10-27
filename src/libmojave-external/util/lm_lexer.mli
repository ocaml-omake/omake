(*
 * Lexer generator.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Lm_location

(*
 * Debug flags.
 *)
val debug_lexgen : bool ref
val debug_lex    : bool ref

(*
 * The lexer takes an input stream as an argument.
 *)
module type LexerInput =
sig
   (*
    * Input channel is a stream of integers.
    * Usually these are just the ASCII codes for characters.
    *)
   type t

   (*
    * The channel has two special characters.
    *    bof: the beginning of file
    *    eof: the end of file.
    *)
   val bof : int
   val eof : int

   (*
    * The next function returns the next character in the input stream.
    *)
   val lex_next : t -> int

   (*
    * The pos function returns the current position of
    * the input buffer within the lexeme
    * (used for collecting \( ... \) arguments.
    *)
   val lex_pos : t -> int

   (*
    * The lexer will call start when it begins lexing.
    * The integer should be the *previous* character in the
    * input channel, or bof if at the beginning.
    *)
   val lex_start : t -> int

   (*
    * In some cases, the lexer may want to restart scanning
    * from a previous point.  If so, it will call this function
    * to reset the start point.
    *)
   val lex_restart : t -> int -> unit

   (*
    * When the lexer is done, it calls lex_stop with
    * the number of characters in the final lexeme.  Note
    * that this can cause data to be pushed back onto the input stream.
    *)
   val lex_stop : t -> int -> unit

   (*
    * Before calling lex_stop, the lexer may ask for the
    * lexeme as a string.  The integer is the number of
    * characters in the lexeme, the same as the argument
    * to lex_stop.
    *)
   val lex_string    : t -> int -> string
   val lex_substring : t -> int -> int -> string
   val lex_loc       : t -> int -> loc
end

(*
 * Semantic actions.
 *)
module type LexerAction =
sig
   (*
    * Values of action type *must* be comparable with =,
    * hopefully quickly.
    *
    * For example, functions are not allowed.
    * If you want a function, you should make an array of functions,
    * and use the index for the action name.
    *)
   type action

   (* For debugging *)
   val pp_print_action : out_channel -> action -> unit

   (* For creating sets and tables *)
   val hash : action -> int
   val compare : action -> action -> int

   (*
    * You can use the function to decide which clauses take
    * precedence for a match of equal length.  The function
    * gets two clause numbers.  If you use the min function,
    * then you get the first clause that matched.  If you
    * use the max function, you get the second clause that
    * matched.
    *)
   val choose : int -> int -> int
end

module MakeLexer (Input : LexerInput) (Action : LexerAction) :
sig
   open Action

   type t

   (* Return values from the searchto function *)
   type searchto_info =
      LexEOF
    | LexSkipped of loc * string
    | LexMatched of action * loc * string * string * string list

   (* The empty lexer accepts the empty language *)
   val empty : t

   (* Add a clause, specified as a regular expression *)
   val add_clause : t -> action -> string -> int * t

   (* Remove a clause by action name *)
   val remove_clause : t -> action -> t

   (*
    * Union of two lexers.
    * The union assumes that actions with the same name
    * have the same regular expression.
    *)
   val union : t -> t -> t

   (*
    * Compile the machine if not already compiled.
    * This is entirely optional.  It is here just in case you
    * want to expand the machine eagerly (for example before
    * marshaling it to a file).
    *)
   val compile : t -> unit

   (*
    * Print the lexer.
    * This is mainly for debugging.
    *)
   val pp_print_lexer : out_channel -> t -> unit

   (*
    * Hash code for the lexer.
    *)
   val hash : t -> int

   (*
    * Now match against an input channel.
    * The result is (clause, lexeme, args)
    *    clause: the index of the clause that matched
    *    lexeme: the entire string that matched
    *    args: the arguments for \(...\) patterns.
    *)
   val lex : t -> Input.t -> action * loc * string * string list

   (*
    * Search for the first occurrence of a match.
    * Return the unmatched data that was skipped as well.
    *    (action, skipped, matched, args)
    * This will not read past EOF.
    *)
   val search : t -> Input.t -> (action * loc * string * string * string list) option

   (*
    * The searchto function is similar, but if it doesn't detect a match,
    * it returns the text to the end of the channel.
    *)
   val searchto : t -> Input.t -> searchto_info

   (*
    * Just check if a string matches.
    *)
   val matches : t -> Input.t -> bool
end

(*
 * Str module replacement.
 *)
module LmStr :
sig
   type t

   (*
    * Construct a regular expression from a string.
    *)
   val regexp : string -> t

   (*
    * Check for a match.
    *)
   val string_match : t -> string -> int -> bool
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
