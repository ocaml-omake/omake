(*
 * String utilities.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_printf

(*
 * Show the file loading.
 *)
let debug_string =
   create_debug (**)
      { debug_name = "string";
        debug_description = "check string bounds";
        debug_value = false
      }

let code0 = Char.code '0'
let codea = Char.code 'a'
let codeA = Char.code 'A'

(*
 * Efficient string ordering (_not_ lexicographic)
 *)
let rec string_compare_aux s1 s2 len i =
   if len = i then
      0
   else
      let c1 = String.unsafe_get s1 i in
      let c2 = String.unsafe_get s2 i in
         if c1 = c2 then
            string_compare_aux s1 s2 len (i+1)
         else
            Pervasives.compare c1 c2

let string_compare s1 s2 =
   let len1 = String.length s1 in
      match Pervasives.compare len1 (String.length s2) with
         0 -> string_compare_aux s1 s2 len1 0
       | i -> i

(*
 * String prefix.
 *)
let rec is_string_prefix_aux s1 s2 i len =
   i = len || (String.unsafe_get s1 i = String.unsafe_get s2 i && is_string_prefix_aux s1 s2 (i + 1) len)

let is_string_prefix s1 s2 =
   let len1 = String.length s1 in
   let len2 = String.length s2 in
      len1 <= len2 && is_string_prefix_aux s1 s2 0 len1

(*
 * Compare a substring.
 *)
let rec equal_substring_aux s1 s2 len2 i1 i2 =
   if i2 = len2 then
      true
   else
      let c1 = String.unsafe_get s1 i1 in
      let c2 = String.unsafe_get s2 i2 in
         c1 = c2 && equal_substring_aux s1 s2 len2 (succ i1) (succ i2)

let equal_substring s1 off s2 =
   let len1 = String.length s1 in
   let len2 = String.length s2 in
      len1 - off >= len2 && equal_substring_aux s1 s2 len2 off 0

(*
 * Check all chars in the string.
 *)
let for_all f s =
   let len = String.length s in
   let rec check i =
      (i = len) or (f s.[i] & check (succ i))
   in
      check 0

(*
 * Find a char in a string.
 *)
let strchr s c =
   let l = String.length s in
   let rec aux i =
      if i < l then
         if s.[i] = c then
            i
         else
            aux (succ i)
      else
         raise Not_found
   in
      aux 0

(*
 * A more efficient reimplementation of String.contains.
 *)
let contains =
   let rec contains_aux s limit c i =
      (i < limit) && ((String.unsafe_get s i) = c || contains_aux s limit c (i+1))
   in
      fun s c ->
         contains_aux s (String.length s) c 0

(*
 * contains_string s1 s2
 * true iff any one of the characters in s2 appears in s1.
 *)
let contains_any =
   let rec search2 s2 len2 c i =
      (i < len2) && ((String.unsafe_get s2 i) = c || search2 s2 len2 c (i + 1))
   and search1 s1 len1 s2 len2 i =
      (i < len1) && (search2 s2 len2 (String.unsafe_get s1 i) 0 || search1 s1 len1 s2 len2 (i + 1))
   in
      (fun s1 s2 ->
            search1 s1 (String.length s1) s2 (String.length s2) 0)

(*
 * Index of first char in a set.
 *)
let index_set s set =
   let len = String.length s in
   let rec loop i =
      if i = len then
         raise Not_found
      else
         let c = s.[i] in
            if String.contains set c then
               i
            else
               loop (succ i)
   in
      loop 0

let rindex_set s set =
   let rec loop i =
      if i < 0 then
         raise Not_found
      else
         let c = s.[i] in
            if String.contains set c then
               i
            else
               loop (i - 1)
   in
      loop (String.length s - 1)

(*
 * Search for a pattern in the indicated buffer, within the start
 * and length constraints applied to the buffer.  Note that this
 * uses a very inefficient algorithm; at some point I (JDS) will
 * get around to converting this to the Knuth-Morris-Pratt or
 * maybe Rabin-Karp algorithm.
 *
 * On success, this returns the offset (RELATIVE TO start!) of
 * the first match found; on failure, this raises Not_found.
 *)
let strpat buffer start len pattern =
   let patlen = String.length pattern in
   let rec pattern_matches_prefix bufcur patcur =
      if patcur >= patlen then
         true
      else if buffer.[bufcur] <> pattern.[patcur] then
         false
      else
         pattern_matches_prefix (bufcur + 1) (patcur + 1)
   in
   let pattern_matches_prefix start = pattern_matches_prefix start 0 in
   let rec is_match start =
      if start + patlen > len then
         raise Not_found
      else if pattern_matches_prefix start then
         start
      else
         is_match (start + 1)
   in
      (is_match start) - start

(*
 * Escape a string using SQL conventions.
 * Apparently, the only char we should escape is the single
 * quote, which is turned into 2 single quotes.
 *)
let sql_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
            if c = '\'' then
               Buffer.add_string buf "''"
            else
               Buffer.add_char buf c;
            loop (i + 1)
   in
      loop 0

let mysql_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '\000' ->
                  Buffer.add_string buf "\\0"
             | '\'' ->
                  Buffer.add_string buf "\\'"
             | '"' ->
                  Buffer.add_string buf "\\\""
             | '\n' ->
                  Buffer.add_string buf "\\n"
             | '\r' ->
                  Buffer.add_string buf "\\r"
             | '\t' ->
                  Buffer.add_string buf "\\t"
             | '\\' ->
                  Buffer.add_string buf "\\\\"
             | _ ->
                  Buffer.add_char buf c
         in
            loop (succ i)
   in
      loop 0

(*
 * Escape a string using the C conventions.
 *)
let c_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '\b' ->
                  Buffer.add_string buf "\\b"
             | '\n' ->
                  Buffer.add_string buf "\\n"
             | '\r' ->
                  Buffer.add_string buf "\\r"
             | '\t' ->
                  Buffer.add_string buf "\\t"
             | '\\' ->
                  Buffer.add_string buf "\\\\"
             | _ ->
                  if c >= ' ' && c <= '~' && c != '"' && c != '\'' then
                     Buffer.add_char buf c
                  else
                     let code = Char.code c in
                        Buffer.add_char buf '\\';
                        Buffer.add_char buf (Char.chr (((code / 64) mod 8) + code0));
                        Buffer.add_char buf (Char.chr (((code / 8) mod 8) + code0));
                        Buffer.add_char buf (Char.chr ((code mod 8) + code0))
         in
            loop (succ i)
   in
      loop 0

(*
 * Escape a string using the Javascript single-quote conventions.
 *)
let js_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '\'' ->
                  Buffer.add_string buf "\\'"
             | '\\' ->
                  Buffer.add_string buf "\\\\"
             | ' '..'~' ->
                  Buffer.add_char buf c
             | _ ->
                  let code = Char.code c in
                     Buffer.add_char buf '\\';
                     Buffer.add_char buf (Char.chr (((code / 64) mod 8) + code0));
                     Buffer.add_char buf (Char.chr (((code / 8) mod 8) + code0));
                     Buffer.add_char buf (Char.chr ((code mod 8) + code0))
         in
            loop (succ i)
   in
      loop 0

(*
 * Escape a string using the HTML conventions.
 *)
let html_pre_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '<' ->
                  Buffer.add_string buf "&lt;"
             | '>' ->
                  Buffer.add_string buf "&gt;"
             | '&' ->
                  Buffer.add_string buf "&amp;"
             | '"' ->
                  Buffer.add_string buf "&quot;"
             | ' '
             | '\r'
             | '\n'
             | '\t' ->
                  Buffer.add_char buf c
             | _ ->
                  if c < ' ' || c >= '\127' then begin
                      Buffer.add_string buf "&#";
                      Buffer.add_string buf (string_of_int (Char.code c));
                      Buffer.add_char buf ';'
                  end
                  else
                      Buffer.add_char buf c
         in
            loop (succ i)
   in
      loop 0

let html_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '<' ->
                  Buffer.add_string buf "&lt;"
             | '>' ->
                  Buffer.add_string buf "&gt;"
             | '&' ->
                  Buffer.add_string buf "&amp;"
             | '"' ->
                  Buffer.add_string buf "&quot;"
             | ' ' ->
                  Buffer.add_string buf "&nbsp;"
             | '\r' ->
                  ()
             | '\n' ->
                  Buffer.add_string buf "<br>\n";
             | '\t' ->
                  Buffer.add_string buf "&nbsp;&nbsp;&nbsp;&nbsp;"
             | _ ->
                  if c < ' ' || c >= '\127' then begin
                      Buffer.add_string buf "&#";
                      Buffer.add_string buf (string_of_int (Char.code c));
                      Buffer.add_char buf ';'
                  end
                  else
                      Buffer.add_char buf c
         in
            loop (succ i)
   in
      loop 0

(*
 * Escape a string using the HTML conventions.
 *)
let html_escaped_nonwhite s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '<' ->
                  Buffer.add_string buf "&lt;"
             | '>' ->
                  Buffer.add_string buf "&gt;"
             | '&' ->
                  Buffer.add_string buf "&amp;"
             | '"' ->
                  Buffer.add_string buf "&quot;"
             | ' '
             | '\t' ->
                  Buffer.add_char buf c
             | '\n'
             | '\r' ->
                  Buffer.add_string buf "<br>\n"
             | _ ->
                  if c < ' ' || c >= '\127' then begin
                      Buffer.add_string buf "&#";
                      Buffer.add_string buf (string_of_int (Char.code c));
                      Buffer.add_char buf ';'
                  end
                  else
                      Buffer.add_char buf c
         in
            loop (succ i)
   in
      loop 0

(*
 * A generic definition of white space.
 *)
let white = " \t\r\n\012"
let quotes = "\"'"

(*
 * Test if a string is all whitespace.
 *)
let is_white =
   let rec test s i len =
      i = len || (match String.unsafe_get s i with
                     ' ' | '\t' | '\r' | '\n' | '\012' ->
                        test s (succ i) len
                   | _ ->
                        false)
   in
      (fun s -> test s 0 (String.length s))

(*
 * Split a string str into a list of substrings.
 * The string is split on any character in delims.  Empty substrings
 * are returned as empty strings in the list.  For example:
 *   split "-." "foo.bar--ba??z"
 * returns
 *   ["foo"; "bar"; ""; "ba??z"]
 *)
let split delims str =
   let strlen = String.length str in

   (* Find the next split index *)
   let rec next_split pos =
      if pos = strlen then
         strlen
      else
         let c = String.get str pos in
            if contains delims c then
               pos
            else
               next_split (pos + 1)
   in

   (* Build the list *)
   let rec str_split pos =
      let pos_end = next_split pos in
         if pos_end = strlen then
            [String.sub str pos (pos_end - pos)]
         else
            (String.sub str pos (pos_end - pos)) :: (str_split (pos_end + 1))
   in
      str_split 0

(*
 * Split a string str into a list of substrings.
 * The string is split on any character in delims.  Quotations
 * are not split.
 *
 * Empty substrings are _not_ returned as empty strings in the list.
 * For example:
 *   split ".-" "foo.bar--ba??z"
 * returns
 *   ["foo"; "bar"; "ba??z"]
 *)
let tokens_fold f x quotes delims str =
   let strlen = String.length str in

   (* Skip white space *)
   let rec skip_split pos =
      if pos = strlen then
         strlen
      else
         let c = str.[pos] in
            if contains delims c then
               skip_split (succ pos)
            else
               pos
   in

   (*
    * Find the next split index.
    *)
   let rec next_split pos =
      if pos = strlen then
         strlen
      else
         let c = str.[pos] in
            if contains delims c then
               pos
            else if contains quotes c then
               next_quote (succ pos)
            else
               next_split (succ pos)

   and next_quote pos =
      if pos = strlen then
         strlen
      else
         let c = str.[pos] in
            if contains quotes c then
               next_split (succ pos)
            else if c = '\\' && pos < pred strlen then
               next_quote (pos + 2)
            else
               next_quote (succ pos)
   in

   (* Build the list *)
   let rec str_split x pos =
      if pos = strlen then
         x
      else
         let pos_end = next_split pos in
         let x = f x str pos (pos_end - pos) in
            str_split x (skip_split pos_end)
   in
      str_split x (skip_split 0)

(*
 * Default token processor.
 *)
let tokens quotes delims str =
   let l =
      tokens_fold (fun l s off len ->
            String.sub s off len :: l) [] quotes delims str
   in
      List.rev l

let tokens_std s =
   tokens quotes white s

(*
 * This is a somewhat optimized form of the above,
 * for parsing based on whitespace and normal quotes.
 *
 * For simple parsing, all the tokens_wrap_* functions are the same,
 * and the lexer is a dummy.
 *)
type 'a tokens_prefix =
   NoPrefix
 | WordPrefix of 'a list
 | QuotePrefix of char * 'a list

type 'a tokens =
   { tokens_lexer         : (string -> int -> int -> int option);
     tokens_wrap_string   : (string -> 'a);
     tokens_wrap_data     : (string -> 'a);
     tokens_wrap_token    : (string -> 'a);
     tokens_group         : ('a list -> 'a);
     tokens_list          : 'a list;
     tokens_prefix        : 'a tokens_prefix
   }

let tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group =
   let group toks =
      match toks with
         []    -> wrap_data ""
       | [tok] -> tok
       | toks  -> group (List.rev toks)
   in
      { tokens_lexer         = lexer;
        tokens_wrap_string   = wrap_string;
        tokens_wrap_data     = wrap_data;
        tokens_wrap_token    = wrap_token;
        tokens_group         = group;
        tokens_list          = [];
        tokens_prefix        = NoPrefix
      }

let tokens_create wrap group =
   tokens_create_lexer ~lexer:(fun _ _ _ -> None) ~wrap_string:wrap ~wrap_data:wrap ~wrap_token:wrap ~group:group

(*
 * Get the tokens list.
 *)
let tokens_flush info =
   let { tokens_group  = group;
         tokens_list   = tokens;
         tokens_prefix = prefix
       } = info
   in
   let tokens =
      match prefix with
         NoPrefix ->
            tokens
       | WordPrefix prefix
       | QuotePrefix (_, prefix) ->
            group prefix :: tokens
   in
      List.rev tokens

(*
 * End the current word.
 *)
let tokens_break info =
   let { tokens_group  = group;
         tokens_list   = tokens;
         tokens_prefix = prefix
       } = info
   in
      match prefix with
         NoPrefix ->
            info
       | WordPrefix prefix
       | QuotePrefix (_, prefix) ->
            { info with tokens_list   = group prefix :: tokens;
                        tokens_prefix = NoPrefix
            }

(*
 * Add a value directly.
 * This also performs a break.
 *)
let tokens_atomic info x =
   let { tokens_group  = group;
         tokens_list   = tokens;
         tokens_prefix = prefix
       } = info
   in
      match prefix with
         NoPrefix ->
            { info with tokens_list   = x :: tokens;
                        tokens_prefix = NoPrefix
            }
       | WordPrefix prefix
       | QuotePrefix (_, prefix) ->
            { info with tokens_list   = x :: group prefix :: tokens;
                        tokens_prefix = NoPrefix
            }

(*
 * Add an value that might be unwrapped.
 * The value is unwrapped only if it is not surrounded by whitespace.
 *)
let tokens_add info x =
   match info.tokens_prefix with
      NoPrefix ->
         { info with tokens_prefix = WordPrefix [x] }
    | WordPrefix prefix ->
         { info with tokens_prefix = WordPrefix (x :: prefix) }
    | QuotePrefix (c, prefix) ->
         { info with tokens_prefix = QuotePrefix (c, x :: prefix) }

(*
 * Insert literal data.
 * The data is not scanned for whitespace.
 *)
let tokens_data info s =
   tokens_add info (info.tokens_wrap_data s)

(*
 * Scan the string for whitespace.
 *)
let tokens_string info s =
   let len = String.length s in
   let wrap = info.tokens_wrap_string in
   let wrap_prefix prefix s off len =
      if len <> 0 then
         wrap (String.sub s off len) :: prefix
      else
         prefix
   in

   (* Scanning whitespace *)
   let rec scan_white tokens i =
      if i = len then
         { info with tokens_list = tokens;
                     tokens_prefix = NoPrefix
         }
      else
         match String.unsafe_get s i with
            ' ' | '\t' | '\n' | '\r' | '\012' ->
               scan_white tokens (succ i)
          | '"' | '\'' as c ->
               scan_quote tokens [] c i (succ i)
          | '\\' ->
               scan_word tokens [] i (i + 2)
          | _ ->
               scan_word tokens [] i (succ i)

   (* Scanning a quoted word *)
   and scan_quote tokens prefix delim start i =
      if i >= len then
         let head = wrap_prefix prefix s start (len - start) in
            { info with tokens_list = tokens;
                        tokens_prefix = QuotePrefix (delim, head)
            }
      else
         let c = String.unsafe_get s i in
            match c with
               '"' | '\'' when c = delim ->
                  scan_word tokens prefix start (succ i)
             | '\\' ->
                  scan_quote tokens prefix delim start (i + 2)
             | _ ->
                  scan_quote tokens prefix delim start (succ i)

   (* Scanning a word *)
   and scan_word tokens prefix start i =
      if i >= len then
         let prefix = wrap_prefix prefix s start (len - start) in
            { info with tokens_list = tokens;
                        tokens_prefix = WordPrefix prefix
            }
      else
         match String.unsafe_get s i with
            ' ' | '\t' | '\n' | '\r' | '\012' ->
               let head = wrap_prefix prefix s start (i - start) in
               let head_tok = info.tokens_group head in
                  scan_white (head_tok :: tokens) (succ i)
          | '"' | '\'' as c ->
               scan_quote tokens prefix c start (succ i)
          | '\\' ->
               scan_word tokens prefix start (i + 2)
          | _ ->
               scan_word tokens prefix start (succ i)

   in
      if len = 0 then
         info
      else
         let { tokens_list = tokens;
               tokens_prefix = prefix
             } = info
         in
            match prefix with
               NoPrefix ->
                  scan_white tokens 0
             | WordPrefix prefix ->
                  scan_word tokens prefix 0 0
             | QuotePrefix (c, prefix) ->
                  scan_quote tokens prefix c 0 0

(*
 * Yet another tokenizer, where we allow for special tokens.
 * This is used, for example, in shell parsing, where some
 * unquoted sequences like && are special.
 *)
type buf_token =
   BufWhite
 | BufQuote of char
 | BufBackslash
 | BufChar
 | BufToken of int

let buffer_get_quoted s i =
   match String.unsafe_get s i with
      ' ' | '\t' | '\n' | '\r' | '\012' ->
         BufWhite
    | '"' | '\'' as c ->
         BufQuote c
    | '\\' ->
         BufBackslash
    | _ ->
         BufChar

let buffer_get_token lexer s i len =
   match String.unsafe_get s i with
      ' ' | '\t' | '\n' | '\r' | '\012' ->
         BufWhite
    | '"' | '\'' as c ->
         BufQuote c
    | '\\' ->
         BufBackslash
    | _ ->
         match lexer s i len with
            Some i ->
               BufToken i
          | None ->
               BufChar

let tokens_lex info s =
   let { tokens_lexer       = lexer;
         tokens_wrap_string = wrap_string;
         tokens_wrap_data   = wrap_data;
         tokens_wrap_token  = wrap_token;
         tokens_group       = group
       } = info
   in
   let len = String.length s in

   (* Don't add empty strings *)
   let wrap_data_prefix prefix s off len =
      if len <> 0 then
         wrap_data (String.sub s off len) :: prefix
      else
         prefix
   in

   let wrap_string_prefix prefix s off len =
      if len <> 0 then
         wrap_string (String.sub s off len) :: prefix
      else
         prefix
   in

  (* Scanning whitespace *)
   let rec scan_white tokens i =
      if i = len then
         { info with tokens_list   = tokens;
                     tokens_prefix = NoPrefix
         }
      else
         match buffer_get_token lexer s i len with
            BufWhite ->
               scan_white tokens (succ i)
          | BufQuote c ->
               scan_quote tokens [] c (succ i) (succ i)
          | BufBackslash ->
               scan_word tokens [] i (i + 2)
          | BufChar ->
               scan_word tokens [] i (succ i)
          | BufToken len ->
               let head = wrap_token (String.sub s i len) in
                  scan_white (head :: tokens) (i + len)

   (* Scanning a quoted word *)
   and scan_quote tokens prefix delim start i =
      if i >= len then
         let head = wrap_data_prefix prefix s start (len - start) in
            { info with tokens_list   = tokens;
                        tokens_prefix =  QuotePrefix (delim, head)
            }
      else
         match buffer_get_quoted s i with
            BufQuote c when c = delim ->
               let prefix = wrap_data_prefix prefix s start (i - start) in
                  scan_word tokens prefix (succ i) (succ i)
          | BufBackslash ->
               scan_quote tokens prefix delim start (i + 2)
          | BufQuote _
          | BufWhite
          | BufChar ->
               scan_quote tokens prefix delim start (succ i)
          | BufToken _ ->
               raise (Invalid_argument "Lm_string_util.tokens_lex: illegal token")

   (* Scanning a word *)
   and scan_word tokens prefix start i =
      if i >= len then
         let head = wrap_string_prefix prefix s start (len - start) in
            { info with tokens_list   = tokens;
                        tokens_prefix = WordPrefix head
            }
      else
         match buffer_get_token lexer s i len with
            BufWhite ->
               let head = group (wrap_string_prefix prefix s start (i - start)) in
                  scan_white (head :: tokens) (succ i)
          | BufToken len ->
               let head1 = group (wrap_string_prefix prefix s start (i - start)) in
               let head2 = wrap_token (String.sub s i len) in
                  scan_white (head2 :: head1 :: tokens) (i + len)
          | BufQuote c ->
               let prefix = wrap_string_prefix prefix s start (i - start) in
                  scan_quote tokens prefix c (succ i) (succ i)
          | BufBackslash ->
               scan_word tokens prefix start (i + 2)
          | BufChar ->
               scan_word tokens prefix start (succ i)

   in
      if len = 0 then
         info
      else
         let { tokens_list = tokens;
               tokens_prefix = prefix
             } = info
         in
            match prefix with
               NoPrefix ->
                  scan_white tokens 0
             | WordPrefix prefix ->
                  scan_word tokens prefix 0 0
             | QuotePrefix (c, prefix) ->
                  scan_quote tokens prefix c 0 0

(*
 * Split a string based on a boundary.
 *)
let split_string boundary s =
   let len_s = String.length s in
   let len_b = String.length boundary in
   let c =
      if len_b = 0 then
         raise (Invalid_argument "split_string");
      boundary.[0]
   in
   let rec matches i j =
      if j = len_b then
         true
      else
         s.[i] = boundary.[j] && matches (succ i) (succ j)
   in
   let buf = Buffer.create 17 in
   let rec split l i =
      if len_s - i < len_b then
         begin
            Buffer.add_substring buf s i (len_s - i);
            Buffer.contents buf :: l
         end
      else if s.[i] = c && matches i 0 then
         let s' = Buffer.contents buf in
            Buffer.clear buf;
            split (s' :: l) (i + len_b)
      else
         begin
            Buffer.add_char buf s.[i];
            split l (succ i)
         end
   in
      List.rev (split [] 0)

(*
 * Split a string based on a MIME boundary.
 *)
let split_mime_string boundary s =
   let len_s = String.length s in
   let len_b = String.length boundary in
   let rec matches i j =
      if j = len_b then
         true
      else
         s.[i] = boundary.[j] && matches (succ i) (succ j)
   in
   let buf = Buffer.create 17 in

   (* Collect the delimited text *)
   let rec split l i =
      if len_s - i < len_b - 2 then
         l
      else if s.[i] = '-' && s.[i + 1] = '-' && matches (i + 2) 0 then
         let l = Buffer.contents buf :: l in
            Buffer.clear buf;
            skip l (i + 2 + len_b)
      else
         begin
            Buffer.add_char buf s.[i];
            split l (succ i)
         end

   (* Skip over garbage after the delimiter *)
   and skip l i =
      if len_s - i < 2 || (s.[i] = '-' && s.[i + 1] = '-') then
         l
      else
         split l (i + 2)

   (* Skip to the first delimiter *)
   and skip_start i =
      if len_s - i < len_b - 2 then
         []
      else if s.[i] = '-' && s.[i + 1] = '-' && matches (i + 2) 0 then
         skip [] (i + 2 + len_b)
      else
         skip_start (succ i)
   in
      List.rev (skip_start 0)

(*
 * Unescape a quoted string.
 *)
let unescape s =
   let slen = String.length s in
   let buf = Buffer.create slen in
   let off, len =
      if slen < 2 then
         0, slen
      else if s.[0] = '"' && s.[slen - 1] = '"' then
         1, slen - 1
      else
         0, slen
   in
   let rec collect i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let c, i =
            if c = '\\' && i + 1 < len then
               match s.[i + 1] with
                  't' -> '\t', i + 2
                | 'r' -> '\r', i + 2
                | 'n' -> '\n', i + 2
                | '\\' -> '\\', i + 2
                | ('0'..'9') when i + 3 < len ->
                     let code =
                        100 * Char.code s.[i + 1]
                        + 10 * Char.code s.[i + 2]
                        + Char.code s.[i + 3]
                        - 111 * Char.code '0'
                     in
                        Char.chr (code land 0xff), i + 4
                | c ->
                     c, i + 2
            else
               c, i + 1
         in
            Buffer.add_char buf c;
            collect i
   in
      collect off

(*
 * Trim all whitespace from a string, respecting quotes.
 *)
let trim_all quotes delims str =
   let scratch_buf = Buffer.create 17 in
      ignore (tokens_fold (fun first s off len ->
                    if not first then
                       Buffer.add_char scratch_buf ' ';
                    Buffer.add_substring scratch_buf s off len;
                    false) true quotes delims str);
      Buffer.contents scratch_buf

let trim_std = trim_all quotes white

(*
 * Trim outer whitespace from a string.
 *)
let trim s =
   let length = String.length s in
   let is_whitespace = String.contains white in
   let rec scan_for_first_nonws index =
      if index < length && is_whitespace s.[index] then
         scan_for_first_nonws (index + 1)
      else
         index
   in
   let rec scan_for_last_nonws index =
      if index >= 0 && is_whitespace s.[index] then
         scan_for_last_nonws (index - 1)
      else
         index
   in
   let first = scan_for_first_nonws 0 in
   let last  = scan_for_last_nonws (length - 1) in
      if first > last then
         ""
      else
         String.sub s first (last - first + 1)

(*
 * Need these for converting numbers.
 *)
let code0 = Char.code '0'
let codea = Char.code 'a'
let codeA = Char.code 'A'

(*
 * Turn a string into an argument list.
 *)
let parse_args_list line =
   let len = String.length line in
   let buf = String.create len in
   let rec skip i =
      if i = len then
         [[]]
      else
         match line.[i] with
            ' ' | '\t' | '\n' | '\r' ->
               skip (succ i)
          | '"' ->
               string 0 (succ i)
          | '\\' ->
               if len >= i + 2 && line.[i + 1] = '\\' then
                  [] :: skip (i + 2)
               else
                  raise (Invalid_argument ("Lm_string_util.parse_args: " ^ line))
          | _ ->
               collect i (succ i)
   and collect i j =
      if j = len then
         [[String.sub line i (j - i)]]
      else
         match line.[j] with
            ' ' | '\t' | '\n' | '\r' | '\\' ->
               let s = String.sub line i (j - i) in
                  (match skip j with
                        [] -> [[s]]
                      | h :: tl -> (s :: h) :: tl)
          | _ ->
               collect i (succ j)
   and string j k =
      if k = len then
         raise (Invalid_argument ("Lm_string_util.parse_args: " ^ line))
      else
         let c = line.[k] in
            if c = '"' then
               let s = String.sub buf 0 j in
                  match skip (succ k) with
                     [] -> raise (Invalid_argument "Lm_string_util.parse_args - internal error")
                   | h::tl -> (s::h)::tl
            else if c = '\\' then
               escape j (succ k)
            else begin
               buf.[j] <- c;
               string (succ j) (succ k)
            end
   and escape j k =
      if k = len then
         raise (Invalid_argument ("Lm_string_util.parse_args: " ^ line))
      else
         let c,k =
            match line.[k] with
               't' -> '\t', succ k
             | 'n' -> '\n', succ k
             | 'r' -> '\r', succ k
             | '\\' -> '\\', succ k
             | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as c ->
                  Char.chr (100 * (Char.code c) +
                            10 * (Char.code line.[succ k]) +
                            (Char.code line.[k+2]) - 111 * code0),
                  k+3
             | c -> c, succ k
         in
            buf.[j] <- c;
            string (succ j) k
   in
   let _ =
      if !debug_string then
         eprintf "Lm_string_util.parse_args: %s@." (String.escaped line)
   in
   let args = skip 0 in
      if !debug_string then
         eprintf "Lm_string_util.parse_args: done@.";
      args

let parse_args s =
   match parse_args_list s with
      [] -> []
    | [l] -> l
    | _ -> raise (Invalid_argument ("Lm_string_util.parse_args - line includes \\\\:" ^ s))

(*
 * Concatenate strings.
 *)
let prepend sep sl =
   let scratch_buf = Buffer.create 17 in
   let collect s =
      Buffer.add_string scratch_buf sep;
      Buffer.add_string scratch_buf s
   in
      Buffer.clear scratch_buf;
      if sl = [] then
         Buffer.add_string scratch_buf sep
      else
         List.iter collect sl;
      Buffer.contents scratch_buf

(*
 * Read a file into a string.
 *)
let string_of_file name =
   let inx = open_in_bin name in
   let scratch_buf = Buffer.create 17 in
   let rec loop () =
      Buffer.add_char scratch_buf (input_char inx);
      loop ()
   in
      Buffer.clear scratch_buf;
      try loop () with
         End_of_file ->
            close_in inx;
            let s = Buffer.contents scratch_buf in
               Buffer.reset scratch_buf;
               s

(************************************************************************
 * DEBUG VERSIONS
 ************************************************************************)

(*
 * Create a new string containing garbage.
 *)
let create name i =
   if !debug_string then
      if i < 0  then
         begin
            eprintf "Lm_string_util.create: %s: %d < 0@." name i;
            raise (Failure "Lm_string_util.create")
         end;
   String.create i

(*
 * Make a string initialized with all chars the same.
 *)
let make name i c =
   if !debug_string then
      if i < 0 then
         begin
            eprintf "Lm_string_util.make: %s: %d < 0@." name i;
            raise (Failure "Lm_string_util.make")
         end;
   String.make i c

(*
 * Substring.
 *)
let sub name s i len =
   if !debug_string then
      let len' = String.length s in
         if i >= 0 & len >= 0 & i + len < len' then
            String.sub s i len
         else
            begin
               eprintf "Lm_string_util.sub error: %s: %s.[%d]@." name s i;
               raise (Failure "Lm_string_util.sub")
            end
   else
      String.sub s i len

let blit name froms i tos j len =
   if !debug_string then
      let from_len = String.length froms in
      let to_len = String.length tos in
         if i >= 0 & j >= 0 & len >= 0 & i + len < from_len & j + len < to_len then
            String.blit froms i tos j len
         else
            begin
               eprintf "String_util.blit_error: %s: %s %d %s %d %d@." name froms i tos j len;
               raise (Failure "String_util.blit")
            end
   else
      String.blit froms i tos j len

let set name s i c =
   if !debug_string then
      let len = String.length s in
         if i >= 0 & i < len then
            String.set s i c
         else
            begin
               eprintf "String_util.set error: %s: %s.[%d] <- %c@." name s i c;
               raise (Failure "String_util.set")
            end
   else
      String.set s i c

let get name s i =
   let len = String.length s in
      if i >= 0 & i < len then
         String.get s i
      else
         begin
            eprintf "String_util.get error: %s: %s[%d]@." name s i;
            raise (Failure "String_util.get")
         end

(************************************************************************
 * Hex notation.
 *)

(*
 * Turn a string into hex.
 *)
let hex_char =
   let zero = Char.code '0' in
   let a = Char.code 'a' - 10 in
   let hex_char code =
      if code < 10 then
         Char.chr (code + zero)
      else
         Char.chr (code + a)
   in
      hex_char

let hexify s =
   let len = String.length s in
   let buf = String.create (2 * len) in
      for i = 0 to pred len do
         let code = Char.code s.[i] in
            buf.[2 * i] <- hex_char ((code lsr 4) land 15);
            buf.[2 * i + 1] <- hex_char (code land 15)
      done;
      buf

let hexify_sub s off len =
   let buf = String.create (2 * len) in
      for i = 0 to pred len do
         let code = Char.code s.[off + i] in
            buf.[2 * i] <- hex_char ((code lsr 4) land 15);
            buf.[2 * i + 1] <- hex_char (code land 15)
      done;
      buf

let unhex i =
   match i with
      '0' .. '9' ->
         (Char.code i) - code0
    | 'a' .. 'f' ->
         (Char.code i) - codea + 10
    | 'A' .. 'F' ->
         (Char.code i) - codeA + 10
    | _ ->
         raise (Failure "unhexify")

let unhexify s =
   let len = String.length s in
      if len mod 2 = 0 then
         let buf = create "String_util.unhexify" (len / 2) in
         let rec unhexify i j =
            if j < len then
               begin
                  buf.[i] <- Char.chr ((unhex s.[j]) * 16 + (unhex s.[succ j]));
                  unhexify (i + 1) (j + 2)
               end
         in
            unhexify 0 0;
            buf
      else
         raise (Failure "unhexify")

let unhexify_int s =
   let len = String.length s in
   let rec unhexify index i =
      if i < len then
         unhexify (index * 16 + (unhex s.[i])) (succ i)
      else
         index
   in
      unhexify 0 0

(*
 * Construct an argv string with proper quoting.
 *
 * We are given a list of arguments that may or may not contain
 * whitespace or quotes.  Quote them in a meaningful way before
 * parsing.
 *)
type mode =
   ModeNormal
 | ModeDouble
 | ModeSingle

let rec needs_quotes mode s i len =
   if i >= len then
      mode <> ModeNormal
   else
      match mode, s.[i] with
         _, '\\' ->
            needs_quotes mode s (i + 2) len
       | ModeNormal, ' '
       | ModeNormal, '\t'
       | ModeNormal, '\012'
       | ModeNormal, '\n'
       | ModeNormal, '\r' ->
            true
       | ModeNormal, '"' ->
            needs_quotes ModeDouble s (succ i) len
       | ModeNormal, '\'' ->
            needs_quotes ModeSingle s (succ i) len
       | ModeSingle, '\''
       | ModeDouble, '"' ->
                  needs_quotes ModeNormal s (succ i) len
       | _ ->
            needs_quotes mode s (succ i) len

let needs_quotes s =
   let len = String.length s in
      len = 0 || needs_quotes ModeNormal s 0 len

let dquote = '"'
let equote = "\\\""

let quotify buf s =
   let len = String.length s in
   let rec copy i =
      if i <> len then
         let c = s.[i] in
            match c with
               '"' ->
                  Buffer.add_string buf equote;
                  copy (succ i)
             | '\\' ->
                  Buffer.add_char buf '\\';
                  if i < len - 1 then
                     begin
                        Buffer.add_char buf s.[i + 1];
                        copy (i + 2)
                     end
             | _ ->
                  Buffer.add_char buf c;
                  copy (succ i)
   in
      Buffer.add_char buf dquote;
      copy 0;
      Buffer.add_char buf dquote

let shell_quotes s =
   let buf = Buffer.create 32 in
      quotify buf s;
      Buffer.contents buf

let quote buf s =
   if needs_quotes s then
      quotify buf s
   else
      Buffer.add_string buf s

let rec concat_argv buf argv =
   match argv with
      [arg] ->
         quote buf arg
    | arg :: argv ->
         quote buf arg;
         Buffer.add_char buf ' ';
         concat_argv buf argv
    | [] ->
         ()

let concat_argv argv =
   let buf = Buffer.create 32 in
      concat_argv buf argv;
      Buffer.contents buf

(*
 * For string quoting.
 *)
let rec concat_string buf argv =
   match argv with
      [arg] ->
         Buffer.add_string buf arg
    | arg :: argv ->
         Buffer.add_string buf arg;
         Buffer.add_char buf ' ';
         concat_string buf argv
    | [] ->
         ()

let concat_string argv =
   match argv with
      [arg] ->
         arg
    | _ :: _ ->
         let buf = Buffer.create 32 in
            concat_string buf argv;
            Buffer.contents buf
    | [] ->
         ""

(*
 * This function adds quotes if needed.
 *)
let string_argv argv =
   let s = concat_string argv in
      if needs_quotes s then
         let buf = Buffer.create 32 in
            quotify buf s;
            Buffer.contents buf
      else
         s

(*
 * This function always adds quotes.
 *)
let quote_string s =
   let len = String.length s in
      if needs_quotes s || (not (s.[0] = '"' && s.[len - 1] = '"' || s.[0] = '\'' && s.[len - 1] = '\'')) then
         let buf = Buffer.create 32 in
            quotify buf s;
            Buffer.contents buf
      else
         s

let quote_argv argv =
   quote_string (concat_string argv)

(************************************************************************
 * Translate between URI enconding.
 *)

(*
 * Convert two hex chars into a new 8-bit char.
 *)
let unhex_char c1 c2 =
   let i1 = unhex c1 in
   let i2 = unhex c2 in
      Char.chr (i1 * 16 + i2)

(*
 * Decode hex characters in the URI.
 *)
let decode_hex_name uri =
   let len = String.length uri in
   let buf = String.create len in
   let rec convert i j =
      if j = len then
         if i = len then
            buf
         else
            String.sub buf 0 i
      else if uri.[j] = '+' then
         begin
            buf.[i] <- ' ';
            convert (i + 1) (j + 1)
         end
      else if uri.[j] = '%' & j < len - 2 then
         begin
            buf.[i] <- unhex_char uri.[j + 1] uri.[j + 2];
            convert (i + 1) (j + 3)
         end
      else
         begin
            buf.[i] <- uri.[j];
            convert (i + 1) (j + 1)
         end
   in
      convert 0 0

(*
 * Encode a string into hex.
 *)
let hex_char code =
   if code < 10 then
      Char.chr (code + Char.code '0')
   else
      Char.chr (code - 10 + Char.code 'a')

let encode_hex_name uri =
   let len = String.length uri in
   let buf = String.create (3 * len) in
   let rec convert i j =
      if i = len then
         String.sub buf 0 j
      else
         match uri.[i] with
            ('0'..'9' | 'A'..'Z' | 'a'..'z' | '/' | '_' | '-' | '.') as c ->
               buf.[j] <- c;
               convert (succ i) (succ j)
          | c ->
               let code = Char.code c in
                  buf.[j] <- '%';
                  buf.[j + 1] <- hex_char ((code lsr 4) land 15);
                  buf.[j + 2] <- hex_char (code land 15);
                  convert (succ i) (j + 3)
   in
      convert 0 0

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
