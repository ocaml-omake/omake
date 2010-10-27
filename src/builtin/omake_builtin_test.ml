(*
 * The Unix-style test command.
 * See the usage comment below.
 *
 * \begin{doc}
 * \section{File predicates}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2007 Mojave Group, California Institute of Technology and
 * HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_glob
open Lm_lexer
open Lm_printf
open Lm_location

open Omake_ir
open Omake_env
open Omake_pos
open Omake_node
open Omake_value
open Omake_symbol
open Omake_builtin
open Omake_builtin_type
open Omake_builtin_util
open Omake_value_type
open Omake_var

module Pos = MakePos (struct let name = "Omake_builtin_test" end)
open Pos

(************************************************************************
 * Types.
 *)

(*
 * Operators.
 *)
type unop =
   IsEmptyStringOp		(* -z *)
 | IsNonEmptyStringOp           (* -n *)
 | IsBlockSpecialFileOp		(* -b *)
 | IsCharSpecialFileOp		(* -c *)
 | IsDirFileOp			(* -d *)
 | ExistsFileOp			(* -e *)
 | IsRegFileOp			(* -f *)
 | IsSetgidFileOp		(* -g *)
 | IsSymlinkFileOp		(* -h, -L *)
 | IsGroupOwnerFileOp		(* -G *)
 | IsStickyFileOp		(* -k *)
 | IsOwnerFileOp		(* -O *)
 | IsNamedPipeOp		(* -p *)
 | IsReadableFileOp		(* -r *)
 | IsNonemptyFileOp		(* -s *)
 | IsSocketFileOp		(* -S *)
 | IsSetuidFileOp		(* -u *)
 | IsWritableFileOp		(* -w *)
 | IsExecutableFileOp		(* -x *)

type binop =
   EqStringOp			(* = *)
 | NeStringOp			(* != *)
 | EqFileOp			(* -ef *)
 | NtFileOp			(* -nt *)
 | OtFileOp			(* -ot *)

type intop =
   EqIntOp			(* -eq *)
 | GeIntOp			(* -ge *)
 | GtIntOp		        (* -gt *)
 | LeIntOp			(* -le *)
 | LtIntOp			(* -lt *)
 | NeIntOp			(* -ne *)

type token =
   TokString of string
 | TokCurrentFile of string
 | TokNot of string
 | TokAnd of string
 | TokOr of string
 | TokLeftParen of string
 | TokRightParen of string
 | TokLengthOp of string
 | TokUnop of unop * string
 | TokBinop of binop * string
 | TokIntop of intop * string
 | TokName of string
 | TokRegex of string

(*
 * Expressions.
 *)
type string_exp = token

type int_exp =
   IntExp of token
 | LengthExp of token

type bool_exp =
   UnopExp of unop * string_exp
 | MatchExp of LmStr.t
 | BinopExp of binop * string_exp * string_exp
 | IntopExp of intop * int_exp * int_exp
 | NotExp of bool_exp
 | AndExp of bool_exp * bool_exp
 | OrExp of bool_exp * bool_exp

(************************************************************************
 * Utilities.
 *)
let rec split_last l =
   match l with
      [x] ->
         [], x
    | h :: l ->
         let l, x = split_last l in
            h :: l, x
    | [] ->
         raise (Invalid_argument "split_last")

(************************************************************************
 * Token operations.
 *)

(*
 * Tokenize the strings.
 * It is unlikely that OCaml optimizes this match,
 * but the performance is not all that critical.
 *)
let token_of_string s =
   match s with
      "("   -> TokLeftParen s
    | ")"   -> TokRightParen s
    | "{}"  -> TokCurrentFile s
    | "!"   -> TokNot s
    | "-a"  -> TokAnd s
    | "-o"  -> TokOr s
    | "="   -> TokBinop (EqStringOp, s)
    | "!="  -> TokBinop (NeStringOp, s)
    | "-eq" -> TokIntop (EqIntOp, s)
    | "-ge" -> TokIntop (GeIntOp, s)
    | "-gt" -> TokIntop (GtIntOp, s)
    | "-le" -> TokIntop (LeIntOp, s)
    | "-lt" -> TokIntop (LtIntOp, s)
    | "-ne" -> TokIntop (NeIntOp, s)
    | "-ef" -> TokBinop (EqFileOp, s)
    | "-nt" -> TokBinop (NtFileOp, s)
    | "-ot" -> TokBinop (OtFileOp, s)
    | "-n"  -> TokUnop (IsNonEmptyStringOp, s)
    | "-z"  -> TokUnop (IsEmptyStringOp, s)
    | "-b"  -> TokUnop (IsBlockSpecialFileOp, s)
    | "-c"  -> TokUnop (IsCharSpecialFileOp, s)
    | "-d"  -> TokUnop (IsDirFileOp, s)
    | "-e"  -> TokUnop (ExistsFileOp, s)
    | "-f"  -> TokUnop (IsRegFileOp, s)
    | "-g"  -> TokUnop (IsSetgidFileOp, s)
    | "-h"
    | "-L"  -> TokUnop (IsSymlinkFileOp, s)
    | "-l"  -> TokLengthOp s
    | "-G"  -> TokUnop (IsGroupOwnerFileOp, s)
    | "-k"  -> TokUnop (IsStickyFileOp, s)
    | "-O"  -> TokUnop (IsOwnerFileOp, s)
    | "-p"  -> TokUnop (IsNamedPipeOp, s)
    | "-r"  -> TokUnop (IsReadableFileOp, s)
    | "-s"  -> TokUnop (IsNonemptyFileOp, s)
    | "-S"  -> TokUnop (IsSocketFileOp, s)
    | "-u"  -> TokUnop (IsSetuidFileOp, s)
    | "-w"  -> TokUnop (IsWritableFileOp, s)
    | "-x"  -> TokUnop (IsExecutableFileOp, s)
    | "-name" -> TokName s
    | "-regex"
    | "-regexp" -> TokRegex s
    | _     -> TokString s

(*
 * Get the string from a token.
 *)
let string_of_token arg =
   match arg with
      TokString s
    | TokCurrentFile s
    | TokNot s
    | TokAnd s
    | TokOr s
    | TokLeftParen s
    | TokRightParen s
    | TokLengthOp s
    | TokUnop (_, s)
    | TokBinop (_, s)
    | TokIntop (_, s)
    | TokRegex s
    | TokName s ->
         s

(*
 * Integer of a token.
 * Since we use the internal int_of_string, we can read numbers
 * that are not in decimal.
 *)
let int_of_token arg =
   int_of_string (string_of_token arg)

(*
 * Length of a token.
 *)
let length_of_token arg =
   String.length (string_of_token arg)

(************************************************************************
 * Stat operations.
 *)

(*
 * Translate the filename.
 *)
let filename_of_string venv pos s =
   filename_of_value venv pos (ValString s)

(*
 * Generic stat operations.
 *)
let stat_function venv pos file f =
   let pos = string_pos "stat_function" pos in
   let file = filename_of_string venv pos file in
   let stat =
      try Some (Unix.LargeFile.stat file) with
         Unix.Unix_error _ ->
            None
   in
      f stat

let stat_function2 venv pos file1 file2 f =
   stat_function venv pos file1 (fun stat1 ->
         stat_function venv pos file2 (fun stat2 ->
               f stat1 stat2))

(*
 * Stat analysis.
 *)
let file_exists stat =
   stat <> None

let is_block_special stat =
   match stat with
      Some { Unix.LargeFile.st_kind = Unix.S_BLK } ->
         true
    | _ ->
         false

let is_char_special stat =
   match stat with
      Some { Unix.LargeFile.st_kind = Unix.S_CHR } ->
         true
    | _ ->
         false

let is_dir stat =
   match stat with
      Some { Unix.LargeFile.st_kind = Unix.S_DIR } ->
         true
    | _ ->
         false

let is_reg_file stat =
    match stat with
      Some { Unix.LargeFile.st_kind = Unix.S_REG } ->
         true
    | _ ->
         false

let is_symlink_file stat =
     match stat with
      Some { Unix.LargeFile.st_kind = Unix.S_LNK } ->
         true
    | _ ->
         false

let is_socket_file stat =
     match stat with
      Some { Unix.LargeFile.st_kind = Unix.S_SOCK } ->
         true
    | _ ->
         false

let is_named_pipe_file stat =
     match stat with
      Some { Unix.LargeFile.st_kind = Unix.S_FIFO } ->
         true
    | _ ->
         false

let is_sticky_file stat =
   match stat with
      Some { Unix.LargeFile.st_perm = perm } ->
         perm land 0o1000 <> 0
    | None ->
         false

let is_setgid_file stat =
   match stat with
      Some { Unix.LargeFile.st_perm = perm } ->
         perm land 0o2000 <> 0
    | None ->
         false

let is_setuid_file stat =
   match stat with
      Some { Unix.LargeFile.st_perm = perm } ->
         perm land 0o4000 <> 0
    | None ->
         false

let is_group_owner_file stat =
   match stat with
      Some { Unix.LargeFile.st_gid = gid } ->
         gid = Unix.getegid ()
    | None ->
         false

let is_owner_file stat =
   match stat with
      Some { Unix.LargeFile.st_uid = uid } ->
         uid = Unix.geteuid ()
    | None ->
         false

let is_readable_file stat =
   match stat with
      Some { Unix.LargeFile.st_uid = uid;
             Unix.LargeFile.st_gid = gid;
             Unix.LargeFile.st_perm = perm
      } ->
         let my_uid = Unix.geteuid () in
         let my_gid = Unix.getegid () in
            (my_uid = uid && perm land 0o400 <> 0)
            || (my_gid = gid && perm land 0o040 <> 0)
            || (perm land 0o004 <> 0)
    | None ->
         false

let is_writable_file stat =
   match stat with
      Some { Unix.LargeFile.st_uid = uid;
             Unix.LargeFile.st_gid = gid;
             Unix.LargeFile.st_perm = perm
      } ->
         let my_uid = Unix.geteuid () in
         let my_gid = Unix.getegid () in
            (my_uid = uid && perm land 0o200 <> 0)
            || (my_gid = gid && perm land 0o020 <> 0)
            || (perm land 0o002 <> 0)
    | None ->
         false

let is_executable_file stat =
   match stat with
      Some { Unix.LargeFile.st_uid = uid;
             Unix.LargeFile.st_gid = gid;
             Unix.LargeFile.st_perm = perm
      } ->
         let my_uid = Unix.geteuid () in
         let my_gid = Unix.getegid () in
            (my_uid = uid && perm land 0o100 <> 0)
            || (my_gid = gid && perm land 0o010 <> 0)
            || (perm land 0o001 <> 0)
    | None ->
         false

let is_nonempty_file stat =
   match stat with
      Some { Unix.LargeFile.st_size = size } ->
         size <> Int64.zero
    | None ->
         false

(*
 * Binary stat operations.
 *)
let eq_file =
   if Sys.os_type = "Win32" then
      (fun venv pos file1 file2 ->
            let file1 = filename_of_string venv pos file1 in
            let file2 = filename_of_string venv pos file2 in
               file1 = file2)
   else
      (fun venv pos file1 file2 ->
            let pos = string_pos "eq_file" pos in
               stat_function2 venv pos file1 file2 (fun stat1 stat2 ->
                     match stat1, stat2 with
                        Some { Unix.LargeFile.st_dev = dev1; Unix.LargeFile.st_ino = ino1 },
                        Some { Unix.LargeFile.st_dev = dev2; Unix.LargeFile.st_ino = ino2 } ->
                           dev1 = dev2 && ino1 = ino2
                      | _ ->
                           false))

let newer_than_file stat1 stat2 =
   match stat1, stat2 with
      Some { Unix.LargeFile.st_mtime = mtime1 }, Some { Unix.LargeFile.st_mtime = mtime2 } ->
         mtime1 > mtime2
    | _ ->
         false

let older_than_file stat1 stat2 =
   match stat1, stat2 with
      Some { Unix.LargeFile.st_mtime = mtime1 }, Some { Unix.LargeFile.st_mtime = mtime2 } ->
         mtime1 < mtime2
    | _ ->
         false

(************************************************************************
 * Term evaluation.
 *)

(*
 * Evaluators.
 *)
let eval_string_exp venv pos arg =
   match arg with
      TokCurrentFile _ ->
         string_of_value venv pos (venv_get_var venv pos braces_var)
    | _ ->
         string_of_token arg

let eval_int_exp venv pos arg =
   match arg with
      IntExp arg ->
         int_of_token arg
    | LengthExp arg ->
         length_of_token arg

(*
 * Interpret a unary operator.
 *)
let eval_unop_exp venv pos op arg =
   let pos = string_pos "unop" pos in
   let arg = eval_string_exp venv pos arg in
      match op with
         IsEmptyStringOp ->
            arg = ""
       | IsNonEmptyStringOp ->
            arg <> ""
       | IsBlockSpecialFileOp ->
            stat_function venv pos arg is_block_special
       | IsCharSpecialFileOp ->
            stat_function venv pos arg is_char_special
       | IsDirFileOp ->
            stat_function venv pos arg is_dir
       | ExistsFileOp ->
            stat_function venv pos arg file_exists
       | IsRegFileOp ->
            stat_function venv pos arg is_reg_file
       | IsSetgidFileOp ->
            stat_function venv pos arg is_setgid_file
       | IsSymlinkFileOp ->
            stat_function venv pos arg is_symlink_file
       | IsGroupOwnerFileOp ->
            stat_function venv pos arg is_group_owner_file
       | IsStickyFileOp ->
            stat_function venv pos arg is_sticky_file
       | IsOwnerFileOp ->
            stat_function venv pos arg is_owner_file
       | IsNamedPipeOp ->
            stat_function venv pos arg is_named_pipe_file
       | IsReadableFileOp ->
            stat_function venv pos arg is_readable_file
       | IsNonemptyFileOp ->
            stat_function venv pos arg is_nonempty_file
       | IsSocketFileOp ->
            stat_function venv pos arg is_socket_file
       | IsSetuidFileOp ->
            stat_function venv pos arg is_setuid_file
       | IsWritableFileOp ->
            stat_function venv pos arg is_writable_file
       | IsExecutableFileOp ->
            stat_function venv pos arg is_executable_file

(*
 * Binary operation.
 *)
let eval_binop_exp venv pos op e1 e2 =
   let pos = string_pos "binop" pos in
   let left = eval_string_exp venv pos e1 in
   let right = eval_string_exp venv pos e2 in
      match op with
         EqStringOp ->
            left = right
       | NeStringOp ->
            left <> right
       | EqFileOp ->
            eq_file venv pos left right
       | NtFileOp ->
            stat_function2 venv pos left right newer_than_file
       | OtFileOp ->
            stat_function2 venv pos left right older_than_file

(*
 * Integer operations.
 *)
let eval_intop_exp venv pos op e1 e2 =
   let i = eval_int_exp venv pos e1 in
   let j = eval_int_exp venv pos e2 in
      match op with
         EqIntOp ->
            i = j
       | GeIntOp ->
            i >= j
       | GtIntOp ->
            i > j
       | LeIntOp ->
            i <= j
       | LtIntOp ->
            i < j
       | NeIntOp ->
            i <> j

(*
 * Match against the regular expression.
 *)
let eval_match_exp venv pos regex =
   let basename =
      match venv_get_var venv pos braces_var with
         ValNode node ->
            Node.tail node
       | v ->
            Filename.basename (string_of_value venv pos v)
   in
      LmStr.string_match regex basename 0

(*
 * General evaluator.
 *)
let rec eval_bool_exp venv pos e =
   match e with
      UnopExp (op, e) ->
         eval_unop_exp venv pos op e
    | MatchExp regex ->
         eval_match_exp venv pos regex
    | BinopExp (op, e1, e2) ->
         eval_binop_exp venv pos op e1 e2
    | IntopExp (op, e1, e2) ->
         eval_intop_exp venv pos op e1 e2
    | NotExp e ->
         not (eval_bool_exp venv pos e)
    | AndExp (e1, e2) ->
         eval_bool_exp venv pos e1 && eval_bool_exp venv pos e2
    | OrExp (e1, e2) ->
         eval_bool_exp venv pos e1 || eval_bool_exp venv pos e2

(************************************************************************
 * Expression parsing.
 *)

(*
 * Integer operation.
 *)
let parse_intop op i right =
   let j, tokens =
      match right with
         TokLengthOp _ :: right :: tokens ->
            LengthExp right, tokens
       | right :: tokens ->
            IntExp right, tokens
       | [] ->
            raise (Invalid_argument "Omake_builtin_string.intop")
   in
      IntopExp (op, i, j), tokens

(*
 * A term can be:
 *    ! term
 *    ( exp )
 *    -l string intop int
 *    int intop int
 *    string binop string
 *    unop string
 *    string
 *)
let no_glob_options = Lm_glob.create_options []

let rec parse_term venv pos tokens =
   let pos = string_pos "parse_term" pos in
      match tokens with
         TokNot _ :: tokens ->
            let e, tokens = parse_term venv pos tokens in
               NotExp e, tokens
       | TokLeftParen _ :: tokens ->
            let e, tokens = parse_exp venv pos tokens in
            let tokens =
               match tokens with
                  TokRightParen _ :: tokens ->
                     tokens
                | tok :: _ ->
                     raise (Failure ("')' expected: found " ^ string_of_token tok))
                | [] ->
                     raise (Failure "')' expected")
            in
               e, tokens
       | TokLengthOp _ :: left :: TokIntop (op, _) :: ((_ :: _) as right) ->
            parse_intop op (LengthExp left) right
       | left :: TokIntop (op, _) :: ((_ :: _) as right) ->
            parse_intop op (IntExp left) right
       | left :: TokBinop (op, _) :: right :: tokens ->
            BinopExp (op, left, right), tokens
       | TokUnop (op, _) :: arg :: tokens ->
            UnopExp (op, arg), tokens
       | TokName _ :: arg :: tokens ->
            MatchExp (regex_of_shell_pattern no_glob_options (string_of_token arg)), tokens
       | TokRegex _ :: arg :: tokens ->
            MatchExp (LmStr.regexp (string_of_token arg)), tokens
       | arg :: tokens ->
            UnopExp (IsNonEmptyStringOp, arg), tokens
       | [] ->
            raise (Failure "argument expected")

(*
 * And has higher precedence that or.
 *)
and parse_and venv pos tokens =
   let pos = string_pos "parse_and" pos in
   let rec parse e1 tokens =
      match tokens with
         TokAnd _ :: tokens ->
            let e2, tokens = parse_term venv pos tokens in
               parse (AndExp (e1, e2)) tokens
       | _ ->
            e1, tokens
   in
   let e1, tokens = parse_term venv pos tokens in
      parse e1 tokens

and parse_or venv pos tokens =
   let pos = string_pos "parse_or" pos in
   let rec parse e1 tokens =
      match tokens with
         TokOr _ :: tokens ->
            let e2, tokens = parse_and venv pos tokens in
               parse (OrExp (e1, e2)) tokens
       | _ ->
            e1, tokens
   in
   let e1, tokens = parse_and venv pos tokens in
      parse e1 tokens

(*
 * Evaluate an entire expression.
 *)
and parse_exp venv pos tokens =
   let pos = string_pos "parse" pos in
      parse_or venv pos tokens

(************************************************************************
 * Usage.
 *
 * \begin{doc}
 * \fun{test}
 *
 * \begin{verbatim}
 *    test(exp) : Bool
 *       exp : String Sequence
 * \end{verbatim}
 *
 * The \emph{expression} grammar is as follows:
 *
 * \begin{itemize}
 * \item    \verb+!+ \emph{expression}                      : \emph{expression} is not true
 * \item    \emph{expression1} \verb+-a+ \emph{expression2} : both expressions are true
 * \item    \emph{expression1} \verb+-o+ \emph{expression2} : at least one expression is true
 * \item    \verb+(+ \emph{expression} \verb+)+             : \emph{expression} is true
 * \end{itemize}
 *
 * The base expressions are:
 *
 * \begin{itemize}
 * \item    \verb+-n+ \emph{string}                    : The \emph{string} has nonzero length
 * \item    \verb+-z+ \emph{string}                    : The \emph{string} has zero length
 * \item    \emph{string} \verb+=+ \emph{string}       : The strings are equal
 * \item    \emph{string} \verb+!=+ \emph{string}      : The strings are not equal
 *
 * \item    \emph{int1} \verb+-eq+ \emph{int2}         : The integers are equal
 * \item    \emph{int1} \verb+-ne+ \emph{int2}         : The integers are not equal
 * \item    \emph{int1} \verb+-gt+ \emph{int2}         : \emph{int1} is larger than \emph{int2}
 * \item    \emph{int1} \verb+-ge+ \emph{int2}         : \emph{int2} is not larger than \emph{int1}
 * \item    \emph{int1} \verb+-lt+ \emph{int2}         : \emph{int1} is smaller than \emph{int2}
 * \item    \emph{int1} \verb+-le+ \emph{int2}         : \emph{int1} is not larger than \emph{int2}
 *
 * \item    \emph{file1} \verb+-ef+ \emph{file2}       : On Unix, \emph{file1} and \emph{file2} have the
 *                                                       same device and inode number.
 *                                                       On Win32, \emph{file1} and \emph{file2} have the
 *                                                       same name.
 * \item    \emph{file1} \verb+-nt+ \emph{file2}       : \emph{file1} is newer than \emph{file2}
 * \item    \emph{file1} \verb+-ot+ \emph{file2}       : \emph{file1} is older than \emph{file2}
 *
 * \item    \verb+-b+ \emph{file}                      : The file is a block special file
 * \item    \verb+-c+ \emph{file}                      : The file is a character special file
 * \item    \verb+-d+ \emph{file}                      : The file is a directory
 * \item    \verb+-e+ \emph{file}                      : The file exists
 * \item    \verb+-f+ \emph{file}                      : The file is a normal file
 * \item    \verb+-g+ \emph{file}                      : The set\verb+-group+\verb+-id+ bit is set on the file
 * \item    \verb+-G+ \emph{file}                      : The file's group is the current effective group
 * \item    \verb+-h+ \emph{file}                      : The file is a symbolic link (also \verb+-L+)
 * \item    \verb+-k+ \emph{file}                      : The file's sticky bit is set
 * \item    \verb+-L+ \emph{file}                      : The file is a symbolic link (also \verb+-h+)
 * \item    \verb+-O+ \emph{file}                      : The file's owner is the current effective user
 * \item    \verb+-p+ \emph{file}                      : The file is a named pipe
 * \item    \verb+-r+ \emph{file}                      : The file is readable
 * \item    \verb+-s+ \emph{file}                      : The file has a non-zero size
 * \item    \verb+-S+ \emph{file}                      : The file is a socket
 * \item    \verb+-u+ \emph{file}                      : The set\verb+-user+\verb+-id+ bit is set on the file
 * \item    \verb+-w+ \emph{file}                      : The file is writable
 * \item    \verb+-x+ \emph{file}                      : The file is executable
 * \end{itemize}
 *
 * A \emph{string} is any sequence of characters; leading \verb+-+ characters are allowed.
 *
 * An \emph{int} is a \emph{string} that can be interpreted as an integer.  Unlike traditional
 * versions of the test program, the leading characters may specify an arity.  The
 * prefix \verb+0b+ means the numbers is in binary; the prefix \verb+0o+ means
 * the number is in octal; the prefix \verb+0x+ means the number is in hexadecimal.
 * An \emph{int} can also be specified as \verb+-l+ \emph{string}, which evaluates to the length of
 * the \emph{string}.
 *
 * A \emph{file} is a \emph{string} that represents the name of a file.
 *
 * The syntax mirrors that of the \Cmd{test}{1} program.  If you are on a Unix system, the man page
 * explains more.  Here are some examples.
 *
 * \begin{verbatim}
 *     # Create an empty file
 *     osh> touch foo
 *     # Is the file empty?
 *     osh> test(-e foo)
 *     - : true
 *     osh> test(! -e foo)
 *     - : false
 *     # Create another file
 *     osh> touch boo
 *     # Is the newer file newer?
 *     osh> test(boo -nt foo)
 *     - : true
 *     # A more complex query
 *     # boo is newer than foo, and foo is empty
 *     osh> test(\( boo -nt foo \) -a -e foo)
 *     - : true
 * \end{verbatim}
 * \end{doc}
 *)
let print_usage venv pos loc =
   let outv = venv_find_var venv pos loc stderr_var in
   let outx = channel_of_value venv pos outv in
      Lm_channel.output_string outx "test <expression>\nFor usage, see the omake manual\n";
      false

let print_version venv pos loc =
   let outv = venv_find_var venv pos loc stdout_var in
   let outx = channel_of_value venv pos outv in
      Lm_channel.output_string outx "test version 1.0.0\n";
      false

(*
 * Evaluate the expression.
 *)
let test_exp venv pos tokens =
   let pos = string_pos "test_exp" pos in
   let tokens = List.map token_of_string tokens in
   let e, tokens = parse_exp venv pos tokens in
      match tokens with
         [] ->
            eval_bool_exp venv pos e
       | tok :: _ ->
            raise (Failure ("unexpected token: " ^ string_of_token tok))

(*
 * Parse the command line.
 *)
let test_cmd_exn venv pos loc argv =
   match argv with
      ["["; "--help"] ->
         print_usage venv pos loc
    | ["["; "--version"] ->
         print_version venv pos loc
    | "[" :: argv ->
         let argv, delim = split_last argv in
            if delim = "]" then
               test_exp venv pos argv
            else
               print_usage venv pos loc
    | _ :: argv ->
         test_exp venv pos argv
    | [] ->
         false

let test_cmd venv pos loc argv =
   try test_cmd_exn venv pos loc argv with
      Failure _ as exn ->
         raise (UncaughtException (loc_pos loc pos, exn))

(************************************************************************
 * Usage.
 *
 * \begin{doc}
 * \fun{find}
 *
 * \begin{verbatim}
 *    find(exp) : Node Array
 *       exp : String Sequence
 * \end{verbatim}
 *
 * The \verb+find+ function searches a directory recursively, returning the
 * files for which the expression evaluates to true.
 *
 * The expression argument uses the same syntax as the \verb+test+ function,
 * with the following exceptions.
 *
 * \begin{enumerate}
 * \item The expression may begin with a directory.  If not specified, the current
 *    directory is searched.
 * \item The \verb+{}+ string expands to the current file being examined.
 * \end{enumerate}
 *
 * The syntax of the expression is the same as \verb+test+, with the following
 * additions.
 *
 * \begin{itemize}
 * \item    \verb+-name+ \emph{string} : The current file matches the glob expression
 * (see Section~\ref{section:globbing}).
 * \item    \verb+-regex+ \emph{string} : The current file matches the regular expression
 * \end{itemize}
 *
 * The \verb+find+ function performs a recursive scan of all subdirectories.
 * The following call is being run from the root of the \verb+omake+ source directory.
 *
 * \begin{verbatim}
 *     osh> find(. -name fo* )
 *     - : <array
 *             /home/jyh/.../omake/mk/.svn/format
 *             /home/jyh/.../omake/RPM/.svn/format
 *             ...
 *             /home/jyh/.../omake/osx_resources/installer_files/.svn/format>
 * \end{verbatim}
 *
 * Another example, listing only those files that are normal files
 * or symbolic links.
 *
 * \begin{verbatim}
 *     osh> find(. -name fo* -a \( -f {} -o -L {} \))
 *     - : <array
 *             /home/jyh/.../omake/mk/.svn/format
 *             /home/jyh/.../omake/RPM/.svn/format
 *             ...
 *             /home/jyh/.../omake/osx_resources/installer_files/.svn/format>
 * \end{verbatim}
 * \end{doc}
 *)
let test venv pos loc args =
   let pos = string_pos "test" pos in
   let code =
      match args with
         [arg] ->
            let argv = "test" :: strings_of_value venv pos arg in
               test_cmd venv pos loc argv
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
      val_of_bool code

let shell_test cmd venv pos loc args =
   let pos = string_pos "shell-test" pos in
   let code =
      match args with
         [arg] ->
            let argv = cmd :: strings_of_value venv pos arg in
               test_cmd venv pos loc argv
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
      if code then
         val_true
      else
         raise (ExitException (pos, 1))

(************************************************************************
 * Find function.
 *)

(*
 * Walk the directory tree, testing for files.
 *)
let rec find_file venv pos nodes name e =
   (* Convert the name to to a value *)
   let v = node_value_of_value venv pos ~follow_symlinks:false (ValString name) in

   (* Add this node if the expression matches *)
   let nodes =
      let venv = venv_add_var venv braces_var v in
         if eval_bool_exp venv pos e then
            v :: nodes
         else
            nodes
   in
      (* If this is a directory, then walk it recursively *)
      match v with
         ValDir dir ->
            let dirname = Dir.fullname dir in
            let names =
               try Lm_filename_util.lsdir dirname with
                  Unix.Unix_error _ ->
                     []
            in
               List.fold_left (fun nodes name' ->
                     let filename = Filename.concat name name' in
                        find_file venv pos nodes filename e) nodes names
       | _ ->
            nodes

(*
 * The main function, from arguments.
 *)
let find_top venv pos loc arg =
   let argv = strings_of_value venv pos arg in
   let tokens = List.map token_of_string argv in
   let dir, tokens =
      match tokens with
         TokString s :: tokens ->
            s, tokens
       | _ ->
            ".", tokens
   in
   let e, tokens = parse_exp venv pos tokens in
   let () =
      match tokens with
         [] ->
            ()
       | tok :: _ ->
            raise (Failure ("unexpected token: " ^ string_of_token tok))
   in
      List.rev (find_file venv pos [] dir e)

(*
 * Find function.
 *)
let find venv pos loc args =
   let pos = string_pos "find" pos in
      match args with
         [arg] ->
            let nodes = find_top venv pos loc arg in
               concat_array nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let shell_find venv pos loc args =
   let pos = string_pos "find" pos in
      match args with
         [arg] ->
            let stdout_fd = venv_find_var venv pos loc stdout_var in
            let outp, close_flag = out_channel_of_any_value venv pos stdout_fd in
            let outx = venv_find_channel venv pos outp in
            let nodes = find_top venv pos loc arg in
               List.iter (fun node ->
                     Lm_channel.output_string outx (string_of_value venv pos node);
                     Lm_channel.output_string outx "\n") nodes;
               Lm_channel.flush outx;
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(************************************************************************
 * External interface.
 *)
let () =
   let builtin_funs =
      [true, "test",                  test,                 ArityExact 1;
       true, "builtin-test",          shell_test "test",    ArityExact 1;
       true, "builtin-test-brack",    shell_test "[",       ArityExact 1;
       true, "find",                  find,                 ArityExact 1;
       true, "builtin-find",          shell_find,           ArityExact 1]
   in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
