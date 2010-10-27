(*
 * Shell expressions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Lm_location
open Lm_symbol
open Lm_debug
open Lm_printf

open Omake_node

(*
 * A shell command.
 *)
type 'arg cmd_exe =
   CmdArg    of 'arg
 | CmdNode   of Node.t

type simple_exe =
   ExeNode   of Node.t
 | ExeString of string
 | ExeQuote  of string

type 'arg redirect =
 | RedirectNode of Node.t
 | RedirectArg  of 'arg
 | RedirectNone

type ('exe, 'arg_command, 'arg_other) poly_cmd =
   { cmd_loc     : loc;
     cmd_env     : (symbol * 'arg_other) list;
     cmd_exe     : 'exe;
     cmd_argv    : 'arg_command list;
     cmd_stdin   : 'arg_other redirect;
     cmd_stdout  : 'arg_other redirect;
     cmd_stderr  : bool;
     cmd_append  : bool
   }

(*
 * An internal command.
 *
 * 'apply with be: venv -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> string list -> int * value
 *)
type ('arg_apply, 'arg_other, 'apply) poly_apply =
   { apply_loc      : loc;
     apply_env      : (symbol * 'arg_other) list;
     apply_name     : symbol;
     apply_fun      : 'apply;
     apply_args     : 'arg_apply list;
     apply_stdin    : 'arg_other redirect;
     apply_stdout   : 'arg_other redirect;
     apply_stderr   : bool;
     apply_append   : bool
   }

(*
 * A pipe may have several cmds in sequence.
 *)
type pipe_op =
   PipeAnd
 | PipeOr
 | PipeSequence

(*
 * A pipe with redirection.
 *)
type ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_group =
   { group_stdin    : 'arg_other redirect;
     group_stdout   : 'arg_other redirect;
     group_stderr   : bool;
     group_append   : bool;
     group_pipe     : ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe
   }

and ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe =
   PipeApply      of loc * ('arg_apply, 'arg_other, 'apply) poly_apply
 | PipeCommand    of loc * ('exe, 'arg_command, 'arg_other) poly_cmd
 | PipeCond       of loc * pipe_op (**)
      * ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe
      * ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe
 | PipeCompose    of loc * bool (**)
      * ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe
      * ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe
 | PipeGroup      of loc * ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_group
 | PipeBackground of loc * ('exe, 'arg_command, 'arg_apply, 'arg_other, 'apply) poly_pipe

(*
 * Signals.
 *)
type signal =
   SigAbrt
 | SigAlrm
 | SigFPE
 | SigHup
 | SigIll
 | SigInt
 | SigKill
 | SigPipe
 | SigQuit
 | SigSegv
 | SigTerm
 | SigUsr1
 | SigUsr2
 | SigChld
 | SigCont
 | SigStop
 | SigTstp
 | SigTtin
 | SigTtou
 | SigVTAlrm
 | SigProf
 | SigNum of int

(*
 * Debug flag.
 *)
let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "print debugging information for the shell";
        debug_value = false
      }

(*
 * Operators.
 *)
let pp_print_pipe_op buf op =
   let s =
      match op with
         PipeAnd -> "&&"
       | PipeOr -> "||"
       | PipeSequence -> ";"
   in
      pp_print_string buf s

(*
 * Parameterized printing.
 *)
module type PrintArgSig =
sig
   type arg_command
   type arg_apply
   type arg_other
   type exe

   val pp_print_exe         : formatter -> exe -> unit
   val pp_print_arg_command : formatter -> arg_command -> unit
   val pp_print_arg_apply   : formatter -> arg_apply -> unit
   val pp_print_arg_other   : formatter -> arg_other -> unit
end;;

module MakePrintPipe (PrintArg : PrintArgSig) =
struct
   open PrintArg

   (*
    * Print redirects.
    *)
   let pp_print_stdin buf stdin =
      match stdin with
         RedirectNode node ->
            fprintf buf " < %a" pp_print_node node
       | RedirectArg name ->
            fprintf buf " < %a" pp_print_arg_other name
       | RedirectNone ->
            ()

   let token_of_stdout stderr append =
      match stderr, append with
         true, true   -> ">>&"
       | true, false  -> ">&"
       | false, true  -> ">>"
       | false, false -> ">"

   let pp_print_stdout buf (stdout, stderr, append) =
      match stdout with
         RedirectNode name ->
            let dir = token_of_stdout stderr append in
               fprintf buf " %s %a" dir pp_print_node name
       | RedirectArg name ->
            let dir = token_of_stdout stderr append in
               fprintf buf " %s %a" dir pp_print_arg_other name
       | RedirectNone ->
            ()

   (*
    * Print the argument lists.
    *)
   let pp_print_args buf = function
      [] ->
         ()
    | arg :: args ->
          pp_print_arg_apply buf arg;
          List.iter (fun arg -> fprintf buf " %a" pp_print_arg_apply arg) args

   let pp_print_argv buf argv =
      List.iter (fun arg -> fprintf buf " %a" pp_print_arg_command arg) argv

   (*
    * Print the environment.
    *)
   let pp_print_env buf env =
      List.iter (fun (v, arg) ->
            fprintf buf "%a=%a " pp_print_symbol v pp_print_arg_other arg) env

   (*
    * An internal function/alias.
    *)
   let pp_print_apply buf apply =
      let { apply_env = env;
            apply_name = f;
            apply_args = args;
            apply_stdin = stdin;
            apply_stdout = stdout;
            apply_stderr = stderr;
            apply_append = append
          } = apply
      in
         fprintf buf "@[<hv 3>%aShell.%a(%a)%a%a@]" (**)
            pp_print_env env
            pp_print_symbol f
            pp_print_args args
            pp_print_stdin stdin
            pp_print_stdout (stdout, stderr, append)

   (*
    * Print a command.
    *)
   let pp_print_command buf command =
      let { cmd_exe = exe;
            cmd_env = env;
            cmd_argv = argv;
            cmd_stdin = stdin;
            cmd_stdout = stdout;
            cmd_stderr = stderr;
            cmd_append = append
          } = command
      in
         fprintf buf "@[<hv 3>%a%a%a%a%a@]" (**)
            pp_print_env env
            pp_print_exe exe
            pp_print_argv argv
            pp_print_stdin stdin
            pp_print_stdout (stdout, stderr, append)

   (*
    * Print a pipe.
    *)
   let rec pp_print_pipe buf pipe =
      match pipe with
         PipeApply (_, apply) ->
            pp_print_apply buf apply
       | PipeCommand (_, command) ->
            pp_print_command buf command
       | PipeCond (_, op, pipe1, pipe2) ->
            fprintf buf "@[<hv 3>%a@ %a %a@]" (**)
               pp_print_pipe pipe1
               pp_print_pipe_op op
               pp_print_pipe pipe2
       | PipeCompose (_, divert_stderr, pipe1, pipe2) ->
            fprintf buf "@[<hv 3>%a@ %s %a@]" (**)
               pp_print_pipe pipe1
               (if divert_stderr then "|&" else "|")
               pp_print_pipe pipe2
       | PipeGroup (_, group) ->
            pp_print_group buf group
       | PipeBackground (_, pipe) ->
            fprintf buf "%a &" pp_print_pipe pipe

   and pp_print_group buf group =
      let { group_stdin  = stdin;
            group_stdout = stdout;
            group_stderr = stderr;
            group_append = append;
            group_pipe   = pipe
          } = group
      in
         fprintf buf "@[<hv 3>(%a)%a%a@]" (**)
            pp_print_pipe pipe
            pp_print_stdin stdin
            pp_print_stdout (stdout, stderr, append)
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
