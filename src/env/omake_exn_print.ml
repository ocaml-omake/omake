(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech
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
open Lm_printf

open Lm_location

open Omake_ast
open Omake_env
open Omake_pos
open Omake_symbol
open Omake_value_type
open Omake_value_print

module Pos = MakePos (struct let name = "Omake_exn_print" end);;
open Pos

(*
 * Other exception.
 *)
let pp_print_other_exn buf exn =
   match exn with
      Unix.Unix_error (errno, f, arg) ->
         fprintf buf "@[<v 3>%s(%s): %s@]" (**)
            f arg (Unix.error_message errno)
    | Sys_error s ->
         fprintf buf "@[<v 3>Sys_error: %s@]" s
    | Sys.Break ->
         fprintf buf "@[<v 3>Break@]"
    | Failure s ->
         fprintf buf "Failure: %s" s
    | Invalid_argument s ->
         fprintf buf "Invalid argument: %s" s
    | exn ->
         fprintf buf "@[<v 3>%s@]" (**)
            (Printexc.to_string exn)

let pp_print_obj_err buf obj =
   if venv_defined_field_internal obj message_sym then
      match venv_find_field_internal_exn obj message_sym with
         ValString s
       | ValData s ->
            begin match Lm_string_util.split "\n" s with
               [] -> ()
             | s :: sl ->
                  pp_print_string buf s;
                  List.iter (fun s -> pp_force_newline buf (); pp_print_string buf s) sl
            end
       | v ->
            pp_print_value buf v
   else
      pp_print_value buf (ValObject obj)

(*
 * Exception printer.
 *)
let pp_print_return_id buf (loc, s) =
   fprintf buf "%s (%a)" s pp_print_location loc

let pp_print_exn buf exn =
   match exn with
      OmakeException (pos, exn) ->
         fprintf buf "@[<v 3>*** omake error:@ %a@ %a@]" (**)
            pp_print_pos pos
            pp_print_exn exn
    | OmakeFatalErr (pos, exn) ->
         fprintf buf "@[<v 3>*** omake fatal error:@ %a@ %a@]" (**)
            pp_print_pos pos
            pp_print_exn exn
    | UncaughtException (pos, exn) ->
         fprintf buf "@[<v 3>*** omake error:@ %a@ %a@]" (**)
            pp_print_pos pos
            pp_print_other_exn exn
    | RaiseException (pos, obj) ->
         fprintf buf "@[<v 3>*** omake error:@ %a@ @[<v3>Uncaught Exception:@ %a@]@]" (**)
            pp_print_pos pos
            pp_print_obj_err obj
    | OmakeFatal s ->
         fprintf buf "@[<v 3>*** omake fatal error:@ %s@]" s
    | ExitParentException (pos, code)
    | ExitException (pos, code) ->
         fprintf buf "@[<v 3>*** omake %s:@ %a@ early exit(%i) requested by an omake file@]" (**)
            (if code = 0 then "warning" else "error")
            pp_print_pos pos
            code
    | Return (loc, _, id) ->
         fprintf buf "@[<v 3>*** omake error:@ %a@ uncaught return from %a@]" (**)
            pp_print_location loc
            pp_print_return_id id
    | exn ->
         fprintf buf "@[<v 3>*** omake error:@ %a@]" pp_print_other_exn exn

(*
 * If one of these exceptions occurs during process creation,
 * treat it as a command failure.
 *)
let is_shell_exn exn =
   match exn with
      OmakeException _
    | OmakeFatalErr _
    | OmakeFatal _
    | UncaughtException _
    | RaiseException _
    | Unix.Unix_error _
    | Sys_error _
    | Failure _
    | Invalid_argument _
    | Return _ ->
         true
    | _ ->
         false

(*
 * Exception handler.
 *)
let catch f x =
   try f x with
      OmakeException _
    | OmakeFatalErr _
    | OmakeFatal _
    | UncaughtException _
    | RaiseException _
    | Unix.Unix_error _
    | Sys_error _
    | Return _ as exn ->
         eprintf "%a@." pp_print_exn exn;
         exit Omake_state.exn_error_code
    | ExitParentException (_, code)
    | ExitException (_, code) as exn ->
         eprintf "%a@." pp_print_exn exn;
         exit code

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
