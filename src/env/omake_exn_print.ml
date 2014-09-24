open Omake_env
open Omake_symbol
open Omake_value_type
open Omake_value_print

module Pos= Omake_pos.Make (struct let name = "Omake_exn_print" end);;


(*
 * Other exception.
 *)
let pp_print_other_exn buf exn =
  match exn with
  | Unix.Unix_error (errno, f, arg) ->
    Format.fprintf buf "@[<v 3>%s(%s): %s@]" (**)
      f arg (Unix.error_message errno)
  | Sys_error s ->
    Format.fprintf buf "@[<v 3>Sys_error: %s@]" s
  | Sys.Break ->
    Format.fprintf buf "@[<v 3>Break@]"
  | Failure s ->
    Format.fprintf buf "Failure: %s" s
  | Invalid_argument s ->
    Format.fprintf buf "Invalid argument: %s" s
  | exn ->
    Format.fprintf buf "@[<v 3>%s@]" (**)
      (Printexc.to_string exn)

let pp_print_obj_err buf obj =
   if venv_defined_field_internal obj message_sym then
      match venv_find_field_internal_exn obj message_sym with
         ValString s
       | ValData s ->
            begin match Lm_string_util.split "\n" s with
               [] -> ()
             | s :: sl ->
                  Format.pp_print_string buf s;
                  List.iter (fun s -> Format.pp_force_newline buf (); Format.pp_print_string buf s) sl
            end
       | v ->
            pp_print_value buf v
   else
      pp_print_value buf (ValObject obj)

(*
 * Exception printer.
 *)
let pp_print_return_id buf (loc, s) =
   Format.fprintf buf "%s (%a)" s Lm_location.pp_print_location loc

let pp_print_exn buf exn =
   match exn with
      OmakeException (pos, exn) ->
         Format.fprintf buf "@[<v 3>*** omake error:@ %a@ %a@]" (**)
            Pos.pp_print_pos pos
            pp_print_exn exn
    | OmakeFatalErr (pos, exn) ->
         Format.fprintf buf "@[<v 3>*** omake fatal error:@ %a@ %a@]" (**)
            Pos.pp_print_pos pos
            pp_print_exn exn
    | UncaughtException (pos, exn) ->
         Format.fprintf buf "@[<v 3>*** omake error:@ %a@ %a@]" (**)
            Pos.pp_print_pos pos
            pp_print_other_exn exn
    | RaiseException (pos, obj) ->
         Format.fprintf buf "@[<v 3>*** omake error:@ %a@ @[<v3>Uncaught Exception:@ %a@]@]" (**)
            Pos.pp_print_pos pos
            pp_print_obj_err obj
    | OmakeFatal s ->
         Format.fprintf buf "@[<v 3>*** omake fatal error:@ %s@]" s
    | ExitParentException (pos, code)
    | ExitException (pos, code) ->
         Format.fprintf buf "@[<v 3>*** omake %s:@ %a@ early exit(%i) requested by an omake file@]" (**)
            (if code = 0 then "warning" else "error")
            Pos.pp_print_pos pos
            code
    | Return (loc, _, id) ->
         Format.fprintf buf "@[<v 3>*** omake error:@ %a@ uncaught return from %a@]" (**)
            Lm_location.pp_print_location loc
            pp_print_return_id id
    | exn ->
         Format.fprintf buf "@[<v 3>*** omake error:@ %a@]" pp_print_other_exn exn

(*
 * If one of these exceptions occurs during process creation,
 * treat it as a command failure.
 *)
let is_shell_exn exn =
  match exn with
  | OmakeException _
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
   | OmakeException _
    | OmakeFatalErr _
    | OmakeFatal _
    | UncaughtException _
    | RaiseException _
    | Unix.Unix_error _
    | Sys_error _
    | Return _ as exn ->
         Format.eprintf "%a@." pp_print_exn exn;
         exit Omake_state.exn_error_code
    | ExitParentException (_, code)
    | ExitException (_, code) as exn ->
         Format.eprintf "%a@." pp_print_exn exn;
         exit code

