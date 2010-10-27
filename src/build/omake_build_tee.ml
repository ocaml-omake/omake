(*
 * Tee operations.
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_string_set

open Omake_env
open Omake_node
open Omake_exec_util
open Omake_exec_print
open Omake_build_type
open Omake_options

(*
 * The empty tee.
 *)
let tee_none = tee_create false

(*
 * Unlink all the tee files.
 *
 * XXX: HACK: It is hard to convince Windows not to hold on to these files,
 * so we keep track of those that we have failed to unlink and delete them
 * later on.
 *)
let failed_unlink = ref StringSet.empty

let try_unlink table name =
   try Unix.unlink name; table with
      Unix.Unix_error(Unix.ENOENT, _, _) ->
         table
    | Unix.Unix_error _ ->
         StringSet.add table name 

let rec unlink_file name =
   let table = StringSet.add (!failed_unlink) name in
      failed_unlink := StringSet.fold try_unlink StringSet.empty table

(*
 * Print all tees.
 *)
let eprint_file_exn copy name =
   let buf = String.create 1024 in
   let fd = Unix.openfile name [Unix.O_RDONLY] 0o000 in
      begin try
         copy buf fd
      with
         Unix.Unix_error _ ->
            Unix.close fd;
            eprintf "*** omake: error reading file %s@." name
       | exn ->
            Unix.close fd;
            raise exn
      end;
      Unix.close fd

let rec copy_stderr buf fd =
   let amount = Unix.read fd buf 0 (String.length buf) in
      if amount > 0 then begin
         let _ = Unix.write Unix.stderr buf 0 amount in
            copy_stderr buf fd
      end

let eprint_file = eprint_file_exn copy_stderr

(* Will omit the trailing newline. Will return a bool indicating whether the newline was omited *)
let rec format_string_with_nl buf s =
   if String.contains s '\n' then begin
      let i = String.index s '\n' in
      let len = String.length s - 1 in
         pp_print_string buf (String.sub s 0 i);
         if len > i then begin
            pp_force_newline buf ();
            format_string_with_nl buf (String.sub s (i + 1) (len - i))
         end else
            (* Omit the trailing newline *)
            true
   end else begin
      pp_print_string buf s;
      false
   end

let rec copy_with_nl_exn out pending_nl buf fd =
   let amount = Unix.read fd buf 0 (String.length buf) in
      if amount > 0 then begin
         if pending_nl then
            pp_force_newline out ();
         let pending_nl = format_string_with_nl out (String.sub buf 0 amount) in
            copy_with_nl_exn out pending_nl buf fd
      end

let format_file_with_nl buf name =
   eprint_file_exn (copy_with_nl_exn buf true) name

(*
 * Close tee channels.
 * For commands that are successful, repeat the diversion.
 *)
let env_close_success_tee env command =
   let { command_venv = venv;
         command_tee  = tee
       } = command
   in
      match tee_file tee with
         Some name ->
            tee_close tee;
            if opt_output (venv_options venv) OutputPostponeSuccess then begin
               progress_flush();
               eprint_file name
            end;
            unlink_file name;
            command.command_tee <- tee_none
       | None ->
            ()

(*
 * For failed commands, repeat the diversion immediately
 * if the DivertRepeat flag is specified.
 *
 * Don't remove the diversion if we are going to print it again
 * at the end of the run.
 *)
let env_close_failed_tee env command =
   let { command_venv = venv;
         command_tee  = tee
       } = command
   in
      match tee_file tee with
         Some name ->
            let options = venv_options venv in
            tee_close tee;
            if opt_output options OutputPostponeError then begin
               progress_flush();
               eprint_file name;
               if not (opt_output options OutputRepeatErrors) then begin
                  unlink_file name;
                  command.command_tee <- tee_none;
               end;
            end
       | None ->
            ()

(*
 * Print a diversion.
 *)
let format_tee_with_nl buf command =
   match tee_file command.command_tee with
      Some name ->
         format_file_with_nl buf name
    | None ->
         ()

(*
 * Unlink the file.
 *)
let unlink_tee command =
   match tee_file command.command_tee with
      Some name ->
         unlink_file name;
         command.command_tee <- tee_none
    | None ->
         ()

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
