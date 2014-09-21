


(*
 * The empty tee.
 *)
(* let tee_none = Omake_exec_util.tee_create false *)

(*
 * Unlink all the tee files.
 *
 * XXX: HACK: It is hard to convince Windows not to hold on to these files,
 * so we keep track of those that we have failed to unlink and delete them
 * later on.
 *)
let failed_unlink = ref Lm_string_set.StringSet.empty

let try_unlink table name =
  try Unix.unlink name; table with
    Unix.Unix_error(Unix.ENOENT, _, _) ->
      table
  | Unix.Unix_error _ ->
      Lm_string_set.StringSet.add table name 

let unlink_file name =
  let table = Lm_string_set.StringSet.add (!failed_unlink) name in
  failed_unlink := Lm_string_set.StringSet.fold try_unlink Lm_string_set.StringSet.empty table

(*
 * Print all tees.
 *)
let eprint_file_exn copy name =
  let buf = Bytes.create 1024 in
  let fd = Unix.openfile name [O_RDONLY] 0o000 in
  begin try
    copy buf fd
  with
    Unix.Unix_error _ ->
      Unix.close fd;
      Format.eprintf "*** omake: error reading file %s@." name
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
    Lm_printf.pp_print_string buf (String.sub s 0 i);
    if len > i then begin
      Format.pp_force_newline buf ();
      format_string_with_nl buf (String.sub s (i + 1) (len - i))
    end else
      (* Omit the trailing newline *)
      true
  end else begin
    Lm_printf.pp_print_string buf s;
    false
  end

let rec copy_with_nl_exn out pending_nl buf fd =
  let amount = Unix.read fd buf 0 (String.length buf) in
  if amount > 0 then begin
    if pending_nl then
      Format.pp_force_newline out ();
    let pending_nl = format_string_with_nl out (String.sub buf 0 amount) in
    copy_with_nl_exn out pending_nl buf fd
  end

let format_file_with_nl buf name =
  eprint_file_exn (copy_with_nl_exn buf true) name

(*
 * Close tee channels.
 * For commands that are successful, repeat the diversion.
 *)
let env_close_success_tee _ (command : Omake_build_type.command) =
  match command with { command_venv = venv;
        command_tee  = tee;
        _
      } -> 
  match Omake_exec_util.tee_file tee with
    Some name ->
      Omake_exec_util.tee_close tee;
      if Omake_options.opt_output
          (Omake_env.venv_options venv) OutputPostponeSuccess then begin
        Omake_exec_print.progress_flush();
        eprint_file name
      end;
      unlink_file name;
      command.command_tee <- Omake_exec_util.tee_none
  | None ->
      ()

(*
 * For failed commands, repeat the diversion immediately
 * if the DivertRepeat flag is specified.
 *
 * Don't remove the diversion if we are going to print it again
 * at the end of the run.
 *)
let env_close_failed_tee _ (command : Omake_build_type.command) =
  match command with { command_venv = venv;
        command_tee  = tee;
        _
      } ->
  match Omake_exec_util.tee_file tee with
    Some name ->
      let options = Omake_env.venv_options venv in
      Omake_exec_util.tee_close tee;
      if Omake_options.opt_output options OutputPostponeError then begin
        Omake_exec_print.progress_flush();
        eprint_file name;
        if not (Omake_options.opt_output options OutputRepeatErrors) then begin
          unlink_file name;
          command.command_tee <- Omake_exec_util.tee_none;
        end;
      end
  | None ->
      ()

(*
 * Print a diversion.
 *)
let format_tee_with_nl buf (command : Omake_build_type.command) =
  match Omake_exec_util.tee_file command.command_tee with
    Some name ->
      format_file_with_nl buf name
  | None -> ()

(*
 * Unlink the file.
 *)
let unlink_tee (command : Omake_build_type.command) =
  match Omake_exec_util.tee_file command.command_tee with
  | Some name ->
      unlink_file name;
      command.command_tee <- Omake_exec_util.tee_none
  | None ->
      ()
