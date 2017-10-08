(* This is the module for subprocess execution derived from OCamlnet.
   It is only meant for Unix. If available, it takes advantage from the
   posix_spawn call.

   Copyright (C) 2014 by Gerd Stolpmann

   This file is licensed under the same conditions as omake. This work
   was sponsored by Lexifi.
 *)

(* Wrapping the library function *)

type wd_spec =
  | Wd_keep
  | Wd_chdir of string
  | Wd_fchdir of Unix.file_descr

type pg_spec =
  | Pg_keep
  | Pg_new_bg_group
  | Pg_new_fg_group
  | Pg_join_group of int

type fd_action =
  | Fda_close of Unix.file_descr
  | Fda_close_ignore of Unix.file_descr
  | Fda_close_except of bool array
  | Fda_dup2 of Unix.file_descr * Unix.file_descr

type sig_action =
  | Sig_default of int
  | Sig_ignore of int
  | Sig_mask of int list

external compat_spawn : wd_spec -> pg_spec -> fd_action list -> 
                        sig_action list -> string array ->
                        string -> string array -> int
  = "omake_shell_spawn_compat_byte" "omake_shell_spawn_compat_nat"

external posix_spawn : pg_spec -> fd_action list -> 
                       sig_action list -> string array ->
                       string -> string array -> int
  = "omake_shell_spawn_posix_byte" "omake_shell_spawn_posix_nat"

external have_posix_spawn : unit -> bool
  = "omake_shell_spawn_have_posix_spawn"

external fchdir : Unix.file_descr -> unit
  = "omake_shell_spawn_fchdir"


let spawn ?(chdir = Wd_keep) ?(pg = Pg_keep) ?(fd_actions = [])
          ?(sig_actions = []) ?(env = Unix.environment()) 
          ?(no_posix_spawn=false) cmd args = (
  (* Check whether we can use the faster posix_spawn *)
  let use_posix_spawn =
    not no_posix_spawn &&
    have_posix_spawn() &&
    pg <> Pg_new_fg_group &&
    not (List.exists
           (fun sa -> match sa with Sig_ignore _ -> true | _ -> false) 
           sig_actions) in
  
  try
    if not use_posix_spawn then
      failwith "USE_FORK_EXEC";

    (* emulate chdir. We are single-threaded, so this is easy: *)
    let cur_dir = Unix.openfile "." [ Unix.O_RDONLY] 0 in
    let change_back() = fchdir cur_dir; Unix.close cur_dir in

    ( try
        ( match chdir with
            | Wd_keep -> ()
            | Wd_chdir file -> Unix.chdir file
            | Wd_fchdir fd -> fchdir fd
        );
        let pid =
          posix_spawn pg fd_actions sig_actions env cmd args in
        (* may also fail with "USE_FORK_EXEC" in some cases *)
        change_back();
        pid
      with
        | error -> change_back(); raise error
    )
  with
    | Failure "USE_FORK_EXEC" ->
        (* Fixup: if pg = Pg_new_fg_group, we remove any Sig_default for
           SIGTTOU from sig_actions. Because of special handling, the effect
           of Sig_default is enforced by the implementation, but this must be
           done at [execve] time.
         *)
        let sig_actions =
          if pg = Pg_new_fg_group then
            List.filter 
              (fun spec -> spec <> Sig_default Sys.sigttou)
              sig_actions
          else
            sig_actions in
        compat_spawn chdir pg fd_actions sig_actions env cmd args
  )[@ocaml.warning "-52"]
