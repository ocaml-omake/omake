(* This is the module for subprocess execution derived from OCamlnet.
   It is only meant for Unix. If available, it takes advantage from the
   posix_spawn call.

   Copyright (C) 2014 by Gerd Stolpmann

   This file is licensed under the same conditions as omake. This work
   was sponsored by Lexifi.
 *)

(** The following function has some similarity with posix_spawn, but
    is extended to our needs, Only special (although frequent) cases
    are implemented with posix_spawn.
 *)

type wd_spec =
  | Wd_keep
      (** Keep the current working directory in the spawned process *)
  | Wd_chdir of string
      (** Change to this directory in the spawned process *)
  | Wd_fchdir of Unix.file_descr
      (** Change to the directory which has been previously been opened *)

type pg_spec =
  | Pg_keep
      (** The new process will be member of the same process group as
          this process *)
  | Pg_new_bg_group
      (** A new background process group is created, and the spawned
          process will be its single member
       *)
  | Pg_new_fg_group
      (** A new foreground process group is created, and the spawned
          process will be its single member
       *)
  | Pg_join_group of int
      (** The spawned process will be member of this process group *)

type fd_action =
  | Fda_close of Unix.file_descr
      (** Close the descriptor *)
  | Fda_close_ignore of Unix.file_descr
      (** Close the descriptor but ignore [EBADF] errors *)
  | Fda_close_except of bool array
      (** Closes all descriptors except those for which
          [except.(k)] is true where [k = int_of_file_descr fd].
          Descriptors outside the array index range are closed.
       *)
  | Fda_dup2 of Unix.file_descr * Unix.file_descr
      (** Duplicate the first descriptor to the second as [dup2] does *)

type sig_action =
  | Sig_default of int
      (** Resets this signal to default behavior in the spawned process *)
  | Sig_ignore of int
      (** Ignores the signal in the spawned process *)
  | Sig_mask of int list
      (** Set the signal mask in the spawned process *)

val spawn : ?chdir:wd_spec ->
            ?pg:pg_spec ->
            ?fd_actions:fd_action list ->
            ?sig_actions:sig_action list ->
            ?env:string array ->
            ?no_posix_spawn:bool ->
            string -> string array ->
              int
  (** [spawn cmd args]: Fork the process and exec [cmd] which gets the
      arguments [args]. On success, the PID of the new process is returned.
      This function does not wait for the completion of the process; use
      [Unix.waitpid] for this purpose.

      - [chdir]: If set, the new process starts with this working directory
        (this is done before anything else)
      - [pg]: If set, the new process will be a member of this process group
      - [fd_actions]: If set, these descriptor actions are executed 
        sequentially
      - [sig_actions]: If set, these signal actions are executed sequentially
      - [env]: If set, the process gets this environment instead of the
        current one
      - [no_posix_spawn]: If set, the [posix_spawn] family of library
        functions is not used to spawn even if possible, and always a
        [fork/exec] approach is taken. This may be slower, but there is
        normally better error reporting.

      Any exceptions in the subprocess are detected, and reported. However,
      if [Fda_close_ignore] leads to [EBADF] for a descriptor, this error is
      ignored.

      If [pg=Pg_new_fg_group], one should include [Sig_ignore Sys.sigttou]
      in [sig_actions].

      There are two implementations for [spawn]: One calls [fork] and [exec]
      directly, and one uses the [posix_spawn] family of library functions.
      The latter is faster on certain conditions, but this is very OS-specific.
      Some features are not supported by [posix_spawn] and will force
      that [fork/exec] is used: [Pg_new_fg_group],
      and [Sig_ignore]. However, note some implementations of [posix_spawn]
      also fall back to [fork/exec] internally for some combinations of flags,
      and it is hard to predict which spawn calls can actually be accelerated.
      The tendency, though, is that recent OS have sped up [posix_spawn]
      so far possible (e.g. by using [vfork] internally, or even by making
      [posix_spawn] a system call).
   *)

