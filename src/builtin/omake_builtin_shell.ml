(*
 * Builtin shell operations.
 *
 * \begin{doc}
 * \input{omake-shell}
 *
 * \section{Basic builtin functions}
 * \end{doc}
 *)
include Omake_pos.MakePos (struct let name = "Omake_builtin_io" end)


(*
 * The string should be a job identifier.
 *)
let pid_of_string pos s =
  try int_of_string s with
    Invalid_argument _
  | Failure _ ->
    let pos = string_pos "pid_of_string" pos in
    raise (Omake_value_type.OmakeException (pos, StringStringError ("not a process identifier", s)))

(*
 * Signal numbers.
 *)
let signal_of_string s =
  match String.uppercase s with
    "-ABRT" -> Omake_shell_type.SigAbrt
  | "-ALRM" -> SigAlrm
  | "-HUP"  -> SigHup
  | "-ILL"  -> SigIll
  | "-INT"  -> SigInt
  | "-KILL" -> SigKill
  | "-QUIT" -> SigQuit
  | "-SEGV" -> SigSegv
  | "-TERM" -> SigTerm
  | "-USR1" -> SigUsr1
  | "-USR2" -> SigUsr2
  | "-CHLD" -> SigChld
  | "-STOP" -> SigStop
  | "-TSTP" -> SigTstp
  | "-TTIN" -> SigTtin
  | "-TTOU" -> SigTtou
  | "-VTALRM" -> SigVTAlrm
  | "-PROF" -> SigProf
  | _ ->
    let len = String.length s in
    if len > 1 && s.[0] = '-' then
      SigNum (int_of_string (String.sub s 1 (len - 1)))
    else
      raise (Failure "signal_of_string")

let signal_of_string pos s =
  try signal_of_string s with
    Failure _ ->
    let pos = string_pos "signal_of_string" pos in
    raise (Omake_value_type.OmakeException (pos, StringStringError ("not a signal", s)))

(*
 * Print some text.
 *
 * \begin{doc}
 * \fun{echo}
 *
 * The \verb+echo+ function prints a string.
 *
 * \begin{verbatim}
 * $(echo <args>)
 * echo <args>
 * \end{verbatim}
 * \end{doc}
 *)
let echo_fun venv pos loc args =
  let pos = string_pos "echo" pos in
  let args = List.map (Omake_value.strings_of_value venv pos) args in
  let args = List.flatten args in
  let outx = Omake_value.channel_of_var venv pos loc Omake_var.stdout_var in
  let rec echo args =
    match args with
      [arg] ->
      Lm_channel.output_string outx arg
    | arg :: args ->
      Lm_channel.output_string outx arg;
      Lm_channel.output_char outx ' ';
      echo args
    | [] ->
      ()
  in
  echo args;
  Lm_channel.output_char outx '\n';
  Lm_channel.flush outx;
  Omake_value_type.ValNone

(*
 * \begin{doc}
 * \fun{cd}
 *
 * The \verb+cd+ function changes the current directory.
 *
 * \begin{verbatim}
 *     cd(dir)
 *        dir : Dir
 * \end{verbatim}
 *
 * The \verb+cd+ function also supports a 2-argument form:
 *
 * \begin{verbatim}
 *     $(cd dir, e)
 *        dir : Dir
 *        e : expression
 * \end{verbatim}
 *
 * In the two-argument form, expression \verb+e+ is evaluated
 * in the directory \verb+dir+.  The current directory is not
 * changed otherwise.
 *
 * The behavior of the \verb+cd+ function can be changed with the
 * \verb+CDPATH+ variable, which specifies a search path for
 * directories.  This is normally useful only in the \Prog{osh}
 * command interpreter.
 *
 * \begin{verbatim}
 *     CDPATH : Dir Sequence
 * \end{verbatim}
 *
 * For example, the following will change directory to the first
 * directory \verb+./foo+, \verb+~/dir1/foo+, \verb+~/dir2/foo+.
 *
 * \begin{verbatim}
 *     CDPATH[] =
 *        .
 *        $(HOME)/dir1
 *        $(HOME)/dir2
 *     cd foo
 * \end{verbatim}
 *
 * \end{doc}
 *)
type cd_status =
    CdFail
  | CdFile
  | CdSuccess

let dir_test name =
  Sys.os_type = "Win32" || (Unix.access name [Unix.X_OK]; true)

let cd_test _ _ _ dir =
  let name = Omake_node.Dir.fullname dir in
  try
    let stat = Unix.LargeFile.stat name in
    if stat.Unix.LargeFile.st_kind <> Unix.S_DIR then
      CdFile
    else if dir_test name then
      CdSuccess
    else
      CdFail
  with
    Unix.Unix_error _ ->
    CdFail

let cd_dir venv pos loc dir =
  match cd_test venv pos loc dir with
    CdFail ->
    let relname = Omake_node.Dir.name (Omake_env.venv_dir venv) dir in
    raise (Omake_value_type.OmakeException (loc_pos loc pos, StringStringError ("no such directory", relname)))
  | CdFile ->
    let relname = Omake_node.Dir.name (Omake_env.venv_dir venv) dir in
    raise (Omake_value_type.OmakeException (loc_pos loc pos, StringStringError ("not a directory", relname)))
  | CdSuccess ->
    dir

let rec cd_search venv cdpath pos loc name =
  match cdpath with
    [] ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, StringStringError ("no such directory", name)))
  | cd_dir :: cd_path ->
    let dir = Omake_node.Dir.chdir cd_dir name in
    match cd_test venv pos loc dir with
      CdFail
    | CdFile ->
      cd_search venv cd_path pos loc name
    | CdSuccess ->
      dir

let cd_aux venv cd_path pos loc arg =
  match Omake_value.values_of_value venv pos arg with
    [dir] ->
    (match Omake_value.eval_prim_value venv pos dir with
      ValDir dir ->
      cd_dir venv pos loc dir
    | _ ->
      let name = Omake_value.string_of_value venv pos dir in
      if Filename.is_relative name then
        cd_search venv cd_path pos loc name
      else
        cd_dir venv pos loc (Omake_node.Dir.chdir Omake_node.Dir.root name))
  | [] ->
    cd_dir venv pos loc (Omake_env.venv_intern_dir venv Omake_state.home_dir)
  | args ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let cd_fun venv pos loc args kargs =
  let pos = string_pos "cd" pos in
  let cd_path =
    try Omake_env.venv_find_var_exn venv Omake_var.cdpath_var with
      Not_found ->
      ValString "."
  in
  let cd_path = Omake_value.values_of_value venv pos cd_path in
  let cd_path = List.map (Omake_value.dir_of_value venv pos) cd_path in
  match args, kargs with
    [arg], [] ->
    let dir = cd_aux venv cd_path pos loc arg in
    let venv = Omake_env.venv_chdir_tmp venv dir in
    venv, Omake_value_type.ValDir dir
  | [dir; e], [] ->
    (* Change temporarily and evaluate the exp *)
    let dir = cd_aux venv cd_path pos loc dir in
    let venv_new = Omake_env.venv_chdir_tmp venv dir in
    let values = Omake_value.values_of_value venv_new pos e in
    venv, Omake_value.concat_array values
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))

(*
 * \begin{doc}
 * \section{Job control builtin functions}
 * \fun{jobs}
 *
 * The \verb+jobs+ function prints a list of jobs.
 *
 * \verb+jobs+
 * \end{doc}
 *)
let jobs_fun venv pos _ _ =
  let _pos = string_pos "jobs" pos in
  Omake_shell_job.jobs venv;
  Omake_value_type.ValNone

(*
 * \begin{doc}
 * \fun{bg}
 *
 * The \verb+bg+ function places a job in the background.
 *
 * \verb+bg <pid...>+
 * \end{doc}
 *)
let jobs_iter_fun f venv pos loc args =
  let pos = string_pos "iter" pos in
  match args with
    [arg] ->
    let pids = Omake_value.strings_of_value venv pos arg in
    let pids = List.map (pid_of_string pos) pids in
    List.iter (f venv pos) pids
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let bg_fun venv pos loc args =
  let pos = string_pos "bg" pos in
  jobs_iter_fun Omake_shell_job.bg venv pos loc args;
  Omake_value_type.ValNone

(*
 * \begin{doc}
 * \fun{fg}
 *
 * The \verb+fg+ function brings a job to the foreground.
 *
 * \verb+fg <pid...>+
 * \end{doc}
 *)
let fg_fun venv pos loc args =
  let pos = string_pos "fg" pos in
  jobs_iter_fun Omake_shell_job.fg venv pos loc args;
  Omake_value_type.ValNone

(*
 * \begin{doc}
 * \fun{stop}
 *
 * The \verb+stop+ function suspends a job.
 *
 * \verb+stop <pid...>+
 * \end{doc}
 *)
let stop_fun venv pos loc args =
  let pos = string_pos "stop" pos in
  jobs_iter_fun Omake_shell_job.stop venv pos loc args;
  Omake_value_type.ValNone

(*
 * \begin{doc}
 * \fun{wait}
 *
 * The \verb+wait+ function waits for a job to finish.
 * If no process identifiers are given, the shell waits for
 * all jobs to complete.
 *
 * \verb+wait <pid...>+
 * \end{doc}
 *)
let wait_fun venv pos loc args =
  let pos = string_pos "wait" pos in
  jobs_iter_fun Omake_shell_job.wait venv pos loc args;
  Omake_value_type.ValNone

(*
 * \begin{doc}
 * \fun{kill}
 *
 * The \verb+kill+ function signals a job.
 *
 * \verb+kill [signal] <pid...>+
 * \end{doc}
 *)
let kill_fun venv pos loc args =
  let pos = string_pos "kill" pos in
  match args with
    [arg] ->
    let args = Omake_value.strings_of_value venv pos arg in
    let signal, args =
      match args with
        signal :: args' ->
        if String.length signal > 1 && signal.[0] = '-' then
          signal_of_string pos signal, args'
        else
          SigInt, args
      | [] ->
        SigInt, []
    in
    let args = List.map (pid_of_string pos) args in
    List.iter (fun pid -> Omake_shell_job.kill venv pos pid signal) args;
    Omake_value_type.ValNone
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \section{Command history}
 * \fun{history}
 *
 * \begin{verbatim}
 *     $(history-index) : Int
 *     $(history) : String Sequence
 *     history-file : File
 *     history-length : Int
 * \end{verbatim}
 *
 * The history variables manage the command-line history in \Prog{osh}.  They have no effect
 * in \Prog{omake}.
 *
 * The \verb+history-index+ variable is the current index into the command-line history.
 * The \verb+history+ variable is the current command-line history.
 *
 * The \verb+history-file+ variable can be redefined if you want the command-line history
 * to be saved.  The default value is \verb+~/.omake/osh_history+.
 *
 * The \verb+history-length+ variable can be redefined to specify the maximum number of
 * lines in the history that you want saved.  The default value is \verb+100+.
 * \end{doc}
 *)
let history_index _ pos loc args =
  let pos = string_pos "history-index" pos in
  match args with
    [] ->
    Omake_value_type.ValInt (Omake_readline.where ())
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

let history _ pos loc args =
  let pos = string_pos "history" pos in
  match args with
  | [] ->
    let strings = Omake_readline.history () in
    let strings = Array.to_list strings in
    let strings = List.map (fun s -> Omake_value_type.ValData s) strings in
    Omake_value_type.ValArray strings
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(************************************************************************
 * Tables.
*)

let () =
  let builtin_vars =
    ["history-file",   (fun _ -> Omake_value_type.ValNode (Omake_node.Node.create_node Omake_node.no_mount_info
           Omake_node.Mount.empty (Omake_node.Dir.cwd ()) (Omake_state.history_file ())));
     "history-length", (fun _ -> ValInt 100);
     "CDPATH",         (fun _ -> ValArray [ValString "."])]
  in

  let builtin_funs =
    [true,  "echo", echo_fun, Omake_ir.ArityAny;
     true,  "jobs", jobs_fun,                 ArityExact 1;
     true,  "bg", bg_fun,                   ArityExact 1;
     true,  "fg", fg_fun,                   ArityExact 1;
     true,  "stop", stop_fun,                 ArityExact 1;
     true,  "wait",                  wait_fun,                 ArityExact 1;
     true,  "kill",                  kill_fun,                 ArityExact 1;
     true,  "history-index",         history_index,            ArityExact 0;
     true,  "history",               history,                  ArityExact 0;
    ]
  in
  let builtin_kfuns =
    [false, "cd",                    cd_fun,                   Omake_ir.ArityRange (1, 2);
    ]
  in

  let builtin_info =
    { Omake_builtin_type.builtin_empty with builtin_vars = builtin_vars;
      builtin_funs = builtin_funs;
      builtin_kfuns = builtin_kfuns
    }
  in
  Omake_builtin.register_builtin builtin_info
