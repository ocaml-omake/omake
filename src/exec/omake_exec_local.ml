(* Execute on the local machine. *)

open Lm_printf
open Omake_exec_type
open Omake_command_type


let buffer_len = 65536


let unix_close debug fd =
   if !Lm_thread_pool.debug_thread then
      eprintf "Closing: %s: %d@." debug (Lm_unix_util.int_of_fd fd);
   try Unix.close fd with
      Unix.Unix_error (errno, f, arg) as exn ->
         if !Lm_thread_pool.debug_thread then
            eprintf "%s: close failed: %s %s(%s)@." debug (Unix.error_message errno) f arg;
         raise exn

type 'value job_state =
  |JobStarted
  | JobRunning of 'value
  | JobFinished of int * 'value * float

type fd_state =
  | Fd_open
  | Fd_eof
  | Fd_eof_ack
  | Fd_closed

        
   (*
    * A job has channels for communication,
    * plus info.
    *)
   type ('exp, 'pid, 'value) job =
      { job_id                 : Omake_exec_id.t;
        job_target             : Omake_node.Node.t;
        job_handle_out         : Omake_exec_type.output_fun;
        job_handle_err         : Omake_exec_type.output_fun;
        job_handle_status      : ('exp, 'pid, 'value) Omake_exec_type.status_fun;
        job_start_time         : float;

        (* Evaluator *)
        job_shell              : ('exp, 'pid, 'value) Omake_exec_type.shell;

        (* State while a job is running *)
        mutable job_pid        : 'pid;
        mutable job_stdout     : Unix.file_descr;
        mutable job_stdout_done: fd_state;
        mutable job_stderr     : Unix.file_descr;
        mutable job_stderr_done: fd_state;
        mutable job_state      : 'value job_state;
        mutable job_print_flag : bool;

        (* The commnds to be run after the current command is done *)
        mutable job_command    : 'exp;
        mutable job_commands   : 'exp list;

        (* A temporary buffer for copying. *)
        job_buffer_len         : int;
        job_buffer             : string * string;
      }

   (*
    * The state:
    *    server_jobs: currently running jobs
    *)
   type ('exp, 'pid, 'value) t =
      { mutable server_table : ('exp, 'pid, 'value) job Omake_exec_util.FdTable.t;
        mutable server_jobs  : ('exp, 'pid, 'value) job list
      }

   let create _ =
      { server_table  = Omake_exec_util.FdTable.empty;
        server_jobs   = []
      }

   (*
    * Printing.
    *)
   (* let pp_print_pid = pp_print_int *)

   let allow_output shell command =
      let flags, _dir, _target = shell.shell_info command in
         List.mem AllowOutputFlag flags
         
   (*
    * Print an error to the error channel.
    *)
   let handle_exn handle_err pp_print_exn id exn =
      let out = make_formatter (handle_err id) ignore in
         fprintf out "@[<v 3>   *** process creation failed:@ %a@]@." pp_print_exn exn

   (*
    * When the server is closed, kill all the jobs.
    *)
   let close server =
      Omake_exec_util.FdTable.iter (fun fd _ -> Unix.close fd) server.server_table;
      List.iter (fun { job_shell = shell; job_pid = pid; job_state = state ; _} ->
            match state with
               JobFinished _ ->
                  ()
             | JobStarted
             | JobRunning _ ->
                  try
                     shell.shell_kill pid
                  with
                     Unix.Unix_error _
                   | Invalid_argument _ ->
                        ()) server.server_jobs

   (*
    * Find a finished job, or raise Not_found if there is none.
    *)
   let find_finished_job server =
      let rec find running_jobs = function
         { job_state = JobFinished (code, value, time) ; _} as job :: jobs ->
            job, code, value, List.rev_append running_jobs jobs, time
       | { job_state = JobStarted | JobRunning _ ; _} as job :: jobs ->
            find (job :: running_jobs) jobs
       | [] ->
            raise Not_found
      in
         find [] server.server_jobs

   (*
    * Start a command.  Takes the output channels, and returns a pid.
    *)
   let start_command _server (shell : _ Omake_exec_type.shell) stdout stderr command =
    shell.shell_eval stdout stderr command

   (*
    * Start a job.
    *)
   let spawn_exn server shell id handle_out handle_err handle_status target commands =
      let command, commands =
         match commands with
            command :: commands ->
               command, commands
          | [] ->
               raise (Invalid_argument "Omake_exec_local.spawn")
      in
      let { server_table  = table;
            server_jobs   = jobs
          } = server
      in
         Omake_exec_util.with_pipe (fun out_read out_write ->
         Omake_exec_util.with_pipe (fun err_read err_write ->
               if !Lm_thread_pool.debug_thread then
                  begin
                     eprintf "out_read: %d, out_write: %d@." (Lm_unix_util.int_of_fd out_read) (Lm_unix_util.int_of_fd out_write);
                     eprintf "err_read: %d, err_write: %d@." (Lm_unix_util.int_of_fd err_read) (Lm_unix_util.int_of_fd err_write)
                  end;
               let () =
                  handle_status id (PrintEager command);
                  Unix.set_close_on_exec out_read;
                  Unix.set_close_on_exec err_read
               in
               let now = Unix.gettimeofday() in
               let pid = start_command server shell out_write err_write command in
               let job =
                  { job_id = id;
                    job_target = target;
                    job_handle_out = handle_out;
                    job_handle_err = handle_err;
                    job_handle_status = handle_status;
                    job_start_time = now;
                    job_pid = pid;
                    job_state = JobStarted;
                    job_stdout = out_read;
                    job_stdout_done = Fd_open;
                    job_stderr = err_read;
                    job_stderr_done = Fd_open;
                    job_command = command;
                    job_commands = commands;
                    job_print_flag = false;
                    job_shell = shell;
                    job_buffer_len = buffer_len;
                    job_buffer = String.create buffer_len, 
                                 String.create buffer_len;
                  }
               in
               let table = Omake_exec_util.FdTable.add table out_read job in
               let table = Omake_exec_util.FdTable.add table err_read job in
                  if !Omake_exec_util.debug_exec then
                     eprintf "Started job %d, stdout=%d, stderr=%d@." (**)
                        (Obj.magic pid) (Lm_unix_util.int_of_fd out_read) (Lm_unix_util.int_of_fd err_read);
                  unix_close "spawn_exn.1" out_write;
                  unix_close "spawn_exn.2" err_write;
                  server.server_table <- table;
                  server.server_jobs <- job :: jobs;
                  ProcessStarted id))

   let err_print_status commands handle_status id =
      match commands with
         command :: _ ->
            handle_status id (PrintLazy command)
       | [] ->
            ()

   let spawn server shell id handle_out handle_err handle_status target commands =
      try spawn_exn server shell id handle_out handle_err handle_status target commands with
         exn ->
            err_print_status commands handle_status id;
            handle_exn handle_err shell.shell_print_exn id exn;
            if shell.shell_is_failure_exn exn then
               ProcessFailed
            else
               raise exn

   (*
    * Start the next part of the job.
    *)
   let spawn_next_part_exn server job =
      let { job_id = id;
            job_shell = shell;
            job_target = _target;
            job_commands = commands;
            job_handle_out = handle_out;
            job_handle_err = handle_err;
            job_handle_status = handle_status;
            _
          } = job
      in
      let { server_table  = table ; _} = server in
         match commands with
            command :: commands ->
               Omake_exec_util.with_pipe (fun out_read out_write ->
               Omake_exec_util.with_pipe (fun err_read err_write ->
                     let () =
                        handle_status id (PrintEager command);
                        Unix.set_close_on_exec out_read;
                        Unix.set_close_on_exec err_read
                     in
                     let pid = start_command server shell out_write err_write command in
                     let table = Omake_exec_util.FdTable.add table out_read job in
                     let table = Omake_exec_util.FdTable.add table err_read job in
                        if !Omake_exec_util.debug_exec then
                           eprintf "Started next job %d, stdout=%d, stderr=%d@." (**)
                              (Obj.magic pid) (Lm_unix_util.int_of_fd out_read) (Lm_unix_util.int_of_fd err_read);
                        unix_close "spawn_next_part.1" out_write;
                        unix_close "spawn_next_part.2" err_write;
                        job.job_pid <- pid;
                        job.job_stdout <- out_read;
                        job.job_stdout_done <- Fd_open;
                        job.job_stderr <- err_read;
                        job.job_stderr_done <- Fd_open;
                        job.job_command <- command;
                        job.job_commands <- commands;
                        job.job_print_flag <- false;
                        server.server_table <- table))
          | [] ->
               match job.job_state with
                  JobRunning v ->
                     (* Close output channels *)
                     handle_out id "" 0 0;
                     handle_err id "" 0 0;
                     job.job_state <- JobFinished (0, v, Unix.gettimeofday() -. job.job_start_time)
                | JobStarted
                | JobFinished _ ->
                     raise (Invalid_argument "spawn_next_part")

   let spawn_next_part server job =
      try spawn_next_part_exn server job with
         exn ->
            let shell = job.job_shell in
               err_print_status job.job_commands job.job_handle_status job.job_id;
               handle_exn job.job_handle_err shell.shell_print_exn job.job_id exn;
               job.job_state <- JobFinished (Omake_state.fork_error_code, shell.shell_error_value, Unix.gettimeofday() -. job.job_start_time);
               if not (shell.shell_is_failure_exn exn) then
                  raise exn

   (*
    * Check if a command is an error.
    *)
   let command_code shell _options command status =
      let flags, _, _ = shell.shell_info command in
      let code =
         match status with
            Unix.WEXITED code ->
               code
          | Unix.WSIGNALED _
          | Unix.WSTOPPED _ ->
               Omake_state.signal_error_code
      in
         if code <> 0 && not (List.mem AllowFailureFlag flags) then
            code
         else
            0

   (*
    * Wait for the current part to finish.
    *)
   let wait_for_job server options job =
      let { job_pid = pid; job_command = command; job_shell = shell ; _} = job in
      let () =
         if !Omake_exec_util.debug_exec then
            eprintf "Waiting for job %d@." (Obj.magic pid)
      in
      let status, v = shell.shell_wait pid in
      let code = command_code shell options command status in
         if !Omake_exec_util.debug_exec then
            eprintf "Job exited with code %d@." code;
         if code <> 0 then
            job.job_state <- JobFinished (code, shell.shell_error_value, Unix.gettimeofday() -. job.job_start_time)
         else
            begin
               job.job_state <- JobRunning v;
               spawn_next_part server job
            end

   let acknowledge_eof server _options fd =
     let job =
       try Omake_exec_util.FdTable.find server.server_table fd
       with Not_found -> assert false in
     if job.job_stdout_done = Fd_eof && fd = job.job_stdout then (
       if !Omake_exec_util.debug_exec then
         eprintf "Ack-eof stdout fd %d@." (Lm_unix_util.int_of_fd fd);
       job.job_stdout_done <- Fd_eof_ack;
     )
     else if job.job_stderr_done = Fd_eof && fd = job.job_stderr then (
       if !Omake_exec_util.debug_exec then
         eprintf "Ack-eof stderr fd %d@." (Lm_unix_util.int_of_fd fd);
       job.job_stderr_done <- Fd_eof_ack;
     )
      (* When both states are Fd_eof_ack we know that both threads are
         done, and that we can reuse the job record for the next command
       *)

   let handle_eof server options fd =
     let job_opt =
       try Some (Omake_exec_util.FdTable.find server.server_table fd)
       with Not_found -> None in
     match job_opt with
       | None -> ()
       | Some job ->
           let ack_stdout = job.job_stdout_done = Fd_eof_ack in
           let ack_stderr = job.job_stderr_done = Fd_eof_ack in
           (* When we close, another thread may get exactly the same fd for a newly
              opened file. Because of this, closing the fd must first happen when
              fd has been removed from all tables.
            *)
           if ack_stdout then (
             let table =
               Omake_exec_util.FdTable.remove server.server_table job.job_stdout in
             server.server_table <- table;
             job.job_stdout_done <- Fd_closed;
             unix_close "handle_eof (stdout)" job.job_stdout;
           );
           if ack_stderr then (
             let table =
               Omake_exec_util.FdTable.remove server.server_table job.job_stderr in
             server.server_table <- table;
             job.job_stderr_done <- Fd_closed;
             unix_close "handle_eof (stderr)" job.job_stderr;
           );
           if job.job_stdout_done = Fd_closed && job.job_stderr_done = Fd_closed then
             wait_for_job server options job

   let unix_read fd buf pos len =
     try Unix.read fd buf pos len
     with Unix.Unix_error _ -> 0 

   let rec unix_really_read fd buf pos len acc =
     let n = unix_read fd buf pos len in
     if n > 0 && n < len then
       unix_really_read fd buf (pos+n) (len-n) (acc+n)
     else
       acc

   let fd_closed = [ Fd_eof_ack; Fd_closed ]
     (* states that must not occur at the beginning of [handle] *)

   (*
    * Handle data on a channel. (This function may be called from a thread.)
    *)
   let handle_1 server _options fd =
      let job =
         try Omake_exec_util.FdTable.find server.server_table fd with
            Not_found ->
               raise (Invalid_argument "Omake_exec.handle_channel: no such job")
      in
      let { job_id = id;
            job_stdout = stdout;
            job_stdout_done = stdout_done;
            job_stderr = stderr;
            job_stderr_done = stderr_done;
            job_handle_out = handle_out;
            job_handle_err = handle_err;
            job_handle_status = handle_status;
            job_command = command;
            job_buffer_len = buffer_len;
            job_buffer = buffer_stdout, buffer_stderr;
            _
          } = job
      in
      let handle_data, buffer, is_closed =
         if fd = stdout then
            handle_out, buffer_stdout, List.mem stdout_done fd_closed
         else if fd = stderr then
            handle_err, buffer_stderr, List.mem stderr_done fd_closed
         else
            raise (Invalid_argument "Omake_exec.handle_channel: unknown file descriptor")
      in

      if is_closed then
        raise (Invalid_argument "Omake_exec.handle_channel: trying to read from closed file descriptor");

      (* Read from the descriptor. For performance reasons try to read the
         whole buffer on Win32.
       *)
      let count, eof =
        if Lm_thread_pool.enabled then
          let n = unix_really_read fd buffer 0 buffer_len 0 in
          n, n<buffer_len
        else
          (* Unix *)
          let n = unix_read fd buffer 0 buffer_len in
          n, n=0 in

      if count > 0 then
        begin
          (* For "AllowOutputFlag" commands (e.g. scanner) stdout does not "count", but stderr still does *)
          if not (job.job_print_flag || (fd = stdout && allow_output job.job_shell command)) then begin
            handle_status id (PrintLazy command);
            job.job_print_flag <- true
          end;
          handle_data id buffer 0 count;
        end;

      (* Handle end of file *)
      if eof then
        begin
          (* NB. handle_eof (above) may be running concurrently, and
             will immediately detect when job_std{out,err}_done is changed.
           *)
          if fd = stdout then (
            if !Omake_exec_util.debug_exec then
              eprintf "stdout EOF fd %d@." (Lm_unix_util.int_of_fd fd);
            job.job_stdout_done <- Fd_eof
          ) else (
            if !Omake_exec_util.debug_exec then
              eprintf "stderr EOF fd %d@." (Lm_unix_util.int_of_fd fd);
            job.job_stderr_done <- Fd_eof;
          );
          true  (* indicate eof to caller *)
        end
      else false

   let handle server options fd =
     (* We run the whole handler outside the Lm_thread_pool lock (which is
        safe as the handlers only write to files, see omake_exec_util)
      *)
     Lm_thread_pool.blocking_section
       (fun () -> handle_1 server options fd)
       ()


   (*
    * Get all the descriptors.
    *)
   let descriptors server =
      Omake_exec_util.FdTable.fold (fun fd_set fd _ ->
            if !Lm_thread_pool.debug_thread then
               eprintf "Local.descriptors: %d@." (Lm_unix_util.int_of_fd fd);
            fd :: fd_set) [] server.server_table

   (*
    * The wait process handles output from each of the jobs.
    * Once both output channels are closed, the job is finished.
    *)
   let wait server _options =
      try
         let job, code, value, jobs, time = find_finished_job server in
         let { job_id = id;
               job_handle_status = handle_status;
               job_command = command;
               _
             } = job
         in
            server.server_jobs <- jobs;
            handle_status id (PrintExit (command, code, value, time));
            WaitInternalExited (id, code, value)
      with
         Not_found ->
            WaitInternalNone

