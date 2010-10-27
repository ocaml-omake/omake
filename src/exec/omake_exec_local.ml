(*
 * Execute on the local machine.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology and
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
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_thread_pool

open Omake_node
open Omake_state
open Omake_command
open Omake_exec_id
open Omake_exec_util
open Omake_exec_type
open Omake_command_type
open Omake_options

let unix_close debug fd =
   if !debug_thread then
      eprintf "Closing: %s: %d@." debug (Lm_unix_util.int_of_fd fd);
   try Unix.close fd with
      Unix.Unix_error (errno, f, arg) as exn ->
         if !debug_thread then
            eprintf "%s: close failed: %s %s(%s)@." debug (Unix.error_message errno) f arg;
         raise exn

module Local =
struct
   (*
    * Status of a job.
    *)
   type 'value job_state =
      JobStarted
    | JobRunning of 'value
    | JobFinished of int * 'value * float

   (*
    * A job has channels for communication,
    * plus info.
    *)
   type ('exp, 'pid, 'value) job =
      { job_id                 : id;
        job_target             : Node.t;
        job_handle_out         : output_fun;
        job_handle_err         : output_fun;
        job_handle_status      : ('exp, 'pid, 'value) status_fun;
        job_start_time         : float;

        (* Evaluator *)
        job_shell              : ('exp, 'pid, 'value) shell;

        (* State while a job is running *)
        mutable job_pid        : 'pid;
        mutable job_fd_count   : int;
        mutable job_stdout     : Unix.file_descr;
        mutable job_stderr     : Unix.file_descr;
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
      { mutable server_table : ('exp, 'pid, 'value) job FdTable.t;
        mutable server_jobs  : ('exp, 'pid, 'value) job list
      }

   let create _ =
      { server_table  = FdTable.empty;
        server_jobs   = []
      }

   (*
    * Printing.
    *)
   let pp_print_pid = pp_print_int

   let allow_output shell command =
      let flags, dir, target = shell.shell_info command in
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
      FdTable.iter (fun fd _ -> Unix.close fd) server.server_table;
      List.iter (fun { job_shell = shell; job_pid = pid; job_state = state } ->
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
         { job_state = JobFinished (code, value, time) } as job :: jobs ->
            job, code, value, List.rev_append running_jobs jobs, time
       | { job_state = JobStarted | JobRunning _ } as job :: jobs ->
            find (job :: running_jobs) jobs
       | [] ->
            raise Not_found
      in
         find [] server.server_jobs

   (*
    * Start a command.  Takes the output channels, and returns a pid.
    *)
   let start_command server shell stdout stderr command =
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
         with_pipe (fun out_read out_write ->
         with_pipe (fun err_read err_write ->
               if !debug_thread then
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
                    job_fd_count = 2;
                    job_stdout = out_read;
                    job_stderr = err_read;
                    job_command = command;
                    job_commands = commands;
                    job_print_flag = false;
                    job_shell = shell;
                    job_buffer_len = 1024;
                    job_buffer = String.create 1024, String.create 1024;
                  }
               in
               let table = FdTable.add table out_read job in
               let table = FdTable.add table err_read job in
                  if !debug_exec then
                     eprintf "Started job %d, stdout=%d, stderr=%d@." (**)
                        (Obj.magic pid) (Lm_unix_util.int_of_fd out_read) (Lm_unix_util.int_of_fd err_read);
                  unix_close "spawn_exn.1" out_write;
                  unix_close "spawn_exn.2" err_write;
                  server.server_table <- table;
                  server.server_jobs <- job :: jobs;
                  ProcessStarted id))

   let err_print_status commands handle_status id =
      match commands with
         command :: commands ->
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
            job_target = target;
            job_commands = commands;
            job_handle_out = handle_out;
            job_handle_err = handle_err;
            job_handle_status = handle_status
          } = job
      in
      let { server_table  = table } = server in
         match commands with
            command :: commands ->
               with_pipe (fun out_read out_write ->
               with_pipe (fun err_read err_write ->
                     let () =
                        handle_status id (PrintEager command);
                        Unix.set_close_on_exec out_read;
                        Unix.set_close_on_exec err_read
                     in
                     let pid = start_command server shell out_write err_write command in
                     let table = FdTable.add table out_read job in
                     let table = FdTable.add table err_read job in
                        if !debug_exec then
                           eprintf "Started next job %d, stdout=%d, stderr=%d@." (**)
                              (Obj.magic pid) (Lm_unix_util.int_of_fd out_read) (Lm_unix_util.int_of_fd err_read);
                        unix_close "spawn_next_part.1" out_write;
                        unix_close "spawn_next_part.2" err_write;
                        job.job_pid <- pid;
                        job.job_fd_count <- 2;
                        job.job_stdout <- out_read;
                        job.job_stderr <- err_read;
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
               job.job_state <- JobFinished (fork_error_code, shell.shell_error_value, Unix.gettimeofday() -. job.job_start_time);
               if not (shell.shell_is_failure_exn exn) then
                  raise exn

   (*
    * Check if a command is an error.
    *)
   let command_code shell options command status =
      let flags, _, _ = shell.shell_info command in
      let code =
         match status with
            Unix.WEXITED code ->
               code
          | Unix.WSIGNALED _
          | Unix.WSTOPPED _ ->
               signal_error_code
      in
         if code <> 0 && not (List.mem AllowFailureFlag flags) then
            code
         else
            0

   (*
    * Wait for the current part to finish.
    *)
   let wait_for_job server options job =
      let { job_pid = pid; job_command = command; job_shell = shell } = job in
      let () =
         if !debug_exec then
            eprintf "Waiting for job %d@." (Obj.magic pid)
      in
      let status, v = shell.shell_wait pid in
      let code = command_code shell options command status in
         if !debug_exec then
            eprintf "Job exited with code %d@." code;
         if code <> 0 then
            job.job_state <- JobFinished (code, shell.shell_error_value, Unix.gettimeofday() -. job.job_start_time)
         else
            begin
               job.job_state <- JobRunning v;
               spawn_next_part server job
            end

   (*
    * Handle data on a channel.
    *)
   let handle server options fd =
      let job =
         try FdTable.find server.server_table fd with
            Not_found ->
               raise (Invalid_argument "Omake_exec.handle_channel: no such job")
      in
      let { job_id = id;
            job_stdout = stdout;
            job_stderr = stderr;
            job_handle_out = handle_out;
            job_handle_err = handle_err;
            job_handle_status = handle_status;
            job_command = command;
            job_buffer_len = buffer_len;
            job_buffer = buffer_stdout, buffer_stderr;
          } = job
      in
      let handle, buffer =
         if fd = stdout then
            handle_out, buffer_stdout
         else if fd = stderr then
            handle_err, buffer_stderr
         else
            raise (Invalid_argument "Omake_exec.handle_channel: unknown file descriptor")
      in

      (* Read from the descriptor *)
      let count =
         Lm_thread_pool.blocking_section (fun () ->
               try Unix.read fd buffer 0 buffer_len with
                  Unix.Unix_error _ ->
                     0) ()
      in
         (* Handle end of file *)
         if count = 0 then
            begin
               let table = FdTable.remove server.server_table fd in
               let fd_count = pred job.job_fd_count in
                  (* Reached eof *)
                  if !debug_exec then
                     eprintf "Removing fd %d@." (Lm_unix_util.int_of_fd fd);
                  server.server_table <- table;
                  unix_close "handle" fd;

                  (* Decrement fd_count, and close job when zero *)
                  job.job_fd_count <- fd_count;
                  if fd_count = 0 then
                     wait_for_job server options job
            end
         else
            begin
               (* For "AllowOutputFlag" commands (e.g. scanner) stdout does not "count", but stderr still does *)
               if not (job.job_print_flag || (fd = stdout && allow_output job.job_shell command)) then begin
                  handle_status id (PrintLazy command);
                  job.job_print_flag <- true
               end;
               handle id buffer 0 count
            end

   (*
    * Get all the descriptors.
    *)
   let descriptors server =
      FdTable.fold (fun fd_set fd _ ->
            if !debug_thread then
               eprintf "Local.descriptors: %d@." (Lm_unix_util.int_of_fd fd);
            fd :: fd_set) [] server.server_table

   (*
    * The wait process handles output from each of the jobs.
    * Once both output channels are closed, the job is finished.
    *)
   let wait server options =
      try
         let job, code, value, jobs, time = find_finished_job server in
         let { job_id = id;
               job_handle_status = handle_status;
               job_command = command
             } = job
         in
            server.server_jobs <- jobs;
            handle_status id (PrintExit (command, code, value, time));
            WaitInternalExited (id, code, value)
      with
         Not_found ->
            WaitInternalNone
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
