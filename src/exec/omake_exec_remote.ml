(*
 * Remote execution of jobs.  This includes both the job server
 * as well as the server handler.
 *
 *)
(* FIXME: the remote servers never terminate regularly *)

let flush_channel = flush

open Lm_printf
open Lm_debug

(* open Omake_util *)
open Omake_node
open Omake_state
open Omake_exec_id
open Omake_exec_util
open Omake_exec_type




(*
 * Build debugging.
 *)
let debug_remote =
   create_debug (**)
      { debug_name = "remote";
        debug_description = "Remote execution debugging";
        debug_value = false
      }

(*
 * Type of messages sent to the service.
 *)
type ('exp, 'pid, 'value) request =
   RequestSpawn of Omake_exec_id.t * Node.t * 'exp

(*
 * Type of messages received from the remote server.
 *)
type ('exp, 'pid, 'value) response =
   ResponseCreate of bool
 | ResponseExited of Omake_exec_id.t * int * 'value
 | ResponseStdout of Omake_exec_id.t * bytes
 | ResponseStderr of Omake_exec_id.t * bytes
 | ResponseStatus of Omake_exec_id.t * ('exp, 'pid, 'value) print_flag

(*
 * A local exception when the connection fails.
 *)


(*
 * During login, there is a synchronization string.
 *)
let sync_char   = '\255'
let sync_string = "\255OMake Remote Execution Protocol Version 1.0"

(************************************************************************
 * Printing.
 *)
(* let pp_print_command_line buf (shell, command) = *)
(*    shell.shell_print_exp buf command *)

(* let pp_print_command_lines buf (shell, commands) = *)
(*    List.iter (fun exp -> fprintf buf "@ %a" shell.shell_print_exp exp) commands *)

(* let pp_print_request _buf (shell, request) = *)
(*    match request with *)
(*       RequestSpawn (id, target, commands) -> *)
(*          eprintf "@[<hv 0>@[<hv 3>RequestSpawn {@ id = %a;@ target = %a;@ @[<v 3>commands = %a@]@]@ }@]" (\**\) *)
(*             pp_print_pid id *)
(*             pp_print_node target *)
(*             pp_print_command_lines (shell, commands) *)

(* let pp_print_flag buf (shell, flag) = *)
(*    match flag with *)
(*       PrintEager command -> *)
(*          fprintf buf "@[<hv 3>Eager@ %a@]" pp_print_command_line (shell, command) *)
(*     | PrintLazy command -> *)
(*          fprintf buf "@[<hv 3>Lazy@ %a@]" pp_print_command_line (shell, command) *)
(*     | PrintExit (command, code, _, time) -> *)
(*          fprintf buf "@[<hv 3>Exit %d,@ %a,@ %a@]" code pp_time time pp_print_command_line (shell, command) *)

(* let pp_print_response _buf (shell, response) = *)
(*    match response with *)
(*       ResponseCreate flag -> *)
(*          eprintf "ResponseCreate %b" flag *)
(*     | ResponseExited (id, code, _) -> *)
(*          eprintf "ResponseExited (%a, %d)" pp_print_pid id code *)
(*     | ResponseStdout (id, s) -> *)
(*          eprintf "ResponseStdout (%a, \"%s\")" pp_print_pid id (String.escaped s) *)
(*     | ResponseStderr (id, s) -> *)
(*          eprintf "ResponseStderr (%a, \"%s\")" pp_print_pid id (String.escaped s) *)
(*     | ResponseStatus (id, flag) -> *)
(*          eprintf "@[<hv 0>@[<hv 3>ResponseStatus {@ id = %a;@ flag = %a@]@ }@]" (\**\) *)
(*             pp_print_pid id *)
(*             pp_print_flag (shell, flag) *)

module Server = struct


  (************************************************************************
   * Data is marshaled.
   *)
  
  let stdin = stdin
  let stdout = stdout
                 
  (*
   * Send the sync string.
   *)
  let send_sync () =
    output_string stdout sync_string;
    flush_channel stdout
                     
  (*
   * The actual marshalers.
   *)
  let sendmsg out msg =
    Marshal.to_channel out msg [];
    flush_channel out

  let send_response response =
    sendmsg stdout response
            
  let send_request = sendmsg

  (*
   * Receive a message.
   *)
  let recvmsg inx =
    Marshal.from_channel inx
                         
  let recv_request () =
    recvmsg stdin
            
  let recv_response = recvmsg

  (************************************************************************
   * Remote server.
   *)

  (*
   * Handle output.
   *)
  let handle_stdout id buf off len =
    send_response (ResponseStdout (id, Bytes.sub buf off len))
                  
  let handle_stderr id buf off len =
    send_response (ResponseStderr (id, Bytes.sub buf off len))
                  
  let handle_status id flag =
    send_response (ResponseStatus (id, flag))
                  
  (*
   * We don't evaluate commands remotely.
   *)
  (* let eval _ = *)
  (*    raise (Invalid_argument "Omake_exec_remote.eval") *)
                  
  (*
   * Start a process.
   *)
  let handle_spawn local shell id target commands =
    let code =
      Omake_exec_local.spawn (**)
        local
        shell
        id
        handle_stdout
        handle_stderr
        handle_status
        target
        commands
    in
    match code with
      | ProcessFailed ->
          send_response (ResponseExited (id, fork_error_code, shell.shell_error_value))
      | ProcessStarted _ ->
          (* Remote server already assumed it started *)
          ()
            
  (*
   * Handle input from a descriptor.
   * Special case if input is from stdin.
   *)
  let handle local shell options fd =
    if !debug_remote then
      eprintf "*** server: got input on fd %d@." (Obj.magic fd);
    if fd = Unix.stdin then
      match recv_request () with
          RequestSpawn (id, target, commands) ->
        handle_spawn local shell id target commands
    else
      let closed = Omake_exec_local.handle local options fd in
      if closed then (
        Omake_exec_local.acknowledge_eof local options fd;
        Omake_exec_local.handle_eof local options fd
      )
                              
  (*
   * Serve.
   *)
  let rec serve local shell options =
    match Omake_exec_local.wait local options with
      | WaitInternalExited (id, code, value) ->
          send_response (ResponseExited (id, code, value));
          serve local shell options
      | WaitInternalNotify _ ->
          raise (Invalid_argument "Omake_exec_remote.serve: received notify message")
      | WaitInternalStarted _ ->
          raise (Invalid_argument "Omake_exec_remote.serve: received started message")
      | WaitInternalNone ->
          let fd_read = Unix.stdin :: Omake_exec_local.descriptors local in
          let fd_read =
            try
              let fd_read, _, _ = Unix.select fd_read [] [] (-1.0) in
              fd_read
            with
                Unix.Unix_error _ ->
              []
          in
          List.iter (handle local shell options) fd_read;
          serve local shell options
                
  (*
   * Remote service.
   *)
  let main_exn shell options =
    (* First, synchronize with the server *)
    if !debug_remote then
      eprintf "*** server: starting@.";
    send_sync ();
    send_response (ResponseCreate true);
    serve (Omake_exec_local.create "local") shell options
          
  let main shell options =
    try
      main_exn shell options
    with
        exn ->
      eprintf "@[<hv 3>*** server: uncaught exception:@ %s@]@." (Printexc.to_string exn);
      exit exn_error_code
end

(************************************************************************
 * Remote service.
 *)

(*
 * Status of a job.
 *)
type 'value job_state =
    JobRunning
  | JobFinished of int * 'value

 (*
  * A Job has some handlers.
  *)
 type ('exp, 'pid, 'value) job =
    { job_id            : Omake_exec_id.t;
      job_handle_out    : output_fun;
      job_handle_err    : output_fun;
      job_handle_status : ('exp, 'pid, 'value) status_fun;
      job_shell         : ('exp, 'pid, 'value) shell;
      mutable job_state : 'value job_state
    }

 (*
  * Server state.
  *)
 type server_state =
    ServerConnecting of int
  | ServerSynced
  | ServerConnected of bool
  | ServerRunning

 (*
  * The state is an ssh channel.
  *)
 type ('exp, 'pid, 'value) t =
    { server_out : out_channel;
      server_in  : in_channel;
      server_pid : int;

      (* Keep track of running jobs, so we can kill them if the connection drops *)
      mutable server_state : server_state;
      mutable server_jobs  : ('exp, 'pid, 'value) job IdTable.t
    }

 (*
  * Wrap the message calls.
  *)
 let send_request server request =
    Server.send_request server.server_out request

 let recv_response server =
    Server.recv_response server.server_in

 (*
  * Create a new service.
  *)
 let create hostname =
    let flags =
       if !debug_remote then
          "-debug-remote"
       else
          ""
    in
    let cmd = Printf.sprintf "omake %s -server %s" flags (Dir.absname (Dir.cwd ())) in
    let ssh = "ssh" in
    let cmd = [|ssh; hostname; cmd|] in
       (* Create the pipes *)
       with_pipe (fun stdin_read stdin_write ->
       with_pipe (fun stdout_read stdout_write ->
             let pid = Unix.create_process ssh cmd stdin_read stdout_write Unix.stderr in
             let server =
                { server_out   = Unix.out_channel_of_descr stdin_write;
                  server_in    = Unix.in_channel_of_descr  stdout_read;
                  server_pid   = pid;
                  server_state = ServerConnecting 0;
                  server_jobs  = IdTable.empty
                }
             in
                Unix.close stdin_read;
                Unix.close stdout_write;
                server))

 (*
  * Close the connection.
  *)
 let close server =
    let { server_out = requestc;
          server_in  = responsec;
          server_pid = pid;
          _
        } = server
    in
    let () =
       try Unix.kill pid Sys.sigterm with
          Unix.Unix_error _ ->
             ()
    in
       close_out requestc;
       close_in responsec

 (*
  * Start a new job.
  *)
 let spawn server shell id handle_out handle_err handle_status target commands =
    if !debug_remote then
       eprintf "*** remote: spawn: %a@." pp_print_node target;

    (* Send the request to the remote server *)
    send_request server (RequestSpawn (id, target, commands));

    (* Pretend that the job started *)
    let job =
       { job_id = id;
         job_shell = shell;
         job_handle_out = handle_out;
         job_handle_err = handle_err;
         job_handle_status = handle_status;
         job_state = JobRunning
       }
    in
       server.server_jobs <- IdTable.add server.server_jobs id job;
       ProcessStarted id

 (*
  * Get descriptors.
  *)
 let descriptors server =
    [Unix.descr_of_in_channel server.server_in]

 (*
  * A job exited.
  *)
 let handle_exit server id code value =
    let job =
       try IdTable.find server.server_jobs id with
          Not_found ->
             raise (Invalid_argument "Omake_exec_remote.handle_exit: no such job")
    in
    job.job_state <- JobFinished (code, value);
    false

 (*
  * Handle data from stdout.
  *)
 let handle_stdout server id buf =
    let job =
       try IdTable.find server.server_jobs id with
          Not_found ->
             raise (Invalid_argument "Omake_exec_remote.handle_stdout: no such job")
    in
    let { job_handle_out = handle_out ; _} = job in
    handle_out id buf 0 (Bytes.length buf);
    false

 let handle_stderr server id buf =
    let job =
       try IdTable.find server.server_jobs id with
          Not_found ->
             raise (Invalid_argument "Omake_exec_remote.handle_stderr: no such job")
    in
    let { job_handle_err = handle_err ; _} = job in
    handle_err id buf 0 (Bytes.length buf);
    false

 let handle_status server id flag =
    let job =
       try IdTable.find server.server_jobs id with
          Not_found ->
             raise (Invalid_argument "Omake_exec_remote.handle_status: no such job")
    in
    let { job_handle_status = handle_status ; _} = job in
    handle_status id flag;
    false

 (*
  * Handle input.
  *)
 let handle_normal server _fd =
    if !debug_remote then
       eprintf "*** handle_normal@.";
    match recv_response server with
       ResponseCreate succeeded ->
          server.server_state <- ServerConnected succeeded;
          not succeeded

     | ResponseExited (id, code, value) ->
          handle_exit server id code value

     | ResponseStdout (id, buf) ->
          handle_stdout server id buf

     | ResponseStderr (id, buf) ->
          handle_stderr server id buf

     | ResponseStatus (id, flag) ->
          handle_status server id flag

 let handle server _options fd =
    let { server_state = state;
          server_in = responsec;
          _
        } = server
    in
       match state with
          ServerConnecting i ->
             (try
                 let c = input_char responsec in
                    if c = sync_char then
                       server.server_state <- ServerConnecting 1
                    else if c = sync_string.[i] then
                       if i = pred (String.length sync_string) then
                          let () =
                             if !debug_remote then
                                eprintf "*** remote: server is synced@."
                          in
                             server.server_state <- ServerSynced
                       else
                          server.server_state <- ServerConnecting (succ i)
                    else
                       server.server_state <- ServerConnecting 0;
                    false
              with
                 End_of_file ->
                    server.server_state <- ServerConnected false; true)
        | ServerSynced
        | ServerConnected _
        | ServerRunning ->
             handle_normal server fd


 let handle_eof _ _ _ = ()
 let acknowledge_eof _ _ _ = ()


 (*
  * Find a finished job, or raise Not_found if there is none.
  *)
 let wait_for_job server =
    let search _ job =
       match job with
          { job_id = id; job_state = JobFinished (code, value) ; _} ->
             Some (id, code, value)
        | { job_state = JobRunning ; _} ->
             None
    in
       match IdTable.find_iter search server.server_jobs with
          Some (id, code, value) ->
             server.server_jobs <- IdTable.remove server.server_jobs id;
             WaitInternalExited (id, code, value)
        | None ->
             WaitInternalNone

 let wait server _options =
    if !debug_remote then
       eprintf "*** remote: wait@.";
       match server.server_state with
          ServerConnecting _
        | ServerSynced ->
             WaitInternalNone
        | ServerConnected succeeded ->
             server.server_state <- ServerRunning;
             WaitInternalStarted succeeded
        | ServerRunning ->
             wait_for_job server


 let main = Server.main
