(*
 * Execution codes.
 *
 *)


(* open Omake_options *)


(*
 * Type of process codes.
 *)
type process_code =
   ProcessFailed
 | ProcessStarted of Omake_exec_id.t

(*
 * Print flags.
 *)
type ('exp, 'pid, 'value) print_flag =
   PrintEager of 'exp
 | PrintLazy  of 'exp
 | PrintExit  of 'exp * int * 'value * float

(*
 * Internal wait status.
 *)
type ('exp, 'pid, 'value) wait_internal_code =
   WaitInternalExited of Omake_exec_id.t * int * 'value
 | WaitInternalNotify of Lm_notify.event
 | WaitInternalStarted of bool
 | WaitInternalNone

(*
 * External wait status.
 * WaitServer count: a new server started, willing to serve "count" more jobs
 *)
type ('exp, 'pid, 'value) wait_code =
 | WaitExited of Omake_exec_id.t * int * 'value
 | WaitServer of int
 | WaitNotify of Lm_notify.event
 | WaitNone

(*
 * Types of upcalls.
 *)
type ('exp, 'pid, 'value) shell =
   { shell_eval           : Unix.file_descr -> Unix.file_descr -> 'exp -> 'pid;
     shell_info           : 
       'exp ->
       Omake_command_type.command_flag list * Omake_node.Dir.t * Omake_node.Node.t;
     shell_kill           : 'pid -> unit;
     shell_wait           : 'pid -> Unix.process_status * 'value;
     shell_error_value    : 'value;
     shell_print_exp      : 'exp Lm_printf.t;
     shell_print_exn      : exn Lm_printf.t ;
     shell_is_failure_exn : exn -> bool
   }

type ('exp, 'pid, 'value) status_fun = Omake_exec_id.t -> ('exp, 'pid, 'value) print_flag -> unit
type output_fun = Omake_exec_id.t -> string -> int -> int -> unit

(*
 * Internal execution server has a few extra functions.
 *)
module type ExecServer =
sig
   (*
    * Command processor.
    *)
   type ('exp, 'pid, 'value) t

   (*
    * Create the processor.
    * The directory is the current working root directory.
    *)
   val create : string -> ('exp, 'pid, 'value) t

   (*
    * Close it, and possibly deallocate state.
    *)
   val close : ('exp, 'pid, 'value) t -> unit

   (*
    * Start a command, and return the process ID.
    *)
   val spawn :
      ('exp, 'pid, 'value) t ->                 (* Current state *)
      ('exp, 'pid, 'value) shell ->             (* The shell that does evaluation *)
      Omake_exec_id.t ->                                     (* Id for the new process *)
      output_fun ->                             (* Function to handle output from stdout *)
      output_fun ->                             (* Function to handle output from stderr *)
      ('exp, 'pid, 'value) status_fun ->        (* Function to handle status commands *)
      Omake_node.Node.t ->                                 (* Target being built *)
      'exp list ->                              (* Commands to execute *)
      process_code                              (* The process id *)

   (*
    * The internal versions are polled using select.
    *)
   val descriptors : ('exp, 'pid, 'value) t -> Unix.file_descr list

   (*
    * Handle input from one of the descriptors.
    *)
   val handle : ('exp, 'pid, 'value) t -> Omake_options.t -> Unix.file_descr -> unit

   (*
    * Wait for any one of the commands to finish.
    *)
   val wait : ('exp, 'pid, 'value) t -> Omake_options.t -> ('exp, 'pid, 'value) wait_internal_code
end

(*
 * The execution service.
 *)
module type ExecService =
sig
   (*
    * Command processor.
    *)
   type ('exp, 'pid, 'value) t

   (*
    * Create the processor.
    * The directory is the current working root directory.
    *)
   val create : Omake_node.Dir.t -> Omake_options.t -> ('exp, 'pid, 'value) t

   (*
    * Close it, and possibly deallocate state.
    *)
   val close : ('exp, 'pid, 'value) t -> unit

   (*
    * Start a command, and return the process ID.
    *)
   val spawn :
      ('exp, 'pid, 'value) t ->                 (* Current state *)
      ('exp, 'pid, 'value) shell ->             (* Evaluate a shell command *)
      Omake_options.t ->                          (* Current options in effect *)
      output_fun ->                             (* Function to handle the OMake messages meant for stdout *)
      output_fun ->                             (* Function to handle output from stdout *)
      output_fun ->                             (* Function to handle output from stderr *)
      string ->                                 (* Name of this command *)
      Omake_node.Node.t ->                                 (* Target being built *)
      'exp list ->                              (* Commands to execute *)
      process_code                              (* The process id *)

   (*
    * Wait for any one of the commands to finish.
    *)
   val wait : ?onblock:(unit->unit) -> 
              ('exp, 'pid, 'value) t -> Omake_options.t -> 
              ('exp, 'pid, 'value) wait_code

   (*
    * Notify when a file changes.
    *)
   val monitor : ('exp, 'pid, 'value) t -> Omake_node.Node.t -> unit
   val monitor_tree : ('exp, 'pid, 'value) t -> Omake_node.Dir.t -> unit

   (*
    * Get the next file change notification.
    * This function blocks.  Use wait if you want
    * nonblocking behavior.
    *)
   val pending : ('exp, 'pid, 'value) t -> bool
   val next_event : ('exp, 'pid, 'value) t -> Lm_notify.event
end


