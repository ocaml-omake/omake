(*
 * Debugging tools.
 *)

(*
 * Info about debug variables.
 * The variables themselves are defined in the Lm_debug module.
 *)
type debug_info =
   { debug_name : string;
     debug_description : string;
     debug_value : bool
   }

(* if "load" debug is true, `show_load (s ^ "%t")' will print s to stderr and flush stderr *)
val show_loading : ((out_channel -> unit) -> unit, out_channel, unit) format -> unit

(*
 * Lm_debug flags.
 *)
val debug_enabled : bool
val debug : bool ref -> bool
val debug_level : int ref -> int -> bool

(*
 * We create named debug variables.
 *)
val create_debug : debug_info -> bool ref
val load_debug : string -> bool ref

(*
 * Operations to inspect debug flags.
 *)
val set_debug : string -> bool -> unit
val get_debug : string -> debug_info
val debuggers : unit -> debug_info array
val debug_usage : unit -> unit

(*
 * We allow flags to be set from the environment.
 * they may be set before the vars are created,
 * so we add them as "possible" debug flags,
 * then check them later.
 *)
val set_possible_debug : string -> bool -> unit
val check_debug : unit -> unit

(*
 * Interface with Arg module.
 *)
val set_debug_flags : string -> unit

(*
 * Helper function for ad-hoc profiling. (timing_wrap s f x) computes (f x)
 * keeping track of the time it took to do it and collects the statistics
 * indexed by s.
 * report_timing prints out the statistics collected so far. This function
 * will be called automatically at function exit.
 *
 * Warning: timing_wrap is currently not threads-safe.
 *)
val timing_wrap : string -> ('a -> 'b) -> 'a -> 'b
val report_timing : unit -> unit

