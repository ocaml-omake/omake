
open Lm_printf
open Omake_build_type

val env_close_success_tee  : env -> command -> unit
val env_close_failed_tee   : env -> command -> unit

val format_tee_with_nl     : formatter -> command -> unit
val unlink_tee             : command -> unit

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
