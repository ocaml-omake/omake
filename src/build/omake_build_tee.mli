

val env_close_success_tee  : 
  Omake_build_type.env -> Omake_build_type.command -> unit
val env_close_failed_tee   : 
  Omake_build_type.env -> Omake_build_type.command -> unit

val format_tee_with_nl     : 
  Omake_build_type.command Lm_printf.t 
val unlink_tee             : 
  Omake_build_type.command -> unit

