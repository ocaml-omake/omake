
val print_location : bool ref

val pp_print_strategy : Lm_printf.out_channel -> Omake_ast.apply_strategy -> unit
val pp_print_exp : Lm_printf.out_channel -> Omake_ast.exp -> unit
val pp_print_prog : Lm_printf.out_channel -> Omake_ast.exp list -> unit
val pp_print_simple_exp : Lm_printf.out_channel -> Omake_ast.exp -> unit


