type arg_string =
  | ArgString of string
  | ArgData of string
type arg = arg_string list
type command_digest =
    Digest.t option
type command_flag =
    QuietFlag
  | AllowFailureFlag
  | AllowOutputFlag
type ('exp, 'argv, 'value)
    poly_command_inst =
    CommandEval of 'exp list
  | CommandPipe of 'argv
  | CommandValues of 'value list
type ('venv, 'exp, 'argv, 'value)
    poly_command_line = 
  {
    command_loc : Lm_location.t;
    command_dir : Omake_node.Dir.t;
    command_target :
      Omake_node.Node.t;
    command_flags : command_flag list;
    command_venv : 'venv;
    command_inst :
      ('exp, 'argv, 'value)
        poly_command_inst;
  }

val simple_string_of_arg :
  arg_string list -> string

val glob_string_of_arg :
  Lm_glob.glob_options ->
  arg_string list -> string

val is_glob_arg :
  Lm_glob.glob_options ->
  arg_string list -> bool

val is_quoted_arg :
  arg_string list -> bool

val pp_arg_data_string : string Lm_printf.t 

val pp_print_arg :  arg_string list Lm_printf.t 

val pp_print_verbose_arg :  arg_string list Lm_printf.t 

val pp_print_command_flag : command_flag Lm_printf.t 

val pp_print_command_flags : command_flag list Lm_printf.t 

module type PrintArgvSig =
sig
  type argv
  val pp_print_argv :
    argv Lm_printf.t
end
module MakePrintCommand :
  functor
    (PrintArgv : PrintArgvSig) ->
  sig  
    val pp_print_command_inst :
    
      (Omake_ir.exp, PrintArgv.argv,
       'a)
        poly_command_inst Lm_printf.t 

    val pp_print_command_line :
      ('a, Omake_ir.exp,
       PrintArgv.argv, 'b)
        poly_command_line Lm_printf.t 

    val pp_print_command_lines :
      ('a, Omake_ir.exp,
       PrintArgv.argv, 'b)
        poly_command_line list Lm_printf.t

  end
