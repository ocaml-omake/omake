module type PrintfArgsSig =
  sig
    type t
    type value
    val print_char : t -> char -> unit
    val print_string : t -> string -> unit
    val open_box : t -> int -> unit
    val open_hbox : t -> unit
    val open_vbox : t -> int -> unit
    val open_hvbox : t -> int -> unit
    val open_hovbox : t -> int -> unit
    val close_box : t -> unit
    val print_cut : t -> unit
    val print_space : t -> unit
    val force_newline : t -> unit
    val print_break : t -> int -> int -> unit
    val print_flush : t -> unit
    val print_newline : t -> unit
    val bool_of_value : t -> value -> bool
    val int_of_value : t -> value -> int
    val char_of_value : t -> value -> char
    val float_of_value : t -> value -> float
    val string_of_value : t -> value -> string
    val print_value : t -> value -> unit
    val apply1 : t -> value -> unit
    val apply2 : t -> value -> value -> unit
    val exit : t -> value list -> value
  end
module MakePrintf :
  functor (Args : PrintfArgsSig) ->
    sig
      type t = Args.t
      type value = Args.value
      val fprintf : t -> string -> value list -> value
    end
