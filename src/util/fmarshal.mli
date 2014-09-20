type 'a item =
  | Bool of bool
  | Char of char
  | Code of int
  | Symbol of int
  | Int of int
  | Magic of 'a
  | Float of float
  | String of string
  | List of 'a item list

module type MarshalIOSig =
  sig
    type t
    type in_channel
    type out_channel
    val magic_of_int : int -> t
    val int_of_magic : t -> int
    val input_byte : in_channel -> int
    val input_buffer : in_channel -> string -> int -> int -> unit
    val output_byte : out_channel -> int -> unit
    val output_buffer : out_channel -> string -> int -> int -> unit
  end
module type MarshalSig =
  sig
    type t
    type in_channel
    type out_channel
    val marshal : out_channel -> t item -> unit
    val unmarshal : in_channel -> t item
  end
module Make :
  functor (IO : MarshalIOSig) ->
    sig
      type t = IO.t
      type in_channel = IO.in_channel
      type out_channel = IO.out_channel
      val marshal : out_channel -> t item -> unit
      val unmarshal : in_channel -> t item
    end
