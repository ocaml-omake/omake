(*
 * $Id: lm_io.mli 7878 2005-10-09 00:13:24Z jyh $
 * Operations to do I/O
 *
 * Moved here from Pervasives
 *)

type in_channel
type out_channel

val stdin : in_channel
val stdout : out_channel
val stderr : out_channel

val print_char : char -> unit
val print_string : string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_endline : string -> unit
val print_newline : unit -> unit

val prerr_char : char -> unit
val prerr_string : string -> unit
val prerr_int : int -> unit
val prerr_float : float -> unit
val prerr_endline : string -> unit
val prerr_newline : unit -> unit

val read_line : unit -> string
val read_int : unit -> int
val read_float : unit -> float

type open_flag =
   Open_rdonly
 | Open_wronly
 | Open_append
 | Open_creat
 | Open_trunc
 | Open_excl
 | Open_binary
 | Open_text
 | Open_nonblock

val open_out : string -> out_channel
val open_out_bin : string -> out_channel
val open_out_gen : open_flag list -> int -> string -> out_channel
val flush : out_channel -> unit

(*
 * General output.
 *)
val output_char : out_channel -> char -> unit
val output_string : out_channel -> string -> unit
val output : out_channel -> string -> int -> int -> unit
val output_byte : out_channel -> int -> unit
val output_binary_int : out_channel -> int -> unit
val seek_out : out_channel -> int -> unit
val pos_out : out_channel -> int
val out_channel_length : out_channel -> int
val close_out : out_channel -> unit
val set_binary_mode_out : out_channel -> bool -> unit

(*
 * General input.
 *)
val open_in : string -> in_channel
val open_in_bin : string -> in_channel
val open_in_gen : open_flag list -> int -> string -> in_channel
val input_char : in_channel -> char
val input_line : in_channel -> string
val input : in_channel -> string -> int -> int -> int
val really_input : in_channel -> string -> int -> int -> unit
val input_byte : in_channel -> int
val input_binary_int : in_channel -> int
val seek_in : in_channel -> int -> unit
val pos_in : in_channel -> int
val in_channel_length : in_channel -> int
val close_in : in_channel -> unit
val set_binary_mode_in : in_channel -> bool -> unit
