
open Unix

val term_size : file_descr -> int * int
val min_screen_width : int ref
val term_width : out_channel -> int -> int

val stdout_width : int
val stderr_width : int
