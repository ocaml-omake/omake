(*
 * $Id: lm_io.ml 7878 2005-10-09 00:13:24Z jyh $
 * Operations to do I/O
 *
 * Moved here from Pervasives
 *)

open String

(*
 * Reading.
 *)
external fopen : string -> int -> int -> int = "%file_open"
external fclose : int -> unit = "%file_close"
external fread : int -> string -> int -> int -> int = "%file_read"
external fwrite : int -> string -> int -> int -> int = "%file_write"
external fseek : int -> int -> unit = "%file_seek"
external ftell : int -> int = "%file_tell"
external flength : int -> int = "%file_len"

let code = int_of_char 
let chr = char_of_int

(*
 * An in-channel contains a buffer.
 *)
type channel =
   { mutable chan_fd : int;
     chan_buf : string;
     mutable chan_min : int;
     mutable chan_max : int
   }

type in_channel = channel
type out_channel = channel

(*
 * Buffer operations.
 *)
let bufsize = 8192

let mk_chan fd =
   { chan_fd = fd;
     chan_buf = screate bufsize;
     chan_min = 0;
     chan_max = 0
   }

let stdin = mk_chan 0
let stdout = mk_chan 1
let stderr = mk_chan 2

(*
 * Convert the open flags.
 *)
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

let o_rdonly   = 1
let o_wronly   = 2
let o_rdwr     = 3
let o_creat    = 4
let o_excl     = 8
let o_noctty   = 16
let o_trunc    = 32
let o_append   = 64
let o_nonblock = 128
let o_sync     = 256
let o_binary   = 512

let rec mask_of_flags mask = function
   h :: t ->
      let mask' =
         match h with
            Open_rdonly -> o_rdonly
          | Open_wronly -> o_wronly
          | Open_append -> o_append
          | Open_creat -> o_creat
          | Open_trunc -> o_trunc
          | Open_excl -> o_excl
          | Open_binary -> o_binary
          | Open_text -> 0
          | Open_nonblock -> o_nonblock
      in
         mask_of_flags (mask lor mask') t
 | [] ->
      mask

(*
 * Opening and closing.
 *)
let open_gen mask flags umask name =
   let mask = mask_of_flags 0 flags in
   let fd = fopen name mask umask in
      if fd < 0 then
         raise (Sys_error name);
      mk_chan fd

let open_out_gen = open_gen o_wronly
let open_in_gen = open_gen o_rdonly

let open_out_bin = open_gen (o_wronly lor o_binary) [] 438
let open_out = open_gen o_wronly [] 438

let open_in_bin = open_gen (o_rdonly lor o_binary) [] 0
let open_in = open_gen o_rdonly [] 0

let close chan =
   let fd = chan.chan_fd in
      if fd >= 0 then
         fclose fd;
      chan.chan_fd <- (-1)

let close_in = close
let close_out = close

(*
 * Filling and flushing.
 *)
let filbuf chan =
   let { chan_fd = fd; chan_buf = buf } = chan in
   let count = fread fd buf 0 bufsize in
      if count <= 0 then
         raise End_of_file;
      chan.chan_min <- 0;
      chan.chan_max <- count

let flush chan =
   let { chan_fd = fd; chan_buf = buf; chan_min = min; chan_max = max } = chan in
   let rec writebuf min =
      if min < max then
         let count = max - min in
         let count = fwrite fd buf min count in
            if count <= 0 then
               begin
                  chan.chan_min <- min;
                  raise End_of_file
               end;
            writebuf (min + count)
   in
      writebuf min;
      chan.chan_min <- 0;
      chan.chan_max <- 0

(*
 * Output.
 *)
let output_char chan c =
   if chan.chan_max = bufsize then
      flush chan;
   let max = chan.chan_min in
      chan.chan_buf.[max] <- c;
      chan.chan_max <- succ max

let rec writebuf fd buf off len =
   if off < len then
      let count = len - off in
      let count = fwrite fd buf off count in
         if count <= 0 then
            raise End_of_file;
         writebuf fd buf (off + count) len

let output chan buf off len =
   if len - off >= bufsize then
      begin
         flush chan;
         writebuf chan.chan_fd buf off len;
         chan.chan_min <- 0;
         chan.chan_max <- 0
      end
   else
      begin
         if chan.chan_max + len - off > bufsize then
            flush chan;
         sblit buf off chan.chan_buf chan.chan_max len;
         chan.chan_max <- chan.chan_max + len
      end

let output_string chan s =
   output chan s 0 (slength s)

let output_byte chan i =
   output_char chan (chr i)

let output_binary_int chan i =
   output_byte chan ((i lsr 24) land 255);
   output_byte chan ((i lsr 16) land 255);
   output_byte chan ((i lsr 8) land 255);
   output_byte chan (i land 255)

let seek_out chan loc =
   flush chan;
   fseek chan.chan_fd loc

let pos_out chan =
   flush chan;
   ftell chan.chan_fd

let out_channel_length chan =
   flush chan;
   flength chan.chan_fd

let set_binary_mode_out chan flag =
   ()

(*
 * Input.
 *)
let ungetc chan =
   let min = chan.chan_min in
      if min = 0 then
         raise (Invalid_argument "ungetc");
      chan.chan_min <- pred min

let set_binary_mode_in chan flag =
   ()

let rec input_char chan =
   let { chan_buf = buf; chan_min = min; chan_max = max } = chan in
      if min = max then
         begin
            filbuf chan;
            input_char chan
         end
      else
         begin
            chan.chan_min <- succ min;
            buf.[min]
         end

let input_byte chan =
   code (input_char chan)

let input_binary_int chan =
   let c1 = input_byte chan in
   let c2 = input_byte chan in
   let c3 = input_byte chan in
   let c4 = input_byte chan in
      ((c1 lsl 24) lor (c2 lsl 16) lor (c3 lsl 8) lor c4)

let rec input_line chan =
   let { chan_buf = buf; chan_min = min; chan_max = max } = chan in
      if min = max then
         begin
            filbuf chan;
            input_line chan
         end
      else
         let rec search s i min max =
            if i = max then
               let s = s ^ (ssub buf min max) in
                  try
                     filbuf chan;
                     search s chan.chan_min chan.chan_min chan.chan_max
                  with
                     End_of_file ->
                        s
            else if buf.[i] = '\n' then
               let s = s ^ (ssub buf min i) in
                  chan.chan_min <- succ i;
                  s
            else
               search s (succ i) min max
         in
            search "" chan.chan_min chan.chan_min chan.chan_max

let input chan buf off len =
   if chan.chan_min = chan.chan_max then
      filbuf chan;
   let { chan_buf = buf'; chan_min = min'; chan_max = max' } = chan in
   let count = max' - min' in
      if count < len then
         begin
            sblit buf' min' buf off count;
            chan.chan_min <- 0;
            chan.chan_max <- 0;
            count
         end
      else
         begin
            sblit buf' min' buf off len;
            chan.chan_min <- min' + len;
            len
         end

let really_input chan buf off len =
   let rec read amount off len =
      if off < len then
         let { chan_buf = buf'; chan_min = min'; chan_max = max' } = chan in
         let count = max' - min' in
            if count < len then
               let amount = amount + count in
               let _ = sblit buf' min' buf off count in
               let off = off + count in
               let len = len - count in
                  filbuf chan;
                  read amount off len
            else
               let amount = amount + len in
                  sblit buf' min' buf off len;
                  chan.chan_min <- chan.chan_min + len
   in
      read 0 off len

(*
 * Seeking.
 *)
let seek_in chan pos =
   chan.chan_min <- 0;
   chan.chan_max <- 0;
   fseek chan.chan_fd pos

let pos_in chan =
   let pos = ftell chan.chan_fd in
      pos - (chan.chan_max - chan.chan_min)

let in_channel_length chan =
   flength chan.chan_fd

(*
 * Standard operations.
 *)
let print_char = output_char stdout

let print_string = output_string stdout

let print_int i =
   print_string (string_of_int i)

let print_float x =
   print_string (string_of_float x)

let print_newline () =
   print_char '\n'

let print_endline s =
   print_string s;
   print_newline ()

let prerr_char = output_char stderr

let prerr_string = output_string stderr

let prerr_int i =
   prerr_string (string_of_int i)

let prerr_float x =
   prerr_string (string_of_float x)

let prerr_newline () =
   prerr_char '\n'

let prerr_endline s =
   prerr_string s;
   prerr_newline ()

let read_line () =
   input_line stdin

let is_digit = function
   '0'
 | '1'
 | '2'
 | '3'
 | '4'
 | '5'
 | '6'
 | '7'
 | '8'
 | '9' ->
      true
 | _ ->
      false

let int_of_digit = function
   '0' -> 0
 | '1' -> 1
 | '2' -> 2
 | '3' -> 3
 | '4' -> 4
 | '5' -> 5
 | '6' -> 6
 | '7' -> 7
 | '8' -> 8
 | '9' -> 9
 | _ ->
      raise (Invalid_argument "int_of_digit")

let is_space = function
   ' '
 | '\t'
 | '\r'
 | '\n' ->
      true

 | _ ->
      false

(*
 * Skip white space.
 *)
let rec skip_white chan =
   let c = input_char chan in
      if is_space c then
         skip_white chan
      else
         ungetc chan

(*
 * Read an integer.
 *)
let input_int chan =
   skip_white chan;
   let c = input_char chan in
   let negate =
      if c = '-' then
         true
      else
         begin
            ungetc chan;
            false
         end
   in
   let rec input i =
      let c = input_char chan in
         if is_digit c then
            input (i * 10 + int_of_digit c)
         else
            begin
               ungetc chan;
               if negate then
                  -i
               else
                  i
            end
   in
      input 0

(*
 * Read a floating point number.
 *)
type float_state =
   FloatInt
 | FloatDecimal
 | FloatExponent
 | FloatSignedExponent

let fbufsize = 256

let input_float chan =
   let buf = screate fbufsize in
   let _ = skip_white chan in
   let c = input_char chan in
   let negate =
      if c == '-' then
             true
      else
         begin
            ungetc chan;
            false
         end
   in
   let rec loop state i =
      if i = fbufsize then
         i
      else
         let c = input_char chan in
            buf.[i] <- c;
            match state with
               FloatInt ->
                  if is_digit c then
                     loop FloatInt (succ i)
                  else if c == '.' then
                     loop FloatDecimal (succ i)
                  else if c == 'e' || c == 'E' then
                     loop FloatExponent (succ i)
                  else
                     (ungetc chan; i)


             | FloatDecimal ->
                  if is_digit c then
                     loop FloatDecimal (succ i)
                  else if c == 'e' || c == 'E' then
                     loop FloatExponent (succ i)
                  else
                     (ungetc chan; i)

             | FloatExponent ->
                  if c == '-'  || c == '+' then
                     loop FloatSignedExponent (succ i)
                  else if is_digit c then
                     loop FloatSignedExponent (succ i)
                  else
                     (ungetc chan; i)

             | FloatSignedExponent ->
                  if is_digit c then
                     loop FloatSignedExponent (succ i)
                  else
                     (ungetc chan; i)
   in
   let i = loop FloatInt 0 in
      float_of_string (ssub buf 0 i)

let read_int () =
   input_int stdin

let read_float () =
   input_float stdin
