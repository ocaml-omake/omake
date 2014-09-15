open Unix
open Lm_debug
open! Lm_printf

let debug_terminal =
   create_debug {
      debug_name = "terminal";
      debug_description = "show terminal size operations";
      debug_value = false
   }

external term_size : file_descr -> int * int = "caml_term_size"

let min_screen_width = ref 40

let term_width_fd fd width =
   try
      let _, cols = term_size fd in
         if !debug_terminal then
            eprintf "Terminal size: requested %i, got %i, minimal witdth is %i%t" width cols (!min_screen_width) eflush;
         max (!min_screen_width) cols
   with
      Failure s ->
         if !debug_terminal then
            eprintf "Can't get terminal size: %s%t" s eflush;
         width

let term_width out width =
   term_width_fd (descr_of_out_channel out) width

let stdout_width = term_width_fd Unix.stdout 80
let stderr_width = term_width_fd Unix.stderr 80

let () =
   if stdout_width <> 80 then pp_set_margin std_formatter stdout_width;
   if stderr_width <> 80 then pp_set_margin err_formatter stderr_width

