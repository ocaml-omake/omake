(*
 * Raw printer interface just includes basic output
 * functions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

(*
 * Raw printer just has two methods.
 *)
type raw_printer =
   { raw_print_string  : string -> int -> int -> unit;
     raw_print_flush   : unit -> unit;
     raw_print_newline : unit -> unit;
     raw_print_spaces  : int -> unit
   }

(*
 * Basic raw printers.
 *)
let raw_channel_printer out =
   { raw_print_string  = output out;
     raw_print_flush   = (fun () -> flush out);
     raw_print_newline = (fun () -> output_char out '\n');
     raw_print_spaces  =
        (fun i ->
              for i = 0 to pred i do
                 output_char out ' '
              done)
   }

let raw_buffer_printer buf =
   { raw_print_string   = Buffer.add_substring buf;
     raw_print_flush    = (fun () -> ());
     raw_print_newline  = (fun () -> Buffer.add_char buf '\n');
     raw_print_spaces   =
        (fun i ->
              for i = 0 to pred i do
                 Buffer.add_char buf ' '
              done)
   }

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
