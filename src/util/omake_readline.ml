(*
 * Simple readline implementation.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, Caltech and HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * 
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
external flush           : unit -> unit                          = "omake_readline_flush"
external isatty          : unit -> bool                          = "omake_isatty"
external is_interactive  : unit -> bool                          = "omake_is_interactive"
external set_interactive : bool -> unit                          = "omake_interactive"
external init            : unit -> unit                          = "omake_readline_init"
external where           : unit -> int                           = "omake_where_history"
external history         : unit -> string array                  = "omake_readline_history"
external load            : string -> unit                        = "omake_readline_load_file"
external save            : unit -> unit                          = "omake_readline_save_file"
external set_length      : int -> unit                           = "omake_readline_set_length"
external set_directory   : string -> unit                        = "omake_readline_set_directory"
external get_prompt_invs : unit -> string * string               = "omake_rl_prompt_wrappers"

let () = init ()

let prompt_invisible =
   match get_prompt_invs () with
      "", "" -> None
    | inv -> Some inv

external ext_readline    : string -> string                      = "omake_readline"
external ext_readstring  : string -> string -> int -> int -> int = "omake_readstring"

let readline s =
   Lm_thread_pool.blocking_section ext_readline s

let readstring s buf off len =
   Lm_thread_pool.blocking_section (ext_readstring s buf off) len

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
