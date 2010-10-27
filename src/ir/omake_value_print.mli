(*
 * Value printers.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006-2010 Mojave Group, Caltech and HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf

open Omake_wild
open Omake_ir
open Omake_value_type

val pp_print_target           : formatter -> target -> unit
val pp_print_wild_list        : formatter -> wild_in_patt list -> unit
val pp_print_source_list      : formatter -> ('a * source_core) list -> unit
val pp_print_value            : formatter -> value -> unit
val pp_print_simple_value     : formatter -> value -> unit
val pp_print_value_list       : formatter -> value list -> unit
val pp_print_path             : formatter -> path -> unit

(* Helpers, used in printing and for $(Fun.arity) function *)
val fun_arity : keyword_param_value list -> param list -> arity
val curry_fun_arity : param_value list -> keyword_param_value list -> param list -> keyword_value list -> arity

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
