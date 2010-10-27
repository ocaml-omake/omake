(*
 * Print the IR.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology and
 * HRL Laboratories, LLC
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf

open Omake_ir

val pp_print_var_scope       : formatter -> var_scope -> unit
val pp_print_var_info        : formatter -> var_info -> unit
val pp_print_arity           : formatter -> arity -> unit
val pp_print_string_exp      : formatter -> string_exp -> unit
val pp_print_string_exp_list : formatter -> string_exp list -> unit
val pp_print_exp             : formatter -> exp -> unit
val pp_print_exp_list        : formatter -> exp list -> unit
val pp_print_exp_list_simple : formatter -> exp list -> unit
val pp_print_export_info     : formatter -> export -> unit

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
