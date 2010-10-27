(*
 * Some type definitions for the builtins.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_location
open Lm_symbol

open Omake_ir
open Omake_env
open Omake_pos
open Omake_exec
open Omake_value_type
open Omake_build_type

type builtin_fun = venv -> pos -> loc -> value list -> value
type builtin_kfun = venv -> pos -> loc -> value list -> keyword_value list -> venv * value
type builtin_env_fun = env -> builtin_fun
type builtin_object_info = string * var * value
type builtin_rule = bool * string list * string list

type builtin_info =
   { builtin_vars       : (string * (venv -> value)) list;
     builtin_funs       : (bool * string * builtin_fun * arity) list;
     builtin_kfuns      : (bool * string * builtin_kfun * arity) list;
     builtin_objects    : builtin_object_info list;
     pervasives_objects : string list;
     phony_targets      : string list;
     builtin_rules      : builtin_rule list
   }

let builtin_empty =
   { builtin_vars       = [];
     builtin_funs       = [];
     builtin_kfuns      = [];
     builtin_objects    = [];
     pervasives_objects = [];
     phony_targets      = [];
     builtin_rules      = []
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
