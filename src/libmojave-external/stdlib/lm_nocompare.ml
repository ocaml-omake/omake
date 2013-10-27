(*
 * A special "custom" value that prohibits Pervasives.compare,
 * but allows marshalling. By temporarily adding this value as a
 * _first_ field in a * data structure that is not supposed to be
 * compared using Pervasives.compare, one can weed out all the
 * inappropriate usages of Pervasives.compare, (=), (<), etc.
 *
 * ------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005, MetaPRL Group
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
 * Author: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
type t

external lm_nocompare_init  : unit -> unit = "lm_nocompare_init"
external lm_nocompare_create : unit -> t = "lm_nocompare_create"
external lm_nomarshal_create : unit -> t = "lm_nomarshal_create"

let nocompare = lm_nocompare_create ()
let nomarshal = lm_nomarshal_create ()
let it = nocompare

