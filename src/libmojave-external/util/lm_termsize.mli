(*
 * Extra terminal commands.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Unix

val term_size : file_descr -> int * int
val min_screen_width : int ref
val term_width : out_channel -> int -> int

val stdout_width : int
val stderr_width : int

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
