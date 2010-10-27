(*
 * Architecture-independent process control.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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

(*
 * Process and group identifiers.
 *)
type pgrp = int
type pid = int

(*
 * Argument records for thread and process creation.
 *)
type create_thread =
   { create_thread_stdin      : Unix.file_descr;
     create_thread_stdout     : Unix.file_descr;
     create_thread_stderr     : Unix.file_descr;
     create_thread_pgrp       : pgrp;
     create_thread_fun        : (Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> pgrp -> int);
     create_thread_background : bool
   }

type create_process =
   { create_process_stdin      : Unix.file_descr;
     create_process_stdout     : Unix.file_descr;
     create_process_stderr     : Unix.file_descr;
     create_process_pgrp       : pgrp;
     create_process_dir        : string;
     create_process_env        : string array;
     create_process_exe        : string;
     create_process_argv       : string array;
     create_process_background : bool
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
