/*
 * This file contains helper functions for the marshal module.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1999 PRL Group, Cornell University and Caltech
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
 * jyh@cs.cornell.edu
 */

#ifdef __GNUC__
#pragma implementation
#endif __GNUC__

#include <caml/memory.h>
#include <caml/mlvalues.h>

/*
 * Write the contents of the block to the string.
 */
value ml_write_block(value buf, value v_start, value v_stop, value obj);

/*
 *
 *
 * $Log$
 * Revision 1.2  2005/07/07 02:34:06  nogin
 * More preamble changes
 *
 * Revision 1.1  2003/07/12 21:59:10  jyh
 * Migrated more code into libmojave.
 *
 * Revision 1.1  1999/05/03 12:11:20  jyh
 * Added an initial incomplete version of the distributed marshaler.
 *
 */
