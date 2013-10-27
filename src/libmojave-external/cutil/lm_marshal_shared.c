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

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <string.h>

/*
 * Write the contents of the block to the string.
 */
value ml_write_block(value obj, value buf, value v_start)
{
    int start = Int_val(v_start);
    int buf_len = string_length(buf);
    int obj_len = Bosize_val(obj);
    char *bufp = String_val(buf);

    /*
     * Check all the bounds.
     */
    if(start < 0 || start + obj_len > buf_len)
        invalid_argument("unsafe_write_block");

    /*
     * Copy the data as a byte stream.
     */
    memcpy(bufp + start, Bp_val(obj), obj_len);

    return Val_unit;
}

/*
 * These are not implemented yet.
 */
value ml_create_pointer_hash(value unit_val)
{
    return Val_unit;
}

value ml_pointer_hash_insert(value table, value v_index, value obj)
{
    return Val_unit;
}

value ml_pointer_hash_lookup(value table, value weak_array, value obj)
{
    return Val_int(0);
}

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
 * Revision 1.3  2002/05/22 17:45:49  justins
 * Fixing numerous compiler warnings in clib.  Most of these have to do with
 * the #pragma directives that were obsoleted; I checked with Jason and he said
 * it was okay to remove them.  The rest are adding header files and function
 * prototypes for missing symbols, and working around a few uninitialized var
 * warnings.  This should not change the semantics of anything in clib.
 *
 * Revision 1.2  2001/05/14 20:53:50  nogin
 * Cosmetic changes to avoid warnings with newer versions of GCC.
 *
 * Revision 1.1  1999/05/03 12:11:20  jyh
 * Added an initial incomplete version of the distributed marshaler.
 *
 */
