/*
 * Get characters from the current locale.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2006 Mojave group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 */
#include <memory.h>
#include <ctype.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

#ifdef WIN32 
#include <windows.h>
/* Disable some of the warnings */
#pragma warning( disable : 4100 )
#endif /* WIN32 */

/*
 * Character classes.
 */
static value get_chars(int (*f)(int))
{
    char buf[256];
    value s;
    char *p;
    int i;

    p = buf;
    for(i = 0; i != 256; i++) {
        if(f(i))
            *p++ = (char) i;
    }
    s = alloc_string(p - buf);
    memcpy(String_val(s), buf, p - buf);
    return s;
}

value omake_alnum(value v_unit)
{
    return get_chars(isalnum);
}

value omake_alpha(value v_unit)
{
    return get_chars(isalpha);
}

value omake_graph(value v_unit)
{
    return get_chars(isgraph);
}

value omake_lower(value v_unit)
{
    return get_chars(islower);
}

value omake_upper(value v_unit)
{
    return get_chars(isupper);
}

value omake_punct(value v_unit)
{
    return get_chars(ispunct);
}

value omake_space(value v_unit)
{
    return get_chars(isspace);
}
