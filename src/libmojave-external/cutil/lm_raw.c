/*
 * Raw data operations on strings.
 *
 * ------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 */

/* Standard includes */
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <sys/types.h>

/* Caml includes */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef WIN32
	typedef short int16_t;
#endif

/*
 * Load numbers from a string.
 */
value load_int8(value v_string, value v_off)
{
    CAMLparam2(v_string, v_off);
    int off, len;
    char *str;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);

    /* Check bounds */
    if(off < 0 || off > len - 1)
        failwith("load_int8");

    /* Get the number */
    CAMLreturn(Val_int(str[off]));
}

value load_int16(value v_string, value v_off)
{
    CAMLparam2(v_string, v_off);
    int off, len;
    char *str;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);

    /* Check bounds */
    if(off < 0 || off > len - 2 || off & 1)
        failwith("load_int16");

    /* Get the number */
    CAMLreturn(Val_int(*(int16_t *)(str + off)));
}

value load_int32(value v_string, value v_off)
{
    CAMLparam2(v_string, v_off);
    CAMLlocal1(result);
    int off, len;
    char *str;
    int32 i;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);

    /* Check bounds */
    if(off < 0 || off > len - 4 || off & 3)
        failwith("load_int32");

    /* Get the number */
    i = *(int32 *)(str + off);
    result = copy_int32(i);
    CAMLreturn(result);
}

value load_int64(value v_string, value v_off)
{
    CAMLparam2(v_string, v_off);
    CAMLlocal1(result);
    int off, len;
    char *str;
    int64 i;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);

    /* Check bounds */
    if(off < 0 || off > len - 8 || off & 3)
        failwith("load_int64");

    /* Get the number */
    i = *(int64 *)(str + off);
    result = copy_int64(i);
    CAMLreturn(result);
}

value load_float(value v_string, value v_off)
{
    CAMLparam2(v_string, v_off);
    CAMLlocal1(result);
    int off, len;
    char *str;
    double x;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);

    /* Check bounds */
    if(off < 0 || off > len - 8 || off & 7)
        failwith("load_float");

    /* Get the number */
    x = *(double *)(str + off);
    result = copy_double(x);
    CAMLreturn(result);
}

/*
 * Store numbers to a string.
 */
value store_int8(value v_string, value v_off, value v_val)
{
    CAMLparam3(v_string, v_off, v_val);
    int off, len, i;
    char *str;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);
    i = Int_val(v_val);

    /* Check bounds */
    if(off < 0 || off > len - 1)
        failwith("store_int8");

    /* Get the number */
    str[off] = i;
    CAMLreturn(Val_unit);
}

value store_int16(value v_string, value v_off, value v_val)
{
    CAMLparam3(v_string, v_off, v_val);
    int off, len, i;
    char *str;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);
    i = Int_val(v_val);

    /* Check bounds */
    if(off < 0 || off > len - 2 || off & 1)
        failwith("store_int16");

    /* Get the number */
    *(int16_t *)(str + off) = i;
    CAMLreturn(Val_unit);
}

value store_int32(value v_string, value v_off, value v_val)
{
    CAMLparam3(v_string, v_off, v_val);
    CAMLlocal1(result);
    int off, len;
    char *str;
    int32 i;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);
    i = Int32_val(v_val);

    /* Check bounds */
    if(off < 0 || off > len - 4 || off & 3)
        failwith("store_int32");

    /* Get the number */
    *(int32 *)(str + off) = i;
    CAMLreturn(Val_unit);
}

value store_int64(value v_string, value v_off, value v_val)
{
    CAMLparam3(v_string, v_off, v_val);
    CAMLlocal1(result);
    int off, len;
    char *str;
    int64 i;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);
    i = Int64_val(v_val);

    /* Check bounds */
    if(off < 0 || off > len - 8 || off & 3)
        failwith("store_int64");

    /* Get the number */
    *(int64 *)(str + off) = i;
    CAMLreturn(Val_unit);
}

value store_float(value v_string, value v_off, value v_val)
{
    CAMLparam2(v_string, v_off);
    CAMLlocal1(result);
    int off, len;
    char *str;
    double x;

    /* Get arguments */
    str = String_val(v_string);
    len = string_length(v_string);
    off = Int_val(v_off);
    x = Double_val(v_val);

    /* Check bounds */
    if(off < 0 || off > len - 8 || off & 7)
        failwith("store_float");

    /* Get the number */
    *(double *)(str + off) = x;
    CAMLreturn(Val_unit);
}



