/*
   Floating-point high precision arithmetic
   Copyright (C) 2001 Justin David Smith, Caltech

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation,
   version 2.1 of the License.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
   
   Additional permission is given to link this library with the
   OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
   and you may distribute the linked executables.  See the file
   LICENSE.libmojave for more details.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <memory.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef WIN32
#  define INLINE
#else
#  define INLINE inline
#endif


/***  Low-level calls  ***/


/* Extract a float out of a block, and vice versa */
static INLINE long double load_float80(value f) {

   long double val;
   
   /* Get the value from the block */
   val = *(long double *)String_val(f);
   return(val);

}


static INLINE void store_float80(value f, long double val) {

   long double *buffer;
   
   /* Store the value into the block */
   buffer = (long double *)String_val(f);
   *buffer = val;

}


static INLINE value copy_float80(long double val) {

   value block;
   
   /* Allocate a string block to store the value */
   block = alloc_string(sizeof(long double));
   store_float80(block, val);
   return(block);

}


/***  Basic Coercions  ***/


/* Output the floating-point value as a string */
value string_of_float80(value f) {

   CAMLparam1(f);
   CAMLlocal1(block);
   char buffer[0x100];
   long double val;
   
   /* Print the floating-point value to a buffer */
   val = load_float80(f);
#ifdef HAVE_SNPRINTF
   snprintf(buffer, sizeof(buffer), "%Lg", val);
#else
   sprintf(buffer, "%Lg", val);
   buffer[sizeof(buffer) - 1] = '\0';
#endif
   
   /* Allocate a new string block to contain result */
   block = copy_string(buffer);
   CAMLreturn(block);

}


/* Coerces a long-double value into an ordinary float */
value float_of_float80(value f) {

   CAMLparam1(f);
   CAMLlocal1(block);
   double val;
   
   /* Load the value and create a new (standard double) block for it */
   val = load_float80(f);
   block = copy_double(val);
   CAMLreturn(block);

}


/* Coerces a long-double value into an int value */
value int_of_float80(value f) {

   CAMLparam1(f);
   int val;
   
   /* Coerce the value to an integer and ML-ize it */
   val = load_float80(f);
   CAMLreturn(Int_val(val));

}


/* Creates a float80 value from a string value. */
value float80_of_string(value s) {

   CAMLparam1(s);
   char *buffer;
   long double val;
   
   /* Load the string value. */
   buffer = String_val(s);
#if 0
   /* BUG: Looks like strtold does not work: glibc-2.2.2, Aug 31, 2001 --jyh */
   val = strtold(buffer, NULL);
#else
   val = strtod(buffer, NULL);
#endif
   
   /* Create a new float80 block */
   CAMLreturn(copy_float80(val));
   
}


/* Creates a float80 from a normal float value. */
value float80_of_float(value f) {

   CAMLparam1(f);
   long double val;
   
   /* Load the float, and create new float80 block */
   val = Double_val(f);
   CAMLreturn(copy_float80(val));
   
}


/* Creates a float80 from an ML int value. */
value float80_of_int(value i) {

   CAMLparam1(i);
   long double val;
   
   /* Load the ML int, and create new float80 block */
   val = Int_val(i);
   CAMLreturn(copy_float80(val));
   
}


/* Formats a floating-point value into a string. */
value float80_format(value fmt, value f) {
   
   CAMLparam2(fmt, f);
   CAMLlocal1(block);
   char buffer[0x100];
   long double val;
   
   /* Print the floating-point value to a buffer */
   val = load_float80(f);
#ifdef HAVE_SNPRINTF
   snprintf(buffer, sizeof(buffer), String_val(fmt), val);
#else
   sprintf(buffer, String_val(fmt), val);
   buffer[sizeof(buffer) - 1] = '\0';
#endif
   
   /* Allocate a new string block to contain result */
   block = copy_string(buffer);
   CAMLreturn(block);

}


/***  Arithmetic  ***/


/* Negation */
value float80_neg(value f) {
   
   CAMLparam1(f);
   long double val;
   
   val = -load_float80(f);
   CAMLreturn(copy_float80(val));

}


/* Absolute value */
value float80_abs(value f) {
   
   CAMLparam1(f);
   long double val;
   
   val = load_float80(f);
   if(val < 0) val = -val;
   CAMLreturn(copy_float80(val));

}


/* Addition */
value float80_add(value f1, value f2) {
   
   CAMLparam2(f1, f2);
   long double val;
   
   val = load_float80(f1) + load_float80(f2);
   CAMLreturn(copy_float80(val));

}


/* Subtraction */
value float80_sub(value f1, value f2) {
   
   CAMLparam2(f1, f2);
   long double val;
   
   val = load_float80(f1) - load_float80(f2);
   CAMLreturn(copy_float80(val));

}


/* Multiplication */
value float80_mul(value f1, value f2) {
   
   CAMLparam2(f1, f2);
   long double val;
   
   val = load_float80(f1) * load_float80(f2);
   CAMLreturn(copy_float80(val));

}


/* Division */
value float80_div(value f1, value f2) {
   
   CAMLparam2(f1, f2);
   long double val;
   
   val = load_float80(f1) / load_float80(f2);
   CAMLreturn(copy_float80(val));

}


/* Comparison operator */
value float80_compare(value f1, value f2) {
   
   CAMLparam2(f1, f2);
   CAMLlocal1(result);
   long double val1;
   long double val2;
   
   val1 = load_float80(f1);
   val2 = load_float80(f2);
   if(val1 == val2) result = Val_int(0);
   else if(val1 > val2) result = Val_int(1);
   else result = Val_int(-1);
   CAMLreturn(result);

}


/* sin/cos/sqrt */
value float80_sin(value f) {
   
   CAMLparam1(f);
   long double val;
   
   val = sin(load_float80(f));
   CAMLreturn(copy_float80(val));
}

value float80_cos(value f) {
   
   CAMLparam1(f);
   long double val;
   
   val = cos(load_float80(f));
   CAMLreturn(copy_float80(val));
}

value float80_sqrt(value f) {
   
   CAMLparam1(f);
   long double val;
   
   val = sqrt(load_float80(f));
   CAMLreturn(copy_float80(val));
}

value float80_atan2(value f1, value f2) {
   
   CAMLparam2(f1, f2);
   long double val;
   
   val = atan2(load_float80(f1), load_float80(f2));
   CAMLreturn(copy_float80(val));
}

/*
 * BUG: these are hopefullu temporary.
 * Use them for assembling floats.
 * Error handling is performed at the C level.
 */
value c_blit_float32(value x_val, value buf_val, value off_val)
{
    char *buf;
    float x;
    int off;

    x = load_float80(x_val);
    buf = String_val(buf_val);
    off = Int_val(off_val);
    assert(sizeof(x) == 4);
    memcpy(buf + off, &x, 4);
    return Val_unit;
}

value c_blit_float64(value x_val, value buf_val, value off_val)
{
    char *buf;
    double x;
    int off;

    x = load_float80(x_val);
    buf = String_val(buf_val);
    off = Int_val(off_val);
    assert(sizeof(x) == 8);
    memcpy(buf + off, &x, 8);
    return Val_unit;
}

value c_blit_float80(value x_val, value buf_val, value off_val)
{
    char *buf;
    long double x;
    int off;

    x = load_float80(x_val);
    buf = String_val(buf_val);
    off = Int_val(off_val);
    /* TEMP: sizeof returns the aligned type, perhaps? */
    assert(sizeof(x) >= 10);
    memcpy(buf + off, &x, 10);
    return Val_unit;
}
