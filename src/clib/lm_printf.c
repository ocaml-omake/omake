/*
 * These printers use printf, both to ensure compatibility
 * with libc, and to make our life easier.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef WIN32 
#include <windows.h>
/* Disable some of the warnings */
#pragma warning( disable : 4996)
#endif /* WIN32 */

/*
 * Some large buffer.
 */
#define BUFSIZE         (1 << 12)

/*
 * Print a char.
 */
value ml_print_char(value v_fmt, value v_char)
{
    char buffer[BUFSIZE];
    const char *fmt = String_val(v_fmt);
    char c = (char) Int_val(v_char);
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, c) < 0)
        failwith("ml_print_char");
#else
    if(sprintf(buffer, fmt, c) < 0)
        failwith("ml_print_char");
#endif
    return copy_string(buffer);
}

/*
 * Print an int.
 */
value ml_print_int(value v_fmt, value v_int)
{
    char buffer[BUFSIZE];
    const char *fmt = String_val(v_fmt);
    int i = Int_val(v_int);
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, i) < 0)
        failwith("ml_print_int");
#else
    if(sprintf(buffer, fmt, i) < 0)
        failwith("ml_print_int");
#endif
    return copy_string(buffer);
}


/*
 * Print an int.
 */
value ml_print_float(value v_fmt, value v_float)
{
    char buffer[BUFSIZE];
    const char *fmt = String_val(v_fmt);
    double x = Double_val(v_float);
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, x) < 0)
        failwith("ml_print_float");
#else
    if(sprintf(buffer, fmt, x) < 0)
        failwith("ml_print_float");
#endif
    return copy_string(buffer);
}

/*
 * Print a string.
 */
value ml_print_string(value v_fmt, value v_string)
{
    char buffer[BUFSIZE], *bufp;
    int len, size, code;
    const char *fmt, *s;
    value v_result;

    /* Degenerate case if the format is %s */
    fmt = String_val(v_fmt);
    if(strcmp(fmt, "%s") == 0)
        return v_string;

    /* Make an attempt to ensure that the buffer is large enough */
    s = String_val(v_string);
    len = strlen(s);
    if(len < BUFSIZE) {
        size = BUFSIZE;
        bufp = buffer;
    }
    else {
        size = len * 2;
        bufp = malloc(size);
        if(bufp == 0)
            failwith("ml_print_string");
    }

#ifdef HAVE_SNPRINTF
    code = snprintf(bufp, size, fmt, s);
#else
    code = sprintf(bufp, fmt, s);
#endif
    if(code < 0) {
        if(bufp != buffer)
            free(bufp);
        failwith("ml_print_string");
    }
    v_result = copy_string(bufp);
    if(bufp != buffer)
        free(bufp);
    return v_result;
}

/*
 * Print a string.
 */
value ml_print_string2(value v_width, value v_fmt, value v_string)
{
    char buffer[BUFSIZE], *bufp;
    int width, len, size, code;
    const char *fmt, *s;
    value v_result;

    /* Degenerate case if the format is %s */
    fmt = String_val(v_fmt);
    if(strcmp(fmt, "%s") == 0)
        return v_string;

    /* Make an attempt to ensure that the buffer is large enough */
    s = String_val(v_string);
    len = strlen(s);
    width = Int_val(v_width);
    if(width > len)
        len = width;
    if(len < BUFSIZE / 2) {
        size = BUFSIZE;
        bufp = buffer;
    }
    else {
        size = len * 2;
        bufp = malloc(size);
        if(bufp == 0)
            failwith("ml_print_string");
    }

#ifdef HAVE_SNPRINTF
    code = snprintf(bufp, size, fmt, s);
#else
    code = sprintf(bufp, fmt, s);
#endif
    if(code < 0) {
        if(bufp != buffer)
            free(bufp);
        failwith("ml_print_string");
    }
    v_result = copy_string(bufp);
    if(bufp != buffer)
        free(bufp);
    return v_result;
}

