/*
 * Get terminal size
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
 * Authors: Jason Hickey <jyh@cs.caltech.edu>
 *          Aleksey Nogin <nogin@cs.cornell.edu>
 */
#include <stdio.h>
#if defined(__CYGWIN__) || defined(__svr4__)
#   include <sys/termios.h>
#endif
#ifdef WIN32
#   include <Windows.h>
#   pragma warning (disable: 4127 4189 4702)
#else
#   include <unistd.h>
#   include <sys/ioctl.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value caml_term_size(value arg)
{
    CAMLparam1(arg);
    CAMLlocal1(buf);

    /* Return a pair of numbers */
    buf = caml_alloc_small(2, 0);

    /* Get the terminal size, return None on failure */
#ifdef WIN32
    {
        HANDLE fd = *(HANDLE *)Data_custom_val(arg);
        CONSOLE_SCREEN_BUFFER_INFO ConsoleInfo;
        if (! GetConsoleScreenBufferInfo(fd, &ConsoleInfo))
            caml_failwith("lm_termsize.c: caml_term_size: GetConsoleScreenBufferInfo failed");

        Field(buf, 0) = Val_int(ConsoleInfo.dwSize.Y);
        Field(buf, 1) = Val_int(ConsoleInfo.dwSize.X);
    }
#else /* WIN32 */
#ifdef TIOCGWINSZ
    {
        int fd = Int_val(arg);
        struct winsize ws;

        if(ioctl(fd, TIOCGWINSZ, &ws) < 0)
            caml_failwith("lm_termsize.c: caml_term_size: not a terminal");
    
        /* Return the pair of numbers */
        Field(buf, 0) = Val_int(ws.ws_row);
        Field(buf, 1) = Val_int(ws.ws_col);
    }
#else /* TIOCGWINSZ */
   /* Assume that the terminal is 80 by 25 */
   Field(buf, 0) = Val_int( 25 );
   Field(buf, 1) = Val_int( 80 );
#endif /* TIOCGWINSZ */
#endif /* WIN32 */

    CAMLreturn(buf);
}
