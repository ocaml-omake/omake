/*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave group, Caltech
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
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

#ifdef WIN32
#include <windows.h>
#include "unixsupport.h"

/*
 * Utilities for pipes, used by Omake_channel.
 * Returns true if the pipe has available input.
 */
value omake_shell_peek_pipe(value v_fd)
{
    HANDLE pipe;
    BOOL status;
    DWORD total;

    pipe = Handle_val(v_fd);
    status = PeekNamedPipe(pipe,
                           NULL,                // Buffer for output data
                           0,                   // Size of the buffer
                           NULL,                // Number of bytes read
                           &total,              // Total number of bytes available
                           NULL);               // Number of bytes in the next message
    if(status == 0)
        failwith("Not a pipe");
    return total ? Val_int(1) : Val_int(0);
}

/*
 * Figure out what kind of file descriptor this is.
 *    0: File
 *    1: Pipe
 *    2: Socket
 */
value omake_shell_pipe_kind(value v_fd)
{
    HANDLE pipe;
    BOOL status;

    if(Descr_kind_val(v_fd) == KIND_SOCKET)
        return Val_int(2);
    pipe = Handle_val(v_fd);
    status = GetNamedPipeInfo(pipe, NULL, NULL, NULL, NULL);
    return status ? Val_int(1) : Val_int(0);
}

#else /* WIN32 */

value omake_shell_peek_pipe(value v_fd)
{
    failwith("omake_shell_peek_pipe: not available on Unix systems");
    return Val_unit;
}

value omake_shell_pipe_kind(value v_fd)
{
    /* Always treat like sockets */
    return Val_int(2);
}

#endif /* WIN32 */
