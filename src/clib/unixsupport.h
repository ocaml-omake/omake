/*
 * Hacking into OCaml.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
#ifndef _UNIXSUPPORT_H
#define _UNIXSUPPORT_H

#ifdef WIN32

/*
 * HACK: this gets the handle from a file_descr.
 * This depends on the OCaml implementation, but
 * it is unlikely to change.
 */
struct filedescr {
    union {
        HANDLE handle;
        SOCKET socket;
    } fd;
    enum { KIND_HANDLE, KIND_SOCKET } kind;
    int crt_fd;
};

#define Handle_val(v)           (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v)           (((struct filedescr *) Data_custom_val(v))->fd.socket)
#define Descr_kind_val(v)       (((struct filedescr *) Data_custom_val(v))->kind)

#else /* !WIN32 */

#define Socket_val(v)           (Int_val(v))

#endif /* !WIN32 */

#endif /* _UNIX_SUPPORT_H */
