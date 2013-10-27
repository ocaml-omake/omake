/*
 * Detect the case-sensitivity of a filesystem, when possible
 *
 * ------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2007 Mojave Group, Caltech
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
 * Author: Nathaniel Gray
 * @email{n8gray@cs.caltech.edu}
 * @end[license]
 */

/* For Caml */
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/memory.h>

#ifdef DETECT_FS_CASE_SENSITIVE_GETATTRLIST
/* This is the OS X implementation, using getattrlist. */

/* For statfs */
#include <sys/param.h>
#include <sys/mount.h>
/* For getattrlist */
#include <sys/attr.h>
#include <unistd.h>
/* Other various includes */
#include <memory.h>
#include <errno.h>

typedef struct vol_caps_buf {
    unsigned long size;
    vol_capabilities_attr_t caps;
} vol_caps_buf_t;

value lm_fs_case_sensitive_available(value _unit) {
    return Val_true;
}

/* 
 * Returns true if the volume containing the path is case-sensitive,
 * or false if it is not.  
 * Raises Failure if the case-sensitivity cannot be detected.
 */
value lm_fs_case_sensitive(value path_val) {
    CAMLparam1(path_val);
    struct statfs stat;
    char *path = String_val(path_val);
    
    do {
        caml_enter_blocking_section();
        if (statfs(path, &stat))
            break;
        
        struct attrlist alist;
        memset(&alist, 0, sizeof(alist));
        alist.bitmapcount = ATTR_BIT_MAP_COUNT;
        alist.volattr = ATTR_VOL_CAPABILITIES;
        vol_caps_buf_t buffer;
        
        if (getattrlist(stat.f_mntonname, &alist, &buffer, sizeof(buffer), 0))
            break;
        
        caml_leave_blocking_section();
        if (!(alist.volattr & ATTR_VOL_CAPABILITIES))
            caml_failwith("Couldn't get volume capabilities");
        
        if (!(buffer.caps.valid[VOL_CAPABILITIES_FORMAT] 
                    & VOL_CAP_FMT_CASE_SENSITIVE))
            caml_failwith("VOL_CAP_FMT_CASE_SENSITIVE not valid on this volume");
        
        if (buffer.caps.capabilities[VOL_CAPABILITIES_FORMAT] 
                    & VOL_CAP_FMT_CASE_SENSITIVE) {
            CAMLreturn(Val_true);
        } else {
            CAMLreturn(Val_false);
        }
        
    } while (0);
    caml_leave_blocking_section();
    caml_failwith(strerror(errno));
    CAMLreturn(Val_false);  /* For the compiler's sake */
}

#else /* not DETECT_FS_CASE_SENSITIVE_GETATTRLIST */

#ifdef _WIN32
#pragma warning( disable : 4100 )
#endif /* _WIN32 */

value lm_fs_case_sensitive_available(value _unit) {
    return Val_false;
}

value lm_fs_case_sensitive(value path_val) {
    caml_failwith("lm_fs_case_sensitive not supported");
    return Val_false;  /* For the compiler's sake */
}

#endif /* DETECT_FS_CASE_SENSITIVE_GETATTRLIST */
