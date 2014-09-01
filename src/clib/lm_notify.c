/*
 * File-change notification.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
#include <caml/signals.h>

#ifdef FAM_ENABLED

#ifdef WIN32 
#include <windows.h>
/* Disable some of the warnings */
#pragma warning( disable : 4100 4189 4127 4702 4996 )
#endif /* WIN32 */

#ifdef FAM_PSEUDO
#include "fam_pseudo.h"
#else /* FAM_PSEUDO */
#include <fam.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/select.h>
#endif /* FAM_PSEUDO */

/*
 * Custom blocks.
 */
typedef struct {
    FAMConnection *fc;
    int is_open;
} FAMInfo;

#define FAMInfo_val(v)         ((FAMInfo *) Data_custom_val(v))
#define FAMConnection_val(v)   ((FAMInfo_val(v)->fc))

#ifdef HAVE_SNPRINTF
#define ErrFmt(buffer, name) snprintf(buffer, sizeof(buffer), "%s: %s", name, FamErrlist[FAMErrno])
#else
#define ErrFmt(buffer, name) sprintf(buffer, "%s: %s", name, FamErrlist[FAMErrno])
#endif

#define CheckCode(fmt, expr)                 \
  do {                                       \
     enter_blocking_section();               \
     code = expr;                            \
     leave_blocking_section();               \
     if(code < 0) {                          \
         char buffer[256];                   \
         ErrFmt(buffer, fmt);                \
         failwith(buffer);                   \
     }                                       \
  } while(0)

static int fam_connection_compare(value v1, value v2)
{
    FAMConnection *info1 = FAMConnection_val(v1);
    FAMConnection *info2 = FAMConnection_val(v2);

#ifdef FAM_PSEUDO
    return info1->id == info2->id ? 0 : info1->id < info2->id ? -1 : 1;
#else /* FAM_PSEUDO */
    return info1->fd == info2->fd ? 0 : info1->fd < info2->fd ? -1 : 1;
#endif /* !FAM_PSEUDO */
}

static long fam_connection_hash(value v)
{
    return (long) FAMConnection_val(v);
}

static void fam_connection_finalize(value v_info)
{
    FAMInfo *info;

    info = FAMInfo_val(v_info);
    if(info->is_open) {
        int code;
        CheckCode("om_notify_close", FAMClose(info->fc));
        free(info->fc);
        info->is_open = 0;
    }
}

/*
 * Pass info in a custom block.
 */
static struct custom_operations fam_connection_ops = {
    "fam_connection",
    fam_connection_finalize,
    fam_connection_compare,
    fam_connection_hash,
    custom_serialize_default,
    custom_deserialize_default
};

/*
 * Is this module enabled?
 */
value om_notify_enabled(value v_unit)
{
    return Val_true;
}

/*
 * Open the FAM connection.
 */
value om_notify_open(value v_unit)
{
    CAMLparam1(v_unit);
    CAMLlocal1(v);
    FAMConnection *fc;
    FAMInfo *info;
    int code;

    v = alloc_custom(&fam_connection_ops, sizeof(FAMInfo), 0, 1);
    info = FAMInfo_val(v);
    fc = malloc(sizeof(FAMConnection));
    if(fc == 0)
        invalid_argument("om_notify_open: out of memory");
    info->fc = fc;
    CheckCode("om_notify_open", FAMOpen(fc));
#ifdef HAVE_FAMNOEXISTS
    CheckCode("om_notify_open: FAMNoExists", FAMNoExists(fc));
#endif /* HAVE_FAMNOEXISTS */
    info->is_open = 1;
    CAMLreturn(v);
}

/*
 * Close the FAM connection.
 */
value om_notify_close(value v_fc)
{
    fam_connection_finalize(v_fc);
    return Val_unit;
}

/*
 * Get the file descriptor.
 */
value om_notify_fd(value v_fc)
{
#ifdef FAM_PSEUDO
#ifdef FAM_INOTIFY
    FAMConnection *fc;

    fc = FAMConnection_val(v_fc);
    return Val_int(fc->id);
#else /* FAM_PSEUDO && !FAM_INOTIFY */
    failwith("No file descriptors in pseudo-FAM");
    return Val_unit;
#endif /* FAM_INOTIFY */
#else /* FAM_PSEUDO */
    FAMConnection *fc;

    fc = FAMConnection_val(v_fc);
    return Val_int(FAMCONNECTION_GETFD(fc));
#endif /* FAM_PSEUDO */
}

/*
 * Monitor a directory.
 */
value om_notify_monitor_directory(value v_fc, value v_name, value v_recursive)
{
    CAMLparam3(v_fc, v_name, v_recursive);
    const char *name;
    FAMConnection *fc;
    FAMRequest request;
    int code, recursive;

    fc = FAMConnection_val(v_fc);
    name = String_val(v_name);
    recursive = Int_val(v_recursive);
    if(recursive) {
#ifdef WIN32
        CheckCode("om_notify_monitor_directory", FAMMonitorDirectoryTree(fc, name, &request, 0));
#else /* WIN32 */
        failwith("om_notify_monitor_directory: recursive monitoring is not allowed");
#endif /* !WIN32 */
    }
    else
        CheckCode("om_notify_monitor_directory", FAMMonitorDirectory(fc, name, &request, 0));
    CAMLreturn(Val_int(request.reqnum));
}

/*
 * Suspend the monitor.
 */
value om_notify_suspend(value v_fc, value v_request)
{
    CAMLparam2(v_fc, v_request);
    FAMConnection *fc;
    FAMRequest request;
    int code;

    fc = FAMConnection_val(v_fc);
    request.reqnum = Int_val(v_request);
    CheckCode("om_notify_suspend", FAMSuspendMonitor(fc, &request));
    CAMLreturn(Val_unit);
}

/*
 * Resume the monitor.
 */
value om_notify_resume(value v_fc, value v_request)
{
    CAMLparam2(v_fc, v_request);
    FAMConnection *fc;
    FAMRequest request;
    int code;

    fc = FAMConnection_val(v_fc);
    request.reqnum = Int_val(v_request);
    CheckCode("om_notify_resume", FAMResumeMonitor(fc, &request));
    CAMLreturn(Val_unit);
}

/*
 * Cancel the monitor.
 */
value om_notify_cancel(value v_fc, value v_request)
{
    CAMLparam2(v_fc, v_request);
    FAMConnection *fc;
    FAMRequest request;
    int code;

    fc = FAMConnection_val(v_fc);
    request.reqnum = Int_val(v_request);
    CheckCode("om_notify_cancel", FAMCancelMonitor(fc, &request));
    CAMLreturn(Val_unit);
}

/*
 * Check for a pending event.
 */
value om_notify_pending(value v_fc)
{
    CAMLparam1(v_fc);
    FAMConnection *fc;
    int code;

    fc = FAMConnection_val(v_fc);
    CheckCode("om_notify_pending", FAMPending(fc));
    CAMLreturn(code ? Val_true : Val_false);
}

/*
 * Get the next event.
 */
value om_notify_next_event(value v_fc)
{
    CAMLparam1(v_fc);
    CAMLlocal2(v_name, v_tuple);
    FAMConnection *fc;
    FAMEvent event;
    int code;

    fc = FAMConnection_val(v_fc);
    CheckCode("om_notify_next_event", FAMNextEvent(fc, &event));
    code = event.code;
    if(code < 1 || code > 10)
        failwith("om_notify_next_event: code out of bounds");

    /* Allocate the string name */
    v_name = copy_string(event.filename);

    /* Allocate the tuple */
    v_tuple = alloc_tuple(3);
    Field(v_tuple, 0) = Val_int(event.fr.reqnum);
    Field(v_tuple, 1) = v_name;
    Field(v_tuple, 2) = Val_int(code - 1);
    CAMLreturn(v_tuple);
}

#else /* FAM_ENABLED */

/*
 * Is this module enabled?
 */
value om_notify_enabled(value v_unit)
{
    return Val_false;
}

/*
 * Open the FAM connection.
 */
value om_notify_open(value v_unit)
{
    return Val_unit;
}

/*
 * Get the file descriptor.
 */
value om_notify_fd(value v_fc)
{
    invalid_argument("FAM not enabled");
    return Val_unit;
}

/*
 * Close the FAM connection.
 */
value om_notify_close(value v_fc)
{
    return Val_unit;
}

/*
 * Monitor a directory.
 */
value om_notify_monitor_directory(value v_fc, value v_name, value v_recursive)
{
    return Val_int(0);
}

/*
 * Suspend the monitor.
 */
value om_notify_suspend(value v_fc, value v_request)
{
    return Val_unit;
}


/*
 * Suspend the monitor.
 */
value om_notify_resume(value v_fc, value v_request)
{
    return Val_unit;
}


/*
 * Suspend the monitor.
 */
value om_notify_cancel(value v_fc, value v_request)
{
    return Val_unit;
}

/*
 * Check for a pending event.
 */
value om_notify_pending(value v_fc)
{
    return Val_false;
}

/*
 * Get the next event.
 */
value om_notify_next_event(value v_fc)
{
    invalid_argument("FAM not enabled");
    return Val_unit;
}

#endif /* !FAM_ENABLED */

/*
 * vim:tw=100:ts=4:et:sw=4:cin
 */
