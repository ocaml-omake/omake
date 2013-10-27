/*
 * System info.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 Mojave Group, Caltech
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
#include <caml/signals.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

#if defined(WIN32) || defined(_WIN32)
/* Disable some of the warnings */
#pragma warning( disable : 4100 4201 4127 4189 4702 4716 4996 )
#endif

/*
 * Lock codes.
 */
#define LM_LOCK_UN      0
#define LM_LOCK_SH      1
#define LM_LOCK_EX      2
#define LM_LOCK_TSH     3
#define LM_LOCK_TEX     5

#define FLOCK_LEN       ((unsigned int) ~0 >> 2)

/*
 * Print the stack pointer for debugging.
 */
value lm_print_stack_pointer(value v_arg)
{
    int sp;

    fprintf(stderr, "Stack pointer: 0x%08lx\n", (unsigned long) &sp);
    return Val_unit;
}

#ifdef WIN32
#include <windows.h>
#include <winerror.h>
#include <shlobj.h>
#include "lm_compat_win32.h"

/*
 * File descriptor.
 */
value int_of_fd(value fd)
{
    return Val_long((long) *(HANDLE *)Data_custom_val(fd));
}

/*
 * Home directory on Win32.
 */
value home_win32(value v_unit)
{
    CAMLparam1(v_unit);
    TCHAR path[MAX_PATH];

    if(SUCCEEDED(CompatSHGetFolderPath(NULL, CSIDL_LOCAL_APPDATA | CSIDL_FLAG_CREATE, NULL, 0, path)))
        CAMLreturn(copy_string(path));

    failwith("home_win32");
    return Val_unit;
}

/*
 * File locking.
 */
#define F_ULOCK         0
#define F_LOCK          1
#define F_TLOCK         2
#define F_TEST          3
#define F_RLOCK         4
#define F_TRLOCK        5

value lockf_win32(value v_fd, value v_kind, value v_len)
{
    HANDLE fd = *(HANDLE *)Data_custom_val(v_fd);
    int kind = Int_val(v_kind);
    int len = Int_val(v_len);
    OVERLAPPED overlapped;
    int code, flags = 0;
    DWORD pos, error = 0;

    /* Get the current position in the file */
    pos = SetFilePointer(fd, 0, 0, FILE_CURRENT);

    /* XXX: HACK: we should probably compute this correctly */
    if(len == 0)
        len = 1;

    /* Unlock case */
    if(kind == F_ULOCK)
        UnlockFile(fd, pos, 0, len, 0);
    else {
        /* Some kind of locking operation */
        switch(kind) {
        case F_LOCK:
            flags = LOCKFILE_EXCLUSIVE_LOCK;
            break;
        case F_TLOCK:
            flags = LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY;
            break;
        case F_RLOCK:
            flags = 0;
            break;
        case F_TRLOCK:
            flags = LOCKFILE_FAIL_IMMEDIATELY;
            break;
        default:
            invalid_argument("lockf_win32");
            break;
        }

        /* Set the offset */
        memset(&overlapped, 0, sizeof(overlapped));
        overlapped.Offset = pos;

        /* Perform the lock */
        enter_blocking_section();
        code = LockFileEx(fd, flags, 0, len, 0, &overlapped);
        if(code == 0)
            error = GetLastError();
        leave_blocking_section();

        /* Fail if the lock was not successful */
        if(code == 0) {
            char szBuf[1024];
            LPVOID lpMsgBuf;

            switch(error) {
            case ERROR_LOCK_FAILED:
            case ERROR_LOCK_VIOLATION:
                /*
                 * XXX: HACK: this exception is being caught
                 *            Do not change the string w/o changing the wrapper code.
                 */
                failwith("lockf_win32: already locked");
                break;
            case ERROR_POSSIBLE_DEADLOCK:
                /*
                 * XXX: HACK: this exception is being caught
                 *            Do not change the string w/o changing the wrapper code.
                 */
                failwith("lockf_win32: possible deadlock");
                break;
            default:
                FormatMessage(
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
                    FORMAT_MESSAGE_FROM_SYSTEM,
                    NULL,
                    error,
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                    (LPTSTR) &lpMsgBuf,
                    0, NULL);

                sprintf(szBuf, "lockf_win32 failed with error %d: %s", error, lpMsgBuf); 
                LocalFree(lpMsgBuf);

                failwith(szBuf);
                break;
            }
        }
    }
    return Val_unit;
}

/*
 * Translate flock operators.
 */
static int lockf_of_flock[] = {
    F_ULOCK,
    F_RLOCK,
    F_LOCK,
    F_TRLOCK,
    F_TLOCK
};

/*
 * flock wrapper.
 */
value lm_flock(value v_fd, value v_op)
{
    value v_kind;

    v_kind = Val_int(lockf_of_flock[Int_val(v_op)]);
    return lockf_win32(v_fd, v_kind, Val_int(FLOCK_LEN));
}

/*
 * Truncate to the current position.
 */
value ftruncate_win32(value v_fd)
{
    HANDLE fd = *(HANDLE *)Data_custom_val(v_fd);
    SetEndOfFile(fd);
    return Val_unit;
}

/************************************************************************
 * Registry.
 */

/*
 * Get the value of a registry key.
 */
value caml_registry_find(value v_hkey, value v_subkey, value v_field)
{
    char buffer[8192];
    const char *subkey, *field;
    DWORD len;
    LONG code;
    HKEY hkey = 0;

    /* Get the arguments */
    switch(Int_val(v_hkey)) {
    case 0:
        hkey = HKEY_CLASSES_ROOT;
        break;
    case 1:
        hkey = HKEY_CURRENT_CONFIG;
        break;
    case 2:
        hkey = HKEY_CURRENT_USER;
        break;
    case 3:
        hkey = HKEY_LOCAL_MACHINE;
        break;
    case 4:
        hkey = HKEY_USERS;
        break;
    default:
        caml_failwith("get_registry: unknown handle");
        break;
    }

    /* Ask Windows */
    subkey = String_val(v_subkey);
    field = String_val(v_field);
    len = sizeof(buffer);

#if 0
    code = RegGetValue(hkey, subkey, field, RRF_RT_REG_SZ, NULL, (LPVOID) buffer, &len);
    if(code != ERROR_SUCCESS)
        caml_raise_not_found();
#else
    {
        HKEY hand;

        code = RegOpenKeyEx(hkey, subkey, 0, KEY_QUERY_VALUE, &hand);
        if(code != ERROR_SUCCESS)
            caml_raise_not_found();

        code = RegQueryValueEx(hand, field, NULL, NULL, (LPBYTE) buffer, &len);
        RegCloseKey(hand);
        if(code != ERROR_SUCCESS)
            caml_raise_not_found();
    }
#endif

    /* Got the value */
    return copy_string(buffer);
}

#else /* WIN32 */
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/types.h>
#include <pwd.h>

value int_of_fd(value fd)
{
    return fd;
}

value home_win32(value v_unit)
{
    caml_failwith("home_win32: not to be used except on Win32");
    return Val_unit;
}

value lockf_win32(value v_fd, value v_kind, value v_len)
{
    caml_failwith("lockf_win32: not to be used except on Win32");
    return Val_unit;
}

value ftruncate_win32(value v_fd)
{
    caml_failwith("ftruncate_current_win32: not to be used except on Win32");
    return Val_unit;
}

value caml_registry_find(value v_key, value v_subkey, value v_field)
{
    caml_raise_not_found();
    return Val_unit;
}

/*
 * Translations.
 */
#if defined(LOCK_UN) && defined(LOCK_SH) && defined(LOCK_EX)
#define FLOCK_ENABLED
static int flock_of_flock[] = {
    LOCK_UN,
    LOCK_SH,
    LOCK_EX,
    LOCK_SH | LOCK_NB,
    LOCK_EX | LOCK_NB
};
#endif

#if defined(F_RDLCK) && defined(F_WRLCK) && defined(F_UNLCK) && defined(F_SETLK) && defined(F_SETLKW) && defined(SEEK_SET)
#define FCNTL_ENABLED
static int fcntl_type_of_flock[] = {
    F_UNLCK,
    F_RDLCK,
    F_WRLCK,
    F_RDLCK,
    F_WRLCK
};

static int fcntl_of_flock[] = {
    F_SETLKW,
    F_SETLKW,
    F_SETLKW,
    F_SETLK,
    F_SETLK
};
#endif

#if defined(F_ULOCK) && defined(F_LOCK) && defined(F_TLOCK)
#define LOCKF_ENABLED
static int lockf_of_flock[] = {
    F_ULOCK,
    F_LOCK,
    F_LOCK,
    F_TLOCK,
    F_TLOCK
};
#endif

value lm_flock(value v_fd, value v_op)
{
    int fd, op, cmd, code;

    fd = Int_val(v_fd);
    op = Int_val(v_op);
#if defined(FLOCK_ENABLED)
    cmd = flock_of_flock[op];
    enter_blocking_section();
    code = flock(fd, cmd);
    leave_blocking_section();
#elif defined(FCNTL_ENABLED)
    {
        struct flock info;
        cmd = fcntl_of_flock[op];
        info.l_type = fcntl_type_of_flock[op];
        info.l_whence = SEEK_SET;
        info.l_start = 0;
        info.l_len = FLOCK_LEN;
        enter_blocking_section();
        code = fcntl(fd, cmd, &info);
        leave_blocking_section();
    }
#elif defined(LOCKF_ENABLED)
    cmd = lockf_of_flock[op];
    caml_enter_blocking_section();
    code = lockf(fd, cmd, FLOCK_LEN);
    caml_leave_blocking_section();
#else
    code = -1;
#endif
    if(code < 0)
        caml_failwith("flock");
    return Val_unit;
}

#endif /* !WIN32 */

/************************************************************************
 * Password file (only on Unix).
 */
#ifdef WIN32

/*
 * The empty array.
 */
value lm_getpwents(value v_unit)
{
    return Val_emptylist;
}

#else /* !WIN32 */

/*
 * Scan the password file.
 *
type passwd_entry = {
  	pw_name : string;
  	pw_passwd : string;
  	pw_uid : int;
  	pw_gid : int;
  	pw_gecos : string;
  	pw_dir : string;
  	pw_shell : string;
}
 */
value lm_getpwents(value v_unit)
{
    CAMLparam1(v_unit);
    CAMLlocal3(users, entry, cons);
    struct passwd *entryp;

    /* Create a list of users */
    users = Val_emptylist;

    /* Scan the password file */
    setpwent();
    while((entryp = getpwent())) {
        entry = caml_alloc_tuple(7);
        Store_field(entry, 0, caml_copy_string(entryp->pw_name));
        Store_field(entry, 1, caml_copy_string(entryp->pw_passwd));
        Store_field(entry, 2, Val_int(entryp->pw_uid));
        Store_field(entry, 3, Val_int(entryp->pw_gid));
#ifdef __BEOS__
        Store_field(entry, 4, copy_string(""));
#else
        Store_field(entry, 4, copy_string(entryp->pw_gecos));
#endif
        Store_field(entry, 5, copy_string(entryp->pw_dir));
        Store_field(entry, 6, copy_string(entryp->pw_shell));
        cons = caml_alloc_tuple(2);
        Store_field(cons, 0, entry);
        Store_field(cons, 1, users);
        users = cons;
    }
    endpwent();

    CAMLreturn(users);
}

#endif /* !WIN32 */
   
value lm_getlk(value v_fd, value v_op)
{
#if defined(FCNTL_ENABLED)
	int fd, op, code;
	struct flock info;

	fd = Int_val(v_fd);
	op = Int_val(v_op);
	info.l_type = op;
	info.l_whence = SEEK_SET;
	info.l_start = 0;
	info.l_len = 0;
	caml_enter_blocking_section();
	code = fcntl(fd, F_GETLK, &info);
	caml_leave_blocking_section();
	if (code < 0) {
		char buf[2048];
		sprintf(buf, "lm_getlk error: %i", errno);
		caml_failwith(buf);
	}
	if (info.l_type == F_UNLCK)
		return Val_int(0);
	else
		return Val_int(info.l_pid);

#else /* FCNTL_ENABLED */
	caml_failwith("lm_getlk: not supported");
#endif /* FCNTL_ENABLED */
}
