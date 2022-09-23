/*
 * System info.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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

#ifdef WIN32
#include <windows.h>
/* Disable some of the warnings */
#pragma warning( disable : 4127 4189 4702 4996 )

/*
 * Fake utsname.
 */
struct utsname {
    char sysname[1024];
    char nodename[1024];
    char release[1024];
    char version[1024];
    char machine[1024];
};

/*
 * uname is undefined as far as I know (jyh).
 * Get most of the info from the environment.
 */
static int uname(struct utsname *name)
{
    DWORD len;
    SYSTEM_INFO sysinfo;
    OSVERSIONINFO osversion;
    unsigned ptype;
    char *osname;

    /* Ask Win32 for OS info */
    osversion.dwOSVersionInfoSize = sizeof(osversion);
    if(GetVersionEx(&osversion) == 0)
        return -1;

    /* String version of the osname */
    osname = (char *) "unknown";
    switch (osversion.dwPlatformId) {
    case VER_PLATFORM_WIN32s:
        osname = (char *) "win32s";
        break;
    case VER_PLATFORM_WIN32_WINDOWS:
        switch (osversion.dwMinorVersion) {
        case 0:
            osname = (char *) "Win95";
            break;
        case 1:
            osname = (char *) "Win98";
            break;
        default:
            osname = (char *) "Win9X";
            break;
        }
        break;
    case VER_PLATFORM_WIN32_NT:
        osname = (char *) "WinNT";
        break;
    }

    /* Collect data */
    strcpy(name->sysname, osname);
    sprintf(name->version, "%d.%d", (int) osversion.dwMajorVersion, (int) osversion.dwMinorVersion);
    sprintf(name->release, "%d %s", (int) osversion.dwBuildNumber, osversion.szCSDVersion);

    /* Computer name */
    len = sizeof(name->nodename) - 1;
    GetComputerNameA(name->nodename, &len);
    name->nodename[len] = 0;

    /* Machine */
    GetSystemInfo(&sysinfo);

    /* CPU type */
    switch (sysinfo.wProcessorArchitecture) {
    case PROCESSOR_ARCHITECTURE_INTEL:
        if(sysinfo.dwProcessorType < 3) /* Shouldn't happen. */
            ptype = 3;
        else if(sysinfo.dwProcessorType > 9) /* P4 */
            ptype = 6;
        else
            ptype = sysinfo.dwProcessorType;
        sprintf(name->machine, "i%d86", ptype);
        break;
    case PROCESSOR_ARCHITECTURE_ALPHA:
        strcpy(name->machine, "alpha");
        break;
    case PROCESSOR_ARCHITECTURE_MIPS:
        strcpy(name->machine, "mips");
        break;
    default:
        strcpy(name->machine, "unknown");
        break;
    }

    return 0;
}

#else /* WIN32 */

#include <sys/utsname.h>

#endif /* !WIN32 */

/*
 * Allocate a 5-tuple of strings.
 */
value lm_uname(value x)
{
    CAMLparam1(x);
    CAMLlocal1(result);
    struct utsname name;

    /* Get sysinfo */
    if(uname(&name) < 0)
        caml_failwith("uname");

    /* Copy data */
    result = caml_alloc_tuple(5);
    Field(result, 0) = Val_unit;
    Field(result, 1) = Val_unit;
    Field(result, 2) = Val_unit;
    Field(result, 3) = Val_unit;
    Field(result, 4) = Val_unit;

    Field(result, 0) = caml_copy_string(name.sysname);
    Field(result, 1) = caml_copy_string(name.nodename);
    Field(result, 2) = caml_copy_string(name.release);
    Field(result, 3) = caml_copy_string(name.version);
    Field(result, 4) = caml_copy_string(name.machine);

    /* Return it */
    CAMLreturn(result);
}
