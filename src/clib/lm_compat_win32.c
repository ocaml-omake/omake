/*
 * Compatibility functions for the various versions of Win32.
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

#ifdef WIN32
/* Disable some of the warnings */
#pragma warning( disable : 4100 4201 ) 

#include <windows.h>
#include <tlhelp32.h>
#include <shlobj.h>

#include "lm_compat_win32.h"

/*
 * Pointers to the functions.
 */
static HANDLE (__stdcall *OpenThreadF)(DWORD, BOOL, DWORD);
static BOOL (__stdcall *GetLongPathNameF)(LPCTSTR, LPTSTR, DWORD);
static HANDLE (__stdcall *CreateToolhelp32SnapshotF)(DWORD, DWORD);
static BOOL (__stdcall *Thread32FirstF)(HANDLE, LPTHREADENTRY32);
static BOOL (__stdcall *Thread32NextF)(HANDLE, LPTHREADENTRY32);
static HRESULT (__stdcall *SHGetFolderPathF)(HWND, int, HANDLE, DWORD, LPTSTR);
static BOOL (__stdcall *SHGetSpecialFolderPathF)(HWND, LPTSTR, int, BOOL);

/*
 * Compatibility.
 */
int ExistsOpenThread(void)
{
    return OpenThreadF ? 1 : 0;
}

HANDLE CompatOpenThread(DWORD arg1, BOOL arg2, DWORD arg3)
{
    return OpenThreadF(arg1, arg2, arg3);
}

BOOL CompatGetLongPathName(LPCTSTR arg1, LPTSTR arg2, DWORD arg3)
{
    BOOL b = 0;

    if(GetLongPathNameF)
        b = GetLongPathNameF(arg1, arg2, arg3);
    return b;
}

HANDLE CompatCreateToolhelp32Snapshot(DWORD arg1, DWORD arg2)
{
    return CreateToolhelp32SnapshotF(arg1, arg2);
}

BOOL CompatThread32First(HANDLE arg1, LPTHREADENTRY32 arg2)
{
    return Thread32FirstF(arg1, arg2);
}

BOOL CompatThread32Next(HANDLE arg1, LPTHREADENTRY32 arg2)
{
    return Thread32NextF(arg1, arg2);
}

HRESULT CompatSHGetFolderPath(HWND hwndOwner, int nFolder, HANDLE hToken, DWORD dwFlags, LPTSTR pszPath)
{
    if(SHGetFolderPathF)
        return SHGetFolderPathF(hwndOwner, nFolder, hToken, dwFlags, pszPath);
    else if(SHGetSpecialFolderPathF) {
        BOOL fCreate = nFolder & CSIDL_FLAG_CREATE ? TRUE : FALSE;
        nFolder &= ~CSIDL_FLAG_CREATE;
        fCreate = SHGetSpecialFolderPathF(hwndOwner, pszPath, nFolder, fCreate);
        return fCreate ? S_OK : E_FAIL;
    }
    else
        return E_NOTIMPL;
}

/*
 * Decide whether OpenThread is available.
 */
static void init(void)
{
    HINSTANCE hinst;

    hinst = LoadLibrary(TEXT("KERNEL32"));
    if(hinst != NULL) {
        *(FARPROC *)&OpenThreadF = GetProcAddress(hinst, "OpenThread");
#ifdef UNICODE
        *(FARPROC *)&GetLongPathNameF = GetProcAddress(hinst, "GetLongPathNameW");
#else
        *(FARPROC *)&GetLongPathNameF = GetProcAddress(hinst, "GetLongPathNameA");
#endif
        *(FARPROC *)&CreateToolhelp32SnapshotF = GetProcAddress(hinst, "CreateToolhelp32Snapshot");
        *(FARPROC *)&Thread32FirstF = GetProcAddress(hinst, "Thread32First");
        *(FARPROC *)&Thread32NextF = GetProcAddress(hinst, "Thread32Next");
    }

    hinst = LoadLibrary(TEXT("SHELL32"));
    if(hinst != NULL) {
#ifdef UNICODE
        *(FARPROC *)&SHGetFolderPathF = GetProcAddress(hinst, "SHGetFolderPathW");
        *(FARPROC *)&SHGetSpecialFolderPathF = GetProcAddress(hinst, "SHGetSpecialFolderPathW");
#else
        *(FARPROC *)&SHGetFolderPathF = GetProcAddress(hinst, "SHGetFolderPathA");
        *(FARPROC *)&SHGetSpecialFolderPathF = GetProcAddress(hinst, "SHGetSpecialFolderPathA");
#endif
    }

    if(SHGetFolderPathF == 0) {
        hinst = LoadLibrary(TEXT("SHFOLDER"));
        if(hinst != NULL) {
#ifdef UNICODE
            *(FARPROC *)&SHGetFolderPathF = GetProcAddress(hinst, "SHGetFolderPathW");
#else
            *(FARPROC *)&SHGetFolderPathF = GetProcAddress(hinst, "SHGetFolderPathA");
#endif
        }
    }
}

/*
 * ML interface.
 */
value lm_compat_init(value v_unit)
{
    init();
    return Val_unit;
}

#else /* !WIN32 */

value lm_compat_init(value v_unit)
{
    return Val_unit;
}

#endif /* !WIN32 */

