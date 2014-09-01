/*
 * Compatibility functions for the various versions of Windows.
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
#ifndef _COMPAT_WIN32_H
#define _COMPAT_WIN32_H

#ifdef WIN32

#include <windows.h>
#include <tlhelp32.h>

int ExistsOpenThread(void);
HANDLE CompatOpenThread(DWORD arg1, BOOL arg2, DWORD arg3);
BOOL CompatGetLongPathName(LPCTSTR arg1, LPTSTR arg2, DWORD arg3);
HANDLE CompatCreateToolhelp32Snapshot(DWORD arg1, DWORD arg2);
BOOL CompatThread32First(HANDLE arg1, LPTHREADENTRY32 arg2);
BOOL CompatThread32Next(HANDLE arg1, LPTHREADENTRY32 arg2);
HRESULT CompatSHGetFolderPath(HWND hwndOwner, int nFolder, HANDLE hToken, DWORD dwFlags, LPTSTR pszPath);

#endif /* WIN32 */

#endif /* _COMPAT_WIN32_H */
