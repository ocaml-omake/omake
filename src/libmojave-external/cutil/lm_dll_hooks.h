/*
 * Each dll should include this file.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
#ifndef _LM_DLL_HOOKS_H
#define _LM_DLL_HOOKS_H

#include "lm_dll.h"

/************************************************************************
 * Initialization code.
 */
static DllHooks *dll_hooks;
static value dll_callback_handlers;
static struct caml__roots_block **dll_local_roots;

#undef alloc
#undef alloc_tuple
#undef alloc_custom
#undef copy_string
#undef copy_string_array
#undef copy_int32
#undef copy_int64
#undef copy_nativeint
#undef copy_double
#undef named_value
#undef callback
#undef callback2
#undef callback_exn
#undef callback2_exn
#undef register_global_root
#undef caml_local_roots
#undef enter_blocking_section
#undef leave_blocking_section
#undef alloc_string

#define alloc(n, t)                     (dll_hooks->alloc_hook(n, t))
#define alloc_tuple(n)                  (dll_hooks->alloc_tuple_hook(n))
#define alloc_custom(ops, size, i, j)   (dll_hooks->alloc_custom_hook(ops, size, i, j))
#define copy_string(s)                  (dll_hooks->copy_string_hook(s))
#define copy_string_array(s)            (dll_hooks->copy_string_array_hook(s))
#define copy_int32(i)                   (dll_hooks->copy_int32_hook(i))
#define copy_nativeint(i)               (dll_hooks->copy_nativeint_hook(i))
#define copy_int64(i)                   (dll_hooks->copy_int64_hook(i))
#define copy_double(x)                  (dll_hooks->copy_double_hook(x))
#define named_value(name)               (dll_hooks->named_value_hook(name))
#define callback(f, arg1)               (dll_hooks->callback1_hook(f, arg1))
#define callback2(f, arg1, arg2)        (dll_hooks->callback2_hook(f, arg1, arg2))
#define callback_exn(f, arg1)           (dll_hooks->callback1_exn_hook(f, arg1))
#define callback2_exn(f, arg1, arg2)    (dll_hooks->callback2_exn_hook(f, arg1, arg2))
#define register_global_root(v)         (dll_hooks->register_global_root_hook(v))
#define caml_modify(vp, v)              (dll_hooks->modify(vp, v))
#define caml_local_roots                (*dll_local_roots)
#define enter_blocking_section()        (dll_hooks->enter_blocking_section_hook())
#define leave_blocking_section()        (dll_hooks->leave_blocking_section_hook())
#define alloc_string(n)                 (dll_hooks->alloc_string_hook(n))

#include "lm_dll_pointers.h"

#endif /* _LM_DLL_HOOKS_H */
