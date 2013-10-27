/*
 * Dynamic linking hooks.
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
#ifndef _LM_DLL_H
#define _LM_DLL_H

/*
 * The normal magic number.
 */
#define LM_DLL_MAGIC    0xabe5a34d

/*
 * Strings are named by identifiers.
 */
typedef short DllId;

typedef DllId DllString;
typedef DllId DllStringArray;
typedef DllId DllFields;

/*
 * Standard type names.  For values of any other type,
 * just define a string name for it.
 */
typedef DllString DllType;

/*
 * Callbacks are identified by number.
 */
typedef unsigned DllCallbackId;

/*
 * A description of a field in a struct.
 */
typedef struct {
    DllString name;
    DllString type;
} DllField;

/*
 * Arguments are also specified as fields.
 */
typedef DllId DllArgTypes;

/*
 * An object if a collection of fields.
 */
typedef struct {
    DllString name;
    DllFields fields;
} DllObject;

/*
 * Fields in enumerations.
 */
typedef struct {
    DllString name;
    int value;
} DllEnumField;

typedef struct {
    DllString name;
    DllEnumField *fields;
} DllEnumInfo;

/*
 * Type information for values that are exported.
 */
typedef value (*ValueFun)(value);

typedef struct {
    DllString name;
    ValueFun value;
    DllArgTypes arg_types;
    DllType result_type;
} DllValue;

/*
 * Hooks to provide OCaml functions.
 */
typedef struct _dll_hooks {
    value (*alloc_hook)(mlsize_t, tag_t);
    value (*alloc_tuple_hook)(mlsize_t);
    value (*alloc_custom_hook)(struct custom_operations *ops, unsigned long size, mlsize_t mem, mlsize_t max);
    value (*copy_string_hook)(const char *s);
    value (*copy_string_array_hook)(const char **s);
    value (*copy_int32_hook)(int32 i);
    value (*copy_nativeint_hook)(intnat i);
    value (*copy_int64_hook)(int64 i);
    value (*copy_double_hook)(double x);
    value *(*named_value_hook)(char *name);
    value (*callback1_hook)(value, value);
    value (*callback2_hook)(value, value, value);
    value (*callback1_exn_hook)(value, value);
    value (*callback2_exn_hook)(value, value, value);
    void (*register_global_root_hook)(value *);
    void (*modify)(value *, value);
    struct caml__roots_block **dll_local_roots;
    void (*enter_blocking_section_hook)(void);
    void (*leave_blocking_section_hook)(void);
    value (*alloc_string_hook)(mlsize_t);
} DllHooks;

/*
 * Every library should define an export list
 * of all its functions.
 */
typedef struct {
    int magic;
    int version;
    value (*initialize_dll)(DllHooks *);
    int callback_count;
    value (*set_callback_handlers)(value handlers);
    char **strings;
    DllField **fields;
    DllObject *objects;
    DllEnumInfo *enums;
    DllValue *values;
} DllExport;

/*
 * Default definitions for the internal keywords.
 */
#define __dll_callback  static
#define __dll_typedef   typedef
#define DLL_REF(x)      *x

#endif /* _LM_DLL_H */
