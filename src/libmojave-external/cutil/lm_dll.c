/*
 * Dynamic linking.  We provide a minimal amount of type checking.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave group, Caltech
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
#include <string.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include "lm_dll.h"
#include "lm_dll_pointers.h"

/************************************************************************
 * More allocation.
 */
value lm_dll_malloc(value v_size)
{
    return dll_malloc(Int_val(v_size));
}

value lm_dll_free(value v)
{
    unsigned long off, size;
    char *p, *base;

    if(v == 0)
        failwith("lm_dll_free: free a null pointer");
    p = DllPointer_pointer_val(v);
    if(p == 0)
        failwith("lm_dll_free: free a null pointer");
    DllPointer_pointer_val(v) = 0;
    return Val_unit;
}

/*
 * Array of pointers.
 * NULL terminated.
 */
value lm_dll_pointer_array(value v_array)
{
    CAMLparam1(v_array);
    CAMLlocal1(v);
    int i, words;
    void **p;

    words = Wosize_val(v_array);
    v = dll_malloc((words + 1) * sizeof(void *));
    p = DllPointer_pointer_val(v);
    for(i = 0; i != words; i++)
        p[i] = DllPointer_pointer_val(Field(v_array, i));
    p[i] = 0;
    CAMLreturn(v);
}

/*
 * String allocation.
 */
value lm_dll_strdup(value v_str)
{
    CAMLparam1(v_str);
    CAMLlocal1(v);
    int size, len;
    char *s, *p;

    s = String_val(v_str);
    len = strlen(s);
    v = dll_malloc(len + 1);
    p = DllPointer_pointer_val(v);
    memcpy(p, s, len);
    p[len] = 0;
    CAMLreturn(v);
}

value lm_dll_pointer_of_string(value v_str)
{
    CAMLparam1(v_str);
    CAMLlocal1(v);
    int size, len;
    char *s, *p;

    s = String_val(v_str);
    len = string_length(v_str);
    v = dll_malloc(len + 1);
    p = DllPointer_pointer_val(v);
    memcpy(p, s, len);
    p[len] = 0;
    CAMLreturn(v);
}

value lm_dll_string_of_pointer(value v_ptr)
{
    return copy_string(DllPointer_pointer_val(v_ptr));
}

/*
 * String array allocation.
 */
value lm_dll_pointer_of_string_array(value v_argv)
{
    CAMLparam1(v_argv);
    CAMLlocal1(v);
    char **p, *top, *s;
    int i, len, words, size;

    /* Calculate the total storage size */
    words = Wosize_val(v_argv);
    size = sizeof(char *) * (words + 1);
    for(i = 0; i != words; i++)
        size += strlen(String_val(Field(v_argv, i))) + 1;

    /* Copy the entries */
    v = dll_malloc(size);
    p = (char **) DllPointer_pointer_val(v);
    top = (char *) (p + (words + 1));
    for(i = 0; i != words; i++) {
        s = String_val(Field(v_argv, i));
        len = strlen(s);
        memcpy(top, s, len);
        p[i] = top;
        top[len] = 0;
        top += len + 1;
    }
    p[i] = 0;
    CAMLreturn(v);
}        

value lm_dll_string_array_of_pointer(value v_ptr)
{
    return caml_copy_string_array(DllPointer_pointer_val(v_ptr));
}

value lm_dll_sub_string_array_of_pointer(value v_off, value v_len, value v_ptr)
{
    CAMLparam3(v_off, v_len, v_ptr);
    CAMLlocal1(v_array);
    int i, off, len;
    char col[24];
    char **argv;

    off  = Int_val(v_off);
    len  = Int_val(v_len);
    argv = DllPointer_pointer_val(v_ptr);
    v_array = caml_alloc(len, 0);
    for(i = 0; i != len; i++) {
        char *str1, *str2;

        str1 = argv[i + off];
        if(str1)
            str2 = str1;
        else {
            sprintf(col, "%d", i);
            str2 = col;
        }        
        Store_field(v_array, i, caml_copy_string(str2));
    }
    CAMLreturn(v_array);
}

/************************************************************************
 * Win32 section.
 */
#ifdef WIN32

/*
 * Open a dynamic link library.
 */
value lm_dlopen(value v_filename, value v_flags)
{
    CAMLparam2(v_filename, v_flags);

    failwith("lm_dlopen");

    /* Return it */
    CAMLreturn(Val_unit);
}

/*
 * Open static info.
 */
value lm_dlopen_static(DllExport *values)
{
    CAMLparam0();

    failwith("lm_dlopen_static");

    CAMLreturn(Val_unit);
}

/*
 * Set the callback handler.
 */
typedef void (*SetHandler)(value);
typedef value (*FunPointer)(value);

value lm_set_callback_handler(value v_set, value v_handler)
{
    SetHandler set = (SetHandler) v_set;
    set(v_handler);
    return Val_unit;
}

/*
 * Apply the function.
 */
value lm_dlapply(value v_sym, value v_args)
{
    CAMLparam2(v_sym, v_args);
    failwith("lm_dlapply");
    CAMLreturn(Val_unit);
}

/*
 * NULL pointers.
 */
value lm_dlnull(value v_arg)
{
    CAMLparam1(v_arg);
    failwith("lm_dlnull");
    CAMLreturn(Val_unit);
}

value lm_dlpointer_of_int(value v_arg)
{
    CAMLparam1(v_arg);
    failwith("lm_dlpointer_of_int");
    CAMLreturn(Val_unit);
}

value lm_dlint_of_pointer(value v_arg)
{
    CAMLparam1(v_arg);
    failwith("lm_dlint_of_pointer");
    CAMLreturn(Val_unit);
}

value lm_dlpointer_of_nativeint(value v_arg)
{
    CAMLparam1(v_arg);
    failwith("lm_dlpointer_of_nativeint");
    CAMLreturn(Val_unit);
}

value lm_dlnativeint_of_pointer(value v_arg)
{
    CAMLparam1(v_arg);
    failwith("lm_dlnativeint_of_pointer");
    CAMLreturn(Val_unit);
}

#else /* !WIN32 */

/************************************************************************
 * UNIX.
 */

#include <dlfcn.h>

#define CAML_RTLD_LAZY          0
#define CAML_RTLD_NOW           1
#define CAML_RTLD_GLOBAL        2

/*
 * Hooks.
 */
static DllHooks hooks = {
    caml_alloc,
    caml_alloc_tuple,
    caml_alloc_custom,
    caml_copy_string,
    caml_copy_string_array,
    caml_copy_int32,
    caml_copy_nativeint,
    caml_copy_int64,
    caml_copy_double,
    (value *(*)(char *name)) caml_named_value,
    caml_callback,
    caml_callback2,
    caml_callback_exn,
    caml_callback2_exn,
    caml_register_global_root,
    caml_modify,
    &caml_local_roots,
    caml_enter_blocking_section,
    caml_leave_blocking_section,
    caml_alloc_string,
};

/************************************************************************
 * Allocate of C data.
 */

/*
 * A single field in an object.
 */
static value alloc_field(value v_strings, DllField *field)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);

    v_tuple = alloc_tuple(2);
    Store_field(v_tuple, 0, Field(v_strings, field->name));
    Store_field(v_tuple, 1, Field(v_strings, field->type));
    CAMLreturn(v_tuple);
}

/*
 * All the fields in an object or argument list.
 */
static value alloc_rttd(value v_strings, DllField *fields)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);
    DllField *p;
    int i;

    /* Get the number of entries */
    i = 0;
    for(p = fields; p->name >= 0; p++)
        i++;
    if(i == 0)
        CAMLreturn(Atom(0));

    /* Alloc them */
    v_tuple = alloc_tuple(i);
    for(i = 0, p = fields; p->name >= 0; i++, p++)
        Store_field(v_tuple, i, alloc_field(v_strings, p));

    CAMLreturn(v_tuple);
}

/*
 * All the objects in the library.
 */
static value alloc_fields(value v_strings, DllField **fields)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);
    DllField **p;
    int i;

    /* Get the number of entries */
    i = 0;
    for(p = fields; *p; p++)
        i++;
    if(i == 0)
        CAMLreturn(Atom(0));

    /* Alloc them */
    v_tuple = alloc_tuple(i);
    for(i = 0, p = fields; *p; i++, p++)
        Store_field(v_tuple, i, alloc_rttd(v_strings, *p));
    CAMLreturn(v_tuple);
}

/*
 * A specific object.
 */
static value alloc_object(value v_strings, value v_fields, DllObject *obj)
{
    CAMLparam2(v_strings, v_fields);
    CAMLlocal1(v_tuple);

    v_tuple = alloc_tuple(2);
    Store_field(v_tuple, 0, Field(v_strings, obj->name));
    Store_field(v_tuple, 1, Field(v_fields, obj->fields));
    CAMLreturn(v_tuple);
}

/*
 * All the objects in the program.
 */
static value alloc_objects(value v_strings, value v_fields, DllObject *objs)
{
    CAMLparam2(v_strings, v_fields);
    CAMLlocal1(v_tuple);
    DllObject *p;
    int i;

    /* Get the number of entries */
    i = 0;
    for(p = objs; p->name >= 0; p++)
        i++;
    if(i == 0)
        CAMLreturn(Atom(0));

    /* Alloc them */
    v_tuple = alloc_tuple(i);
    for(i = 0, p = objs; p->name >= 0; i++, p++)
        Store_field(v_tuple, i, alloc_object(v_strings, v_fields, p));

    CAMLreturn(v_tuple);
}

/*
 * Enumerations.
 */
static value alloc_enum_field(value v_strings, DllEnumField *field)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);

    v_tuple = alloc_tuple(2);
    Store_field(v_tuple, 0, Field(v_strings, field->name));
    Store_field(v_tuple, 1, copy_nativeint(field->value));
    CAMLreturn(v_tuple);
}

static value alloc_enum_fields(value v_strings, DllEnumField *fields)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);
    DllEnumField *p;
    int i;

    /* Get the number of entries */
    i = 0;
    for(p = fields; p->name >= 0; p++)
        i++;
    if(i == 0)
        CAMLreturn(Atom(0));

    /* Alloc them */
    v_tuple = alloc_tuple(i);
    for(i = 0, p = fields; p->name >= 0; i++, p++)
        Store_field(v_tuple, i, alloc_enum_field(v_strings, p));

    CAMLreturn(v_tuple);
}

static value alloc_enum_info(value v_strings, DllEnumInfo *info)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);

    v_tuple = alloc_tuple(2);
    Store_field(v_tuple, 0, Field(v_strings, info->name));
    Store_field(v_tuple, 1, alloc_enum_fields(v_strings, info->fields));
    CAMLreturn(v_tuple);
}

static value alloc_enums(value v_strings, DllEnumInfo *enums)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);
    DllEnumInfo *p;
    int i;

    /* Get the number of entries */
    i = 0;
    for(p = enums; p->name >= 0; p++)
        i++;
    if(i == 0)
        CAMLreturn(Atom(0));

    /* Alloc them */
    v_tuple = alloc_tuple(i);
    for(i = 0, p = enums; p->name >= 0; p++, i++)
        Store_field(v_tuple, i, alloc_enum_info(v_strings, p));

    CAMLreturn(v_tuple);
}

/*
 * A single function value.
 */
static value alloc_value(value v_strings, value v_fields, DllValue *val)
{
    CAMLparam2(v_strings, v_fields);
    CAMLlocal1(v_tuple);

    v_tuple = alloc_tuple(4);
    Store_field(v_tuple, 0, Field(v_strings, val->name));
    Store_field(v_tuple, 1, dll_unmarshal_pointer(val->value));
    Store_field(v_tuple, 2, Field(v_fields, val->arg_types));
    Store_field(v_tuple, 3, Field(v_strings, val->result_type));
    CAMLreturn(v_tuple);
}

/*
 * All the values in the program.
 */
static value alloc_values(value v_strings, value v_fields, DllValue *values)
{
    CAMLparam1(v_strings);
    CAMLlocal1(v_tuple);
    DllValue *p;
    int i;

    /* Get the number of entries */
    i = 0;
    for(p = values; p->name >= 0; p++)
        i++;
    if(i == 0)
        CAMLreturn(Atom(0));

    /* Alloc them */
    v_tuple = alloc_tuple(i);
    for(i = 0, p = values; p->name >= 0; i++, p++)
        Store_field(v_tuple, i, alloc_value(v_strings, v_fields, p));

    CAMLreturn(v_tuple);
}

/*
 * Allocate everything.
 */
static value alloc_dll(DllExport *values, value v_globals)
{
    CAMLparam1(v_globals);
    CAMLlocal3(v_strings, v_fields, v_tuple);

    /* Allocate the strings */
    v_strings = copy_string_array((const char **) values->strings);
    v_fields = alloc_fields(v_strings, values->fields);

    /* Allocate the result */
    v_tuple = alloc_tuple(6);
    Store_field(v_tuple, 0, alloc_objects(v_strings, v_fields, values->objects));
    Store_field(v_tuple, 1, alloc_enums(v_strings, values->enums));
    Store_field(v_tuple, 2, alloc_values(v_strings, v_fields, values->values));
    Store_field(v_tuple, 3, Val_int(values->callback_count));
    Store_field(v_tuple, 4, (value) values->set_callback_handlers);
    Store_field(v_tuple, 5, v_globals);
    CAMLreturn(v_tuple);
}

/************************************************************************
 * DLL functions.
 */

/*
 * Open a dynamic link library.
 */
value lm_dlopen(value v_path, value v_filename, value v_tag, value v_flags)
{
    CAMLparam4(v_path, v_filename, v_tag, v_flags);
    CAMLlocal1(v_globals);
    char export[1024];
    DllExport *values;
    char *tag;
    int flags;
    void *dll;
    value v;

    char path[1 << 16];
    int index, argc, i, len;
    char *s;

    /* Parse the flags */
    flags = RTLD_LAZY;
    while(Is_block(v_flags)) {
        value v_flag = Field(v_flags, 0);
        v_flags = Field(v_flags, 1);
        switch(Int_val(v_flag)) {
        case CAML_RTLD_LAZY:
            flags = (flags & ~RTLD_NOW) | RTLD_LAZY;
            break;
        case CAML_RTLD_NOW:
            flags = (flags & ~RTLD_LAZY) | RTLD_NOW;
            break;
        case CAML_RTLD_GLOBAL:
            flags |= RTLD_GLOBAL;
            break;
        default:
            caml_invalid_argument("caml_dlopen");
            break;
        }
    }

    /* Compute and set the path */
    index = 0;
    argc = Wosize_val(v_path);
    for(i = 0; i < argc; i++) {
        s = String_val(Field(v_path, i));
        len = strlen(s);
        if(index + len > sizeof(path) - 2) {
            fprintf(stderr, "LD_LIBRARY_PATH is too long\n");
            break;
        }
        if(i)
            path[index++] = ':';
        memcpy(path + index, s, len);
        index += len;
    }
    path[index] = 0;
    setenv("DYLD_LIBRARY_PATH", path, 1);

    /* Now open the lib */
    dll = dlopen(String_val(v_filename), flags);
    if(dll == 0)
        failwith(dlerror());

    /* Load the value description */
    tag = String_val(v_tag);
    if(*tag)
        sprintf(export, "dll_%s_export", tag);
    else
        strcpy(export, "dll_export");
    values = dlsym(dll, export);
    if(values) {
        if(values->magic != LM_DLL_MAGIC) {
            dlclose(dll);
            failwith(String_val(v_filename));
        }
        v_globals = values->initialize_dll(&hooks);
        v = alloc_dll(values, v_globals);
    }
    else
        v = Val_int(0);

    /* Return it */
    CAMLreturn(v);
}

/*
 * Open static info.
 */
value lm_dlopen_static(DllExport *values)
{
    CAMLparam0();
    CAMLlocal2(v_globals, v);

    if(values->magic == LM_DLL_MAGIC) {
        v_globals = values->initialize_dll(&hooks);
        v = alloc_dll(values, v_globals);
    }
    else
        v = Val_int(0);
    CAMLreturn(v);
}

/*
 * Set the callback handler.
 */
typedef value (*SetHandler)(value);
typedef value (*FunPointer)(value);

value lm_set_callback_handler(value v_set, value v_handler)
{
    SetHandler set = (SetHandler) v_set;
    return set(v_handler);
}

/*
 * Apply the function.
 */
value lm_dlapply(value v_sym, value v_args)
{
    CAMLparam2(v_sym, v_args);
    CAMLlocal1(v_result);
    FunPointer f;

    f = (FunPointer) dll_marshal_pointer(v_sym);
    v_result = f(v_args);
    CAMLreturn(v_result);
}

/*
 * NULL pointers.
 */
value lm_dlnull(value v_arg)
{
    return dll_unmarshal_pointer(0);
}

value lm_dlpointer_of_int(value v_arg)
{
    return dll_unmarshal_pointer((void *) Long_val(v_arg));
}

value lm_dlint_of_pointer(value v_arg)
{
    CAMLparam1(v_arg);
    CAMLlocal1(v);

    if(Is_long(v_arg))
        v = v_arg;
    // TODO: DN - Should we compare against both "dll_pointer" and "dll_malloc" or just use "dll_pointer" when we malloc...?
    else if(Tag_val(v_arg) == Custom_tag && (strcmp(Custom_ops_val(v_arg)->identifier, "dll_pointer") == 0 || strcmp(Custom_ops_val(v_arg)->identifier, "dll_malloc") == 0))
        v = Val_int((value) dll_marshal_pointer(v_arg));
    else
        failwith("int_of_pointer: not a pointer or integer");
    CAMLreturn(v);
}

value lm_dlpointer_of_nativeint(value v_arg)
{
    return dll_unmarshal_pointer((void *) Nativeint_val(v_arg));
}

value lm_dlnativeint_of_pointer(value v_arg)
{
    CAMLparam1(v_arg);
    CAMLlocal1(v);

    if(Is_long(v_arg))
        v = v_arg;
    // TODO: DN - Should we compare against both "dll_pointer" and "dll_malloc" or just use "dll_pointer" when we malloc...?
    else if(Tag_val(v_arg) == Custom_tag && (strcmp(Custom_ops_val(v_arg)->identifier, "dll_pointer") == 0 || strcmp(Custom_ops_val(v_arg)->identifier, "dll_malloc") == 0))
        v = copy_nativeint((value) dll_marshal_pointer(v_arg));
    else
        failwith("nativeint_of_pointer: not a pointer or integer");
    CAMLreturn(v);
}

#endif /* !WIN32 */
