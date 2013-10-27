/*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 */
#ifndef _LM_DLL_POINTERS_H
#define _LM_DLL_POINTERS_H

/************************************************************************
 * Pointers are custom blocks.
 */
typedef struct _dll_pointer {
    void *p;
} DllPointer;

static DllPointer dll_null_pointer;

#define DllPointer_val(v)         ((DllPointer *) Data_custom_val(v))
#define DllPointer_pointer_val(v) (DllPointer_val(v)->p)

static int dll_pointer_compare(value v1, value v2)
{
    void *p1 = DllPointer_pointer_val(v1);
    void *p2 = DllPointer_pointer_val(v2);
    int i;

    if(p1 < p2)
        i = -1;
    else if(p1 > p2)
        i = 1;
    else
        i = 0;
    return i;
}

static long dll_pointer_hash(value v)
{
    CAMLparam1(v);
    void *p = DllPointer_pointer_val(v);
    CAMLreturn((long) p);
}

static void dll_pointer_serialize(value v, unsigned long *wsize_32, unsigned long *wsize_64)
{
    *wsize_32 = 4;
    *wsize_64 = 8;
}

static unsigned long dll_pointer_deserialize(void *dst)
{
    *(DllPointer *)dst = dll_null_pointer;
    return sizeof(DllPointer);
}

static struct custom_operations dll_pointer_ops = {
    "dll_pointer",
    custom_finalize_default,
    dll_pointer_compare,
    dll_pointer_hash,
    dll_pointer_serialize,
    dll_pointer_deserialize
};

static inline value dll_unmarshal_pointer(void *p)
{
    value v;

    if(p) {
        v = alloc_custom(&dll_pointer_ops, sizeof(DllPointer), 0, 1);
        DllPointer_pointer_val(v) = p;
    }
    else
        v = 0;
    return v;
}

static inline void *dll_marshal_pointer(value v)
{
    void *p;

    if(v)
        p = DllPointer_pointer_val(v);
    else
        p = 0;
    return p;
}

/*
 * For allocated values, we have to allocate with malloc(3),
 * because otherwise the value may move around.
 */
static void dll_malloc_finalize(value v)
{
    void *p = DllPointer_pointer_val(v);

    if(p) {
        free(p);
        DllPointer_pointer_val(v) = 0;
    }
}

static struct custom_operations dll_malloc_ops = {
    "dll_malloc",
    dll_malloc_finalize,
    dll_pointer_compare,
    dll_pointer_hash,
    dll_pointer_serialize,
    dll_pointer_deserialize
};

static value dll_malloc(unsigned size)
{
    value v;
    void *p;

    v = alloc_custom(&dll_malloc_ops, sizeof(DllPointer), 0, 1);
    p = malloc(size);
    if(p == 0) {
        fprintf(stderr, "dll_malloc: out of memory");
        exit(1);
    }
    memset(p, 0, size);
    DllPointer_pointer_val(v) = p;
    return v;
}

#endif /* _LM_DLL_POINTERS */
