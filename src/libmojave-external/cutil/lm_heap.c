/*
 * Some debugging code for the heap.
 * WARNING: if you want to use this, you _must_ link against a DEBUG
 * version of OCaml!
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave group, Caltech
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

#if defined(WIN32) || defined(_WIN32)
/* Disable some of the warnings */
#pragma warning( disable : 4146 )
#endif

extern char *caml_young_start, *caml_young_ptr, *caml_young_limit, *caml_young_end;

static char *null = 0;

#undef abort
#define abort()    (*null = 0)

static void search_pointer(char **pointers, char *name, unsigned bound, char *p, char *v, unsigned index)
{
    unsigned i, j, k;
    char *p2;

    i = 0;
    j = bound;
    while(j - i > 1) {
        k = (i + j) >> 1;
        p2 = pointers[k];
        if(p2 <= p)
            i = k;
        else
            j = k;
    }
    p2 = pointers[i];
    if((p2 != p) && (Tag_val(p) != Infix_tag)) {
        fprintf(stderr, "%s: illegal pointer: 0x%08lx < 0x%08lx < 0x%08lx, size = %lud, tag = %d\n", 
                name,
                (unsigned long) p2, (unsigned long) p, (unsigned long) pointers[i + 1],
                Wosize_val(p), Tag_val(p));
        fprintf(stderr, "points into: 0x%08lx: index = %d, size = %lud, tag = %d\n",
                (unsigned long) p2, i, Wosize_val(p2), Tag_val(p2));
        fprintf(stderr, "from block: 0x%08lx: size = %lud, tag = %d, field = %d\n",
                (unsigned long) v, Wosize_val(v), Tag_val(v), index);
        fflush(stderr);
        abort();
    }
}
    
static void lm_heap_check_aux1(char *name)
{
    char *start, *ptr, *end;
    char *v;
    value p, *next;
    mlsize_t size;
    unsigned i, index, found;
	 char *pointers[1 << 16];

    start = caml_young_start;
    ptr = caml_young_ptr;
    end = caml_young_end;

    fprintf(stderr, "AAA: %s: [0x%08lx, 0x%08lx, 0x%08lx, 0x%08lx] (%ld/%ld/%ld bytes)\n",
            name,
            (unsigned long) caml_young_start,
            (unsigned long) caml_young_ptr,
            (unsigned long) caml_young_limit,
            (unsigned long) caml_young_end, 
            ((unsigned long) caml_young_end) - (unsigned long) caml_young_ptr,
            ((unsigned long) caml_young_end) - (unsigned long) caml_young_limit,
            ((unsigned long) caml_young_end) - (unsigned long) caml_young_start);
    fflush(stderr);

    /*
     * Phase 1: check that the headers have the right sizes.
     */
    v = (char *) Val_hp(caml_young_ptr);
    index = 0;
    while(v < caml_young_end) {
        pointers[index++] = (char *) v;
        size = Wosize_val(v);
        fprintf(stderr, "%s: 0x%08lx: size %lud, tag = %d\n",
                name, (unsigned long) v, size, Tag_val(v));
        found = 0;
        for(i = 0; i != 10; i++) {
            next = &Field(v, size + i);
            if(next < (value *) caml_young_end) {
                p = *next;
#define Debug_free_minor 0xD700D6D7ul
                if(p == Debug_free_minor) {
                    fprintf(stderr, "\tnext[%d]:0x%08lx = 0x%08lx\n", i, (unsigned long) next, (unsigned long) p);
                    found = 1;
                }
                else if(found)
                    fprintf(stderr, "\tnext[%d]:0x%08lx = 0x%08lx, size = %lud, tag = %d\n",
                            i, (unsigned long) next, (unsigned long) p, Wosize_hd(p), Tag_hd(p));
            }
        }
        fflush(stderr);
        v = (char *) &Field(v, size + 1);
    }
    if(v > (char *) Val_hp(caml_young_end)) {
        fprintf(stderr, "%s: heap is bogus\n", name);
        fflush(stderr);
        return;
    }

    /*
     * Phase 2: check that all the fields point to actual
     * values.
     */
    v = (char *) Val_hp(caml_young_ptr);
    while(v < caml_young_end) {
        size = Wosize_val(v);
        if(Tag_val(v) < No_scan_tag) {
            fprintf(stderr, "%s: scanning 0x%08lx: size %lud, tag = %d\n", name, (unsigned long) v, size, Tag_val(v));
            fflush(stderr);
            for(i = 0; i != size; i++) {
                char *p = (char *) Field(v, i);
                if(Is_block((value) p)) {
                    if(p >= caml_young_limit && p < caml_young_ptr) {
                        fprintf(stderr, "%s: pointer refers to empty young space\n", name);
                        fflush(stderr);
                        return;
                    }
                    if(p >= caml_young_ptr && p < caml_young_end)
                        search_pointer(pointers, name, index, p, v, i);
                }
            }
        }
        v = (char *) &Field(v, size + 1);
    }
}

static void lm_heap_check_aux2(char *name)
{
    char *start, *ptr, *end;
    char *v;
    header_t hd;
    mlsize_t size;
    unsigned i;

    start = caml_young_start;
    ptr = caml_young_ptr;
    end = caml_young_end;

    fprintf(stderr, "AAA: %s: [0x%08lx, 0x%08lx, 0x%08lx, 0x%08lx] (%ld/%ld/%ld bytes)\n",
            name,
            (unsigned long) caml_young_start,
            (unsigned long) caml_young_ptr,
            (unsigned long) caml_young_limit,
            (unsigned long) caml_young_end, 
            ((unsigned long) caml_young_end) - (unsigned long) caml_young_ptr,
            ((unsigned long) caml_young_end) - (unsigned long) caml_young_limit,
            ((unsigned long) caml_young_end) - (unsigned long) caml_young_start);

    fflush(stderr);

    /*
     * Phase 1: check that the headers have the right sizes.
     */
    v = (char *) Val_hp(caml_young_ptr);
    while(v < caml_young_end) {
        hd = Hd_val(v);
        if(hd == Debug_free_minor) {
            fprintf(stderr, "Bogus pointer: 0x%08lx\n", (unsigned long) v);
            fflush(stderr);
            v += sizeof(header_t);
        }
        else {
            size = Wosize_val(v);
            for(i = 0; i != size; i++) {
                char *p = (char *) Field(v, i);
                if(p >= caml_young_end && p < caml_young_ptr) {
                    fprintf(stderr, "%s: Found a bogus pointer: 0x%08lx[%d] = 0x%08lx\n",
                            name, (unsigned long) v, i, (unsigned long) p);
                    fflush(stderr);
                    abort();
                }
            }
            v = (char *) &Field(v, size + 1);
        }
    }
}

value lm_heap_check(value v_name)
{
    lm_heap_check_aux1(String_val(v_name));
    lm_heap_check_aux2(String_val(v_name));
    return Val_unit;
}
