/*
 * Provide an mmap implementation of a file.  This
 * returns a large string to ML.
 *
 * ------------------------------------------------------------
 *
 * This is part of the Ensemble Juke Box, a program for
 * distributed digital audio.
 *
 * Copyright (C) 1997 Cornell University
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
 * Authors: Jason Hickey, Mark Hayden
 * jyh,hayden@cs.cornell.edu
 * http://www.cs.cornell.edu/home/jyh/ejb/index.html
 */

/* Standard includes */
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>

#ifdef _WIN32
/* Windows include files */
#  include <windows.h>

#else /* _WIN32 */

/* Unix include files */
#  include <unistd.h>
#  include <sys/file.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/mman.h>
#  include <fcntl.h>
#endif /* _WIN32 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/************************************************************************
 * TYPES                                                                *
 ************************************************************************/

/*
 * Maps are all this size.
 */
#define MAX_SIZE        (1 << 23)

/*
 * Info we keep per file.
 */
typedef struct mmap {
#ifdef _WIN32
    HANDLE fd, map;
#else /* _WIN32 */
    int fd;
#endif /* _WIN32 */
    int size;
    char *data;
} Mmap;

/************************************************************************
 * FILE OPERATIONS                                                      *
 ************************************************************************/

/*
 * All these functions have the same usage as in the Unix library,
 * except open also specifies a write-ahead parameter.
 */
static Mmap *mmap_open(const char *name, int perm, int mode,
                       int append, int sequential, int flags)
{
    Mmap *infop;
    int prot;

#ifdef _WIN32
    HANDLE fd, map;
    int mapped, size;
    char *data;

    /*
     * Open the file.
     * Options to consider:
     *    SEQUENTIAL_SCAN
     */
    fd = CreateFile(name, perm, 0, 0,
                    flags, FILE_ATTRIBUTE_NORMAL,
                    0);
    if(fd == INVALID_HANDLE_VALUE)
        failwith("mmap_open");

    /*
     * Create a map, but do not allocate storage.
     */
    if(perm & GENERIC_WRITE) {
        prot = PAGE_READWRITE | SEC_RESERVE;
        size = MAX_SIZE;
        mapped = 0;
    }
    else {
        prot = PAGE_READONLY;
        size = GetFileSize(fd, 0);
        mapped = size;
    }
    map = CreateFileMapping(fd, 0, prot, 0, size, 0);
    if(map == 0) {
        CloseHandle(fd);
        failwith("mmap_open");
    }

    /*
     * Map the region of memory.
     */
    if(perm & GENERIC_WRITE)
        prot = FILE_MAP_WRITE;
    else
        prot = FILE_MAP_READ;
    data = MapViewOfFile(map, prot, 0, 0, size);
    if(data == 0) {
        CloseHandle(map);
        CloseHandle(fd);
        failwith("mmap_open");
    }

    /*
     * Return the struct.
     */
    infop = (Mmap *) malloc(sizeof(Mmap));
    if(infop == 0) {
        UnmapViewOfFile(data);
        CloseHandle(map);
        CloseHandle(fd);
        failwith("mmap_open");
    }
    memset(infop, 0, sizeof(infop));
    infop->fd = fd;
    infop->map = map;
    infop->size = mapped;
    infop->data = data;

#else /* _WIN32 */
    struct stat buf;

    /* Open the file and get the existing size */
    int fd = open(name, perm, mode);
    if(fd < 0)
        failwith("mmap_open");
    if(fstat(fd, &buf) < 0) {
        close(fd);
        failwith("mmap_open");
    }

    /* Figure out read/write */
    if(perm & O_RDWR)
        prot = PROT_READ | PROT_WRITE;
    else
        prot = PROT_READ;

    /* Extend the file if necessary */
    if(buf.st_size < MAX_SIZE && prot & PROT_WRITE) {
        char c = 0;
        lseek(fd, MAX_SIZE - 1, SEEK_SET);
        write(fd, &c, 1);
    }

    /* Allocate the map */
    infop = (Mmap *) malloc(sizeof(Mmap));
    if(infop == 0) {
        close(fd);
        failwith("mmap_open");
    }
    infop->fd = fd;
    infop->size = MAX_SIZE;
    infop->data = mmap((char *) 0, MAX_SIZE, prot, MAP_SHARED, fd, 0);
    if(infop->data == (char *) -1) {
        close(fd);
        free(infop);
        failwith("mmap");
    }
#endif /* _WIN32 */

    return infop;
}

/*
 * Close the file.
 * Release all resources.
 */
static void mmap_close(Mmap *mmap)
{
#ifdef _WIN32
    if(mmap->size)
        VirtualFree(mmap->data, mmap->size, 0);
    UnmapViewOfFile(mmap->data);
    CloseHandle(mmap->map);
    CloseHandle(mmap->fd);

#else /* _WIN32 */
    munmap(mmap->data, MAX_SIZE);
    ftruncate(mmap->fd, mmap->size);
    fprintf(stderr, "mmap_close: %d\n", mmap->fd);
    close(mmap->fd);

#endif /* _WIN32 */
    free(mmap);
}

/************************************************************************
 * ML INTERFACE                                                         *
 ************************************************************************/

value ml_mmap_open(value name, value perms, value mode)
{
    int perm, append, flags;

    /* Collect permission */
    perm = 0;
    append = 0;
#ifdef _WIN32
    flags = OPEN_EXISTING;
#else /* _WIN32 */
    flags = 0;
#endif /* _WIN32 */
    while(perms != Val_int(0)) {
        switch(Int_val(Field(perms, 0))) {
        case 0:
#ifdef _WIN32
            perm |= GENERIC_READ;
#else
            perm |= O_RDONLY;
#endif
            break;
        case 1:
#ifdef _WIN32
            perm |= GENERIC_WRITE;
#else
            perm |= O_WRONLY;
#endif
            break;
        case 2:
#ifdef _WIN32
            perm |= GENERIC_READ | GENERIC_WRITE;
#else
            perm |= O_RDWR;
#endif
            break;
        case 3:
#ifndef _WIN32
            perm |= O_NONBLOCK;
#endif
            break;
        case 4:
#ifndef _WIN32
            perm |= O_APPEND;
#endif
            append = 1;
            break;
        case 5:
#ifdef _WIN32
            flags = OPEN_ALWAYS;
#else
            perm |= O_CREAT;
#endif
            break;
        case 6:
#ifdef _WIN32
            flags = CREATE_ALWAYS;
#else
            perm |= O_TRUNC;
#endif
            break;
        case 7:
#ifndef _WIN32
            perm |= O_EXCL;
#endif
            break;
        }
        perms = Field(perms, 1);
    }

    /* Return the C struct */
    return (value) mmap_open(String_val(name), perm, Int_val(mode), append, 0, flags);
}

value ml_mmap_close(value mmap)
{
    mmap_close((Mmap *) mmap);
    return Val_int(0);
}

value ml_string_of_mmap(value mmap_val)
{
    Mmap *mmap;
    value data;
    int words;

    mmap = (Mmap *) mmap_val;
    data = (value) ((header_t *) mmap->data + 1);
    words = (mmap->size >> 2) - 1;
    Hd_val(data) = (words << 10) | String_tag;
    return data;
}

/*
 *
 */
