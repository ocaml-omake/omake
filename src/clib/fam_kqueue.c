/*
 * This implements a FAM-like service for systems that support the FreeBSD
 * kqueue interface.  In particular, this has been tested on Mac OS X 10.3.
 *
 * Here's the basic design:
 *  We keep an array of DirInfo pointers.  When the client requests a dir to
 *  be monitored, we find a free slot and allocate a DirInfo for it.  We then
 *  create a kevent_t to monitor that directory, storing a pointer to its
 *  DirInfo as its userdata.
 *
 *  When we retrieve an event, we turn it into one or more FAMEvents, putting
 *  each new event on a per-FAMConnection queue
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Nathaniel Gray, Caltech
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
 * Author: Nathaniel Gray
 * @email{n8gray@cs.caltech.edu}
 * @end[license]
 */
#ifdef FAM_KQUEUE

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <errno.h>

#ifdef HAVE_STRING_H
#include <string.h>
#else /* HAVE_STRING_H */
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif /* HAVE_STRINGS_H */
#endif /* HAVE_STRING_H */

#include "fam_pseudo.h"

/*
 * The events we want to watch for.
 *
 * Monitoring a directory foo containing bar
 *      When I do:          I get flags:
 *      create bar          NOTE_ATTRIB NOTE_WRITE
 *      edit bar & save     NOTE_ATTRIB NOTE_WRITE
 *      rm -f bar           NOTE_ATTRIB NOTE_WRITE
 *      chmod 000 bar       <nothing>
 *      touch existing bar  <nothing>
 *      edit bar via link   <nothing>
 *      rm -rf foo          NOTE_DELETE
 *      mv foo floo         NOTE_RENAME
 *      chmod 000 foo       NOTE_ATTRIB
 *      unmount the fs      NOTE_REVOKE
 *
 * I guess we'll just look for writes for now.
 */
#define MON_FLAGS    (NOTE_WRITE) /* | NOTE_DELETE | NOTE_RENAME | NOTE_REVOKE
                        | NOTE_ATTRIB) | NOTE_EXTEND | NOTE_LINK */

/* Any write to the directory means a file within changed */
#define MEMBER_CHANGED_FLAGS NOTE_WRITE

/*
 * The mode for opening monitored directories.  Someday maybe we can do
 * this without even needing read perms.
 */
#define DIR_OPEN_FLAGS O_RDONLY

/*
 * Max utility.
 */
#define MAX(i, j)               ((i) < (j) ? (j) : (i))

static char *code_names[] = {
    "No Code",
    "Changed",
    "Deleted",
    "StartExecuting",
    "StopExecuting",
    "Created",
    "Moved",
    "Acknowledge",
    "Exists",
    "EndExist",
    "DirectoryChanged"
};

typedef struct kevent kevent_t;

/*
 * Info for each directory.
 * We keep a request number, for compatibility
 * with Unix FAM.
 */
typedef struct dir_info {
    unsigned request;                   // Request number
    unsigned running;                   // Is this entry running or suspended?
    int handle;                         // Directory handle
    void *userdata;                     // User data for this directory
    kevent_t *kevent;
    char name[1];                       // Name of the directory
} DirInfo;

int FAMErrno = 0;

char *FamErrlist[] = {
    "No Error",
    "Too many directories",
    "Directory does not exist",
    "Windows error",
    "Out of memory",
    "Bad request number",
    "Request already exists",
    "Not implemented",
    "Permission denied",
    "Unknown error"
};

/* We need a "zero" timespec for specifying non-blocking kevent calls */
static struct timespec gTime0;

static int gRequestNum = 0;

/************************************************************************
 * LOCAL FUNCTIONS
 */

/*
 * Close a directory entry.
 */
static void free_dir(DirInfo *dir)
{
    close(dir->handle);
    if(dir->kevent)
        free(dir->kevent);
    free(dir);
}

/*
 * Free a FAM event.
 */
static void free_fevent(FAMEvent *event)
{
    free(event);
}

/*
 * Allocate a kevent
 */
static kevent_t *new_kevent() {
    kevent_t *ev;
    ev = (kevent_t *)malloc(sizeof(kevent_t));
    if (ev)
        memset(ev, 0, sizeof(*ev));
    return ev;
}

#if defined(__NetBSD__)
typedef intptr_t kqueue_udata_t;
#else
typedef void *kqueue_udata_t;
#endif

/*
 * Start monitoring a directory.
 * We store the DirInfo pointer as the userdata in the kevent.
 */
static int monitor_start(FAMConnection *fc, DirInfo *dir)
{
    int code;
    kevent_t *kev;

#ifdef FAM_DEBUG
    fprintf(stderr, "Monitoring directory %s\n", dir->name);
    fflush(stderr);
#endif
    code = -1;
    if ((kev = new_kevent())) {
        dir->kevent = kev;
        /* Register interest in the MON_FLAGS flags of the dir */
        EV_SET(kev, dir->handle, EVFILT_VNODE, EV_ADD | EV_CLEAR, MON_FLAGS,
                (intptr_t) NULL, (kqueue_udata_t) dir);
        code = kevent(fc->id, kev, 1, NULL, 0, &gTime0);
#ifdef FAM_DEBUG
        fprintf(stderr,
                "  Directory id: %d\n"
                "  kevent code: %d\n", dir->handle, code);
        fflush(stderr);
#endif
    }
    if(code < 0)
    {
        if(kev)
        {
            free(kev);
            dir->kevent = NULL;
        }
        perror("monitor_start");
    }
    return code;
}

/* Put the new event at the tail end of the queue */
static int add_famevent_of_kevent( FAMConnection *fc,
        kevent_t *kev, FAMCodes fcode )
{
    DirInfo *dir_info;
    FAMEvent *fevent;

    fevent = (FAMEvent *)malloc(sizeof(FAMEvent));
    if(!fevent)
        return 0;

    dir_info = (DirInfo *)kev->udata;
    fevent->fc = fc;
    fevent->fr.reqnum = dir_info->request;
    fevent->code = fcode;
    fevent->userdata = dir_info->userdata;
    fevent->next = NULL;
    if( NULL != fc->last ) {
        fc->last->next = fevent;
    } else {
        fc->event = fevent;
    }
    fc->last = fevent;
    strncpy(fevent->filename, dir_info->name, NAME_MAX);
    return 1;
}


/*
 * Poll for an event and put any on the fc->event list
 */
static int poll_kevent(FAMConnection *fc, struct timespec *timeptr)
{
    int result, more_events=1;
    FAMEvent *fevent;
    kevent_t *kev;

#ifdef FAM_DEBUG
    fprintf(stderr, "Polling FAM Connection\n");
    fflush(stderr);
#endif

    if ((kev = new_kevent())) {
        result = kevent( fc->id, NULL, 0, kev, 1, timeptr );
        if(result == 0) {
            free(kev);
            return 0;
        } else if(result < 0) {
            free(kev);
            switch(errno) {
                case EACCES:
                    FAMErrno = FAM_PERMISSION_DENIED;
                    break;
                case ENOMEM:
                    FAMErrno = FAM_OUT_OF_MEMORY;
                    break;
                default:
                    FAMErrno = FAM_UNKNOWN_ERROR;
            };
            return -1;
        }

        /* Convert each possible event flag to a FAM event */
        if(kev->fflags & MEMBER_CHANGED_FLAGS) {
            if (add_famevent_of_kevent( fc, kev, FAMDirectoryChanged ) == 0)
                goto poll_error;
        };
        if(kev->fflags & NOTE_DELETE) {
            if (add_famevent_of_kevent( fc, kev, FAMDeleted ) == 0)
                goto poll_error;
        };
        if(kev->fflags & NOTE_RENAME) {
            if (add_famevent_of_kevent( fc, kev, FAMMoved ) == 0)
                goto poll_error;
        };
        if(kev->fflags & NOTE_REVOKE) { /* Lost access */
            if (add_famevent_of_kevent( fc, kev, FAMDeleted ) == 0)
                goto poll_error;
        };
        if(kev->fflags & NOTE_ATTRIB) {
            if (add_famevent_of_kevent( fc, kev, FAMChanged ) == 0)
                goto poll_error;
        };
        free(kev);
        return 1;
poll_error:
        free(kev);
    }
    FAMErrno = FAM_OUT_OF_MEMORY;
    return -1;
}

/************************************************************************
 * Public functions.
 */

/*
 * Open the server.
 */
int FAMOpen(FAMConnection *fc)
{
    int id;
#ifdef FAM_DEBUG
    fprintf(stderr, "Opening FAM Connection\n");
    fflush(stderr);
#endif
    memset(&gTime0, 0, sizeof(struct timespec));
    memset(fc, 0, sizeof(*fc));
    id = kqueue();
#ifdef FAM_DEBUG
    fprintf(stderr, "kqueue file descriptor: %d\n", id);
    fflush(stderr);
#endif
    if (id < 0)
        return -1;
    else {
        fc->id = id;
        return 0;
    }
}

/*
 * Close the fc.
 */
int FAMClose(FAMConnection *fc)
{
    FAMEvent *event, *next;
    unsigned i;

    /* Free all the directories */
    for(i = 0; i != fc->dir_count; i++)
        if(fc->dirs[i])
            free_dir(fc->dirs[i]);

    /* Free all the events */
    event = fc->event;
    while(event) {
        next = event->next;
        free_fevent(event);
        event = next;
    }

    /* Reset the fc */
    close(fc->id);
    memset(fc, 0, sizeof(*fc));
    return 0;
}

/*
 * Monitor a directory.
 */
int FAMMonitorDirectory(FAMConnection *fc, const char *name, FAMRequest *requestp, void *userdata)
{
    int dir_handle;
    unsigned request, length;
    DirInfo *dir;

#ifdef FAM_DEBUG
    fprintf(stderr, "Asking to monitor directory: %s\n", name);
    fflush(stderr);
#endif

    /* Search for a slot */
    for(request = 0; request != fc->dir_count; request++) {
        if(fc->dirs[request] == 0)
            break;
    }

    /* Watch for overflows */
    if(request == MAX_DIR_COUNT) {
        FAMErrno = FAM_TOO_MANY_DIRECTORIES;
        return -1;
    }
    requestp->reqnum = request;

    /* Get a descriptor for the directory */
    dir_handle = open(name, DIR_OPEN_FLAGS, 0);

    if(dir_handle < 0) {
        /* This is potentially bogus.  It could be any number of things. */
        FAMErrno = FAM_DIRECTORY_DOES_NOT_EXIST;
        return -1;
    }

    /* Allocate a directory struct */
    length = sizeof(DirInfo) + strlen(name);
    dir = (DirInfo *) malloc(length);
    if(dir == 0) {
        close(dir_handle);
        FAMErrno = FAM_OUT_OF_MEMORY;
        return -1;
    }
    memset(dir, 0, length);

    /* Initialize */
    dir->request = request;
    dir->running = 1;
    dir->handle = dir_handle;
    dir->userdata = userdata;
    strcpy(dir->name, name);

    /* Save it in the fc */
    fc->dirs[request] = dir;
    fc->dir_count = MAX(request + 1, fc->dir_count);

    /* Start monitoring */
    monitor_start(fc, dir);
    return 0;
}

/*
 * Cancel monitoring.
 */
int FAMCancelMonitor(FAMConnection *fc, FAMRequest *requestp)
{
    DirInfo *dir;
    unsigned request;
    int code = 0;
    kevent_t *kev;

    request = requestp->reqnum;
    if(request >= fc->dir_count || fc->dirs[request] == 0) {
        FAMErrno = FAM_BAD_REQUEST_NUMBER;
        return -1;
    }
    dir = fc->dirs[request];
    kev = dir->kevent;
    kev->flags = EV_DELETE;
    if (kevent(fc->id, kev, 1, NULL, 0, &gTime0) < 0)
        perror("FAMCancelMonitor");
    free_dir(dir);
    fc->dirs[request] = 0;
    return 0;
}

/*
 * Get the next event.  Block for one if there is no event.
 */
int FAMNextEvent(FAMConnection *fc, FAMEvent *event)
{
    FAMEvent *current;
    int request;

    while(1) {
        /* See if there is already an event */
        current = fc->event;
        if(current) {
#if FAM_DEBUG
            fprintf(stderr, "Request: %d, Name: %s, Event: %s\n", current->fr.reqnum, current->filename, code_names[current->code]);
            fflush(stderr);
#endif
            *event = *current;
            fc->event = current->next;
            if(fc->event == NULL)
                fc->last = NULL;
            free_fevent(current);
            return 0;
        }

        /* If not, blocking poll for an event */
#if FAM_DEBUG
        fprintf( stderr, "FAMNextEvent: polling...\n" );
#endif
        request = poll_kevent(fc, NULL);
#if FAM_DEBUG
        fprintf( stderr, "FAMNextEvent: request = %d\n", request );
#endif
        if(request < 0)
            return -1;
    }
}

/*
 * See if there is a pending event.
 */
int FAMPending(FAMConnection *fc)
{
    int result;

#ifdef FAM_DEBUG
    fprintf(stderr, "Checking for pending FAM events.\n");
    fflush(stderr);
#endif

    /* See if there is already an event */
    if(fc->event)
        return 1;

    /* If not, non-blocking poll for input */
    result = poll_kevent( fc, &gTime0 );

    if(result == 0)
        return 0;
    else if(result < 0)
        return -1;
    return 1;
}

/*****************************************************************************
 * Functions from the FAM interface that aren't implemented
 * The first two probably work, but they've never been tested.
 */

/*
 * Suspend monitoring.
 */
int FAMSuspendMonitor(FAMConnection *fc, FAMRequest *requestp)
{
#if 0
    DirInfo *dir;
    unsigned request;
    int code = 0;

    request = requestp->reqnum;
    if(request >= fc->dir_count || fc->dirs[request] == 0) {
        FAMErrno = FAM_BAD_REQUEST_NUMBER;
        return -1;
    }
    dir = fc->dirs[request];
    if(dir->running) {
        kevent_t *kev = dir->kevent;
        kev->flags = EV_DISABLE;
        if (kevent(fc->id, kev, 1, NULL, 0, &gTime0) < 0)
            perror("FAMSuspendMonitor");
        dir->running = 0;
    }
    return 0;
#else
    FAMErrno = FAM_NOT_IMPLEMENTED;
    return -1;
#endif
}

/*
 * Resume monitoring.
 */
int FAMResumeMonitor(FAMConnection *fc, FAMRequest *requestp)
{
#if 0
    DirInfo *dir;
    unsigned request;
    int code = 0;

    request = requestp->reqnum;
    if(request >= fc->dir_count || fc->dirs[request] == 0) {
        FAMErrno = FAM_BAD_REQUEST_NUMBER;
        return -1;
    }
    dir = fc->dirs[request];
    if(!(dir->running)) {
        kevent_t *kev = dir->kevent;
        kev->flags = EV_ENABLE;
        if (kevent(fc->id, kev, 1, NULL, 0, &gTime0) < 0)
            perror("FAMResumeMonitor");
        dir->running = 1;
    }
    return 0;
#else
    FAMErrno = FAM_NOT_IMPLEMENTED;
    return -1;
#endif
}

/* This is not supposed to be called on kqueue systems */
int FAMMonitorDirectoryTree(FAMConnection *fc, const char *name, FAMRequest *request, void *userdata)
{
    FAMErrno = FAM_NOT_IMPLEMENTED;
    return -1;
}

#else /* FAM_KQUEUE */

#if defined(WIN32) || defined(_WIN32)
/* Disable the "translation unit is empty" warning */
#pragma warning( disable : 4206 )
#endif

#endif /* FAM_KQUEUE */
