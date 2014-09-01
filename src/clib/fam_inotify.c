/*
 * This implements a FAM-like service based on the inotify Linux interface.
 * For simplicity, we turn this into a FAM-like interface.
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 */
#ifdef FAM_INOTIFY

#include <sys/types.h>
#include <sys/time.h>

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#include "fam_pseudo.h"

#ifdef HAVE_INOTIFY_H
#  include <sys/inotify.h>
#else
#  include "inotify.h"
#  include "inotify-syscalls.h"
#endif

/*
 * Max utility.
 */
#define MAX(i, j)               ((i) < (j) ? (j) : (i))
#define MIN(i, j)               ((i) < (j) ? (i) : (j))

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

/*
 * Info for each directory.
 * We keep a request number, for compatibility
 * with Unix FAM.
 */
typedef struct dir_info {
    unsigned request;                   // Request number
    int wd;                             // Watch descriptor
    void *userdata;                     // User data for this directory
    char name[1];                       // Name of the directory
} DirInfo;

/************************************************************************
 * LOCAL FUNCTIONS
 */

/*
 * Close a directory entry.
 */
static void free_dir(DirInfo *dir)
{
    free(dir);
}

/*
 * Free an event.
 */
static void free_event(FAMEvent *event)
{
    free(event);
}

/*
 * Events are read directly from the file descriptor.
 * Use select(2) to check if there is pending input.
 */
static int monitor_poll(FAMConnection *fc)
{
    fd_set fd_in;
    struct timeval timeout;
    
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    FD_ZERO(&fd_in);
    FD_SET(fc->id, &fd_in);
    select(fc->id + 1, &fd_in, 0, 0, &timeout);
    return FD_ISSET(fc->id, &fd_in);
}

/*
 * The FAM code corresponding to an inotify code.
 */
static FAMCodes fam_code(int mask)
{
    FAMCodes code;

    if(mask & (IN_MOVED_FROM | IN_MOVED_TO))
        code = FAMMoved;
    else if(mask & IN_DELETE)
        code = FAMDeleted;
    else 
        code = FAMChanged;
    return code;
}

static int monitor_read(FAMConnection *fc)
{
    char buf[(sizeof(struct inotify_event) + 64) * 256];
    struct inotify_event *ievent;
    int i, len, amount, rd;
    FAMEvent *eventp;
    DirInfo *dirp;

    /* Try reading multiple events at once */
    len = read(fc->id, buf, sizeof(buf));
    if(len < 0) {
        perror ("read");
        return -1;
    }

    /* Turn each even into a FAM event */
    i = 0;
    while(i < len) {
        ievent = (struct inotify_event *) &buf[i];

        /* Find the directory this belongs to */
        for(rd = 0; rd != fc->dir_count; rd++) {
            dirp = fc->dirs[rd];
            if(dirp && dirp->wd == ievent->wd)
                break;
        }
        if(rd == fc->dir_count) {
            // This wan't happen if things are working correctly
            fprintf(stderr, "Inotify: bogus watch descriptor: %d\n", ievent->wd);
            break;
        }

        /* Translate it to a FAM event */
        eventp = malloc(sizeof(FAMEvent));
        if(eventp == 0)
            break;
        eventp->fc = fc;
        eventp->fr.reqnum = rd;
        eventp->userdata = dirp->userdata;
        eventp->code = fam_code(ievent->mask);
        eventp->next = 0;

        /* Copy the name */
        amount = MIN(NAME_MAX - 1, ievent->len);
        memcpy(eventp->filename, ievent->name, amount);
        eventp->filename[amount] = 0;

        /* Link it in */
        if(fc->last == 0)
            fc->event = fc->last = eventp;
        else
            fc->last = fc->last->next = eventp;
        i += sizeof(struct inotify_event) + ievent->len;
    }
    return 0;
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

    memset(fc, 0, sizeof(*fc));
    fc->id = inotify_init();
    if(fc ->id < 0)
        perror("inotify_init");
    return fc->id;
}

/*
 * Close the fc.
 */
int FAMClose(FAMConnection *fc)
{
    FAMEvent *event, *next;
    int i;

    /* Free all the directories */
    for(i = 0; i != MAX_DIR_COUNT; i++) {
        if(fc->dirs[i])
            free_dir(fc->dirs[i]);
    }

    /* Free all the events */
    event = fc->event;
    while(event) {
        next = event->next;
        free_event(event);
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
    unsigned request, length;
    DirInfo *dir;
    int i, wd;

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

    /* Add the watch */
    wd = inotify_add_watch(fc->id, name, IN_MODIFY | IN_CLOSE_WRITE | IN_MOVED_TO | IN_DELETE | IN_CREATE | IN_ATTRIB);
    if(wd < 0) {
        FAMErrno = FAM_DIRECTORY_DOES_NOT_EXIST;
        return -1;
    }

    /* Allocate a directory struct */
    length = sizeof(DirInfo) + strlen(name);
    dir = (DirInfo *) malloc(length);
    if(dir == 0) {
        FAMErrno = FAM_OUT_OF_MEMORY;
        return -1;
    }
    memset(dir, 0, length);

    /* Initialize */
    dir->request = request;
    dir->userdata = userdata;
    dir->wd = wd;
    strcpy(dir->name, name);

    /* Save it in the fc */
    fc->dirs[request] = dir;
    fc->dir_count = MAX(request + 1, fc->dir_count);
    return 0;
}

/*
 * Cancel monitoring.
 */
int FAMCancelMonitor(FAMConnection *fc, FAMRequest *requestp)
{
    DirInfo *dir;
    unsigned request;

    request = requestp->reqnum;
    if(request >= fc->dir_count || fc->dirs[request] == 0) {
        FAMErrno = FAM_BAD_REQUEST_NUMBER;
        return -1;
    }
    dir = fc->dirs[request];
    inotify_rm_watch(fc->id, dir->wd);
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
            free_event(current);
            return 0;
        }

        /* If not, blocking poll for an event */
#if FAM_DEBUG
        fprintf( stderr, "FAMNextEvent: blocking...\n" );
#endif
        request = monitor_read(fc);
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
    return monitor_poll(fc);
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
    FAMErrno = FAM_NOT_IMPLEMENTED;
    return -1;
}

/*
 * Resume monitoring.
 */
int FAMResumeMonitor(FAMConnection *fc, FAMRequest *requestp)
{
    FAMErrno = FAM_NOT_IMPLEMENTED;
    return -1;
}

int FAMMonitorDirectoryTree(FAMConnection *fc, const char *name, FAMRequest *requestp, void *userdata)
{
    FAMErrno = FAM_NOT_IMPLEMENTED;
    return -1;
}
#else /* FAM_INOTIFY */

#if defined(WIN32) || defined(_WIN32)
/* Disable the "translation unit is empty" warning */
#pragma warning( disable : 4206 )
#endif

#endif /* FAM_INOTIFY */

