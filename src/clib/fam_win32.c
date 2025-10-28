/*
 * Implement a FAM-like service for Win32.
 * This uses the ReadDirectoryChangesW function, which is
 * available only on NT+.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@metaprl.org}
 * @end[license]
 */
#ifdef WIN32
#ifdef FAM_ENABLED

/* Disable some of the warnings */
#pragma warning( disable : 4127 4996 )

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#endif

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>

#define HAS_STDINT_H 1
#if defined(HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC)
#  define HAS_NANOSECOND_STAT 1
#elif defined(HAVE_STRUCT_STAT_ST_ATIMESPEC_TV_NSEC)
#  define HAS_NANOSECOND_STAT 2
#elif defined(HAVE_STRUCT_STAT_ST_ATIMENSEC)
#  define HAS_NANOSECOND_STAT 3
#endif

#ifndef _WIN32
/* unistd.h is assumed to be available */
#define HAS_UNISTD 1
#endif

#include "lm_compat_win32.h"
#include "fam_pseudo.h"

/*
 * The events we want to watch for.
 */
#define FILE_CHANGES    (FILE_NOTIFY_CHANGE_FILE_NAME\
                        | FILE_NOTIFY_CHANGE_SIZE\
                        | FILE_NOTIFY_CHANGE_LAST_WRITE\
                        | FILE_NOTIFY_CHANGE_CREATION)

/*
 * Size of the event buffer.  This is the maximum size we can
 * use without getting errors on SMB volumes.
 */
#define NOTIFY_BUFFER_SIZE      (1 << 16)

/*
 * Max utility.
 */
#define MAX(i, j)          ((i) < (j) ? (j) : (i))

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
    "EndExist"
};

/*
 * Unique identifiers.
 */
static int id_counter;

/*
 * Info for each directory.
 * We keep a request number, for compatibility
 * with Unix FAM.
 */
typedef struct dir_info {
    unsigned request;                   // Request number
    unsigned recursive;                 // Is the request recursive
    unsigned running;                   // Is this entry running or suspended?
    HANDLE handle;                      // Directory handle
    char buffer[NOTIFY_BUFFER_SIZE];    // Event buffer
    OVERLAPPED overlapped;              // For asynchronous IO
    void *userdata;                     // User data for this directory
    char name[1];                       // Name of the directory
} DirInfo;

/*
 * Error codes.
 */
int FAMErrno = 0;

char *FamErrlist[] = {
    "FAM: No Error",
    "FAM: Too many directories",
    "FAM: Directory does not exist",
    "FAM: Windows error",
    "FAM: Out of memory",
    "FAM: Bad request number",
    "FAM: Request already exists",
    "FAM: Not implemented"
};

/************************************************************************
 * LOCAL FUNCTIONS
 */

/*
 * Close a directory entry.
 */
static void free_dir(DirInfo *dir)
{
    CloseHandle(dir->overlapped.hEvent);
    CloseHandle(dir->handle);
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
 * Print an error message.
 */
static void print_error_code(const char *name, DWORD code)
{
    LPTSTR buffer;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,  // Format flags
                  NULL,                                                         // Location of the message
                  code,                                                         // Error code
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),                    // Language
                  (LPTSTR) &buffer,                                             // Message buffer
                  0,                                                            // Buffer size
                  NULL);                                                        // Arguments

    /* Print the message */
    fprintf(stderr, "%s: failed with code %d: %s\n", name, errno, buffer);
    fflush(stderr);
    LocalFree(buffer);
}

static void print_error(const char *name)
{
    print_error_code(name, GetLastError());
}

/*
 * Start monitoring a directory.
 */
static void monitor_start(DirInfo *dir)
{
    BOOL code;

#ifdef FAM_DEBUG
    fprintf(stderr, "Monitoring directory %s\n", dir->name);
    fflush(stderr);
#endif
    code = ReadDirectoryChangesW(dir->handle,       // Directory
                                 dir->buffer,       // Result buffer
                                 sizeof(dir->buffer),
                                 dir->recursive,    // Monitor subdirectories?
                                 FILE_CHANGES,      // Monitor the standard changes
                                 NULL,              // Return length is ignored for async IO
                                 &dir->overlapped,  // Use asynchronous IO
                                 NULL);             // Completion routine, we'll poll for results
    if(code == 0)
        print_error("ReadDirectoryChangesW");
}

/*
 * Wait for an event to happen.
 */
static int monitor_wait(FAMConnection *fc, DWORD interval)
{
    HANDLE handles[MAX_DIR_COUNT];
    unsigned map[MAX_DIR_COUNT];
    unsigned i, ncount;
    DirInfo *dir;
    DWORD status;
    DWORD code;

    /* Make an array of the events to wait on */
    ncount = 0;
    for(i = 0; i != fc->dir_count; i++) {
        dir = fc->dirs[i];
        if(dir && dir->running) {
            handles[ncount] = dir->overlapped.hEvent;
            map[ncount] = i;
            ncount++;
        }
    }

    /*
     * Now wait for an event
     *
     * The enter/leave_blocking_section is performed in
     * omake_cnotify.c now.
     */
    // enter_blocking_section();
    status = WaitForMultipleObjects(ncount, handles, FALSE, interval);
    if(status == WAIT_FAILED)
        code = GetLastError();
    // leave_blocking_section();

    /* Return the index of the event */
    if(status >= WAIT_OBJECT_0 && status < WAIT_OBJECT_0 + ncount)
        status = map[status - WAIT_OBJECT_0];
    else if(status >= WAIT_ABANDONED_0 && status < WAIT_ABANDONED_0 + ncount)
        status = map[status - WAIT_ABANDONED_0];
    else {
#ifdef FAM_DEBUG
        fprintf(stderr, "WaitForMultipleObjects: status=%d ncount=%d\n", status, ncount);
        print_error_code("WaitForMultipleObjects", code);
#endif
        status = WAIT_FAILED;
    }
#ifdef FAM_DEBUG
    fprintf(stderr, "Woke up on request %d\n", status);
    fflush(stderr);
#endif
    return (status == WAIT_FAILED ? -1 : 0);
}

/*
 * Create events from the completed monitor.
 */
static void monitor_read(FAMConnection *fc, unsigned request)
{
    const FILE_NOTIFY_INFORMATION *info;
    unsigned dir_length, name_length, i;
    char name[NAME_MAX];
    char *buffer;
    FAMEvent *event;
    FAMCodes fam_code;
    DirInfo *dir;
    DWORD length;
    BOOL code;

    /* Get the result */
    length = 0;
    dir = fc->dirs[request];
    code = GetOverlappedResult(dir->handle,             // Directory
                               &dir->overlapped,        // Overlapped result
                               &length,                 // Number of bytes transfered
                               FALSE);                  // Do not wait

    /* Add the events */
    if(code && length) {
        buffer = dir->buffer;
        while(1) {
            info = (const FILE_NOTIFY_INFORMATION *) buffer;

            /* Get the action */
            switch(info->Action) {
            case FILE_ACTION_ADDED:
                fam_code = FAMCreated;
                break;
            case FILE_ACTION_REMOVED:
                fam_code = FAMDeleted;
                break;
            case FILE_ACTION_MODIFIED:
                fam_code = FAMChanged;
                break;
            case FILE_ACTION_RENAMED_OLD_NAME:
                fam_code = FAMDeleted;
                break;
            case FILE_ACTION_RENAMED_NEW_NAME:
                fam_code = FAMCreated;
                break;
            default:
                fam_code = FAMCreated;
                break;
            }

            /* Get the long name */
            dir_length = strlen(dir->name);
            name_length = info->FileNameLength / 2;
            length = dir_length + name_length + 2;
            if(length < NAME_MAX) {
                strcpy(name, dir->name);
                name[dir_length] = '\\';
                for(i = 0; i != name_length; i++)
                    name[dir_length + i + 1] = (char) info->FileName[i];
                name[length - 1] = 0;

                /* Create the event struct */
                event = (FAMEvent *) malloc(sizeof(*event));
                if(event) {
                    event->fc = fc;
                    event->fr.reqnum = dir->request;
                    event->userdata = dir->userdata;
                    event->code = fam_code;
                    event->next = 0;
                    if(CompatGetLongPathName(name, event->filename, NAME_MAX) == 0)
                        strcpy(event->filename, name);
                    if(fc->last)
                        fc->last = fc->last->next = event;
                    else
                        fc->event = fc->last = event;
                }
            }

            /* Go to the next record */
            if(info->NextEntryOffset)
                buffer += info->NextEntryOffset;
            else
                break;
        }
    }

    /* Restart the monitor */
    if(dir->running)
        monitor_start(dir);
}

/*
 * Monitor a directory.
 * The request may be recursive.
 */
static int monitor_directory(FAMConnection *fc, const char *name, FAMRequest *requestp, void *userdata, int recursive)
{
    HANDLE dir_handle, change_handle;
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

    /* Get a handle to the directory for synchronous operation */
    dir_handle = CreateFile(name,
                            GENERIC_READ,
                            FILE_SHARE_READ | FILE_SHARE_DELETE | FILE_SHARE_WRITE,
                            NULL,
                            OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
                            NULL);
    if(dir_handle == INVALID_HANDLE_VALUE) {
        FAMErrno = FAM_DIRECTORY_DOES_NOT_EXIST;
        return -1;
    }

    /* We will be using asynchronous operations */
    change_handle = CreateEvent(NULL,           // Default security
                                FALSE,          // Auto-reset
                                FALSE,          // Initially non-signaled
                                NULL);          // No name
    if(change_handle == INVALID_HANDLE_VALUE) {
        CloseHandle(dir_handle);
        FAMErrno = FAM_WINDOWS_ERROR;
        return -1;
    }

    /* Allocate a directory struct */
    length = sizeof(DirInfo) + strlen(name);
    dir = (DirInfo *) malloc(length);
    if(dir == 0) {
        CloseHandle(dir_handle);
        CloseHandle(change_handle);
        FAMErrno = FAM_OUT_OF_MEMORY;
        return -1;
    }
    memset(dir, 0, length);

    /* Initialize */
    dir->request = request;
    dir->recursive = recursive;
    dir->running = 1;
    dir->handle = dir_handle;
    dir->overlapped.hEvent = change_handle;
    dir->userdata = userdata;
    strcpy(dir->name, name);

    /* Save it in the fc */
    fc->dirs[request] = dir;
    fc->dir_count = MAX(request + 1, fc->dir_count);

    /* Start polling */
    monitor_start(dir);
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
    memset(fc, 0, sizeof(*fc));
    fc->id = ++id_counter;
    return 0;
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
        free_dir(fc->dirs[i]);

    /* Free all the events */
    event = fc->event;
    while(event) {
        next = event->next;
        free_event(event);
        event = next;
    }

    /* Reset the fc */
    memset(fc, 0, sizeof(*fc));
    return 0;
}

/*
 * Monitor a directory.
 */
int FAMMonitorDirectory(FAMConnection *fc, const char *name, FAMRequest *requestp, void *userdata)
{
    return monitor_directory(fc, name, requestp, userdata, 0);
}

int FAMMonitorDirectoryTree(FAMConnection *fc, const char *name, FAMRequest *requestp, void *userdata)
{
    return monitor_directory(fc, name, requestp, userdata, 1);
}

/*
 * Suspend monitoring.
 */
int FAMSuspendMonitor(FAMConnection *fc, FAMRequest *requestp)
{
    DirInfo *dir;
    unsigned request;

    request = requestp->reqnum;
    if(request >= fc->dir_count || fc->dirs[request] == 0) {
        FAMErrno = FAM_BAD_REQUEST_NUMBER;
        return -1;
    }
    dir = fc->dirs[request];
    if(dir->running) {
        CancelIo(dir->handle);
        dir->running = 0;
    }
    return 0;
}

/*
 * Resume monitoring.
 */
int FAMResumeMonitor(FAMConnection *fc, FAMRequest *requestp)
{
    DirInfo *dir;
    unsigned request;

    request = requestp->reqnum;
    if(request >= fc->dir_count || fc->dirs[request] == 0) {
        FAMErrno = FAM_BAD_REQUEST_NUMBER;
        return -1;
    }
    dir = fc->dirs[request];
    if(dir->running == 0) {
        dir->running = 1;
        monitor_start(dir);
    }
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
    free_dir(dir);
    fc->dirs[request] = 0;
    return 0;
}

/*
 * Get the next event.
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
            if(fc->event == 0)
                fc->last = 0;
            free_event(current);
            return 0;
        }

        /* If not, wait for an event */
        request = monitor_wait(fc, INFINITE);
        if(request < 0)
            return -1;
        monitor_read(fc, request);
    }
}

/*
 * See if there is a pending event.
 */
int FAMPending(FAMConnection *fc)
{
    int request;

    while(1) {
        /* See if there is already an event */
        if(fc->event)
            return 1;

        /* If not, poll for input */
        request = monitor_wait(fc, 0);
        if(request < 0)
            return 0;
        monitor_read(fc, request);
    }
}

#endif /* FAM_ENABLED */
#endif /* WIN32 */
