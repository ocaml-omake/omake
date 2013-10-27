/*
 * This presents a FAM-like interface for systems without FAM.
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
#ifdef FAM_PSEUDO
#ifdef FAM_ENABLED

#ifndef _FAM_PSEUDO
#define _FAM_PSEUDO

/*
 * Maximum number of directories that we will monitor.
 */
#define MAX_DIR_COUNT           1024

/*
 * Maximum file name length.
 */
#define NAME_MAX                1024

/*
 * Possible events.
 */
typedef enum {
    FAMChanged          = 1,
    FAMDeleted          = 2,
    FAMStartExecuting   = 3,
    FAMStopExecuting    = 4,
    FAMCreated          = 5,
    FAMMoved            = 6,
    FAMAcknowledge      = 7,
    FAMExists           = 8,
    FAMEndExist         = 9,
    FAMDirectoryChanged = 10    /* Not standard FAM -- member of dir changed */
} FAMCodes;

/*
 * A request is just a number.
 */
typedef struct request {
    int reqnum;
} FAMRequest;

/*
 * Events are saved in a linked list.
 */
typedef struct fam_event {
    struct fam_connection *fc;          // Server
    FAMRequest fr;                      // Request number for the directory
    FAMCodes code;                      // Event code
    void *userdata;			// User data from the directory
    char filename[NAME_MAX];            // Name of the file that changed
    struct fam_event *next;             // Linked list
} FAMEvent;

/*
 * Server has a collection of directories.
 */
typedef struct fam_connection {
    /* Unique identifier */
    int id;

    /* Directories are kept in an array by request number */
    unsigned dir_count;
    struct dir_info *dirs[MAX_DIR_COUNT];

    /* Events to pass back to the user */
    FAMEvent *event;
    FAMEvent *last;
} FAMConnection;

/*
 * Errors.
 */
#define FAM_NO_ERROR                    0
#define FAM_TOO_MANY_DIRECTORIES        1
#define FAM_DIRECTORY_DOES_NOT_EXIST    2
#define FAM_WINDOWS_ERROR               3
#define FAM_OUT_OF_MEMORY               4
#define FAM_BAD_REQUEST_NUMBER          5
#define FAM_ALREADY_EXISTS              6
#define FAM_NOT_IMPLEMENTED             7
#define FAM_PERMISSION_DENIED           8
#define FAM_UNKNOWN_ERROR					 9

extern int FAMErrno;
extern char *FamErrlist[];

int FAMOpen(FAMConnection *fc);
int FAMClose(FAMConnection *fc);
int FAMMonitorDirectory(FAMConnection *fc, const char *name, FAMRequest *request, void *userdata);
int FAMSuspendMonitor(FAMConnection *fc, FAMRequest *request);
int FAMResumeMonitor(FAMConnection *fc, FAMRequest *request);
int FAMCancelMonitor(FAMConnection *fc, FAMRequest *request);
int FAMNextEvent(FAMConnection *fc, FAMEvent *event);
int FAMPending(FAMConnection *fc);
int FAMMonitorDirectoryTree(FAMConnection *fc, const char *name, FAMRequest *request, void *userdata);

#endif /* _FAM_PSEUDO */
#endif /* FAM_ENABLED */
#endif /* FAM_PSEUDO */
