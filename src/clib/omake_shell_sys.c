/*
 * Shell utilities.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * 
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
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

#ifdef WIN32
#include <caml/signals.h>

/* Disable some of the warnings */
#pragma warning( disable : 4127 4189 4702 4706 4996 )

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#endif

#include <windows.h>
#include <tlhelp32.h>
#include <winsock.h>

#include "unixsupport.h"
#include "lm_compat_win32.h"

#define Val_nil         (Val_int(0))

/*
 * Fields of the create_process struct.
 */
#define CREATE_PROCESS_STDIN            0
#define CREATE_PROCESS_STDOUT           1
#define CREATE_PROCESS_STDERR           2
#define CREATE_PROCESS_PGRP             3
#define CREATE_PROCESS_DIR              4
#define CREATE_PROCESS_ENV              5
#define CREATE_PROCESS_EXE              6
#define CREATE_PROCESS_ARGV             7

/*
 * Tags for Unix status codes.
 */
#define TAG_WEXITED                     0
#define TAG_WSIGNALED                   1
#define TAG_WSTOPPED                    2

/*
 * String sizes for creating commands.
 */
#define SIZEOF_ENVIRONMENT      (1 << 15)
#define SIZEOF_COMMAND          (1 << 15)

/*
 * Max number of threads we can enumerate in a process.
 */
#define MAX_THREAD_COUNT        1024

/*
 * Processes and groups.
 *
 * Win32 doesn't really have much idea of process groups.
 * The CreateProcess function just allows processes to be created
 * in a "process group" that does not get signals from the
 * console.
 *
 * However, we try to do something more.  We assign each
 * process to a process group (number).  An entire process
 * group can be signaled at once.
 *
 * However, if more than one process wants input from the
 * console, they fight it out...
 */

/* Process status */
#define STATUS_RUNNING          0
#define STATUS_STOPPED          1
#define STATUS_EXITED           2

/*
 * The root process has pid/pgrp 1.
 * The null process id usually refers to the current process.
 */
#define NULL_PID                0
#define INIT_PID                1
#define MAX_PID                 29999

/*
 * The process is group leader if pid and pgrp are the same.
 */
typedef struct _process {
    int pid;                            // Process identifier
    int pgrp;                           // Process group
    DWORD thread;                       // Identifier if this is a thread
    unsigned is_thread : 1;             // Set if this is a thread
    unsigned killed    : 1;             // Set if the process is being killed
    unsigned changed   : 1;             // Some even happened on this process
    unsigned status    : 2;             // Process status
    unsigned code      : 8;             // Exit code, if the process exited
    HANDLE handle;                      // The actual process (or event if this is a thread)
    DWORD wid;          // Process identifier
    struct _process *next;              // Linked list
} Process;

typedef struct _shell_state {
    int pid_counter;                    // Index for assigning process ids
    HANDLE changed;                     // Wake up the wait handler
    HANDLE console;                     // Console handle, if it exists
    int current_pgrp;                   // Current tty process group
    Process *processes;                 // List of processes
} ShellState;

/*
 * The state.
 */
static ShellState *state = NULL;

/************************************************
 * Utilities.
 */

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
 * Check if a string contains spaces, and return the
 * number of extra characters.
 */
static int string_escape_length(const char *strp)
{
    int c, escaped, extra;

    /* Empty arguments must be quoted too */
    if(*strp == 0)
        return 2;

    /* Check for special characters */
    extra = 0;
    escaped = 0;
    while(c = *strp++) {
        switch(c) {
        case '"':
            escaped++;
            extra++;
            break;
        case '*':
        case '?':
        case '[':
        case ']':
        case '{':
        case '}':
        case '~':
        case '\'':
            escaped++;
            break;
        default:
            if(c <= ' ')
                escaped++;
            break;
        }
    }
    return escaped ? extra + 2 : 0;
}

/*
 * Quote the string while copying.
 */
static void string_copy_escaped(char *dstp, const char *srcp)
{
    char c;

    *dstp++ = '"';
    while(c = *srcp) {
        if(c == '"')
            *dstp++ = '\\';
        *dstp++ = c;
        srcp++;
    }
    *dstp++ = '"';
}

/*
 * List all the threads in a process.
 * This may be slightly out-of-date because of the
 * delay from taking the snapshot.
 */
static int list_process_threads(DWORD pid, DWORD *threads, int limit)
{
    THREADENTRY32 entry;
    HANDLE handle;
    int index;

    /* Take a snapshot of all running threads */
    handle = CompatCreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if(handle == INVALID_HANDLE_VALUE)
        return 0;

    /*
     * Retrieve information about the first thread,
     * and exit if unsuccessful.
     */
    entry.dwSize = sizeof(THREADENTRY32);
    if(CompatThread32First(handle, &entry) == FALSE) {
        CloseHandle(handle);
        return 0;
    }

    /*
     * Now walk the thread list of the system,
     * and display information about each thread
     * associated with the specified process.
     */
    index = 0;
    do {
        if(entry.th32OwnerProcessID == pid)
            threads[index++] = entry.th32ThreadID;
    } while(CompatThread32Next(handle, &entry) && index < limit);

    CloseHandle(handle);
    return index;
}

/*
 * Suspend all the threads in the process.
 */
static int suspend_process(Process *processp)
{
    DWORD threads[MAX_THREAD_COUNT];
    HANDLE handle;
    int i, count;

#ifdef OSH_DEBUG
    fprintf(stderr, "suspend_process\n");
    fflush(stderr);
#endif

    /* Not available before WindowsXP */
    if(ExistsOpenThread() == 0) {
        fprintf(stderr, "Process control is not supported\n");
        fflush(stderr);
        return -1;
    }

    /* Don't do anything if already suspended */
    if(processp->status != STATUS_RUNNING)
        return 0;

    /* If it is a thread, suspend it */
    if(processp->is_thread) {
        handle = CompatOpenThread(THREAD_SUSPEND_RESUME, FALSE, processp->thread);
        if(handle != INVALID_HANDLE_VALUE) {
            SuspendThread(handle);
            CloseHandle(handle);
            processp->changed = 1;
            processp->status = STATUS_STOPPED;
            SetEvent(state->changed);
        }
        return 0;
    }

    /* Otherwise it is a process; suspend all the subthreads */
    if(processp->wid == 0)
        return -1;

    /* Enumerate all threads for that process */
    count = list_process_threads(processp->wid, threads, MAX_THREAD_COUNT);

#ifdef OSH_DEBUG
    fprintf(stderr, "suspend_process: suspend %d threads\n", count);
    fflush(stderr);
#endif

    /* Suspend them all */
    for(i = 0; i != count; i++) {
        handle = CompatOpenThread(THREAD_SUSPEND_RESUME, FALSE, threads[i]);
        if(handle == INVALID_HANDLE_VALUE)
            print_error("OpenThread");
        if(handle != INVALID_HANDLE_VALUE) {
            if(SuspendThread(handle) < 0)
                print_error("SuspendThread");
            CloseHandle(handle);
        }
    }

    /* Notify the wait handler */
    processp->changed = 1;
    processp->status = STATUS_STOPPED;
    SetEvent(state->changed);
    return 0;
}

/*
 * Resume all the threads in the process.
 */
static int resume_process(Process *processp)
{
    DWORD threads[MAX_THREAD_COUNT];
    HANDLE handle;
    int i, count;

    /* Not available before WindowsXP */
    if(ExistsOpenThread() == 0) {
        fprintf(stderr, "Process control is not supported\n");
        fflush(stderr);
        return -1;
    }

    /* Don't do anything if already suspended */
    if(processp->status != STATUS_STOPPED)
        return 0;

#ifdef OSH_DEBUG
    fprintf(stderr, "resume_process: pid %i, wid %i\n", processp->pid, processp->wid);
    fflush(stderr);
#endif

    /* If it is a thread, resume it */
    if(processp->is_thread) {
        handle = CompatOpenThread(THREAD_SUSPEND_RESUME, FALSE, processp->thread);
        if(handle != INVALID_HANDLE_VALUE) {
            ResumeThread(handle);
            CloseHandle(handle);
            processp->status = STATUS_RUNNING;
            SetEvent(state->changed);
        }
        return 0;
    }

    /* Get the process id */
    if(processp->wid == 0)
        return -1;

    /* Enumerate all threads for that process */
    count = list_process_threads(processp->wid, threads, MAX_THREAD_COUNT);

    /* Suspend them all */
    for(i = 0; i != count; i++) {
        handle = CompatOpenThread(THREAD_SUSPEND_RESUME, FALSE, threads[i]);
        if(handle != INVALID_HANDLE_VALUE) {
            ResumeThread(handle);
            CloseHandle(handle);
        }
    }

    processp->status = STATUS_RUNNING;
    return 0;
}

/*
 * Terminate the process.
 * Try to do this by sending the CTRL_C event.
 * If that fails, just terminate it.
 */
static int kill_process(Process *processp)
{
    /*
     * Set the killed flag.
     * For threads, this will generate an exception
     * then next time it calls check_thread.
     */
    processp->killed = 1;
    if(processp->is_thread)
        return 0;

    /* Get the process id */
    if(processp->wid == 0)
        return -1;

#if 0
    /* Resume it so it can handle the exception */
    GenerateConsoleCtrlEvent(CTRL_C_EVENT, processp->wid);
    resume_process(processp);
#endif

    /* A lot of processes ignore the CTRL_C_EVENT, so just temrinate it */
    TerminateProcess(processp->handle, 1);
    return 0;
}

/*
 * The generic handler.
 */
static int process_group_map(int (*op)(Process *processp), int pgrp)
{
    Process *processp;
    int code;

    code = -1;
    for(processp = state->processes; processp; processp = processp->next) {
        if(processp->pgrp == pgrp) {
            if(op(processp) == 0)
                code = 0;
        }
    }
    return code;
}

/*
 * Allocate a new process identifier.
 * Make sure it isn't in use as a id or a group.
 */
static int alloc_pid(void)
{
    Process *processp;
    int pid;

    do {
        /* Get the next candidate */
        if(state->pid_counter == MAX_PID)
            state->pid_counter = 0;
        pid = ++state->pid_counter;

        /* Check that it isn't used */
        for(processp = state->processes; processp; processp = processp->next) {
            if(processp->pid == pid || processp->pgrp == pid)
                break;
        }
    } while(processp);

    return pid;
}

/*
 * Terminate all jobs and exit.
 */
static void terminate_processes(void)
{
    Process *processp;
    int wait;

    /* Send all processes the termination signal */
    wait = 0;
    processp = state->processes;
    while(processp) {
        if(processp->pid != INIT_PID) {
            processp->killed = 1;
            if((processp->wid) && (processp->pid != INIT_PID)) {
#ifdef OSH_DEBUG
                fprintf(stderr, "terminate_processes: Generating CTRL-C for process pid %i, group, status %i, is_thread %i, wid %i, \n", processp->pid, processp->pgrp, processp->status, processp->is_thread, processp->wid);
                fflush(stderr);
#endif
                GenerateConsoleCtrlEvent(CTRL_C_EVENT, processp->wid);
                wait++;
            }
            resume_process(processp);
        }
        processp = processp->next;
    }

    /* Wait for them to die */
    if(wait) {
        Sleep(1000);
        processp = state->processes;
        while(processp) {
            if((processp->wid) && (processp->pid != INIT_PID)) {
#ifdef OSH_DEBUG
                fprintf(stderr, "terminate_processes: terminating process pid %i, wid %i\n", processp->pid, processp->wid);
                fflush(stderr);
#endif
                TerminateProcess(processp->handle, 1);
            }
            processp = processp->next;
        }
    }
}

/************************************************************************
 * OCaml interface.
 */

/*
 * Switch the preprocessor to send input to
 * the specified group.
 */
value omake_shell_sys_set_tty_pgrp(value v_pgrp)
{
    CAMLparam1(v_pgrp);
    int pgrp;

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_set_tty_pgrp\n");
    fflush(stderr);
#endif

    pgrp = Int_val(v_pgrp);
    if(pgrp == 0)
        pgrp = INIT_PID;
    state->current_pgrp = pgrp;
    CAMLreturn(Val_unit);
}

/*
 * Process control.
 */
value omake_shell_sys_suspend(value v_pgrp)
{
    CAMLparam1(v_pgrp);
#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_suspend\n");
    fflush(stderr);
#endif
    if(process_group_map(suspend_process, Int_val(v_pgrp)) < 0)
        caml_failwith("omake_shell_sys_suspend");
    CAMLreturn(Val_unit);
}

value omake_shell_sys_resume(value v_pgrp)
{
    CAMLparam1(v_pgrp);
#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_resume\n");
    fflush(stderr);
#endif
    if(process_group_map(resume_process, Int_val(v_pgrp)) < 0)
        caml_failwith("omake_shell_sys_resume");
    CAMLreturn(Val_unit);
}

value omake_shell_sys_kill(value v_pgrp)
{
    CAMLparam1(v_pgrp);
#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_kill\n");
    fflush(stderr);
#endif
    if(process_group_map(kill_process, Int_val(v_pgrp)) < 0)
        caml_failwith("omake_shell_sys_kill");
    CAMLreturn(Val_unit);
}

/*
 * Create a thread process.  We don't fill in the thread id
 * yet (because we don't know what it is).
 */
value omake_shell_sys_create_thread_pid(value v_pgrp)
{
    CAMLparam1(v_pgrp);
    Process *processp;
    HANDLE event;
    int pgrp;
    int pid;

    pid = alloc_pid();
#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_create_thread_pid: %d\n", pid);
    fflush(stderr);
#endif

    /* Allocate the process data */
    processp = (Process *) malloc(sizeof(Process));
    if(processp == 0)
        caml_failwith("omake_shell_sys_create_thread_pid: out of memory");
    memset(processp, 0, sizeof(Process));

    /* Create an event for waiting on the thread */
    event = CreateEvent(NULL, FALSE, FALSE, NULL);
    if(event == NULL) {
        free(processp);
        caml_failwith("omake_shell_sys_create_thread_pid: can't create event");
    }

    pgrp = Int_val(v_pgrp);
    if(pgrp == 0)
        pgrp = pid;
    processp->pid = pid;
    processp->pgrp = pgrp;
    processp->status = STATUS_RUNNING;
    processp->is_thread = 1;
    processp->handle = event;
    processp->wid = 0;
    processp->next = state->processes;
    state->processes = processp;
    CAMLreturn(Val_int(pid));
}

/*
 * Thread prologue.
 */
value omake_shell_sys_init_thread_pid(value v_pid)
{
    CAMLparam1(v_pid);
    Process *processp;
    int pid;

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_init_thread_pid\n");
    fflush(stderr);
#endif

    /* Find the process */
    pid = Int_val(v_pid);
    for(processp = state->processes; processp; processp = processp->next) {
        if(processp->pid == pid)
            break;
    }
    if(processp == 0)
        caml_raise_not_found();

    /* Process has terminated */
    processp->thread = GetCurrentThreadId();
    CAMLreturn(Val_unit);
}

/*
 * Release the thread when it terminates.
 * Don't release the process struct yet,
 * just mark it as changed.
 */
value omake_shell_sys_release_thread_pid(value v_pid, value v_code)
{
    CAMLparam2(v_pid, v_code);
    Process *processp;
    int pid;

    pid = Int_val(v_pid);
#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_release_thread_pid: %d\n", pid);
    fflush(stderr);
#endif

    /* Find the process */
    for(processp = state->processes; processp; processp = processp->next) {
#ifdef OSH_DEBUG
        fprintf(stderr, "omake_shell_sys_release_thread_pid: comparing with pid = %d\n", processp->pid);
        fflush(stderr);
#endif
        if(processp->pid == pid)
            break;
    }
    if(processp == 0)
        caml_raise_not_found();

    /* Process has terminated */
    processp->changed = 1;
    processp->status = STATUS_EXITED;
    processp->code = Int_val(v_code);
    SetEvent(processp->handle);

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_release_thread_pid: pid = %d\n", processp->pid);
    fflush(stderr);
#endif
    CAMLreturn(Val_unit);
}

/*
 * Check if the thread has been killed.
 */
value omake_shell_sys_check_thread(value v_unit)
{
    CAMLparam1(v_unit);
    Process *processp;
    DWORD id;

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_check_thread\n");
    fflush(stderr);
#endif

    /* Find the process */
    id = GetCurrentThreadId();
    for(processp = state->processes; processp; processp = processp->next) {
        if(processp->thread == id)
            break;
    }
    if(processp == 0)
        CAMLreturn(Val_int(0));

    CAMLreturn(Val_int(processp->killed));
}

/*
 * Wait for any of the processes in the group to complete.
 * There are several modes:
 *    pgrp == 0: wait for any process group leader
 *    pgrp <> 0: wait for a specific process group
 *       leader: if true, wait only for the group leader
 *               if false, wait only for the children
 *    nohang: if true, don't block
 *
 * JYH: this code has caused trouble with bogus exit codes
 * and memory corruption.  For safety:
 *    - Return a triple of results, as (bool * int * int),
 *      not a (int * Unix.process_status).
 *    - Perform all allocated within this function,
 *      hence the gotos.
 */
value omake_shell_sys_wait(value v_pgrp, value v_leader, value v_nohang)
{
    CAMLparam3(v_pgrp, v_leader, v_nohang);
    CAMLlocal1(tuple);
    int processes[MAXIMUM_WAIT_OBJECTS];
    HANDLE handles[MAXIMUM_WAIT_OBJECTS];
    Process **processpp, *processp;
    int ncount, code, pid, pgrp, leader;
    DWORD exitcode, timeout, index;

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_wait\n");
    fflush(stderr);
#endif

    /* Parameters */
    pgrp = Int_val(v_pgrp);
    leader = Int_val(v_leader);
    timeout = Int_val(v_nohang) ? 0 : INFINITE;

  restart:
    /* Collect the processes and their handles */
    ncount = 1;
    handles[0] = state->changed;
    for(processpp = &state->processes; processp = *processpp; processpp = &(*processpp)->next) {
        if((pgrp && processp->pgrp != pgrp)
           || (pgrp == 0 && processp->pgrp == INIT_PID)
           || (leader && processp->pid != processp->pgrp)
           || (leader == 0 && processp->pid == processp->pgrp)) {
            continue;
        }
        else if(processp->changed)
            goto done;
        else {
            if(ncount == MAXIMUM_WAIT_OBJECTS)
                caml_invalid_argument("omake_shell_sys_wait: too many processes");
            processes[ncount] = processp->pid;
            handles[ncount] = processp->handle;
            ncount++;
        }
    }

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_wait: waiting for %d events\n", ncount);
    fprintf(stderr, "\tpgrp = %d, leader = %d, timeout = %d\n", pgrp, leader, timeout);
    fflush(stderr);
#endif

    /* Wait for an event */
    while(1) {
        /* Perform the wait */
        caml_enter_blocking_section();
        index = WaitForMultipleObjects(ncount, handles, FALSE, timeout);
        if(index == WAIT_FAILED)
            code = GetLastError();
        caml_leave_blocking_section();

        /* See if something has changed */
        if(index == WAIT_OBJECT_0) {
            for(processpp = &state->processes; processp = *processpp; processpp = &(*processpp)->next) {
                if(processp->pgrp == pgrp && processp->changed)
                    goto done;
            }
        }
        else
            break;
    }

    /* Get the index of the event */
    if(index >= WAIT_OBJECT_0 + 1 && index < WAIT_OBJECT_0 + ncount)
        index -= WAIT_OBJECT_0;
    else if(index >= WAIT_ABANDONED_0 + 1 && index < WAIT_ABANDONED_0 + ncount)
        index -= WAIT_ABANDONED_0;
    else
        caml_raise_not_found();

    /* Adjust process */
    pid = processes[index];
    for(processpp = &state->processes; processp = *processpp; processpp = &(*processpp)->next) {
        if(processp->pid == pid)
            break;
    }

    /* If the process is not found, some other thread waited for it */
    if(processp == 0)
        goto restart;

    /* Otherwise, handle the wait */
    processp->changed = 1;
    processp->status = STATUS_EXITED;

    /* Get the return code */
    if(processp->is_thread == 0) {
        if(GetExitCodeProcess(handles[index], &exitcode) == FALSE)
            exitcode = 111;
        else if(exitcode == STILL_ACTIVE)
            goto restart;
        processp->code = exitcode;
    }

    /*
     * processpp points to the process that successfully exited.
     * Build the result as
     *    bool * pid * exitcode
     * bool is true iff exited
     */
  done:
    processp = *processpp;
    processp->changed = 0;
#ifdef OSH_DEBUG
    fprintf(stderr, "+++ wait: pid=%d, group=%d, status=%d\n", processp->pid, processp->pgrp, processp->status);
#endif
    tuple = caml_alloc_small(3, 0);
    Field(tuple, 1) = Val_int(processp->pid);
    Field(tuple, 2) = Val_int(processp->code);
    switch(processp->status) {
    case STATUS_STOPPED:
        Field(tuple, 0) = Val_false;
        break;
    case STATUS_EXITED:
        Field(tuple, 0) = Val_true;
        CloseHandle(processp->handle);
        *processpp = processp->next;
        free(processp);
        break;
    case STATUS_RUNNING:
    default:
        caml_invalid_argument("wait_process: process is running");
        break;
    }

    CAMLreturn(tuple);
}

/*
 * Create a process.
 *
 * The process may have a new environment,
 * and its own directory.
 */
value omake_shell_sys_create_process(value v_info)
{
    CAMLparam1(v_info);
    CAMLlocal2(v_envp, v_argvp);
    STARTUPINFO startup;
    PROCESS_INFORMATION process;
    char env[SIZEOF_ENVIRONMENT];
    char argv[SIZEOF_COMMAND];
    char *strp, *argp, *dir, *command;
    int i, white, count, index, length, creation_flags, status, pid, pgrp;
    Process *processp;

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_create_process\n");
    fflush(stderr);
#endif

    /* Allocate a new pid */
    pid = alloc_pid();
    pgrp = Int_val(Field(v_info, CREATE_PROCESS_PGRP));
    if(pgrp == 0)
        pgrp = pid;

    /* Collect the environment */
    v_envp = Field(v_info, CREATE_PROCESS_ENV);
    count = Wosize_val(v_envp);
    index = 0;
    env[1] = 0;
    for(i = 0; i != count; i++) {
        strp = String_val(Field(v_envp, i));
        length = strlen(strp);
        if(index + length + 2 > SIZEOF_ENVIRONMENT)
            caml_failwith("omake_shell_sys_create_process: environment is too big");
        strcpy(env + index, strp);
        index += length + 1;
    }
    env[index] = 0;

    /* Collect the arguments */
    command = String_val(Field(v_info, CREATE_PROCESS_EXE));
    v_argvp = Field(v_info, CREATE_PROCESS_ARGV);
    count = Wosize_val(v_argvp);
    if(count == 0)
        caml_invalid_argument("omake_shell_sys_create_process: command line is empty");
    index = 0;
    for(i = 0; i != count; i++) {
        /* Win32 doesn't deal well when the command name differs from the executable */
        if(i == 0)
            argp = command;
        else
            argp = String_val(Field(v_argvp, i));
        length = strlen(argp);
        white = string_escape_length(argp);
        if(index + length + white + 4 >= SIZEOF_COMMAND)
            caml_failwith("omake_shell_sys_create_process: command line is too long");
        if(index)
            argv[index++] = ' ';
        if(white)
            string_copy_escaped(argv + index, argp);
        else
            strcpy(argv + index, argp);
        index += length + white;
    }
    argv[index++] = 0;
#if 0
    fprintf(stderr, "Command: %s\n", argv);
    fflush(stderr);
#endif

    /* Get the directory */
    dir = String_val(Field(v_info, CREATE_PROCESS_DIR));

    /* Provide redirection */
    GetStartupInfo(&startup);
    startup.dwFlags |= STARTF_USESTDHANDLES;
    startup.hStdInput = Handle_val(Field(v_info, CREATE_PROCESS_STDIN));
    startup.hStdOutput = Handle_val(Field(v_info, CREATE_PROCESS_STDOUT));
    startup.hStdError = Handle_val(Field(v_info, CREATE_PROCESS_STDERR));
    startup.lpReserved = NULL;
    startup.lpReserved2 = NULL;
    startup.cbReserved2 = 0;

    /* Do not give it the console */
    creation_flags = CREATE_NEW_PROCESS_GROUP;

#ifdef OSH_DEBUG
    fprintf(stderr, "creating process %d:\n", pid);
    fprintf(stderr, "\tcommand: %s\n", command);
    fprintf(stderr, "\tcommand line: %s\n", argv);
    /*
     * XXX - For some reason, I get the
     * "unresolved external symbol __imp__CommandLineToArgvW@8"
     * here.
     * 
     * Aleksey
     *
    {
        LPWSTR *args;
        int argc, i;

        args = CommandLineToArgvW((LPCWSTR) argv, &argc);
        if (args) {
            fprintf(stderr, "\targv:\n");
            for(i = 0; i < argc; i++)
               fprintf(stderr, "\t\t%ls\n", args[i]);
        }
    }
     */
    fflush(stderr);
#endif

    /* Create the process */
    status = CreateProcess(command,                        // Application name
                           argv,                           // Command line
                           NULL,                           // Process attributes
                           NULL,                           // Thread attributes
                           TRUE,                           // Inherit handles
                           creation_flags,                 // Creation flags
                           env,                            // Environment
                           dir,                            // Current directory
                           &startup,                       // Startup info
                           &process);                      // Process info
    if(status == FALSE) {
        char * lpMsgBuf = NULL;
        int bufLen = FormatMessageA( 
            FORMAT_MESSAGE_ALLOCATE_BUFFER | 
            FORMAT_MESSAGE_FROM_SYSTEM | 
            FORMAT_MESSAGE_IGNORE_INSERTS,
            NULL,
            GetLastError(),
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
            (LPTSTR) &lpMsgBuf,
            0,
            NULL );

        #ifdef OSH_DEBUG
            print_error("CreateProcess");
            fprintf(stderr, "Command: %s\n", command);
            fflush(stderr);
        #endif
        if ((bufLen < 1) || (bufLen > 1024)) {
            if (lpMsgBuf != NULL) 
                LocalFree( lpMsgBuf );
            caml_failwith("omake_shell_sys_create_process: process creation failed");
        } else {
            char err[2048];
            sprintf(err, "omake_shell_sys_create_process: process creation failed: %s", (char *)lpMsgBuf);
            if (lpMsgBuf != NULL) 
                LocalFree( lpMsgBuf );
            caml_failwith(err);
        }
    }
    CloseHandle(process.hThread);

    /* Allocate a new process struct */
    processp = (Process *) malloc(sizeof(Process));
    if(processp == 0) {
        CloseHandle(process.hProcess);
        caml_failwith("omake_shell_sys_create_process: out of memory");
    }
    memset(processp, 0, sizeof(Process));
    processp->pid = pid;
    processp->pgrp = pgrp;
    processp->status = STATUS_RUNNING;
    processp->handle = process.hProcess;
    processp->wid = process.dwProcessId;
    processp->next = state->processes;
    state->processes = processp;
    CAMLreturn(Val_int(pid));
}

/************************************************************************
 * Initialization.
 */

/*
 * The control handler.
 *    CTRL-C: signal the current process group.
 *    CTRL-BREAK: stop the current process group.
 *    default: abort the foreground and stopped processes, and exit
 */
static BOOL WINAPI console_ctrl_handler(DWORD code)
{
    BOOL rval;

#ifdef OSH_DEBUG
    fprintf(stderr, "console_ctrl_handler\n");
    fflush(stderr);
#endif

    rval = TRUE;
    switch(code) {
    case CTRL_C_EVENT:
        if(state->current_pgrp != INIT_PID) {
#ifdef OSH_DEBUG
            fprintf(stderr, "console_ctrl_handler: ctrl-c, pgrp=%d, state = 0x%08x\n", state->current_pgrp, (unsigned long) state);
            fflush(stderr);
#endif
            process_group_map(suspend_process, state->current_pgrp);
        }
        break;
    case CTRL_BREAK_EVENT:
        /* Ignore CTRL+BREAK, but it will get passed to all children... */
        if(state->current_pgrp != INIT_PID)
            process_group_map(suspend_process, state->current_pgrp);
        break;
    case CTRL_CLOSE_EVENT:
    case CTRL_LOGOFF_EVENT:
    case CTRL_SHUTDOWN_EVENT:
        fprintf(stderr, "Exiting\n");
        fflush(stderr);
        terminate_processes();
        /* Now we exit too */
        ExitProcess(1);
        break;
    default:
        fprintf(stderr, "console_ctrl_handler: unknown code: %d\n", code);
        fflush(stderr);
        rval = FALSE;
        break;
    }
#ifdef OSH_DEBUG
    fprintf(stderr, "console_ctrl_handler: returning %d\n", rval);
    fflush(stderr);
#endif
    return rval;
}

/*
 * Create the shell struct.
 * We try to get a handle on the console,
 * but don't stress if it doesn't exist.
 */
value omake_shell_sys_init(value v_unit)
{
    CAMLparam1(v_unit);
    Process *processp;
    HANDLE c_stdin;
    DWORD mode;
    int status;

#ifdef OSH_DEBUG
    fprintf(stderr, "omake_shell_sys_init\n");
    fflush(stderr);
#endif

    if (state)
      /* Init was already called before */
      CAMLreturn(Val_unit);

    /* Allocate a struct for the current process */
    processp = (Process *) malloc(sizeof(Process));
    if(processp == 0)
        caml_failwith("Omake_shell_csys.create_state: out of memory");
    memset(processp, 0, sizeof(Process));

    /* Allocate the state */
    state = (ShellState *) malloc(sizeof(ShellState));
    if(state == 0)
        caml_failwith("Omake_shell_csys.create_state: out of memory");
    memset(state, 0, sizeof(ShellState));
    state->pid_counter = INIT_PID;
    state->changed = CreateEvent(NULL, FALSE, FALSE, NULL);
    state->current_pgrp = INIT_PID;

    /* Initialize this process */
    processp->pid = INIT_PID;
    processp->pgrp = INIT_PID;
    processp->status = STATUS_RUNNING;
    processp->handle = GetCurrentProcess();
    processp->wid = GetCurrentProcessId();
    state->processes = processp;

    /* Try to get the console */
    c_stdin = GetStdHandle(STD_INPUT_HANDLE);
    if(c_stdin == INVALID_HANDLE_VALUE)
        CAMLreturn(Val_unit);
    status = GetConsoleMode(c_stdin, &mode);
    if(status)
        state->console = c_stdin;

    /* Install the console control handler */
    SetConsoleCtrlHandler(console_ctrl_handler, TRUE);
    CAMLreturn(Val_unit);
}

/*
 * Stop everything.
 */
value omake_shell_sys_close(value v_unit)
{
    CAMLparam1(v_unit);
    terminate_processes();
    CAMLreturn(Val_unit);
}

#else /* WIN32 */
#include <unistd.h>
#include <sys/ioctl.h>

/*
 * Unix needs very few functions.
 */
value omake_shell_sys_set_tty_pgrp(value v_pgrp)
{
    pid_t pgrp;

    pgrp = Int_val(v_pgrp);
    if(pgrp == 0)
        pgrp = getpgrp();
    if(tcsetpgrp(0, pgrp) < 0)
        perror("tcsetpgrp");
    return Val_unit;
}

value omake_shell_sys_setpgid(value v_pid, value v_pgrp)
{
    pid_t pid, pgrp;

    pid = Int_val(v_pid);
    pgrp = Int_val(v_pgrp);
    if(setpgid(pid, pgrp) < 0)
        perror("setpgid");
    return Val_unit;
}

#endif /* !WIN32 */
