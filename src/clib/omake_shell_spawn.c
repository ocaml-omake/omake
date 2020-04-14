/* This is the module for subprocess execution derived from OCamlnet.
   It is only meant for Unix. If available, it takes advantage from the
   posix_spawn call.

   Copyright (C) 2014 by Gerd Stolpmann

   This file is licensed under the same conditions as omake. This work
   was sponsored by Lexifi. */

#define _GNU_SOURCE
/* we need that to make the POSIX_SPAWN_USEVFORK macro visible under Linux */


#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/unixsupport.h"

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef WIN32
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#endif

#ifdef HAVE_POSIX_SPAWN
#include <spawn.h>
#endif

/* needed from caml/signals.h */
CAMLextern int caml_convert_signal_number(int);


/* This version here is a bit simplified compared with the OCamlnet version.
   In particular, we can assume that we are single-threaded. */


/**********************************************************************/
/* spawn                                                              */
/**********************************************************************/

static void empty_signal_handler(int sig) {}


#define MARSHALLED_ERROR_BUFFER_SIZE 80
#define FUNCTION_BUFFER_SIZE 64

typedef union {
    char buffer[MARSHALLED_ERROR_BUFFER_SIZE];
    struct {
        int b_errno;
        char b_function[FUNCTION_BUFFER_SIZE];
    } decoded;
} marshalled_error_t;


/* When we have failed to execvp(3) the new program we must reap the
   child process (to avoid a zombie process).
   MAXIMUM_NUMBER_OF_REAP_TRIES is the maximum number of waitpid(2)
   calls we issue before giving up (and leaving a zombie behind). */
#define MAXIMUM_NUMBER_OF_REAP_TRIES 30

/* REAP_SLEEP_INTERVAL is the wall-clock duration we wait in between
   two calls to waitpid(2) when we must reap the child process. */
#define REAP_SLEEP_INTERVAL 3000000L /* nano seconds */


#define SET_ERROR(m_errno, m_message)           \
    do {                                        \
        uerror_errno = (m_errno);               \
        uerror_function = (m_message);          \
    } while (0)

#define RAISE_MAIN_ERROR(m_errno, m_message)    \
    do {                                        \
        SET_ERROR(m_errno, m_message);          \
        goto main_exit;                         \
    } while (0)

#define RAISE_SUB_ERROR(m_errno, m_message)     \
    do {                                        \
        SET_ERROR(m_errno, m_message);          \
        goto sub_error;                         \
    } while (0)



/* Note: In the following function we can assume that we are not on Win32.
   Hence, file descriptors are simply ints. */


/* omake_shell_spawn_spawn_nat is the full-featured version using fork/exec */

CAMLprim value omake_shell_spawn_compat_nat(value v_chdir,
                                            value v_pg,
                                            value v_fd_actions,
                                            value v_sig_actions,
                                            value v_env,
                                            value v_cmd,
                                            value v_args)
{
#ifndef WIN32
    int uerror_errno = 0;
    char *uerror_function = "<uninit>";
    value return_value = Val_int(0);

    sigset_t mask;
    sigset_t save_mask;
    sigset_t spawn_mask;
    bool cleanup_mask = false;

    int ctrl_pipe[2];
    bool cleanup_pipe[2] = {false, false};

    bool cleanup_bsection = false;

    pid_t pid;
    char **sub_argv = NULL;
    char **sub_env = NULL;
    bool cleanup_sub_argv = false;
    bool cleanup_sub_env = false;

    marshalled_error_t marshalled_error;

    struct sigaction sigact;

    value v_sig_actions_l;

    value v_fd_actions_l;

    /* First thing is that we have to block all signals.  In a
       multi-threaded program this is only done for the thread calling
       us, otherwise for the whole process.  [fork] will reset all
       pending signals, so we can be sure the subprocess won't get any
       signals until we perform the signal actions in the subprocess.

       In the calling process, the mask is reset below at [exit]. */
    if (sigfillset(&mask) == -1) {
        unix_error(EINVAL, "Omake_shell_spawn/sigfillset [000]", Nothing);
    }
    if (sigprocmask(SIG_SETMASK, &mask, &save_mask) == -1) {
        uerror("Omake_shell_spawn/sigprocmask [002]", Nothing);
    }
    memcpy(&spawn_mask, &save_mask, sizeof(sigset_t));

    /* From now on, we don't jump out with uerror, but leave via "exit" below. */
    cleanup_mask = true;

    /* Create the control pipe for reporting errors. */
    if (pipe(ctrl_pipe) == -1) {
        RAISE_MAIN_ERROR(errno, "Omake_shell_spawn/pipe [010]");
    }
    cleanup_pipe[0] = true;
    cleanup_pipe[1] = true;

    /* Prepare sub_argv and sub_env: */
    sub_argv = malloc((Wosize_val(v_args) + 1) * sizeof(char*));
    if (sub_argv == NULL) {
        RAISE_MAIN_ERROR(ENOMEM, "Omake_shell_spawn/malloc [020]");
    }
    for (mlsize_t k = 0; k < Wosize_val(v_args); k++) {
        sub_argv[k] = Bp_val(Field(v_args, k));
    }
    sub_argv[Wosize_val(v_args)] = NULL;
    cleanup_sub_argv = true;

    sub_env = malloc((Wosize_val(v_env) + 1) * sizeof(char*));
    if (sub_env == NULL) {
        RAISE_MAIN_ERROR(ENOMEM, "Omake_shell_spawn/malloc [021]");
    }
    for (mlsize_t k = 0; k < Wosize_val(v_env); k++) {
        sub_env[k] = Bp_val(Field(v_env, k));
    }
    sub_env[ Wosize_val(v_env)] = NULL;
    cleanup_sub_env = true;

    /* Because fork() can be slow we allow here that other threads can run */
    /* caml_enter_blocking_section();
       cleanup_bsection = true;
       -- TODO: check this more carefully before enabling it
       -- see also leave_blocking_section below */

    /* Fork the process. */
    pid = fork();
    if (pid == (pid_t) -1) {
        RAISE_MAIN_ERROR(errno, "Omake_shell_spawn/fork [031]");
    }
    if (pid != (pid_t) 0) {
        goto main_process;
    }

/* sub_process: */
    /* The start of the sub process. */
    pid = getpid();

    /* Close the read side of the control pipe.  Set the close-on-exec
       flag for the write side. */
    if (close(ctrl_pipe[0]) == -1) {
        RAISE_SUB_ERROR(errno, "Omake_shell_spawn/close [100]");
    }
    if (fcntl(ctrl_pipe[1], F_SETFD, FD_CLOEXEC) == -1) {
        RAISE_SUB_ERROR(errno, "Omake_shell_spawn/fcntl [101]");
    }

    /* If required, change the working directory */
    if (Is_block(v_chdir)) {
        switch (Tag_val(v_chdir)) {
        case 0:  /* Wd_chdir */
            if (chdir(String_val(Field(v_chdir, 0))) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/chdir [110]");
            }
            break;
        case 1:  /* Wd_fchdir */
            if (fchdir(Int_val(Field(v_chdir, 1))) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/fchdir [111]");
            }
            break;
        default:
            RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/assert_chdir [112]");
        }
    }

    /* If required, create/join the process group */
    if (Is_block(v_pg)) {
        /* Must be Pg_join_group */
        if (setpgid(0, Int_val(Field(v_pg, 0))) == -1) {
            RAISE_SUB_ERROR(errno, "Omake_shell_spawn/setpgid [120]");
        }
    } else {
        switch (Int_val(v_pg)) {
        case 0: /* Pg_keep */
            break;
        case 1: /* Pg_new_bg_group */
            if (setpgid(0, 0) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/setpgid [130]");
            }
            break;
        case 2: /* Pg_new_fg_group */
        {
            if (setpgid(0, 0) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/setpgid [140]");
            }
            char term[L_ctermid];
            const char *ttyname = ctermid(term);
            if (*ttyname == '\0') {
                break; /* We could not determine the name of the terminal. */
            }
            const int ttyfd = open(ttyname, O_RDWR);
            if (ttyfd == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/open [141]");
            }
            /* tcsetpgrp() may send a SIGTTOU signal to this process.
               We want to hide this signal, so we set this signal to
               be ignored.  We do this by setting an empty signal
               handler.  On exec, the SIGTTOU signal will be reset to
               the default action in this case - which is ok because
               the set of pending signals is also cleared. */
            sigact.sa_sigaction = NULL;
            sigact.sa_handler = &empty_signal_handler;
            sigact.sa_flags = 0;
            if (sigemptyset(&(sigact.sa_mask)) == -1) {
                RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/sigemptyset [142]");
            }
            if (sigaction(SIGTTOU, &sigact, NULL) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/sigaction [143]");
            }
            if (tcsetpgrp(ttyfd, pid) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/tcsetpgrp [144]");
            }
            if (close(ttyfd) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/close [145]");
            }
            break;
        }
        default:
            RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/assert_pg [160]");
        }
    }

    /* do the signal stuff: */
    v_sig_actions_l = v_sig_actions;
    while (Is_block(v_sig_actions_l)) {
        value v_sig_actions_hd = Field(v_sig_actions_l, 0);
        v_sig_actions_l = Field(v_sig_actions_l, 1);
        switch (Tag_val(v_sig_actions_hd)) {
        case 0:  /* Sig_default */
        {
            const int signr = caml_convert_signal_number(Int_val(Field(v_sig_actions_hd, 0)));
            sigact.sa_sigaction = NULL;
            sigact.sa_handler = SIG_DFL;
            sigact.sa_flags = 0;
            if (sigemptyset(&(sigact.sa_mask)) == -1) {
                RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/sigemptyset [170]");
            }
            if (sigaction(signr, &sigact, NULL) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/sigaction [171]");
            }
            break;
        }
        case 1: /* Sig_ignore */
        {
            const int signr = caml_convert_signal_number(Int_val(Field(v_sig_actions_hd, 0)));
            sigact.sa_sigaction = NULL;
            sigact.sa_handler = SIG_IGN;
            sigact.sa_flags = 0;
            if (sigemptyset(&(sigact.sa_mask)) == -1) {
                RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/sigemptyset [180]");
            }
            if (sigaction(signr, &sigact, NULL) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/sigaction [181]");
            }
            break;
        }
        case 2: /* Sig_mask */
            sigemptyset(&spawn_mask);
            value v_signals_l = Field(v_sig_actions_hd, 0);
            while (Is_block(v_signals_l)) {
                value v_signals_hd = Field(v_signals_l, 0);
                v_signals_l  = Field(v_signals_l, 1);
                const int signr = caml_convert_signal_number(Int_val(v_signals_hd));
                if (sigaddset(&spawn_mask, signr) == -1) {
                    RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/sigaddset [190]");
                }
            }
            break;
        default:
            RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/assert_sig [199]");
        }
    }

    /* do the fd stuff: */
    v_fd_actions_l = v_fd_actions;
    while (Is_block(v_fd_actions_l)) {
        value v_fd_actions_hd = Field(v_fd_actions_l, 0);
        v_fd_actions_l  = Field(v_fd_actions_l, 1);
        switch (Tag_val(v_fd_actions_hd)) {
        case 0: /* Fda_close */
        {
            const int fd1 = Int_val(Field(v_fd_actions_hd, 0));
            if (fd1 == ctrl_pipe[1]) {
                RAISE_SUB_ERROR(EBADF, "Omake_shell_spawn/fda_close [200]");
            }
            if (close(fd1) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/close [201]");
            }
            break;
        }
        case 1: /* Fda_close_ignore */
        {
            const int fd1 = Int_val(Field(v_fd_actions_hd, 0));
            if (fd1 != ctrl_pipe[1]) {
                if (close(fd1) == -1 && errno != EBADF) {
                    RAISE_SUB_ERROR(errno, "Omake_shell_spawn/close [210]");
                }
            }
            /* ignore requests to close the ctrl_pipe, it's closed anyway later */
            break;
        }
        case 2:  /* Fda_close_except */
        {
            const int nofile = (int) sysconf(_SC_OPEN_MAX);
            value v_fd_actions_0 = Field(v_fd_actions_hd, 0);
            const int j = (int) Wosize_val(v_fd_actions_0);   /* array length */
            for (int k = 0; k < nofile; k++) {
                if (k >= j || !Bool_val(Field(v_fd_actions_0, k))) {
                    if (k != ctrl_pipe[1]) {
                        close(k);   /* ignore any error */
                    }
                }
            }
            break;
        }
        case 3: /* Fda_dup2 */
        {
            const int fd1 = Int_val(Field(v_fd_actions_hd, 0));
            const int fd2 = Int_val(Field(v_fd_actions_hd, 1));
            /* If fd1 is the ctrl_pipe, return EBADF: */
            if (fd1 == ctrl_pipe[1]) {
                RAISE_SUB_ERROR(EBADF, "Omake_shell_spawn/fda_dup2 [220]");
            }
            /* Check that fd1 is valid by reading the fd flags: */
            if (fcntl(fd1, F_GETFD) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/fcntl [221]");
            }
            /* Be careful when fd2 is the ctrl_pipe: */
            if (fd2 == ctrl_pipe[1]) {
                const int new_fd = dup(ctrl_pipe[1]);
                if (new_fd == -1) {
                    RAISE_SUB_ERROR(errno, "Omake_shell_spawn/dup [222]");
                }
                ctrl_pipe[1] = new_fd;
                if (fcntl(ctrl_pipe[1], F_SETFD, FD_CLOEXEC) == -1) {
                    RAISE_SUB_ERROR(errno, "Omake_shell_spawn/fcntl [223]");
                }
            }
            if (dup2(fd1, fd2) == -1) {
                RAISE_SUB_ERROR(errno, "Omake_shell_spawn/dup2 [224]");
            }
            /* The FD_CLOEXEC flag remains off for the duped descriptor */
            break;
        }
        default:
            RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/assert_fd [230]");
        }
    }

    /* set the signal mask */
    if (sigprocmask(SIG_SETMASK, &spawn_mask, NULL) == -1) {
        RAISE_SUB_ERROR(errno, "Omake_shell_spawn/sigprocmask [241]");
    }

    /* exec the new program */
    if (execve(String_val(v_cmd), sub_argv, sub_env) == -1) {
        RAISE_SUB_ERROR(errno, "Omake_shell_spawn/execve [290]");
    }

    RAISE_SUB_ERROR(EINVAL, "Omake_shell_spawn/assert_execve [291]");

 sub_error:
    /* Marshal the error in uerror_errno and uerror_function */
    marshalled_error.decoded.b_errno = uerror_errno;
    strncpy(marshalled_error.decoded.b_function, uerror_function, FUNCTION_BUFFER_SIZE - 1);
    if (FUNCTION_BUFFER_SIZE - 1 > 0) {
        marshalled_error.decoded.b_function[FUNCTION_BUFFER_SIZE - 1] = '\0';
    }

    /* It doesn't make much sense here to check for errors as we are
       about to ship the error message. */
    write(ctrl_pipe[1], marshalled_error.buffer, MARSHALLED_ERROR_BUFFER_SIZE);
    fsync(ctrl_pipe[1]);

    _exit(127);

 main_process:
    /* Here the main process continues after forking.  There's not
       much to do here: We close the write side of the control pipe,
       so the read side can see EOF.  We check then whether the read
       side is just closed (meaning no error), or whether there are
       bytes, the marshalled error condition. */
    if (cleanup_bsection) {
        caml_leave_blocking_section();
        cleanup_bsection = false;
    }

    if (close(ctrl_pipe[1]) == -1) {
        RAISE_MAIN_ERROR(errno, "Omake_shell_spawn/close [300]");
    }
    cleanup_pipe[1] = false;

    {
        const ssize_t n = read(ctrl_pipe[0], marshalled_error.buffer, MARSHALLED_ERROR_BUFFER_SIZE);

        if (n == (ssize_t) -1) {
            RAISE_MAIN_ERROR(errno, "Omake_shell_spawn/read [301]");
        }

        if (n == 0) {
            /* Hey, we have success! */
            return_value = Val_int(pid);
            goto main_exit;
        }

        /* There is an error message in marshalled_error.  Look at it! */
        if (n != MARSHALLED_ERROR_BUFFER_SIZE) {
            RAISE_MAIN_ERROR(EINVAL, "Omake_shell_spawn/assert_marshalled_error [302]");
        }
    }

    /* Also don't forget to wait on the child to avoid zombies. */
    {
        int tries_left = MAXIMUM_NUMBER_OF_REAP_TRIES;
        const struct timespec sleep_duration = {0, REAP_SLEEP_INTERVAL};
        struct timespec remaining_duration;

        while (waitpid(pid, NULL, 0) == -1 && errno == EINTR && tries_left > 0) {
            nanosleep(&sleep_duration, &remaining_duration);
            --tries_left;
        }
    }

    SET_ERROR(marshalled_error.decoded.b_errno, marshalled_error.decoded.b_function);
    /* now exit... */

main_exit:
    /* Policy: If we already have an error to report, and any of the
       cleanup actions also indicates an error, we return the first
       error to the caller. */

    if (cleanup_bsection) {
        caml_leave_blocking_section();
    }

    if (cleanup_mask) {
        if (sigprocmask(SIG_SETMASK, &save_mask, NULL) == -1 && uerror_errno == 0) {
            SET_ERROR(errno, "Omake_shell_spawn/sigprocmask [401]");
        }
    }

    if (cleanup_pipe[0]) {
        if (close(ctrl_pipe[0]) == -1 && uerror_errno == 0) {
            SET_ERROR(errno, "Omake_shell_spawn/close [410]");
        }
    }

    if (cleanup_pipe[1]) {
        if (close(ctrl_pipe[1]) == -1 && uerror_errno == 0) {
            SET_ERROR(errno, "Omake_shell_spawn/close [411]");
        }
    }

    if (cleanup_sub_argv) {
        free(sub_argv);
    }

    if (cleanup_sub_env) {
        free(sub_env);
    }

    if (uerror_errno != 0) {
        unix_error(uerror_errno, uerror_function, Nothing);
    }

    return return_value;
#else
     invalid_argument("Omake_shell_spawn.compat_spawn");
#endif
}


CAMLprim value omake_shell_spawn_compat_byte(value *argv, int argn)
{
    return omake_shell_spawn_compat_nat(argv[0], argv[1], argv[2], argv[3],
                                        argv[4], argv[5], argv[6]);
}


CAMLprim value omake_shell_spawn_have_posix_spawn(value dummy)
{
#ifdef HAVE_POSIX_SPAWN
    return Val_bool(1);
#else
    return Val_bool(0);
#endif
}


CAMLprim value omake_shell_spawn_posix_nat(value v_pg,
                                           value v_fd_actions,
                                           value v_sig_actions,
                                           value v_env,
                                           value v_cmd,
                                           value v_args)
{
#ifdef HAVE_POSIX_SPAWN
    int uerror_errno = 0;
    char *uerror_function = "<uninit>";
    value return_value = Val_int(0);

    int code;
    short flags = 0;
    bool use_fork_exec = false;

    pid_t pid;
    char **sub_argv = NULL;
    char **sub_env = NULL;
    bool cleanup_sub_argv = false;
    bool cleanup_sub_env = false;

    posix_spawn_file_actions_t fd_actions;
    int n_fd_actions;
    posix_spawnattr_t attr;
    bool cleanup_fd_actions = false;
    bool cleanup_attr = false;
    value v_fd_actions_l;

    int fd_known;
    bool cleanup_fd_known = false;
    value v_sig_actions_l;

    sigset_t sigdfl;
    sigset_t spawn_mask;

    /* Set fd_known to a valid file descriptor */
    fd_known = open(".", O_RDONLY, 0);
    if (fd_known == -1) {
        RAISE_MAIN_ERROR(errno, "omake_shell_spawn_posix/open");
    }
    cleanup_fd_known = true;
    if (fcntl(fd_known, F_SETFD, FD_CLOEXEC) == -1) {
        RAISE_MAIN_ERROR(errno, "omake_shell_spawn_posix/fcntl");
    }

    /* Prepare sub_argv and sub_env: */
    sub_argv = malloc((Wosize_val(v_args) + 1) * sizeof(char*));
    if (sub_argv == NULL) {
        RAISE_MAIN_ERROR(ENOMEM, "omake_shell_spawn_posix/malloc [1]");
    }
    for (mlsize_t k = 0; k < Wosize_val(v_args); k++) {
        sub_argv[k] = Bp_val(Field(v_args, k));
    }
    sub_argv[Wosize_val(v_args)] = NULL;
    cleanup_sub_argv = true;

    sub_env = malloc((Wosize_val(v_env) + 1) * sizeof(char*));
    if (sub_env == NULL) {
        RAISE_MAIN_ERROR(ENOMEM, "omake_shell_spawn_posix/malloc [2]");
    }
    for (mlsize_t k = 0; k < Wosize_val(v_env); k++) {
        sub_env[k] = Bp_val(Field(v_env, k));
    }
    sub_env[Wosize_val(v_env)] = NULL;
    cleanup_sub_env = true;

    /* Init fd_actions */
    code = posix_spawn_file_actions_init(&fd_actions);
    n_fd_actions = 0;
    if (code != 0) {
        RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/posix_spawn_file_actions_init");
    }
    cleanup_fd_actions = true;

    code = posix_spawnattr_init(&attr);
    if (code != 0) {
        RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/posix_spawnattr_init");
    }
    cleanup_attr = true;

    /* initialize the attributes */

    /* If required, create/join the process group */
    if (Is_block(v_pg)) {
        /* Must be Pg_join_group */
        code = posix_spawnattr_setpgroup(&attr, Int_val(Field(v_pg, 0)));
        if (code != 0) {
            RAISE_MAIN_ERROR(errno, "omake_shell_spawn/psa_setpgroup [1]");
        }
        flags |= POSIX_SPAWN_SETPGROUP;
    } else {
        switch (Int_val(v_pg)) {
        case 0: /* Pg_keep */
            break;
        case 1: /* Pg_new_bg_group */
            code = posix_spawnattr_setpgroup(&attr, 0);
            if (code != 0) {
                RAISE_MAIN_ERROR(errno, "omake_shell_spawn/psa_setpgroup [2]");
            }
            flags |= POSIX_SPAWN_SETPGROUP;
            break;
        case 2: /* Pg_new_fg_group */
            invalid_argument("Omake_Shell_Spawn_posix.posix_spawn: Pg_new_fg_group not supported");
            break;
        default:
            RAISE_MAIN_ERROR(EINVAL, "omake_shell_spawn_posix/assert_pg");
        }
    }

    /* do the signal stuff: */
    sigemptyset(&sigdfl);
    sigemptyset(&spawn_mask);
    v_sig_actions_l = v_sig_actions;
    while (Is_block(v_sig_actions_l)) {
        value v_sig_actions_hd = Field(v_sig_actions_l, 0);
        v_sig_actions_l  = Field(v_sig_actions_l, 1);
        switch (Tag_val(v_sig_actions_hd)) {
        case 0:  /* Sig_default */
        {
            const int signr = caml_convert_signal_number(Int_val(Field(v_sig_actions_hd, 0)));
            if (sigaddset(&sigdfl, signr) == -1) {
                RAISE_MAIN_ERROR(EINVAL, "omake_shell_spawn_posix/sigemptyset");
            }
            flags |= POSIX_SPAWN_SETSIGDEF;
            break;
        }
        case 1: /* Sig_ignore */
            invalid_argument("Omake_Shell_Spawn_posix.posix_spawn: Sig_ignore not supported");
            break;
        case 2: /* Sig_mask */
        {
            value v_signals_l = Field(v_sig_actions_hd, 0);
            while (Is_block(v_signals_l)) {
                value v_signals_hd = Field(v_signals_l, 0);
                v_signals_l = Field(v_signals_l, 1);
                const int signr = caml_convert_signal_number(Int_val(v_signals_hd));
                if (sigaddset(&spawn_mask, signr) == -1) {
                    RAISE_MAIN_ERROR(errno, "omake_shell_spawn/sigaddset [190]");
                }
            }
            flags |= POSIX_SPAWN_SETSIGMASK;
            break;
        }
        default:
            RAISE_MAIN_ERROR(EINVAL, "omake_shell_spawn_posix/assert_sig");
        }
    }
    code = posix_spawnattr_setsigdefault(&attr, &sigdfl);
    if (code != 0) {
        RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psa_setsigdefault");
    }

    code = posix_spawnattr_setsigmask(&attr, &spawn_mask);
    if (code != 0) {
        RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psa_setsigmask");
    }

    /* See http://sources.redhat.com/bugzilla/show_bug.cgi?id=378
       This is Linux-specific. However, contrary to this discussion
       recent versions of glibc always use vfork. */
#ifdef POSIX_SPAWN_USEVFORK
    flags |= POSIX_SPAWN_USEVFORK;
#endif

    code = posix_spawnattr_setflags(&attr, flags);
    if (code != 0) {
        RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psa_setflags");
    }

    /* do the fd stuff: */
    v_fd_actions_l = v_fd_actions;
    while (Is_block(v_fd_actions_l)) {
        value v_fd_actions_hd = Field(v_fd_actions_l, 0);
        v_fd_actions_l  = Field(v_fd_actions_l, 1);
        switch (Tag_val(v_fd_actions_hd)) {
        case 1: /* Fda_close_ignore */
        {
            /* We translate this into a dup2 + close */
            const int fd1 = Int_val(Field(v_fd_actions_hd, 0));
            if (fd1 != fd_known) {
                code = posix_spawn_file_actions_adddup2(&fd_actions, fd_known, fd1);
                if (code != 0) {
                    RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psfa_adddup2 [1]");
                }

                n_fd_actions++;
                code = posix_spawn_file_actions_addclose(&fd_actions, fd1);
                if (code != 0) {
                    RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psfa_addclose [2]");
                }
                n_fd_actions++;
            }
            break;
        }
        case 0: /* Fda_close */
        {
            const int fd1 = Int_val(Field(v_fd_actions_hd, 0));
            if (fd1 != fd_known) {
                code = posix_spawn_file_actions_addclose(&fd_actions, fd1);
                if (code != 0) {
                    RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psfa_addclose [1]");
                }
                n_fd_actions++;
            }
            break;
        }
        case 2:  /* Fda_close_except */
        {
            const int nofile = (int) sysconf(_SC_OPEN_MAX);
            value v_fd_actions_0 = Field(v_fd_actions_hd, 0);
            const int j = (int) Wosize_val(v_fd_actions_0);   /* array length */
            for (int k = 0; k < nofile; k++) {
                if (k >= j || !Bool_val(Field(v_fd_actions_0, k))) {
                    if (k != fd_known) {
                        code = posix_spawn_file_actions_adddup2(&fd_actions, fd_known, k);
                        if (code != 0) {
                            RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psfa_actions_adddup2 [2]");
                        }
                        n_fd_actions++;
                        code = posix_spawn_file_actions_addclose(&fd_actions, k);
                        if (code != 0) {
                            RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psfa_addclose [3]");
                        }
                        n_fd_actions++;
                    }
                }
            }
            break;
        }
        case 3: /* Fda_dup2 */
        {
            /* Ignore here fd_known - even if fd1 or fd2 is fd_known it will
               remain open, and that's all we need */
            const int fd1 = Int_val(Field(v_fd_actions_hd, 0));
            const int fd2 = Int_val(Field(v_fd_actions_hd, 1));
            code = posix_spawn_file_actions_adddup2(&fd_actions, fd1, fd2);
            if (code != 0) {
                RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/psfa_actions_adddup2 [3]");
            }
            n_fd_actions++;
            break;
        }
        default:
            RAISE_MAIN_ERROR(EINVAL, "omake_shell_spawn_posix/assert_fd");
        }
    }

#ifdef __APPLE__
    /* MacOS limits the number of file actions arbitrarily to the max number
       of open files.  We catch this here, and cause a failure "USE_FORK_EXEC",
       so the alternate fork/exec-based spawn will be used instead. */
    if (n_fd_actions > sysconf(_SC_OPEN_MAX)) {
        use_fork_exec = true;
        goto main_exit;
    }
#endif


    code = posix_spawn(&pid, String_val(v_cmd), &fd_actions, &attr, sub_argv, sub_env);
    if (code != 0) {
        RAISE_MAIN_ERROR(code, "omake_shell_spawn_posix/posix_spawn");
    }

    return_value = Val_int(pid);

 main_exit:
    /* Policy: If we already have an error to report, and any of the
       cleanup actions also indicates an error, we return the first
       error to the caller. */

    if (cleanup_sub_argv) {
        free(sub_argv);
    }

    if (cleanup_sub_env) {
        free(sub_env);
    }

    if (cleanup_fd_known) {
        close(fd_known);
    }

    if (cleanup_fd_actions) {
        posix_spawn_file_actions_destroy(&fd_actions);
    }

    if (cleanup_attr) {
        posix_spawnattr_destroy(&attr);
    }

    if (uerror_errno != 0) {
        unix_error(uerror_errno, uerror_function, Nothing);
    }

    if (use_fork_exec) {
        failwith("USE_FORK_EXEC");
    }

    return return_value;

#else
    invalid_argument("Omake_shell_spawn.posix_spawn not available");
#endif
}


CAMLprim value omake_shell_spawn_posix_byte(value *argv, int argn)
{
    return omake_shell_spawn_posix_nat(argv[0], argv[1], argv[2], argv[3],
                                       argv[4], argv[5]);
}


CAMLprim value omake_shell_spawn_fchdir(value fd)
{
#ifndef WIN32
    if (fchdir(Int_val(fd)) == -1) {
        uerror("fchdir", Nothing);
    }
    return Val_unit;
#else
    invalid_argument("Omake_shell_spawn.fchdir not available");
#endif
}
