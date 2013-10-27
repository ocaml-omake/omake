/*
 * rusage interface for MCC
 * Copyright(c) 2002 Justin David Smith, Caltech
 */
#include <stdio.h>
#include <errno.h>

#ifndef WIN32
#  include <unistd.h>
#  include <sys/time.h>
#  include <sys/resource.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/*
 * These values correspond to the rusage_who constant constructors.
 */
#ifndef WIN32
int translate_rusage_who(int who)
{
    switch(who) {
    case 0:
        return RUSAGE_SELF;
    case 1:
        return RUSAGE_CHILDREN;
    default:
        failwith("translate_rusage_who: bad rusage_who constant constructor");
    }
}
#endif /* !WIN32 */

/*
 * This is a simplified version of getrusage that simply reports on the
 * process run times of the current process or the aggregate of its
 * children.
 */
value caml_getrusage_time(value mlwho)
{
    CAMLparam1(mlwho);
    CAMLlocal1(result);
   
    /* Allocate the result block and initialise it (in case we fail) */
    result = alloc_tuple(4);
    Field(result, 0) = Val_int(0);
    Field(result, 1) = Val_int(0);
    Field(result, 2) = Val_int(0);
    Field(result, 3) = Val_int(0);

#ifndef WIN32
    {
        struct rusage usage;
        int who = translate_rusage_who(Int_val(mlwho));

        /* Get the current process stats */
        if(getrusage(who, &usage) != 0) {
            failwith("caml_getrusage_times: getrusage call failed");
        }
   
        /* Load the fields of interest into our block */
        modify(&Field(result, 0), Val_int(usage.ru_utime.tv_sec));
        modify(&Field(result, 1), Val_int(usage.ru_utime.tv_usec));
        modify(&Field(result, 2), Val_int(usage.ru_stime.tv_sec));
        modify(&Field(result, 3), Val_int(usage.ru_stime.tv_usec));
    }
#endif

    /* Return the result */
    CAMLreturn(result);
}


/*
 * Set the rlimit maximum CPU time for a process (in seconds).
 */
value caml_setrlimit_time(value time) {

   CAMLparam1(time);

#ifndef WIN32
   struct rlimit rlim;

   /* Set the rlimit max field */
   rlim.rlim_cur = Int_val(time);
   rlim.rlim_max = Int_val(time);
   
   /* Call setrlimit */
   if(setrlimit(RLIMIT_CPU, &rlim) != 0) {
      failwith("caml_setrlimit_time: setrlimit call failed");
   }
#endif
   
   /* We were successful */
   CAMLreturn(Val_unit);
   
}

#ifdef WIN32

/*
 * Not implemented on Win32.
 */
value ml_getrusage(value unit_val)
{
    CAMLparam1(unit_val);
    CAMLlocal1(rval);
    int i;

    rval = alloc_tuple(16);
    for(i = 0; i != 16; i++)
        Field(rval, i) = Val_unit;
    Field(rval, 0) = copy_double(0.0);
    Field(rval, 1) = copy_double(0.0);
    Field(rval, 2) = Val_int(0);
    Field(rval, 3) = Val_int(0);
    Field(rval, 4) = Val_int(0);
    Field(rval, 5) = Val_int(0);
    Field(rval, 6) = Val_int(0);
    Field(rval, 7) = Val_int(0);
    Field(rval, 8) = Val_int(0);
    Field(rval, 9) = Val_int(0);
    Field(rval, 10) = Val_int(0);
    Field(rval, 11) = Val_int(0);
    Field(rval, 12) = Val_int(0);
    Field(rval, 13) = Val_int(0);
    Field(rval, 14) = Val_int(0);
    Field(rval, 15) = Val_int(0);

    CAMLreturn(rval);
}

#else /* not WIN32 */

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

/* JDS: this isn't defined in any OCaml header, seems to be 
        an internal function.  Oh well... *shrug* */
extern void uerror(char *cmdname, value arg) Noreturn;

#define  Nothing  ((value) 0)

/*
 * Get the resources consumed.
 */
value ml_getrusage(value unit_val)
{
	 CAMLparam1(unit_val);
    struct rusage rsrc;
    CAMLlocal1(rval);
    int ret, i;

    ret = getrusage(RUSAGE_SELF, &rsrc);
    if(ret < 0)
        uerror("getrusage", Nothing);
    rval = alloc_tuple(16);
    for(i = 0; i != 16; i++)
        Field(rval, i) = Val_unit;
    fprintf(stderr, "User: %f System: %f\n",
            rsrc.ru_utime.tv_sec + rsrc.ru_utime.tv_usec * 1e-6,
            rsrc.ru_stime.tv_sec + rsrc.ru_stime.tv_usec * 1e-6);
    Field(rval, 0) = copy_double(rsrc.ru_utime.tv_sec + rsrc.ru_utime.tv_usec * 1e-6);
    Field(rval, 1) = copy_double(rsrc.ru_stime.tv_sec + rsrc.ru_stime.tv_usec * 1e-6);
    Field(rval, 2) = Val_int(rsrc.ru_maxrss);
    Field(rval, 3) = Val_int(rsrc.ru_ixrss);
    Field(rval, 4) = Val_int(rsrc.ru_idrss);
    Field(rval, 5) = Val_int(rsrc.ru_isrss);
    Field(rval, 6) = Val_int(rsrc.ru_minflt);
    Field(rval, 7) = Val_int(rsrc.ru_majflt);
    Field(rval, 8) = Val_int(rsrc.ru_nswap);
    Field(rval, 9) = Val_int(rsrc.ru_inblock);
    Field(rval, 10) = Val_int(rsrc.ru_oublock);
    Field(rval, 11) = Val_int(rsrc.ru_msgsnd);
    Field(rval, 12) = Val_int(rsrc.ru_msgrcv);
    Field(rval, 13) = Val_int(rsrc.ru_nsignals);
    Field(rval, 14) = Val_int(rsrc.ru_nvcsw);
    Field(rval, 15) = Val_int(rsrc.ru_nivcsw);

    CAMLreturn(rval);
}
#endif
