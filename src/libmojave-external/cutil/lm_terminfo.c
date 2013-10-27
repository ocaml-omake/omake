/*
 * Simple Terminfo Interface for ML
 * Copyright(c) 2002 Justin David Smith, Caltech
 */
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#ifdef WIN32
#   pragma warning (disable: 4127 4189 4702 4996)
#endif

#ifdef NCURSES

/* Headers that are readline-specific must be included here. */
#include <ncurses.h>

#ifdef TERMH_IN_NCURSES
#  include <ncurses/term.h>
#else
#  include <term.h>
#endif

static int loaded_terminfo = 0;

static int load_terminfo() {

   char *termname = NULL;

   /* Check to see if we already loaded the terminal data */
   if(loaded_terminfo) return(0);

   /* We haven't loaded anything yet (or we had an error). */
   if(setupterm(NULL, 1, NULL) == OK) {
      /* We were successful! */
      loaded_terminfo = 1;
      return(0);
   }

   /* Failure. */
   return(-1);

}

#endif /* NCURSES support? */

/*
 * Terminfo is enabled only of TERM is defined.
 */
value caml_tgetstr_enabled(value unit)
{
   CAMLparam1(unit);
   CAMLreturn(getenv("TERM") ? Val_true : Val_false);
}


/*
 * Read the indicated terminfo by string.
 */
value caml_tgetstr(value id) {

   CAMLparam1(id);
   CAMLlocal1(result);
   char *termdata = NULL;

   /* Lookup the requested capability name.  Note that we only get terminfo
      if we compiled with readline support; otherwise it will not be linked
      in.  */
#ifdef NCURSES
   if(load_terminfo() == 0) {
      termdata = tigetstr(String_val(id));
   }
#endif /* NCURSES */

   /* Note that tigetstr will return either 0 or -1 on error. */
   if(termdata == NULL || termdata == (char *)(-1)) {
      result = copy_string("");
   } else {
      result = copy_string(termdata);
      /* apparently we're not supposed to free termdata here */
      /* TEMP:  I cannot find specs on this! */
      //free(termdata);
   }

   /* Return the result */
   CAMLreturn(result);

}
