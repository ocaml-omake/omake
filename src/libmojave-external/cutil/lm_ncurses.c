/*
 * Simple NCurses Interface for MCC
 * Copyright(c) 2002 Justin David Smith, Caltech
 */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>


/*
 * Macro to define key binding function
 */
#define  CAML_BIND_KEY(name, key)      \
   value name(value unit) {            \
      CAMLparam1(unit);                \
      CAMLreturn(Val_int(key));        \
   }


#ifdef NCURSES
/* From this point, we assume ncurses support is present. */


/* Headers which are ncurses-specific must be included here. */
#include <ncurses.h>


#define  ml_of_window(win)    (copy_nativeint((long)(win)))
#define  window_of_ml(win)    ((WINDOW *)Nativeint_val(win))


/***  Key definitions  ***/


CAML_BIND_KEY(caml_key_down,  KEY_DOWN)
CAML_BIND_KEY(caml_key_up,    KEY_UP)
CAML_BIND_KEY(caml_key_left,  KEY_LEFT)
CAML_BIND_KEY(caml_key_right, KEY_RIGHT)
CAML_BIND_KEY(caml_key_home,  KEY_HOME)
CAML_BIND_KEY(caml_key_end,   KEY_END)
CAML_BIND_KEY(caml_key_npage, KEY_NPAGE)
CAML_BIND_KEY(caml_key_ppage, KEY_PPAGE)
CAML_BIND_KEY(caml_key_enter, KEY_ENTER)
CAML_BIND_KEY(caml_key_cancel,KEY_CANCEL)


/***  Standard Curses Interface  ***/


static void setwinopts(WINDOW *win, bool scrollable) {

   intrflush(win, FALSE);
   keypad(win, TRUE);
   scrollok(win, scrollable);

}


/*
 * The values here correspond to the order of the constant 
 * constructors in the attr data type, in mc_ncurses.ml.
 */
static int translate_attr(int attr) {

   switch(attr) {
      case 0:  return(A_NORMAL);
      case 1:  return(A_STANDOUT);
      case 2:  return(A_UNDERLINE);
      case 3:  return(A_REVERSE);
      case 4:  return(A_BLINK);
      case 5:  return(A_DIM);
      case 6:  return(A_BOLD);
      case 7:  return(A_PROTECT);
      case 8:  return(A_INVIS);
      case 9:  return(A_ALTCHARSET);
      case 10: return(A_CHARTEXT);
      default: failwith("translate_attr: bad attr constant constructor");
   }

}


/*
 * Report that we support ncurses.
 */
value caml_curses_enabled(value unit) {

   CAMLparam1(unit);
   CAMLreturn(getenv("TERM") ? Val_true : Val_false);
   
}


value caml_curses_initscr(value unit) {

   CAMLparam1(unit);

   /* Initialise ncurses interface */   
   initscr();              /* Initialise screen */
   cbreak();               /* Disable character buffering */
   noecho();               /* Do not echo typed characters */
   nonl();                 /* Do not translate newline seq */

   /* Setup options for the default window */   
   setwinopts(stdscr, TRUE);

   CAMLreturn(Val_unit);

}


value caml_curses_endwin(value unit) {

   CAMLparam1(unit);

   /* Temporarily end the ncurses interface */
   endwin();

   CAMLreturn(Val_unit);

}


value caml_curses_newwin(value nlines, value ncols, value beginy, value beginx) {

   CAMLparam4(nlines, ncols, beginy, beginx);
   CAMLlocal1(result);
   WINDOW *window;
   
   /* Create and initialise the new window */
   window = newwin(Int_val(nlines), Int_val(ncols), Int_val(beginy), Int_val(beginx));
   if(window == NULL) {
      failwith("caml_curses_newwin: newwin call failed");
   }
   setwinopts(window, TRUE);

   /* Return the result, packaged into a nativeint */
   result = ml_of_window(window);
   CAMLreturn(result);

}


value caml_curses_delwin(value mlwindow) {

   CAMLparam1(mlwindow);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Delete the window */
   delwin(window);
   
   CAMLreturn(Val_unit);

}


value caml_curses_waddch(value mlwindow, value ch) {

   CAMLparam2(mlwindow, ch);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Display this character */
   waddch(window, Int_val(ch));
   
   CAMLreturn(Val_unit);

}


value caml_curses_waddstr(value mlwindow, value str) {

   CAMLparam2(mlwindow, str);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Display this character */
   waddstr(window, String_val(str));
   
   CAMLreturn(Val_unit);

}


value caml_curses_wattron(value mlwindow, value mlattr) {

   CAMLparam2(mlwindow, mlattr);
   WINDOW *window = window_of_ml(mlwindow);
   int attr = translate_attr(Int_val(mlattr));
   
   /* Turn on the indicated attribute */
   wattron(window, attr);
   
   CAMLreturn(Val_unit);
   
}


value caml_curses_wattroff(value mlwindow, value mlattr) {

   CAMLparam2(mlwindow, mlattr);
   WINDOW *window = window_of_ml(mlwindow);
   int attr = translate_attr(Int_val(mlattr));
   
   /* Turn off the indicated attribute */
   wattroff(window, attr);
   
   CAMLreturn(Val_unit);
   
}


value caml_curses_wgetch(value mlwindow) {

   CAMLparam1(mlwindow);
   WINDOW *window = window_of_ml(mlwindow);
   int ch;
   
   /* Get a character from curses */
   ch = wgetch(window);
   
   /* Return the character read */
   CAMLreturn(Val_int(ch));

}


/*
 * Get a string from the user. This is a fancy version which will size
 * the maximum length of the line based on the amount of space remaining
 * on the current line in the ncurses window.
 */
value caml_curses_wgetstr(value mlwindow) {

   CAMLparam1(mlwindow);
   CAMLlocal1(result);
   WINDOW *window = window_of_ml(mlwindow);
   char *buffer = NULL;
   int cury, curx;
   int maxy, maxx;
   int len;
   
   /* Get the window extents to determine a line length */
   getyx(window, cury, curx);
   getmaxyx(window, maxy, maxx);
   len = maxx - curx - 1;
   
   /* Allocate a buffer to store the results in. */
   buffer = malloc(len + 1);
   if(buffer == NULL) {
      CAMLreturn(copy_string(""));
   }
   
   /* Get the line input */
   wgetnstr(window, buffer, len);
   
   /* Return the character read */
   result = copy_string(buffer);
   free(buffer);
   CAMLreturn(result);

}


value caml_curses_wrefresh(value mlwindow) {

   CAMLparam1(mlwindow);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Refresh the window display */
   wrefresh(window);
   
   CAMLreturn(Val_unit);

}


value caml_curses_wnoutrefresh(value mlwindow) {

   CAMLparam1(mlwindow);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Refresh the window display; does not write to terminal! */
   wnoutrefresh(window);
   
   CAMLreturn(Val_unit);

}


value caml_curses_doupdate(value unit) {

   CAMLparam1(unit);
   
   /* Draw pending wnoutrefresh calls to the screen. */
   doupdate();
   
   CAMLreturn(Val_unit);

}


/*
 * This forces a redraw of the entire screen.
 */
value caml_curses_refreshscreen(value unit) {

   CAMLparam1(unit);
   
   /* Force redraw of entire screen (assume all lines are tainted) */
   wrefresh(curscr);
   
   CAMLreturn(Val_unit);
   
}


value caml_curses_werase(value mlwindow) {

   CAMLparam1(mlwindow);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Erase the window display */
   werase(window);
   
   CAMLreturn(Val_unit);

}


value caml_curses_wclrtoeol(value mlwindow) {

   CAMLparam1(mlwindow);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Erase to end of current line */
   wclrtoeol(window);
   
   CAMLreturn(Val_unit);

}


value caml_curses_wclrtobot(value mlwindow) {

   CAMLparam1(mlwindow);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Erase to end of current line, then clear all remaining lines. */
   wclrtobot(window);
   
   CAMLreturn(Val_unit);

}


value caml_curses_wmove(value mlwindow, value y, value x) {

   CAMLparam3(mlwindow, y, x);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Move the cursor in the indicated window */
   wmove(window, Int_val(y), Int_val(x));
   
   CAMLreturn(Val_unit);

}


value caml_curses_getyx(value mlwindow) {

   CAMLparam1(mlwindow);
   CAMLlocal1(result);
   WINDOW *window = window_of_ml(mlwindow);
   int y, x;
   
   /* Get the current cursor position in the indicated window. */
   getyx(window, y, x);
   
   /* Construct a tuple to store the results in. */
   result = alloc_tuple(2);
   Field(result, 0) = Val_int(y);
   Field(result, 1) = Val_int(x);
   CAMLreturn(result);

}


value caml_curses_getmaxyx(value mlwindow) {

   CAMLparam1(mlwindow);
   CAMLlocal1(result);
   WINDOW *window = window_of_ml(mlwindow);
   int y, x;
   
   /* Get the maximum cursor position in the indicated window. */
   getmaxyx(window, y, x);
   
   /* Construct a tuple to store the results in. */
   result = alloc_tuple(2);
   Field(result, 0) = Val_int(y);
   Field(result, 1) = Val_int(x);
   CAMLreturn(result);

}


value caml_curses_scrollok(value mlwindow, value scrollable) {

   CAMLparam2(mlwindow, scrollable);
   WINDOW *window = window_of_ml(mlwindow);

   /* Update window settings */   
   scrollok(window, Bool_val(scrollable));
   
   CAMLreturn(Val_unit);

}


value caml_curses_echook(value ok) {

   CAMLparam1(ok);

   /* Set or clear echo flag, based on the flag given. */   
   if(Bool_val(ok)) {
      echo();
   } else {
      noecho();
   }
   
   CAMLreturn(Val_unit);

}


value caml_curses_wscrl(value mlwindow, value lines) {

   CAMLparam2(mlwindow, lines);
   WINDOW *window = window_of_ml(mlwindow);
   
   /* Scroll the window; lines > 0, scroll UP; lines < 0, scroll DN */
   wscrl(window, Int_val(lines));
   
   CAMLreturn(Val_unit);

}


#else /* No NCURSES support! */


CAML_BIND_KEY(caml_key_down,  0)
CAML_BIND_KEY(caml_key_up,    0)
CAML_BIND_KEY(caml_key_left,  0)
CAML_BIND_KEY(caml_key_right, 0)
CAML_BIND_KEY(caml_key_home,  0)
CAML_BIND_KEY(caml_key_end,   0)
CAML_BIND_KEY(caml_key_npage, 0)
CAML_BIND_KEY(caml_key_ppage, 0)
CAML_BIND_KEY(caml_key_enter, 0)
CAML_BIND_KEY(caml_key_cancel,0)


/*
 * Report that we do NOT support ncurses.
 */
value caml_curses_enabled(value unit) {

   CAMLparam1(unit);
   CAMLreturn(Val_false);
   
}


value caml_curses_initscr(value unit) {

   CAMLparam1(unit);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_endwin(value unit) {

   CAMLparam1(unit);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_newwin(value nlines, value ncols, value beginy, value beginx) {

   CAMLparam4(nlines, ncols, beginy, beginx);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_delwin(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_waddch(value mlwindow, value ch) {

   CAMLparam2(mlwindow, ch);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_waddstr(value mlwindow, value str) {

   CAMLparam2(mlwindow, str);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wattron(value mlwindow, value mlattr) {

   CAMLparam2(mlwindow, mlattr);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wattroff(value mlwindow, value mlattr) {

   CAMLparam2(mlwindow, mlattr);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wgetch(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wgetstr(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wrefresh(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wnoutrefresh(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_doupdate(value unit) {

   CAMLparam1(unit);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_refreshscreen(value unit) {

   CAMLparam1(unit);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_werase(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wclrtoeol(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wclrtobot(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wmove(value mlwindow, value y, value x) {

   CAMLparam3(mlwindow, y, x);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_getyx(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_getmaxyx(value mlwindow) {

   CAMLparam1(mlwindow);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_scrollok(value mlwindow, value scrollable) {

   CAMLparam2(mlwindow, scrollable);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_echook(value ok) {

   CAMLparam1(ok);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


value caml_curses_wscrl(value mlwindow, value lines) {

   CAMLparam2(mlwindow, lines);
   failwith("No ncurses support enabled");
   CAMLreturn(Val_unit);

}


#endif /* NCURSES support enabled or disabled? */
