(*
   Ncurses display library (for standard display functions)
   Copyright (C) 2002 Justin David Smith, Caltech

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation,
   version 2.1 of the License.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
   
   Additional permission is given to link this library with the
   OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
   and you may distribute the linked executables.  See the file
   LICENSE.libmojave for more details.
 *)
open Lm_printf

(***  Basic Display Features  ***)


(* curses_enabled
   True if ncurses support has been enabled in the configure system.
   If this flag is false, then calls to functions exported by this
   module will almost certainly result in Failure exceptions.  *)
val curses_enabled : bool


(* display_active ()
   Returns true if an ncurses display is currently active.  *)
val display_active : unit -> bool


(* update_status msg
   Updates the status window with the indicated message.  The message may
   be truncated to fit into the window.  This raises Failure if no display
   is currently active.  *)
val update_status : string -> unit


(* putchar ch
   Writes the indicated character to the MAIN window.  The main window
   is scrolled if necessary.  Raises Failure if no display is active.  *)
val putchar : char -> unit


(* putsubstr buf off len
   Writes the indicated substring of buf to the MAIN window.  The main
   window is scrolled if necessary.  This raises Failure if no display
   is active.  *)
val putsubstr : string -> int -> int -> unit


(* getchar ()
   Reads a character in from the display.  The character may be an extended
   key, such as an arrow key.  The character will NOT be echoed onto the
   screen by default.  This raises Failure if no display is active.  *)
val getchar : unit -> int


(* flush ()
   Redraws the main window.  You must call this after calls to putchar and/
   or putsubstr, otherwise the text will not show up on the screen.  This
   raises Failure if no display is active.  *)
val flush : unit -> unit


(* stop_display ()
   Ends ncurses mode and deletes the indicated display windows.  If no
   display is active, then this function is a NOP.  *)
val stop_display : unit -> unit


(* start_display ()
   Starts ncurses mode and creates a status and main window.  If we are
   already in ncurses mode, then the old mode is deleted and a fresh
   mode is started.  If ncurses support is not available, then this will
   most definitely raise a Failure.  *)
val start_display : unit -> unit


(* in_display f arg
   Executes (f arg) inside an ncurses display.  This will clean up the
   ncurses display, even if f raises an exception.  *)
val in_display : ('a -> 'b) -> 'a -> 'b


(* make_formatter ()
   Makes a formatter suitable for printing to the ncurses display.  *)
val make_formatter : unit -> formatter


(***  Text Viewer Interface  ***)


(* text_viewer title text
   Let the user scroll through a buffer of text.  Once the user exits the
   session, control is returned and the scroll buffer is left at its current
   location.  The current contents of the main window are cleared at the
   beginning of this call.  *)
val text_viewer : string -> string -> unit


(***  Menu Selection  ***)


(* menu_select numbered title entries
   Allow the user to select an element from a menu of entries.  If
   successful, then the (zero-indexed) entry number that was selected
   is returned.  If failed, then None is returned.

   If numbered is true, then a numerical index is displayed next to
   each entry.  *)
val menu_select : bool -> string -> (char option * string * string) array -> int option
