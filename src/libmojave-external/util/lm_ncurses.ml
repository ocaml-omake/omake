(*
 * Simple NCurses interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
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
 *)
 
 
(***  Types  ***)


(* window
   Type of all ncurses window interfaces.  This type is abstract in 
   OCaml but is a real pointer in the C code (interned by ncurses).  *)
type window


(* attr
   Attributes which can be passed to the various attr() functions.
   These control the display properties of the window.  *)
type attr =
   A_NORMAL
 | A_STANDOUT
 | A_UNDERLINE
 | A_REVERSE
 | A_BLINK
 | A_DIM
 | A_BOLD
 | A_PROTECT
 | A_INVIS 
 | A_ALTCHARSET
 | A_CHARTEXT


(***  C Interface Functions for Key Values  ***)


external caml_key_down  : unit -> int = "caml_key_down"
external caml_key_up    : unit -> int = "caml_key_up"
external caml_key_left  : unit -> int = "caml_key_left"
external caml_key_right : unit -> int = "caml_key_right"
external caml_key_home  : unit -> int = "caml_key_home"
external caml_key_end   : unit -> int = "caml_key_end"
external caml_key_npage : unit -> int = "caml_key_npage"
external caml_key_ppage : unit -> int = "caml_key_ppage"
external caml_key_enter : unit -> int = "caml_key_enter"
external caml_key_cancel: unit -> int = "caml_key_cancel"


(***  Key Values  ***)


let key_down   = caml_key_down   ()
let key_up     = caml_key_up     ()
let key_left   = caml_key_left   ()
let key_right  = caml_key_right  ()
let key_home   = caml_key_home   ()
let key_end    = caml_key_end    ()
let key_npage  = caml_key_npage  ()
let key_ppage  = caml_key_ppage  ()
let key_enter  = caml_key_enter  ()
let key_cancel = caml_key_cancel ()

let key_err    = -1
let key_ctrla  = 1
let key_ctrld  = 4
let key_ctrle  = 5
let key_ctrlj  = 10
let key_ctrll  = 12
let key_ctrlm  = 13
let key_ctrlu  = 21
let key_ctrlv  = 22


(***  C Interface Functions for Curses  ***)


external caml_curses_enabled : unit -> bool = "caml_curses_enabled"
external caml_curses_initscr : unit -> unit = "caml_curses_initscr"
external caml_curses_endwin : unit -> unit = "caml_curses_endwin"
external caml_curses_newwin : int -> int -> int -> int -> window = "caml_curses_newwin"
external caml_curses_delwin : window -> unit = "caml_curses_delwin"
external caml_curses_waddch : window -> char -> unit = "caml_curses_waddch"
external caml_curses_waddstr : window -> string -> unit = "caml_curses_waddstr"
external caml_curses_wattron : window -> attr -> unit = "caml_curses_wattron"
external caml_curses_wattroff : window -> attr -> unit = "caml_curses_wattroff"
external caml_curses_wgetch : window -> int = "caml_curses_wgetch"
external caml_curses_wgetstr : window -> string = "caml_curses_wgetstr"
external caml_curses_wrefresh : window -> unit = "caml_curses_wrefresh"
external caml_curses_wnoutrefresh : window -> unit = "caml_curses_wnoutrefresh"
external caml_curses_doupdate : unit -> unit = "caml_curses_doupdate"
external caml_curses_refreshscreen : unit -> unit = "caml_curses_refreshscreen"
external caml_curses_werase : window -> unit = "caml_curses_werase"
external caml_curses_wclrtoeol : window -> unit = "caml_curses_wclrtoeol"
external caml_curses_wclrtobot : window -> unit = "caml_curses_wclrtobot"
external caml_curses_wmove : window -> int -> int -> unit = "caml_curses_wmove"
external caml_curses_getyx : window -> int * int = "caml_curses_getyx"
external caml_curses_getmaxyx : window -> int * int = "caml_curses_getmaxyx"
external caml_curses_scrollok : window -> bool -> unit = "caml_curses_scrollok"
external caml_curses_echook : bool -> unit = "caml_curses_echook"
external caml_curses_wscrl : window -> int -> unit = "caml_curses_wscrl"


(***  Exported Interface  ***)


let curses_enabled = caml_curses_enabled ()
let initscr = caml_curses_initscr
let endwin = caml_curses_endwin
let newwin = caml_curses_newwin
let delwin = caml_curses_delwin
let waddch = caml_curses_waddch
let waddstr = caml_curses_waddstr
let wattron = caml_curses_wattron
let wattroff = caml_curses_wattroff
let wgetch = caml_curses_wgetch
let wgetstr = caml_curses_wgetstr
let wrefresh = caml_curses_wrefresh
let wnoutrefresh = caml_curses_wnoutrefresh
let doupdate = caml_curses_doupdate
let refreshscreen = caml_curses_refreshscreen
let werase = caml_curses_werase
let wclrtoeol = caml_curses_wclrtoeol
let wclrtobot = caml_curses_wclrtobot
let wmove = caml_curses_wmove
let getyx = caml_curses_getyx
let getmaxyx = caml_curses_getmaxyx
let scrollok = caml_curses_scrollok
let echook = caml_curses_echook
let wscrl = caml_curses_wscrl
