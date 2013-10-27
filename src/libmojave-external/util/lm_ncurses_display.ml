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
open Lm_ncurses
open Lm_printf


(***  Configuration  ***)


(* first_number
   Numerical value of first entry in a list.  Should be set to
   0 or 1, to determine whether items are zero-indexed or not.  *)
let first_number = 1


(***  Basic Display Features  ***)


(* curses_enabled
   True if ncurses support has been enabled in the configure system.
   If this flag is false, then calls to functions exported by this
   module will almost certainly result in Failure exceptions.  *)
let curses_enabled = curses_enabled


(* current_display
   Set to the pair (main, status) if a display has been started.
   This is set to None if no display is currently active.  *)
let current_display = ref None


(* display_active ()
   Returns true if an ncurses display is currently active.  *)
let display_active () =
   match !current_display with
      Some _ ->
         true
    | None ->
         false


(* get_current_display ()
   Returns the current display.  Raises an exception if no display
   has been opened currently.  *)
let get_current_display () =
   match !current_display with
      Some display ->
         display
    | None ->
         raise (Failure "No ncurses display is open!")


(* update_status_deferred msg
   Updates the status window with the indicated message.  The message may
   be truncated to fit into the window.  This raises Failure if no display
   is currently active.  This call does NOT immediately refresh the status
   bar.  *)
let update_status_deferred msg =
   (* Get the current status window *)
   let _, status = get_current_display () in

   (* Get the width of the window, and truncate message if necessary. *)
   let _, width = getmaxyx status in
   let width = width - 2 in
   let len = String.length msg in
   let msg =
      if len > width then
         " " ^ String.sub msg 0 width ^ " "
      else
         " " ^ msg ^ String.make (width - len + 1) ' '
   in

      (* Make sure we are on reverse video, and redraw the window. *)
      wattron status A_REVERSE;
      wmove status 0 0;
      waddstr status msg


(* update_status msg
   Updates the status window with the indicated message.  The message may
   be truncated to fit into the window.  This raises Failure if no display
   is currently active.  *)
let update_status msg =
   let _, status = get_current_display () in
      update_status_deferred msg;
      wrefresh status


(* putchar ch
   Writes the indicated character to the MAIN window.  The main window
   is scrolled if necessary.  Raises Failure if no display is active.  *)
let putchar ch =
   let main, _ = get_current_display () in
      waddch main ch


(* putsubstr buf off len
   Writes the indicated substring of buf to the MAIN window.  The main
   window is scrolled if necessary.  This raises Failure if no display
   is active.  *)
let putsubstr buf off len =
   let main, _ = get_current_display () in
      waddstr main (String.sub buf off len)


(* getchar ()
   Reads a character in from the display.  The character may be an extended
   key, such as an arrow key.  The character will NOT be echoed onto the
   screen by default.  This raises Failure if no display is active.  *)
let getchar () =
   let main, _ = get_current_display () in
      wgetch main


(* flush ()
   Redraws the main window.  You must call this after calls to putchar and/
   or putsubstr, otherwise the text will not show up on the screen.  This
   raises Failure if no display is active.  *)
let flush () =
   let main, _ = get_current_display () in
      wrefresh main


(* flush_all ()
   Redraws ALL windows.  Internal use only.  *)
let flush_all () =
   let main, status = get_current_display () in
      wnoutrefresh main;
      wnoutrefresh status;
      doupdate ()


(* stop_display ()
   Ends ncurses mode and deletes the indicated display windows.  If no
   display is active, then this function is a NOP.  *)
let stop_display () =
   match !current_display with
      Some _ ->
         current_display := None;
         endwin ()
    | None ->
         ()


(* start_display ()
   Starts ncurses mode and creates a status and main window.  If we are
   already in ncurses mode, then the old mode is deleted and a fresh
   mode is started.  If ncurses support is not available, then this will
   most definitely raise a Failure.  *)
let start_display () =
   (* Terminate previous ncurses, and start a new display. *)
   stop_display ();
   initscr ();

   (* Create the status and main windows. *)
   let status = newwin 1 0 0 0 in
   let main = newwin 0 0 1 0 in
      current_display := Some (main, status);

      (* Setup default properties of each window, and force updates. *)
      scrollok status false;
      update_status "";
      wrefresh main


(* in_display f arg
   Executes (f arg) inside an ncurses display.  This will clean up the
   ncurses display, even if f raises an exception.  *)
let in_display f arg =
   start_display ();
   try
      let result = f arg in
         stop_display ();
         printf "@.";
         result
   with
      e ->
         stop_display ();
         printf "@.";
         raise e


(* make_formatter ()
   Makes a formatter suitable for printing to the ncurses display.  *)
let make_formatter () =
   make_formatter putsubstr flush


(***  Text Viewer Interface  ***)


(* update_lines win first last f
   Redraws everything from the first line to the last line indicated
   (inclusive).  f takes the window and the line number to redraw; f
   may assume the cursor is already on the first column of the requested
   line.  *)
let update_lines win first last f =
   let height, width = getmaxyx win in
   let rec loop n =
      if n > last then
         ()
      else begin
         wmove win n 0;
         f win n;
         loop (n + 1)
      end
   in
      loop first;
      wmove win (height - 1) (width - 1)


(* update_top_lines win n f
   Update the top n lines of the display.  *)
let update_top_lines win n f =
   update_lines win 0 (n - 1) f


(* update_bottom_lines win n f
   Update the bottom n lines of the display.  *)
let update_bottom_lines win n f =
   let height, _ = getmaxyx win in
      update_lines win (height - n - 1) (height - 1) f


(* update_all_lines win f
   Updates ALL lines on the display.  *)
let update_all_lines win f =
   let height, _ = getmaxyx win in
      update_lines win 0 (height - 1) f


(* scroll win last_row rqst_delta cur_row f
   Scrolls the window; if rqst_delta < 0, then scroll up; otherwise scroll
   down.  last_row is the last virtual ROW NUMBER that can display text (it
   is NOT a line count!).  cur_row indicates current virtual row number.
   This function will never scroll before line 0; however, it is possible
   that it will display lines past last_row, so f needs to be prepared to
   display something for those lins.  The actual delta that could be granted
   is returned.  *)
let scroll win last_row rqst_delta cur_row f =
   (* Determine what the ACTUAL delta is that we can give the user. *)
   let new_delta =
      if rqst_delta < 0 then
         (* User requested to scroll up *)
         let new_row =
            if (-rqst_delta) > cur_row then
               (* Attemped to scroll up before line 0 *)
               0
            else
               cur_row + rqst_delta
         in
            new_row - cur_row
      else
         (* User requested to scroll down *)
         let height, _ = getmaxyx win in
         let cur_last_row = cur_row + height - 1 in

         (* Check that the last row doesn't go off-screen *)
         let new_last_row =
            if cur_last_row + rqst_delta > last_row then
               last_row
            else
               cur_last_row + rqst_delta
         in

         (* NOW, check that in scrollback we didn't accidentally go off the
            FIRST line of the screen (can happen if we are displaying less
            than a screenful of text). *)
         let new_row =
            if new_last_row - height + 1 < 0 then
               0
            else
               new_last_row - height + 1
         in
            new_row - cur_row
   in

      (* Scroll the window *)
      scrollok win true;
      wscrl win new_delta;
      scrollok win false;

      (* Determine whether we need to update top or bottom of screen *)
      if new_delta < 0 then
         update_top_lines win (-new_delta) (fun win -> f win new_delta)
      else
         update_bottom_lines win new_delta (fun win -> f win new_delta);
      new_delta


(* print_limited_line win buf col
   Prints a single line of text, starting from character position col,
   onto the main window at the current cursor position.  The line will
   NOT wrap if it is too long to display.  The end of line is cleared
   if the line is wider than the text.  The cursor is left after the
   last character.  *)
let print_limited_line win buf col =
   let _, width = getmaxyx win in
   let _, x = getyx win in
   let width = width - x in
   let len = String.length buf in
   let rec loop n =
      if n >= width then
         ()
      else if n + col >= len then
         (* Screen is wider than the text *)
         wclrtoeol win
      else begin
         waddch win buf.[n + col];
         loop (n + 1)
      end
   in
      loop 0


(* text_viewer title text
   Let the user scroll through a buffer of text.  Once the user exits the
   session, control is returned and the scroll buffer is left at its current
   location.  The current contents of the main window are cleared at the
   beginning of this call.  *)
let text_viewer title text =
   (* Get basic information about the display, and determine how much text
      to skip for a pageup/pagedown request. *)
   let main, _ = get_current_display () in
   let height, _ = getmaxyx main in
   let pageskip =
      if height > 2 then
         height - 2
      else
         height
   in

   (* Reformat the text so it is stored in an array, one array entry per
      line of text.  This makes the text a lot easier to work with when we
      are actually displaying it to the terminal. *)
   let text = Lm_string_util.split "\n" text in
   let text = Array.of_list text in
   let text_length = Array.length text in
   let max_col_bound = 1024 in

   (* Setup the help text. *)
   let help = "Arrows scroll  Enter, ^D exit  ^U page-up  ^V page-dn  ^A begin-line  ^L redraw" in
   let scroll_help row col =
      update_status_deferred (sprintf "%s   Type ? for help   r%-5dc%-4dli%-5d"
         title (row + 1) (col + 1) text_length)
   in

   (* Prints a blank line at the current cursor position. *)
   let print_line row col win n =
      if row + n >= text_length then begin
         wattron win A_BOLD;
         print_limited_line win "~" col;
         wattroff win A_BOLD
      end else
         print_limited_line win text.(row + n) col
   in

   (* Scrolling helper functions. *)
   let print_delta_line row col win delta =
      print_line (row + delta) col win
   in
   let scroll n row col =
      let delta = scroll main (text_length - 1) n row (print_delta_line row col) in
         row + delta
   in

   (* This processes keystrokes until the user exits.  The current top-left
      position is given as (row, col), and the status text to display (if
      any) is given in status.  *)
   let rec process_key status row col =
      (match status with
         Some status ->
            update_status_deferred status
       | None ->
            scroll_help row col);
      flush_all ();
      let ch = getchar () in
         if ch = key_err || ch = key_ctrld || ch = key_ctrlj
          || ch = key_ctrlm || ch = key_enter || ch = key_cancel
          || ch = int_of_char 'q' || ch = int_of_char 'Q' then
            ()
         else if ch = key_up then
            process_key None (scroll (-1) row col) col
         else if ch = key_ppage || ch = key_ctrlu then
            process_key None (scroll (-pageskip) row col) col
         else if ch = key_down then
            process_key None (scroll 1 row col) col
         else if ch = key_npage || ch = key_ctrlv then
            process_key None (scroll pageskip row col) col
         else if ch = key_left && col > 0 then begin
            update_all_lines main (print_line row (col - 1));
            process_key None row (col - 1)
         end else if ch = key_right && col < max_col_bound - 1 then begin
            update_all_lines main (print_line row (col + 1));
            process_key None row (col + 1)
         end else if (ch = key_home || ch = key_ctrla) && col > 0 then begin
            update_all_lines main (print_line row 0);
            process_key None row 0
         end else if ch = key_ctrll then begin
            update_all_lines main (print_line row col);
            process_key None row col
         end else if ch = int_of_char '?' || ch = int_of_char '/' then
            process_key (Some help) row col
         else
            process_key None row col
   in

      (* Setup the main window; display initial text and wait for keys *)
      scrollok main false;
      update_all_lines main (print_line 0 0);
      process_key None 0 0;
      update_status "";
      scrollok main true


(***  Menu Selection  ***)


(* menu_select numbered title entries
   Allow the user to select an element from a menu of entries.  If
   successful, then the (zero-indexed) entry number that was selected
   is returned.  If failed, then None is returned.

   If numbered is true, then a numerical index is displayed next to
   each entry.  *)
let menu_select numbered title entries =
   (* Get basic information about the display, and determine how much text
      to skip for a pageup/pagedown request. *)
   let main, _ = get_current_display () in
   let height, _ = getmaxyx main in
   let entry_count = Array.length entries in
   let pageskip = height in

   (* Entry accessors *)
   let entry_key n =
      let key, _, _ = entries.(n) in
         key
   in
   let entry_name n =
      let _, name, _ = entries.(n) in
         name
   in
   let entry_desc n =
      let _, _, desc = entries.(n) in
         desc
   in

   (* Setup the help text. *)
   let help = "Up/Dn highlight  Enter selects  ^D cancels  ^U page-up  ^V page-dn  ^L redraw" in
   let scroll_help selrow =
      update_status_deferred (sprintf "%s   Type ? for help   sel%-4dli%-4d   %s"
         title (selrow + 1) entry_count (entry_desc selrow))
   in

   (* Display a menu entry *)
   let print_line row win prefix n =
      print_limited_line win prefix 0;
      if numbered then
         print_limited_line win (sprintf "#%03d " (row + n + first_number)) 0;
      print_limited_line win (entry_name (row + n)) 0;
      match entry_key (row + n) with
         Some ch ->
            print_limited_line win (sprintf " (%c)" (Char.uppercase ch)) 0
       | None ->
            ()
   in
   let print_line row selrow win n =
      if row + n >= entry_count then
         print_limited_line win "" 0
      else if row + n = selrow then begin
         wattron win A_BOLD;
         print_line row win " > " n;
         wattroff win A_BOLD
      end else begin
         print_line row win "   " n
      end
   in

   (* Match against a keystroke *)
   let match_key ch =
      if ch >= 0 && ch < 256 then
         let ch = Char.uppercase (char_of_int ch) in
         let rec scan n =
            if n >= entry_count then
               None
            else
               match entry_key n with
                  Some ch'
                  when ch = Char.uppercase ch' ->
                     Some n
                | _ ->
                     scan (n + 1)
         in
            scan 0
      else
         None
   in

   (* Scrolling helper functions. *)
   let print_delta_line row selrow win delta =
      print_line (row + delta) selrow win
   in
   let scroll delta row selrow =
      let oldrow = selrow - row in
      let selrow = selrow + delta in
      let selrow =
         if selrow < 0 then
            0
         else if selrow >= entry_count then
            entry_count - 1
         else
            selrow
      in
      let realrow = selrow - row in
      let () = update_lines main oldrow oldrow (print_line row selrow) in
      let delta =
         if realrow < 0 then begin
            (* Change requires scrolling UP *)
            let delta = realrow in
            let delta = scroll main (entry_count - 1) delta row (print_delta_line row selrow) in
               delta
         end else if realrow >= height then begin
            (* Change requires scrolling DOWN *)
            let delta = realrow - height + 1 in
            let delta = scroll main (entry_count - 1) delta row (print_delta_line row selrow) in
               delta
         end else begin
            (* Simple update; simply refresh two lines *)
            update_lines main realrow realrow (print_line row selrow);
            0
         end
      in
         row + delta, selrow
   in

   (* Process keys until the user accepts or cancels. *)
   let rec process_key status row selrow =
      (match status with
         Some status ->
            update_status_deferred status
       | None ->
            scroll_help selrow);
      flush_all ();
      let ch = getchar () in
         match match_key ch with
            Some n ->
               Some n
          | None ->
               if ch = key_err || ch = key_ctrld || ch = key_cancel
                || ch = int_of_char 'q' || ch = int_of_char 'Q' then
                  None
               else if ch = key_ctrlj || ch = key_ctrlm || ch = key_enter then
                  Some selrow
               else if ch = key_up then
                  let row, selrow = scroll (-1) row selrow in
                     process_key None row selrow
               else if ch = key_ppage || ch == key_ctrlu then
                  let row, selrow = scroll (-pageskip) row selrow in
                     process_key None row selrow
               else if ch = key_down then
                  let row, selrow = scroll 1 row selrow in
                     process_key None row selrow
               else if ch = key_npage || ch == key_ctrlv then
                  let row, selrow = scroll pageskip row selrow in
                     process_key None row selrow
               else if ch = key_ctrll then begin
                  update_all_lines main (print_line row selrow);
                  process_key None row selrow
               end else if ch = int_of_char '?' || ch = int_of_char '/' then
                  process_key (Some help) row selrow
               else
                  process_key None row selrow
   in

      (* Setup the main window; display initial text and wait for keys *)
      scrollok main false;
      update_all_lines main (print_line 0 0);
      let result = process_key None 0 0 in
         update_status "";
         scrollok main true;
         result
