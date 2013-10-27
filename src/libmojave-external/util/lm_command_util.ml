(*
   Command-line Interface Utilities
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
open Lm_format_util
open Lm_string_util
open Format

(***  Command Processing (Extract Commands)  ***)


(* parse_command input
   Attempt to process a line of input as a command name and space-
   delimited arguments.  The list of arguments is returned; use the
   get_next_argument command to extract arguments one-at-a-time. *)
let parse_command input =
   let values = Lm_string_util.split " \t\n" input in
   let values = List.fold_left (fun new_values value ->
      if value = "" then
         new_values
      else
         value :: new_values) [] values
   in
   let values = List.rev values in
      values


(* get_next_argument args
   Return the first argument in an argument list, along with the
   tail of the list.  This is NOT List.hd; if the list is empty,
   then this returns an empty string for the next argument.  *)
let get_next_argument args =
   match args with
      arg :: args ->
         arg, args
    | [] ->
         "", []


(* reconstruct_arguments args
   Return a string representing the indicated argument list.  The
   arguments are NOT quoted by this function; think of it as a
   ``feature'' at this time ;)  *)
let reconstruct_arguments args =
   let buf = Buffer.create 4096 in
   let _ = List.fold_left (fun first arg ->
      if not first then
         Buffer.add_string buf " ";
      Buffer.add_string buf arg;
      false) true args
   in
      Buffer.contents buf


(***  Prompt Formatting and Display Properties  ***)


(* tcap_set_bold
   tcap_clear_attr
   Get terminal capabilities for formatting text.  *)
let tcap_set_bold   = Lm_terminfo.tgetstr Lm_terminfo.enter_bold_mode
let tcap_clear_attr = Lm_terminfo.tgetstr Lm_terminfo.exit_attribute_mode


(* xterm_escape_begin
   xterm_escape_end
   XTerm escape sequences for setting and clearing the title text.  *)
let xterm_escape_begin = Lm_terminfo.xterm_escape_begin ()
let xterm_escape_end   = Lm_terminfo.xterm_escape_end ()


(* bold_text text
   Makes the indicated text bold. If no termcap support was available, this
   returns the original prompt, unaltered. This is used primarily for prompt
   formatting.  *)
let bold_text text =
   match tcap_set_bold, tcap_clear_attr with
      Some set, Some clr ->
         set ^ text ^ clr
    | _ ->
         text


(* title_text appname text
   Format text to be displayed in a title, as in an xterm title.  If the
   terminal is not an xterm, then this returns an empty string.  This will
   NOT format the text to display anything to the console itself, under
   any circumstance.  This may prepend the application name to the text. *)
let title_text appname text =
   match xterm_escape_begin, xterm_escape_end with
      Some set, Some clr ->
         if text = "" then
            set ^ appname ^ clr
         else
            set ^ appname ^ ":  " ^ text ^ clr
    | _ ->
         ""


(***  Read-Eval-Print Lm_loop  ***)


(* run_status
   Used to indicate whether a read-eval-print loop should continue accepting
   commands, or if it should terminate.  Once the loop is terminated, an
   arbitrary value may be returned.
      Continue          Command wants the read-eval-print loop to continue.
      Exit value        Command wants loop to exit and return given value.
 *)
type 'result run_status =
   Continue
 | Exit of 'result


(* run_command
   Type of command functions that are called by the read-eval-print loop.
   'args is the type of the arguments (usually string list), and 'result is
   the value returned by commands once they are ready to exit the loop.  *)
type ('args, 'result) run_command = 'args -> 'result run_status


(* command
   Type of a single command entry.  This corresponds to a command name, the
   function to call when the command is invoked, and the help text for the
   command.  *)
type ('args, 'result) command = string * ('args, 'result) run_command * string


(* run_info
   Information given to run_loop which determines how the command prompt is
   formatted and how help text is presented to the user.  Contains these
   fields:
      ri_appname        Name of the application (used in title prompt).
      ri_curses         If true, the help display can use curses interface.
      ri_hack_title     If true, title text will be displayed in prompts.
      ri_help_prefix    Text/overview to prepend to any help display.
      ri_prompt         Function which, when run, returns text to display on
                        each command prompt.  The function is passed a unit
                        argument.
      ri_commands       List of commands to accept, and their actions.
      ri_cmd_quit       Function to call if ^D is struck.
      ri_arg_filter     Command to run on the arguments before passing them
                        to the command function.  space_filter gives the
                        usual string list, split on whitespace.
 *)
type ('args, 'result) run_info =
   { ri_appname      :  string;
     ri_curses       :  bool;
     ri_hack_title   :  bool;
     ri_help_prefix  :  string;
     ri_prompt       :  unit -> string;
     ri_commands     :  ('args, 'result) command list;
     ri_cmd_quit     :  ('args, 'result) run_command;
     ri_arg_filter   :  string -> 'args
   }


(* continue f args
   Takes f : args -> unit and builds a new function which will run f,
   then return Continue.  *)
let continue f args =
   f args;
   Continue


(* get_help_item buf name help
   Displays a single entry of help.  *)
let get_help_item buf name help =
   fprintf buf "@[<hov 3>%s@ " name;
   pp_print_paragraph_bare buf help;
   fprintf buf "@]"


(* get_help buf run
   Displays the help text.  Invoked when the user types "help" at a
   typical read-eval-print loop prompt.  Help is emitted to the indicated
   formatter (usually stdout).  *)
let get_help buf run =
   let { ri_help_prefix = help_prefix;
         ri_commands    = commands;
       } = run
   in
      fprintf buf "@[<v 0>%s" help_prefix;
      fprintf buf "@ Commands available for this prompt are listed below.";
      fprintf buf "@ You can type \"help <name>\" for help on a specific command.";
      List.iter (fun (name, _, help) ->
         fprintf buf "@ @ ";
         get_help_item buf name help) commands;
      fprintf buf "@]@."


(* display_or_dump run f arg
   Depending on the value of ri_curses, this either tries to use the
   ncurses interface, or dumps text straight to the screen.  In either
   case, f will be passed a formatter which it may use for its output. *)
let display_or_dump run f arg =
   if run.ri_curses then
      (* Use the curses text viewer for help display. *)
      let buf = Buffer.create 4096 in
      let fmt = formatter_of_buffer buf in
      let () = f fmt arg in
      let help = Buffer.contents buf in
         Lm_ncurses_display.in_display (fun () ->
            Lm_ncurses_display.text_viewer "Command Help" help) ()
   else
      (* No curses, dump help straight to console *)
      f std_formatter arg


(* cmd_help run arg
   Display help; if the argument is nonempty, then help is displayed
   for the named command only.  *)
let cmd_help run arg =
   if arg = "" then
      display_or_dump run get_help run
   else
      let text = List.fold_left (fun text (name, _, help) ->
         match text with
            None
            when name = arg ->
               Some help
          | _ ->
               text) None run.ri_commands
      in
         match text with
            None ->
               printf "No applicable help found for \"%s\"@." arg
          | Some help ->
               display_or_dump run (fun buf help ->
                  get_help_item buf arg help;
                  fprintf buf "@.") help


(* space_filter arguments
   Process arguments into a list of nonempty strings, which contain
   the arguments separated by whitespace.  This is the typical filter
   given in the run_info structure.  *)
let space_filter arguments =
   parse_command arguments


(* no_filter arguments
   Pass the arguments through unaltered.  *)
let no_filter arguments =
   arguments


(* read_eval_print run
   Read a single command, evaluate it, and print the results.  This
   processes at most one command for constructing a test case; call
   it from within a loop to accept multiple commands.  *)
let read_eval_print run =
   let { ri_prompt      = prompt;
         ri_commands    = commands;
         ri_hack_title  = hack_title;
         ri_arg_filter  = arg_filter;
       } = run
   in
   let cnames = List.map (fun (name, _, _) -> name) commands in
   let cnames = "help" :: cnames in
   let () = Lm_readline.register_commands cnames in
   let () =
      if hack_title then
         printf "%s@." (title_text run.ri_appname "")
      else
         printf "@."
   in
   let input = Lm_readline.readline (bold_text (prompt ())) in
   let command, _ = get_next_argument (parse_command input) in
   let command_len = String.length command in
   let arguments = String.sub input command_len (String.length input - command_len) in
   let arguments = Lm_string_util.trim arguments in
   let arguments = arg_filter arguments in
   let rec find_and_dispatch = function
      (command', call, _) :: commands ->
         if command = command' then
            call arguments
         else
            find_and_dispatch commands
    | [] ->
         printf "Unrecognized command \"%s\".  Try \"help\".@." command;
         Continue
   in
      if command = "help" then
         let _, arguments = get_next_argument (parse_command input) in
         let arg, _ = get_next_argument arguments in
            continue (cmd_help run) arg
      else if command <> "" then
         find_and_dispatch commands
      else
         (* Blank commands are ignored *)
         Continue


(* run_loop read_eval_print run
   Runs the loop for evaluating ri_commands until the user quits and/or
   presses ^D.  This catches any exceptions that may have been thrown
   by the read-eval-print loop, above.  You must indicate what function
   should be called when ^D is pressed, using ri_cmd_quit.  The given
   read_eval_print function is used; usually you want Lm_command_util's
   version of read_eval_print.

   If the ri_curses flag is true, then the help system is allowed to
   use curses interface to display help on the commands.  Any help text
   will be preceded by the contents of the string ri_help_prefix; the
   rest of the help is extracted from commands.

   This function catches EOF, ^C, Unix errors, Failures, and Invalid
   argument exceptions.  If you wish to catch other exceptions in a
   manner that will not abort the run loop, you should pass in a custom
   read_eval_print which catches those exceptions and returns Continue.  *)
let run_loop read_eval_print run =
   let rec loop () =
      let continue =
         try
            read_eval_print run
         with
            End_of_file ->
               run.ri_cmd_quit (run.ri_arg_filter "")
          | Sys.Break ->
               Continue
          | Unix.Unix_error (error, f, arg) ->
               printf "Lm_command_util.run_loop: Unix error on command: %s: %s %s@." (Unix.error_message error) f arg;
               Continue
          | Sys_error msg ->
               printf "Lm_command_util.run_loop: System error on command: %s@." msg;
               Continue
          | Failure msg ->
               printf "Lm_command_util.run_loop: Failure on command: %s@." msg;
               Continue
      in
         match continue with
            Continue ->
               loop ()
          | Exit v ->
               (* We are ready to terminate this loop *)
               v
   in
      loop ()


(* bad_arguments ()
   Helper that indicates the arguments to a command were bad.  *)
let bad_arguments () =
   printf "Arguments given to command were malformed. Try \"help\".@."


(***  Specialised Prompts  ***)


(* int_yes_no_cancel appname hack_title prompt default
   Internal implementation of yes/no(/cancel) prompt.  If the cancel_ok
   flag is true, then the user is presented with a cancel option and may
   respond specifically with cancel; otherwise, cancel is mapped to "no"
   and the user may not type "cancel" (although ^D will map to "no").  *)
let rec int_yes_no_cancel appname hack_title prompt default cancel_ok =
   let rec loop () =
      try
         let default_name =
            match default, cancel_ok with
               Some true, _ ->
                  "yes"
             | Some false, _
             | None, false ->
                  "no"
             | None, true ->
                  "cancel"
         in
         let commands, prompt' =
            if cancel_ok then
               ["yes"; "no"; "cancel"], "[yes,no,cancel]"
            else
               ["yes"; "no"], "[yes,no]"
         in
         let () = Lm_readline.register_commands commands in
         let () =
            if hack_title then
               printf "%s@." (title_text appname "")
            else
               printf "@."
         in
         let () = printf "%s?@." prompt in
         let prompt = sprintf "%s (default %s)? " prompt' default_name in
         let input = Lm_readline.readline (bold_text prompt) in
            match trim input with
               "y" | "yes" ->
                  Some true
             | "n" | "no" ->
                  Some false
             | "c" | "cancel" ->
                  None
             | "" ->
                  (* Use the default option *)
                  printf "Assuming \"%s\"@." default_name;
                  default
             | _ ->
                  printf "I don't understand that response. Please use \"yes\" or \"no\".@.";
                  loop ()
      with
         End_of_file
       | Sys.Break ->
            printf "cancel@.";
            None
   in
      loop ()


(* yes_no_cancel appname hack_title prompt default
   Presents a yes/no/cancel prompt (^D maps to ``cancel'') for the user
   to respond to.  Returns None if the user cancels, otherwise returns
   Some true if the user responds yes and Some false if the user responds
   no.  The default argument is None indicating cancel as default; other-
   wise it is (Some true) for yes or (Some false) for no.  *)
let rec yes_no_cancel appname hack_title prompt default =
   int_yes_no_cancel appname hack_title prompt default true


(* yes_no appname hack_title prompt default
   Similar to yes_no_cancel, but ``cancel'' gets mapped to ``no''.  *)
let yes_no appname hack_title prompt default =
   match int_yes_no_cancel appname hack_title prompt (Some default) false with
      Some result ->
         result
    | None ->
         false


(* ok_to_overwrite_file appname hack_title filename
   If the named file exists, this puts up a prompt if it is okay to
   overwrite the named file, which defaults to yes.  No prompt is
   displayed if the file doesn't exist.  This returns true if either
   the file doesn't exist, or the user says it is okay to overwrite.  *)
let ok_to_overwrite_file appname hack_title filename =
   if Sys.file_exists filename then
      yes_no appname hack_title (sprintf "File \"%s\" exists, overwrite" filename) true
   else
      true
