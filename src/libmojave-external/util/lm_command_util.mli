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


(***  Command Processing (Extract Commands)  ***)


(* get_next_argument args
   Return the first argument in an argument list, along with the
   tail of the list.  This is NOT List.hd; if the list is empty,
   then this returns an empty string for the next argument.  *)
val get_next_argument : string list -> string * string list


(* reconstruct_arguments args
   Return a string representing the indicated argument list.  The
   arguments are NOT quoted by this function; think of it as a
   ``feature'' at this time ;)  *)
val reconstruct_arguments : string list -> string


(***  Prompt Formatting and Display Properties  ***)


(* tcap_set_bold
   tcap_clear_attr
   Get terminal capabilities for formatting text.  *)
val tcap_set_bold : string option
val tcap_clear_attr : string option


(* xterm_escape_begin
   xterm_escape_end
   XTerm escape sequences for setting and clearing the title text.  *)
val xterm_escape_begin : string option
val xterm_escape_end : string option


(* bold_text text
   Makes the indicated text bold. If no termcap support was available, this
   returns the original prompt, unaltered. This is used primarily for prompt
   formatting.  *)
val bold_text : string -> string


(* title_text appname text
   Format text to be displayed in a title, as in an xterm title.  If the
   terminal is not an xterm, then this returns an empty string.  This will
   NOT format the text to display anything to the console itself, under
   any circumstance.  This may prepend the application name to the text. *)
val title_text : string -> string -> string


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
val continue : ('a -> unit) -> 'a -> 'result run_status


(* space_filter arguments
   Process arguments into a list of nonempty strings, which contain
   the arguments separated by whitespace.  This is the typical filter
   given in the run_info structure.  *)
val space_filter : string -> string list


(* no_filter arguments
   Pass the arguments through unaltered.  *)
val no_filter : string -> string


(* read_eval_print run
   Read a single command, evaluate it, and print the results.  This
   processes at most one command for constructing a test case; call
   it from within a loop to accept multiple commands.  *)
val read_eval_print : ('args, 'result) run_info -> 'result run_status


(* run_loop read_eval_print run
   Runs the loop for evaluating ri_commands until the user quits and/or
   presses ^D.  This catches any exceptions that may have been thrown
   by the read-eval-print loop, above.  You must indicate what function
   should be called when ^D is pressed, using ri_cmd_quit.  The given
   read_eval_print function is used; usually you want Mcc_command_util's
   version of read_eval_print.

   If the ri_curses flag is true, then the help system is allowed to
   use curses interface to display help on the commands.  Any help text
   will be preceded by the contents of the string ri_help_prefix; the
   rest of the help is extracted from commands.

   This function catches EOF, ^C, Unix errors, Failures, and Invalid
   argument exceptions.  If you wish to catch other exceptions in a
   manner that will not abort the run loop, you should pass in a custom
   read_eval_print which catches those exceptions and returns Continue.  *)
val run_loop : (('args, 'result) run_info -> 'result run_status) -> ('args, 'result) run_info -> 'result


(* bad_arguments ()
   Helper that indicates the arguments to a command were bad.  *)
val bad_arguments : unit -> unit


(***  Specialised Prompts  ***)


(* yes_no_cancel appname hack_title prompt default
   Presents a yes/no/cancel prompt (^D maps to ``cancel'') for the user
   to respond to.  Returns None if the user cancels, otherwise returns
   Some true if the user responds yes and Some false if the user responds
   no.  *)
val yes_no_cancel : string -> bool -> string -> bool option -> bool option


(* yes_no appname hack_title prompt default
   Similar to yes_no_cancel, but ``cancel'' gets mapped to ``no''.  *)
val yes_no : string -> bool -> string -> bool -> bool


(* ok_to_overwrite_file appname hack_title filename
   If the named file exists, this puts up a prompt if it is okay to
   overwrite the named file, which defaults to yes.  No prompt is
   displayed if the file doesn't exist.  This returns true if either
   the file doesn't exist, or the user says it is okay to overwrite.  *)
val ok_to_overwrite_file : string -> bool -> string -> bool
