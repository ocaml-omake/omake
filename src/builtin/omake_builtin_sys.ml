(*
 * Miscellaneous system functions.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, Caltech and HRL Laboratories, LLC
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
 * Authors: Jason Hickey @email{jyh@cs.caltech.edu}
 *          Erick Tryzelaar @email{erickt@dslextreme.com}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_printf
open Lm_location

open Omake_ir
open Omake_env
open Omake_pos
open Omake_var
open Omake_node
open Omake_eval
open Omake_value
open Omake_symbol
open Omake_builtin
open Omake_builtin_type
open Omake_builtin_util
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_builtin_sys" end);;
open Pos

(************************************************************************
 * Passwd database access.
 *)

(*
 * \begin{doc}
 * \obj{Passwd}
 *
 * The \verb+Passwd+ object represents an entry in the system's user database.
 * It contains the following fields.
 *
 * \begin{description}
 * \itemidx{pw\_name}: the login name.
 * \itemidx{pw\_passwd}: the encrypted password.
 * \itemidx{pw\_uid}: user id of the user.
 * \itemidx{pw\_gid}: group id of the user.
 * \itemidx{pw\_gecos}: the user name or comment field.
 * \itemidx{pw\_dir}: the user's home directory.
 * \itemidx{pw\_shell}: the user's default shell.
 * \end{description}
 *
 * Not all the fields will have meaning on all operating systems.
 *
 * \twofuns{getpwnam}{getpwuid}
 *
 * \begin{verbatim}
 *     $(getpwnam name...) : Passwd
 *        name : String
 *     $(getpwuid uid...) : Passwd
 *        uid : Int
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+getpwnam+ function looks up an entry by the user's login and the \verb+getpwuid+
 * function looks up an entry by user's numerical id (uid). If no entry is found, an exception
 * will be raised.
 *
 * \fun{getpwents}
 *
 * \begin{verbatim}
 *     $(getpwents) : Array
 * \end{verbatim}
 *
 * The \verb+getpwents+ function returns an array of \verb+Passwd+ objects, one for every user
 * fund in the system user database. Note that depending on the operating system and on the setup
 * of the user database, the returned array may be incomplete or even empty.
 * \end{doc}
 *)

let create_passwd_obj obj passwd =
   let obj = venv_add_field_internal obj pw_name_sym   (ValString passwd.Unix.pw_name) in
   let obj = venv_add_field_internal obj pw_passwd_sym (ValString passwd.Unix.pw_passwd) in
   let obj = venv_add_field_internal obj pw_uid_sym    (ValInt    passwd.Unix.pw_uid) in
   let obj = venv_add_field_internal obj pw_gid_sym    (ValInt    passwd.Unix.pw_gid) in
   let obj = venv_add_field_internal obj pw_gecos_sym  (ValString passwd.Unix.pw_gecos) in
   let obj = venv_add_field_internal obj pw_dir_sym    (ValString passwd.Unix.pw_dir) in
   let obj = venv_add_field_internal obj pw_shell_sym  (ValString passwd.Unix.pw_shell) in
      ValObject obj

let getpwnam venv pos loc args =
   let pos = string_pos "getpwnam" pos in
   let obj = venv_find_object_or_empty venv passwd_object_var in
   let user =
      match args with
         [user] -> string_of_value venv pos user
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let passwd =
      try Unix.getpwnam user with
         Not_found ->
            raise (OmakeException (loc_pos loc pos, StringStringError ("unknown user", user)))
   in
      create_passwd_obj obj passwd

let getpwuid venv pos loc args =
   let pos = string_pos "getpwuid" pos in
   let obj = venv_find_object_or_empty venv passwd_object_var in
   let uid =
      match args with
         [uid] -> int_of_value venv pos uid
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let passwd =
      try Unix.getpwuid uid with
         Not_found ->
            raise (OmakeException (loc_pos loc pos, StringIntError ("unknown uid", uid)))
   in
      create_passwd_obj obj passwd

let getpwents venv _pos _loc _args =
   let obj = venv_find_object_or_empty venv passwd_object_var in
   let ents = List.map (create_passwd_obj obj) (Lm_unix_util.getpwents ()) in
      ValArray ents

(************************************************************************
 * Group database access.
 *)

(*
 * \begin{doc}
 * \obj{Group}
 *
 * The \verb+Group+ object represents an entry in the system's user group database.
 * It contains the following fields.
 *
 * \begin{description}
 * \itemidx{gr\_name}: the group name.
 * \itemidx{gr\_group}: the encrypted password.
 * \itemidx{gr\_gid}: group id of the group.
 * \itemidx{gr\_mem}: the group member's user names.
 * \end{description}
 *
 * Not all the fields will have meaning on all operating systems.
 *
 * \twofuns{getgrnam}{getgrgid}
 *
 * \begin{verbatim}
 *     $(getgrnam name...) : Group
 *        name : String
 *     $(getgrgid gid...) : Group
 *        gid : Int
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+getgrnam+ function looks up a group entry by the group's name and the \verb+getgrgid+
 * function looks up an entry by groups's numerical id (gid). If no entry is found, an exception
 * will be raised.
 *
 * \end{doc}
 *)
let create_group_obj obj group =
   let gr_mem = Array.fold_right (fun s x -> ValString s::x) group.Unix.gr_mem [] in
   let obj = venv_add_field_internal obj gr_name_sym   (ValString group.Unix.gr_name) in
   let obj = venv_add_field_internal obj gr_passwd_sym (ValString group.Unix.gr_passwd) in
   let obj = venv_add_field_internal obj gr_gid_sym    (ValInt    group.Unix.gr_gid) in
   let obj = venv_add_field_internal obj gr_mem_sym    (ValArray  gr_mem) in
      ValObject obj

let getgrnam venv pos loc args =
   let pos = string_pos "getgrnam" pos in
   let obj = venv_find_object_or_empty venv group_object_var in
   let user =
      match args with
         [user] -> string_of_value venv pos user
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let group =
      try Unix.getgrnam user with
         Not_found ->
            raise (OmakeException (loc_pos loc pos, StringStringError ("unknown user", user)))
   in
      create_group_obj obj group

let getgrgid venv pos loc args =
   let pos = string_pos "getgruid" pos in
   let obj = venv_find_object_or_empty venv group_object_var in
   let gid =
      match args with
         [gid] -> int_of_value venv pos gid
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let group =
      try Unix.getgrgid gid with
         Not_found ->
            raise (OmakeException (loc_pos loc pos, StringIntError ("unknown gid", gid)))
   in
      create_group_obj obj group

(*
 * \begin{doc}
 * \fun{tgetstr}
 *
 * \begin{verbatim}
 *    $(tgetstr id) : String
 *       id : String
 * \end{verbatim}
 *
 * The \verb+tgetstr+ function looks up the terminal capability with the indicated \verb+id+.
 * This assumes the terminfo to lookup is given in the \verb+TERM+ environment variable. This
 * function returns an empty value if the given terminal capability is not defined.
 *
 * Note: if you intend to use the value returned by \verb+tgetstr+ inside the shell
 * \hypervarn{prompt}, you need to wrap it using the \hyperfun{prompt-invisible}.
 * \end{doc}
 *)
let tgetstr venv pos loc args =
   let pos = string_pos "tgetstr" pos in
      match args with
         [arg] ->
            begin match Lm_terminfo.tgetstr (string_of_value venv pos arg) with
               Some s -> ValData s
             | None -> ValNone
            end
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let str_wrap name f v venv pos loc args =
   let pos = string_pos name pos in
      if args <> [] then
         raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))
      else
         match f v with
            Some s -> ValData s
          | None -> ValNone

(*
 * \begin{doc}
 * \twofuns{xterm-escape-begin}{xterm-escape-end}
 *
 * \begin{verbatim}
 *    $(xterm-escape-begin) : String
 *    $(xterm-escape-end) : String
 * \end{verbatim}
 *
 * The \verb+xterm-escape-begin+ and \verb+xterm-escape-end+ functions return the escape sequences
 * that can be used to set the XTerm window title. Will return empty values if this capability is
 * not available.
 *
 * Note: if you intend to use these strings inside the shell \hypervarn{prompt}, you need to use
 * \verb+$(prompt_invisible_begin)$(xterm-escape-begin)+ and
 * \verb+$(xterm-escape-end)$(prompt_invisible_end)+.
 * \end{doc}
 *)
let xterm_escape_begin = str_wrap "xterm-escape-begin" Lm_terminfo.xterm_escape_begin ()
let xterm_escape_end   = str_wrap "xterm-escape-end"   Lm_terminfo.xterm_escape_end   ()

(*
 * \begin{doc}
 * \fun{xterm-escape}
 *
 * \begin{verbatim}
 *    $(xterm-escape s) : Sequence
 * \end{verbatim}
 *
 * When the \verb+TERM+ environment variable indicates that the XTerm title setting capability is available,
 * \verb+$(xterm-escape s)+ is equivalent to \verb+$(xterm-escape-begin)s$(xterm-escape-end)+. Otherwise, it
 * returns an empty value.
 *
 * Note: if you intend to use the value returned by \verb+xterm-escape+ inside the shell
 * \hypervarn{prompt}, you need to wrap it using the \hyperfun{prompt-invisible}.
 * \end{doc}
 *
 * Implemented in Pervasives.om
 *)

(*
 * \begin{doc}
 * \twofuns{prompt-invisible-begin}{prompt-invisible-end}
 *
 * \begin{verbatim}
 *    $(prompt-invisible-begin) : String
 *    $(prompt-invisible-end) : String
 * \end{verbatim}
 *
 * The \verb+prompt-invisible-begin+ and \verb+prompt-invisible-end+ functions return the escape sequences
 * that must used to mark the ``invisible'' sections of the shell \hypervarn{prompt} (such as various escape sequences).
 * \end{doc}
 *)
let opt_wrap f = function
   Some x -> Some (f x)
 | None -> None

let prompt_invisible_begin = str_wrap "prompt-invisible-begin" (opt_wrap fst) Omake_readline.prompt_invisible
let prompt_invisible_end   = str_wrap "prompt-invisible-end"   (opt_wrap snd) Omake_readline.prompt_invisible

(*
 * \begin{doc}
 * \fun{prompt-invisible}
 *
 * \begin{verbatim}
 *    $(prompt-invisible s) : Sequence
 * \end{verbatim}
 *
 * The \verb+prompt-invisible+ will wrap its argument with \verb+$(prompt-invisible-begin)+ and
 * \verb+$(prompt-invisible-end)+. All the `invisible'' sections of the shell \hypervarn{prompt} (such as various
 * escape sequences) must be wrapped this way.
 * \end{doc}
 *
 * Implemented in Pervasives.om
 *)


(*
 * \begin{doc}
 * \fun{gettimeofday}
 *
 * \begin{verbatim}
 *    $(gettimeofday) : Float
 * \end{verbatim}
 *
 * The \verb+gettimeofday+ function returns the time of day in seconds
 * since January 1, 1970.
 *
 * \end{doc}
 *)
let gettimeofday venv pos loc args =
   let pos = string_pos "gettimeofday" pos in
      match args with
         [] ->
            ValFloat (Unix.gettimeofday ())
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(*
 * \begin{doc}
 * \obj{Tm}
 * The \verb+Tm+ object is a structure that represents the time and date.
 *
 * \begin{description}
 * \itemidx{tm\_sec} \verb+: Int+ Seconds (0--59).
 * \itemidx{tm\_min} \verb+: Int+ Minutes (0--59).
 * \itemidx{tm\_hour} \verb+: Int+ Hours (0--23).
 * \itemidx{tm\_mday} \verb+: Int+ Day of the month (0--31).
 * \itemidx{tm\_mon} \verb+: Int+ Month (0--11).
 * \itemidx{tm\_year} \verb+: Int+ Year (minus 1900).
 * \itemidx{tm\_wday} \verb+: Int+ Day of the week (0--6, Sunday is 0).
 * \itemidx{tm\_yday} \verb+: Int+ Day of the year (0--365).
 * \itemidx{tm\_isdst} \verb+: Bool+ True iff daylight savings time is in effect.
 * \end{description}
 *
 * \twofuns{gmtime}{localtime}
 * \begin{verbatim}
 *    $(gmtime time) : tm
 *    $(localtime time) : tm
 *        time : Float
 * \end{verbatim}
 *
 * Convert the time in seconds since the Unix epoch to calendar format.
 * The function \verb+gmtime+ assumes UTC (Coordinated Universal Time);
 * the function \verb+localtime+ uses the local time zone.
 * \end{doc}
 *)
let tm_object venv info =
   let obj = venv_find_object_or_empty venv tm_object_var in
   let obj = venv_add_field_internal obj tm_sec_sym   (ValInt info.Unix.tm_sec) in
   let obj = venv_add_field_internal obj tm_min_sym   (ValInt info.Unix.tm_min) in
   let obj = venv_add_field_internal obj tm_hour_sym  (ValInt info.Unix.tm_hour) in
   let obj = venv_add_field_internal obj tm_mday_sym  (ValInt info.Unix.tm_mday) in
   let obj = venv_add_field_internal obj tm_mon_sym   (ValInt info.Unix.tm_mon) in
   let obj = venv_add_field_internal obj tm_year_sym  (ValInt info.Unix.tm_year) in
   let obj = venv_add_field_internal obj tm_wday_sym  (ValInt info.Unix.tm_wday) in
   let obj = venv_add_field_internal obj tm_yday_sym  (ValInt info.Unix.tm_yday) in
   let obj = venv_add_field_internal obj tm_isdst_sym (if info.Unix.tm_isdst then val_true else val_false) in
      obj

let gmtime venv pos loc args =
   let pos = string_pos "gmtime" pos in
      match args with
         [now] ->
            let info = Unix.gmtime (float_of_value venv pos now) in
               ValObject (tm_object venv info)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let localtime venv pos loc args =
   let pos = string_pos "gmtime" pos in
      match args with
         [now] ->
            let info = Unix.localtime (float_of_value venv pos now) in
               ValObject (tm_object venv info)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \twofuns{mktime}{normalize-time}
 * \begin{verbatim}
 *    $(mktime tm) : Float
 *    $(normalize-time tm) : Tm
 *        tm : Tm
 * \end{verbatim}
 *
 * Convert the calendar time to time in seconds since the Unix epoch.
 * Assumes the local time zone.
 *
 * The fields \verb+tm_wday+, \verb+tm_mday+, \verb+tm_yday+ are ignored.
 * The other components are not restricted to their normal ranges and will be
 * normalized as needed.
 *
 * The function \verb+normalize-time+ normalizes the
 * calendar time.  The returned object contains an additional field
 * \verb+tm_time : Float+ that represnets the time in seconds since the Unix epoch
 * (the same value returned by \verb+mktime+).
 * \end{doc}
 *)
let mktime_aux select venv pos loc args =
   let pos = string_pos "mktime" pos in
   let obj =
      match args with
         [obj] ->
            eval_object venv pos obj
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let info =
      { Unix.tm_sec   = int_of_value venv pos (venv_find_field_internal obj pos tm_sec_sym);
        Unix.tm_min   = int_of_value venv pos (venv_find_field_internal obj pos tm_min_sym);
        Unix.tm_hour  = int_of_value venv pos (venv_find_field_internal obj pos tm_hour_sym);
        Unix.tm_mday  = int_of_value venv pos (venv_find_field_internal obj pos tm_mday_sym);
        Unix.tm_mon   = int_of_value venv pos (venv_find_field_internal obj pos tm_mon_sym);
        Unix.tm_year  = int_of_value venv pos (venv_find_field_internal obj pos tm_year_sym);
        Unix.tm_wday  = int_of_value venv pos (venv_find_field_internal obj pos tm_wday_sym);
        Unix.tm_yday  = int_of_value venv pos (venv_find_field_internal obj pos tm_yday_sym);
        Unix.tm_isdst = bool_of_value venv pos (venv_find_field_internal obj pos tm_isdst_sym)
      }
   in
      select (Unix.mktime info)

let mktime = mktime_aux (fun (secs, _) -> ValFloat secs)
let normalize_tm venv pos loc args =
   mktime_aux (fun (secs, tm) ->
         let obj = tm_object venv tm in
         let obj = venv_add_field_internal obj tm_time_sym (ValFloat secs) in
            ValObject obj) venv pos loc args

(************************************************************************
 * Tables.
 *)

let () =
   let builtin_funs = [
      true, "gettimeofday",  gettimeofday,  ArityExact 0;
      true, "getpwnam",      getpwnam,      ArityExact 1;
      true, "getpwuid",      getpwuid,      ArityExact 1;
      true, "getpwents",     getpwents,     ArityExact 0;
      true, "getgrnam",      getgrnam,      ArityExact 1;
      true, "getgrgid",      getgrgid,      ArityExact 1;
      true, "tgetstr",       tgetstr,       ArityExact 1;
      true, "xterm-escape-begin", xterm_escape_begin, ArityExact 0;
      true, "xterm-escape-end",   xterm_escape_end,   ArityExact 0;
      true, "prompt-invisible-begin", prompt_invisible_begin, ArityExact 0;
      true, "prompt-invisible-end",   prompt_invisible_end,   ArityExact 0;
      true, "gmtime",        gmtime,        ArityExact 1;
      true, "localtime",     localtime,     ArityExact 1;
      true, "mktime",        mktime,        ArityExact 1;
      true, "normalize-tm",  normalize_tm,  ArityExact 1;
   ] in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
