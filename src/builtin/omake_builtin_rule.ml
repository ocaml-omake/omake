(*
 * Some builtin functions.
 *
 * \begin{doc}
 * \chapter{Build functions and utilities}
 * \label{chapter:build}
 * \cutname{omake-build.html}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf

open Lm_symbol
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_pos
open Omake_eval
open Omake_wild
open Omake_node
open Omake_exec
open Omake_rule
open Omake_lexer
open Omake_value
open Omake_state
open Omake_symbol
open Omake_builtin
open Omake_value_type
open Omake_builtin_type
open Omake_builtin_util
open Omake_command_type

module Pos = MakePos (struct let name = "Omake_builtin_rule" end)
open Pos

(*
 * These targets are decribed in doc/src/omake-rules.tex
 *
 * \begin{doc}
 * \section{Builtin .PHONY targets}
 *
 * The complete set of builtin \verb+.PHONY+ targets include the following.
 *
 * \begin{description}
 * \item[.PHONY] Declares new phony targets (Section~\ref{target:.PHONY}).
 * \item[.DEFAULT] Declare the default build targets (Section~\ref{target:.DEFAULT}).
 * \item[.SUBDIRS] Include a directory as part of the project (Section~\ref{target:.SUBDIRS}).
 * \item[.SCANNER] Define a dependency scanner (Section~\ref{target:.SUBDIRS}).
 * \item[.INCLUDE] Include a file (Section~\ref{target:.INCLUDE}).
 * \item[.ORDER] Define a file-dependency ordering rule (Section~\ref{target:.ORDER}).
 * \item[.BUILD\_BEGIN] Commands to be executed at the beginning of a build.
 * \item[.BUILD\_SUCCESS] Commands to be executed if the build is successful.
 * \item[.BUILD\_FAILURE] Commands to be executed if the build fails.
 * \end{description}
 *
 * \targetlabelref{.BUILD_BEGIN}{.BUILD\_BEGIN}
 * \targetlabelref{.BUILD_SUCCESS}{.BUILD\_SUCCESS}
 * \targetlabelref{.BUILD_FAILURE}{.BUILD\_FAILURE}
 *
 * The \verb+.BUILD+ targets can be used to specify commands to be executed at
 * the beginning and end of the build.  The \verb+.BUILD_BEGIN+ target is built
 * at the beginning of a project build, and one of \verb+.BUILD_FAILURE+ or
 * \verb+.BUILD_SUCCESS+ is executed when the build terminates.
 *
 * For example, the following set of rules simply print additional messages
 * about the status of the build.
 *
 * \begin{verbatim}
 *    .BUILD_BEGIN:
 *        echo Build starting
 *
 *    .BUILD_SUCCESS:
 *        echo The build was successful
 *
 *    .BUILD_FAILURE:
 *        println($"The build failed: $(length $(find-build-targets Failed)) targets could not be built")
 * \end{verbatim}
 *
 * Another common use is to define notifications to be performed when
 * the build completes.  For example, the following rule will create
 * a new X terminal displaying the summary of the build
 * (using the \hypervarx{BUILD_SUMMARY}{BUILD\_SUMMARY}).
 *
 * \begin{verbatim}
 *     .BUILD_FAILURE:
 *         xterm -e vi $(BUILD_SUMMARY)
 * \end{verbatim}
 *
 * If you do not wish to add these rules directly to your project (which
 * is probably a good idea if you work with others), you can
 * define them in your \verb+.omakerc+ (see Section~\ref{section:.omakerc}).
 *
 * The \hyperfun{find-build-targets}
 * is useful for obtaining a firther summary of the build.  Note that
 * when output diversions are in effect (with the \verb+--output-*+ options --- see Chapter~\ref{chapter:options}),
 * any output produced by the commands is copied to a file.  The name of the
 * file is specified by the \verb+output-file+ field of the \hyperobj{Target}.
 * You may find this useful in defining custom build summaries.
 * \end{doc}
 *)
let phony_targets =
   [".PHONY"; ".DEFAULT"; ".SUBDIRS"; ".SCANNER"; ".INCLUDE"; ".ORDER";
    ".BUILD_BEGIN"; ".BUILD_SUCCESS"; ".BUILD_FAILURE"]

(************************************************************************
 * Set options.
 *
 * \begin{doc}
 * \section{Options and versioning}
 * \fun{OMakeFlags}
 *
 * \begin{verbatim}
 *    OMakeFlags(options)
 *       options : String
 * \end{verbatim}
 *
 * The \verb+OMakeFlags+ function is used to set \verb+omake+ options from
 * within \File{OMakefile}s.  The options have exactly the same format as
 * options on the command line.
 *
 * For example, the following code displays the progress bar unless
 * the \verb+VERBOSE+ environment variable is defined.
 *
 * \begin{verbatim}
 *     if $(not $(defined-env VERBOSE))
 *         OMakeFlags(-S --progress)
 *         export
 * \end{verbatim}
 * \end{doc}
 *)
let set_options venv pos loc args kargs =
   let pos = string_pos "OMakeFlags" pos in
      match args with
         [arg] ->
            let argv = strings_of_value venv pos arg in
            let venv = venv_set_options venv loc pos argv in
               venv, ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Version checking.
 *
 * \begin{doc}
 * \fun{OMakeVersion}
 *
 * \begin{verbatim}
 *    OMakeVersion(version1)
 *    OMakeVersion(version1, version2)
 *       version1, version2 : String
 * \end{verbatim}
 *
 * The \verb+OMakeVersion+ function is used for version checking
 * in \File{OMakefile}s.  It takes one or two arguments.
 *
 * In the one argument form, if the \Prog{omake} version number
 * is less than \verb+<version1>+,
 * then an exception is raised.  In the two argument form,
 * the version must lie between \verb+version1+ and \verb+version2+.
 *
 * \fun{cmp-versions}
 * \begin{verbatim}
 *    $(cmp-versions version1, version2)
 *       version1, version2 : String
 * \end{verbatim}
 *
 * The \verb+cmp-versions\+ functions can be used to compare arbitrary version strings.
 * It returns 0 when the two version strings are equal, a negative number when the first
 * string represents an earlier version, and a positive number otherwise.
 * \end{doc}
 *)
let split_int =
   let rec split_int_aux i s =
      match String.length s with
         0 -> i, s
       | l ->
            begin
               match s.[0] with
                  '0'..'9' as c ->
                     split_int_aux (i * 10 + (Char.code c - 48)) (String.sub s 1 (l - 1))
                | _ ->
                     i, s
            end
   in
      split_int_aux 0

let rec compare_versions v1 v2 =
   match String.length v1, String.length v2 with
      0, 0 -> 0
    | 0, _ -> -1
    | _, 0 -> 1
    | l1, l2 ->
         begin
            match v1.[0],v2.[0] with
               '0'..'9', '0'..'9' ->
                  let i1, s1 = split_int v1 in
                  let i2, s2 = split_int v2 in
                     begin
                        match i1 - i2 with
                           0 -> compare_versions s1 s2
                         | i -> i
                     end
             | c1, c2 when c1 = c2 ->
                  compare_versions (String.sub v1 1 (l1 - 1)) (String.sub v2 1 (l2 - 1))
             | c1, c2 ->
                  Char.code c1 - Char.code c2
         end

let check_version venv pos loc args =
   let pos = string_pos "check_version" pos in
   let version = Omake_magic.version in
   let check lowest highest =
      if compare_versions version lowest < 0 then
         raise (OmakeFatalErr (loc_pos loc pos, LazyError (fun out ->
            fprintf out "@[<0>This version of OMake is too old,@ you need to upgrade to at least version@ %s;@ current OMake version is@ %s.@ You should be able to download the latest version of OMake from http://omake.metaprl.org/download.html@]" lowest version)));
      match highest with
         Some highest ->
            if compare_versions version highest > 0 then
               raise (OmakeFatalErr (loc_pos loc pos, LazyError (fun out ->
                  fprintf out "@[<0>This version of OMake is too new or the given file is too old.@ This file accepts versions@ %s-%s;@ current OMake version is@ %s@]" lowest highest version)))
       | None ->
            ()
   in
      match args with
         [lowest] ->
            let lowest = Lm_string_util.trim (string_of_value venv pos lowest) in
               check lowest None;
               ValString version
       | [lowest; highest] ->
            let lowest = Lm_string_util.trim (string_of_value venv pos lowest) in
            let highest = Lm_string_util.trim (string_of_value venv pos highest) in
               check lowest (Some highest);
               ValString version
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1,2), List.length args)))

let cmp_version venv pos loc args =
   let pos = string_pos "cmp_version" pos in
      match args with
         [v1; v2] ->
            let v1 = Lm_string_util.trim (string_of_value venv pos v1) in
            let v2 = Lm_string_util.trim (string_of_value venv pos v2) in
               ValInt (compare_versions v1 v2)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Add the command-line vars.
 *
 * \begin{doc}
 * \fun{DefineCommandVars}
 *
 * \begin{verbatim}
 *    DefineCommandVars()
 * \end{verbatim}
 *
 * The \verb+DefineCommandVars+ function redefines the variables passed on
 * the commandline.  Variables definitions are passed on the command line
 * in the form \verb+name=value+.  This function is primarily for internal
 * use by \Prog{omake} to define these variables for the first time.
 * \end{doc}
 *)
let define_command_vars venv pos loc args kargs =
   let pos = string_pos "DefineCommandVars" pos in
      match args, kargs with
         [], []
       | [_], [] ->
            venv_add_command_defs venv, ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (0, 1), List.length args)))

(*
 * Table of built-in functions.
 *)
let () =
   let builtin_funs =
      [true,  "OMakeVersion",          check_version,       ArityRange (1, 2);
       true,  "cmp-versions",          cmp_version,         ArityExact 2;
      ]
   in
   let builtin_kfuns =
      [true,  "OMakeFlags",            set_options,         ArityExact 1;
       true,  "DefineCommandVars",     define_command_vars, ArityRange (0, 1);
      ]
   in
   let builtin_rules =
      [true, [".PHONY"], phony_targets]
   in
   let builtin_info =
      { builtin_empty with builtin_funs  = builtin_funs;
                           builtin_kfuns = builtin_kfuns;
                           builtin_rules = builtin_rules;
                           phony_targets = phony_targets
      }
   in
      register_builtin builtin_info

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
