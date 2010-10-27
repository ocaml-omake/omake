(*
 * Builtin operations on files.
 *
 * \begin{doc}
 * \chapter{File, I/O and system operations}
 * \label{chapter:system}
 * \cutname{omake-system.html}
 * \end{doc}
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2007 Mojave Group, California Institute of Technology,
 * and HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}, @email{anogin@hrl.com}
 * @end[license]
 *)
open Lm_arg
open Lm_glob
open Lm_printf
open Lm_location
open Lm_string_set

open Omake_ir
open Omake_env
open Omake_var
open Omake_pos
open Omake_eval
open Omake_node
open Omake_rule
open Omake_value
open Omake_target
open Omake_symbol
open Omake_builtin
open Omake_node_sig
open Omake_build_util
open Omake_builtin_util
open Omake_builtin_type
open Omake_build_type
open Omake_cache_type
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_builtin_file" end)
open Pos

(*
 * Utilities.
 *)
let is_dir dir =
   try (Unix.LargeFile.lstat dir).Unix.LargeFile.st_kind = Unix.S_DIR with
      Unix.Unix_error _ ->
         false

(*
 * Get the file from a string.
 *
 * \begin{doc}
 * \section{File names}
 * \twofuns{file}{dir}
 *
 * \begin{verbatim}
 *    $(file sequence) : File Sequence
 *       sequence : Sequence
 *    $(dir sequence) : Dir Sequence
 *       sequence : Sequence
 * \end{verbatim}
 *
 * The \verb+file+ and \verb+dir+ functions define location-independent references to files and directories.
 * In \Prog{omake}, the commands to build a target are executed in the target's directory.  Since there may be
 * many directories in an \Prog{omake} project, the build system provides a way to construct a reference to a file
 * in one directory, and use it in another without explicitly modifying the file name.  The functions have the following
 * syntax, where the name should refer to a file or directory.
 *
 * For example, we can construct a reference to a file \verb+foo+ in the current directory.
 *
 * \begin{verbatim}
 *    FOO = $(file foo)
 *    .SUBDIRS: bar
 * \end{verbatim}
 *
 * If the \verb+FOO+ variable is expanded in the \verb+bar+ subdirectory, it will expand to \verb+../foo+.
 *
 * These commands are often used in the top-level OMakefile to provide location-independent references to
 * top-level directories, so that build commands may refer to these directories as if they were absolute.
 *
 * \begin{verbatim}
 *    ROOT = $(dir .)
 *    LIB  = $(dir lib)
 *    BIN  = $(dir bin)
 * \end{verbatim}
 *
 * Once these variables are defined, they can be used in build commands in subdirectories as follows, where
 * \verb+$(BIN)+ will expand to the location of the \verb+bin+ directory relative to the command being executed.
 *
 * \begin{verbatim}
 *    install: hello
 * 	cp hello $(BIN)
 * \end{verbatim}
 * \end{doc}
 *)
let file venv pos loc args =
   let pos = string_pos "file" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
            let values = List.map (node_value_of_value venv pos) values in
               concat_array values
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let dir venv pos loc args =
   let pos = string_pos "dir" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
            let values = List.map (dir_value_of_value venv pos) values in
               concat_array values
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{tmpfile}
 *
 * \begin{verbatim}
 *     $(tmpfile prefix) : File
 *     $(tmpfile prefix, suffix) : File
 *         prefix : String
 *         suffix : String
 * \end{verbatim}
 *
 * The \verb+tmpfile+ function returns the name of a fresh temporary file in
 * the temporary directory.
 * \end{doc}
 *)
let tmpfile venv pos loc args =
   let pos = string_pos "tmpfile" pos in
   let prefix, suffix =
      match args with
         [prefix] ->
            string_of_value venv pos prefix, ".omake"
       | [prefix; suffix] ->
            string_of_value venv pos prefix, string_of_value venv pos suffix
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))
   in
      ValNode (venv_intern venv PhonyProhibited (Filename.temp_file prefix suffix))

(*
 * Display something from a different directory.
 *
 * \begin{doc}
 * \fun{in}
 *
 * \begin{verbatim}
 *    $(in dir, exp) : String Array
 *       dir : Dir
 *       exp : expression
 * \end{verbatim}
 *
 * The \verb+in+ function is closely related to the \verb+dir+ and
 * \verb+file+ functions.  It takes a directory and an expression, and
 * evaluates the expression in that effective directory.
 * For example, one common way to install a file is to define a symbol link, where the
 * value of the link is relative to the directory where the link is created.
 *
 * The following commands create links in the \verb+$(LIB)+ directory.
 *
 * \begin{verbatim}
 *     FOO = $(file foo)
 *     install:
 *        ln -s $(in $(LIB), $(FOO)) $(LIB)/foo
 * \end{verbatim}
 *
 * Note that the \verb+in+ function only affects the expansion of \verb+Node+
 * (\verb+File+ and \verb+Dir+) values.
 * \end{doc}
 *)
let ind venv pos loc args =
   let pos = string_pos "ind" pos in
      match args with
         [dir; arg] ->
            (*
             * BUG: JYH: evaluate the arguments early, so that commands
             * like the following work.
             *
             *    %.out: %.in
             *       echo $(in foo, $(file $<))
             *
             * Without this eager evaluation, this command wouldn't
             * work because expressions in rules bodies are evaluated
             * lazily, and the $(file $<) needs to be evaluated early.
             *)
            let arg = concat_array (values_of_value venv pos arg) in
            let dir = dir_of_value venv pos dir in
            let venv = venv_chdir_tmp venv dir in
            let strings = strings_of_value venv pos arg in
               concat_strings strings
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Strip the directory.
 *
 * \begin{doc}
 * \fun{basename}
 *
 * \begin{verbatim}
 *    $(basename files) : String Sequence
 *       files : String Sequence
 * \end{verbatim}
 *
 * The \verb+basename+ function returns the base names for a list of files.
 * The basename is the filename with any leading directory components removed.
 *
 * For example, the expression \verb+$(basename dir1/dir2/a.out /etc/modules.conf /foo.ml)+ evaluates to
 * \verb+a.out modules.conf foo.ml+.
 * \end{doc}
 *)
let basename venv pos loc args =
   let pos = string_pos "basename" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map Filename.basename args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Strip the directory.
 *
 * \begin{doc}
 * \fun{dirname}
 *
 * \begin{verbatim}
 *    $(dirname files) : String Sequence
 *       files : String Sequence
 * \end{verbatim}
 *
 * The \verb+dirname+ function returns the directory name for a list of files.
 * The directory name is the filename with the basename removed.  If a name
 * does not have a directory part, the directory is ``.''
 *
 * For example, the expression \verb+$(dirname dir1\dir2\a.out /etc/modules.conf /foo.ml bar.ml)+ evaluates to
 * \verb+dir1/dir2 /etc / .+.
 *
 * \textbf{Note}: this function is different from the \verb+dirof+ function.
 * The function \verb+dirname+ is simple a function over strings, while
 * \verb+dirof+ is a function on filenames.
 * \end{doc}
 *)
let dirname venv pos loc args =
   let pos = string_pos "dirname" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map Filename.dirname args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Strip the directory.
 *
 * \begin{doc}
 * \fun{rootname}
 *
 * \begin{verbatim}
 *    $(rootname files) : String Sequence
 *       files : String Sequence
 * \end{verbatim}
 *
 * The \verb+rootname+ function returns the root name for a list of files.
 * The rootname is the filename with the final suffix removed.
 *
 * For example, the expression \verb+$(rootname dir1/dir2/a.out /etc/a.b.c /foo.ml)+ evaluates to
 * \verb+dir1/dir2/a /etc/a.b /foo+.
 * \end{doc}
 *)
let rootname venv pos loc args =
   let pos = string_pos "rootname" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map Lm_filename_util.root args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Get the directory.
 *
 * \begin{doc}
 * \fun{dirof}
 *
 * \begin{verbatim}
 *    $(dirof files) : Dir Sequence
 *       files : File Sequence
 * \end{verbatim}
 *
 * The \verb+dirof+ function returns the directory for each of the listed files.
 *
 * For example, the expression \verb+$(dirof dir/dir2/a.out /etc/modules.conf /foo.ml)+ evaluates
 * to the directories \verb+dir1/dir2 /etc /+.
 * \end{doc}
 *)
let dirof venv pos loc args =
   let pos = string_pos "dirof" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
            let dirs =
               List.map (fun v ->
                     let v = node_value_of_value venv pos v in
                        match v with
                           ValNode node ->
                              ValDir (Node.dir node)
                         | ValDir _ ->
                              v
                         | _ ->
                              raise (Invalid_argument "dirof")) values
            in
               concat_array dirs
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{fullname}
 *
 * \begin{verbatim}
 *    $(fullname files) : String Sequence
 *       files : File Sequence
 * \end{verbatim}
 *
 * The \verb+fullname+ function returns the pathname relative to the project root
 * for each of the files or directories.
 * \end{doc}
 *)
let fullname venv pos loc args =
   let pos = string_pos "fullname" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
            let strings =
               List.map (fun v ->
                     let s =
                        match node_value_of_value venv pos v with
                           ValDir dir ->
                              Dir.fullname dir
                         | ValNode node ->
                              Node.fullname node
                         | v ->
                              raise (OmakeFatalErr (loc_pos loc pos, StringValueError ("not a file", v)))
                     in
                        ValString s) values
            in
               concat_array strings
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{absname}
 *
 * \begin{verbatim}
 *    $(absname files) : String Sequence
 *       files : File Sequence
 * \end{verbatim}
 *
 * The \verb+absname+ function returns the absolute pathname for each of the files
 * or directories.
 * \end{doc}
 *)
let absname venv pos loc args =
   let pos = string_pos "absname" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
            let strings =
               List.map (fun v ->
                     let s =
                        match node_value_of_value venv pos v with
                           ValDir dir ->
                              Dir.absname dir
                         | ValNode node ->
                              Node.absname node
                         | v ->
                              raise (OmakeFatalErr (loc_pos loc pos, StringValueError ("not a file", v)))
                     in
                        ValString s) values
            in
               concat_array strings
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Strip the directory.
 *
 * \begin{doc}
 * \fun{homename}
 *
 * \begin{verbatim}
 *    $(homename files) : String Sequence
 *       files : File Sequence
 * \end{verbatim}
 *
 * The \verb+homename+ function returns the name of a file in
 * tilde form, if possible.  The unexpanded forms are computed
 * lazily: the \verb+homename+ function will usually evaluate to an absolute
 * pathname until the first tilde-expansion for the same directory.
 * \end{doc}
 *)
let homename venv pos loc args =
   let pos = string_pos "rootname" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let args = List.map (node_value_of_value venv pos) args in
            let venv = venv_chdir_tmp venv Dir.root in
            let args =
               List.map (fun v ->
                     ValString (tilde_collapse (string_of_value venv pos v))) args
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Strip the directory.
 *
 * \begin{doc}
 * \fun{suffix}
 *
 * \begin{verbatim}
 *    $(suffix files) : String Sequence
 *       files : StringSequence
 * \end{verbatim}
 *
 * The \verb+suffix+ function returns the suffixes for a list of files.
 * If a file has no suffix, the function returns the empty string.
 *
 * For example, the expression \verb+$(suffix dir1/dir2/a.out /etc/a /foo.ml)+ evaluates
 * to \verb+.out .ml+.
 * \end{doc}
 *)
let suffix venv pos loc args =
   let pos = string_pos "suffix" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let args = List.map Lm_filename_util.suffix args in
               concat_strings args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Search the PATH.
 *
 * \begin{doc}
 * \section{Path search}
 * \fun{which}
 *
 * \begin{verbatim}
 *    $(which files) : File Sequence
 *       files : String Sequence
 * \end{verbatim}
 *
 * The \verb+which+ function searches for executables in the
 * current command search path, and returns \verb+file+ values
 * for each of the commands.  It is an error if a command is
 * not found.
 * \end{doc}
 *)
let which venv pos loc args =
   let pos = string_pos "which" pos in
      match args with
         [arg] ->
            let path = venv_find_var venv pos loc path_var in
            let path = Omake_eval.path_of_values venv pos (values_of_value venv pos path) "." in
            let cache = venv_cache venv in
            let path = Omake_cache.ls_exe_path cache path in
            let args = strings_of_value venv pos arg in
            let args =
               List.map (fun s ->
                     try ValNode (Omake_cache.exe_find cache path s) with
                        Not_found ->
                           raise (OmakeException (loc_pos loc pos, StringStringError ("command not found", s)))) args
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{where}
 *
 * The \verb+where+ function is similar to which, except it returns the list of
 * all the locations of the given executable (in the order in which the
 * corresponding directories appear in \verb+$PATH+). In case a command is handled
 * internally by the \verb+Shell+ object, the first string in the output will
 * describe the command as a built-in function.
 *
 * \begin{verbatim}
 *     % where echo
 *     echo is a Shell object method (a built-in function)
 *     /bin/echo
 * \end{verbatim}
 * \end{doc}
 *)
let where venv pos loc args =
   let pos = string_pos "where" pos in
      match args with
         [arg] ->
            (match strings_of_value venv pos arg with
                [arg] ->
                   let path = venv_find_var venv pos loc path_var in
                   let path = Omake_eval.path_of_values venv pos (values_of_value venv pos path) "." in
                   let cache = venv_cache venv in
                   let path = Omake_cache.ls_exe_path cache path in
                   let res = Omake_cache.exe_find_all cache path arg in
                   let res = List.map (fun v -> ValNode v) res in
                   let res =
                      try
                         let obj = venv_find_var_exn venv shell_object_var in
                            match eval_single_value venv pos obj with
                               ValObject obj ->
                                  let v = venv_find_field_internal_exn obj (Lm_symbol.add arg) in
                                  let kind =
                                     match eval_value venv pos v with
                                        ValPrim _ ->
                                           "Shell object method (a built-in function)"
                                      | ValFun _ ->
                                           "Shell object method (an omake function)"
                                      | _ ->
                                           "Shell object method"
                                  in
                                     ValData (arg ^ " is a " ^ kind) :: res
                             | _ ->
                                  res
                      with
                         Not_found ->
                            res
                   in
                      concat_array res
              | args ->
                   raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args))))
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{rehash}
 *
 * \begin{verbatim}
 *     rehash()
 * \end{verbatim}
 *
 * The \verb+rehash+ function resets all search paths.
 * \end{doc}
 *)
let rehash venv pos loc args =
   let cache = venv_cache venv in
      Omake_cache.rehash cache;
      ValNone

(*
 * \begin{doc}
 * \fun{exists-in-path}
 *
 * \begin{verbatim}
 *    $(exists-in-path files) : String
 *       files : String Sequence
 * \end{verbatim}
 *
 * The \verb+exists-in-path+ function tests whether all executables
 * are present in the current search path.
 * \end{doc}
 *)
let exists_in_path venv pos loc args =
   let pos = string_pos "exists-in-path" pos in
      match args with
         [arg] ->
            let args = strings_of_value venv pos arg in
            let test =
               List.for_all (fun s ->
                     try ignore (Lm_filename_util.which s); true with
                        Failure _
                      | Not_found ->
                           false) args
            in
               val_of_bool test
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \threefuns{digest}{digest-optional}{digest-string}
 *
 * \begin{verbatim}
 *      $(digest files) : String Array
 *         file : File Array
 *      raises RuntimeException
 *
 *      $(digest-optional files) : String Array
 *         file : File Array
 *
 *      $(digest-string s) : String
 *         s : String
 * \end{verbatim}
 *
 * The \verb+digest+ and \verb+digest-optional+ functions compute MD5 digests
 * of files.  The \verb+digest+ function raises an exception if a file
 * does no exist.  The \verb+digest-optional+ returns \verb+false+ if a
 * file does no exist.   MD5 digests are cached.
 * \end{doc}
 *)
let digest_aux fail venv pos loc args =
   let pos = string_pos "digest" pos in
      match args with
         [arg] ->
            let cache = venv_cache venv in
            let values = values_of_value venv pos arg in
            let values =
               List.map (fun v ->
                     match node_value_of_value venv pos v with
                        ValNode node ->
                           (match Omake_cache.stat cache node with
                               Some digest ->
                                  ValData (Lm_string_util.hexify digest)
                             | None ->
                                  if fail then
                                     raise (OmakeException (loc_pos loc pos, StringNodeError ("file does not exist", node)))
                                  else
                                     val_false)
                      | _ ->
                           if fail then
                              raise (OmakeException (loc_pos loc pos, StringValueError ("not a file", v)))
                           else
                              val_false) values
            in
               concat_array values
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let digest = digest_aux true
let digest_optional = digest_aux false

(*
 * A simple string.
 *)
let digest_string venv pos loc args =
   let pos = string_pos "digest_string" pos in
   let s =
      match args with
         [arg] ->
            string_of_value venv pos arg
       | _ ->
            raise (OmakeException (pos, ArityMismatch (ArityExact 1, List.length args)))
   in
      ValData (Digest.string s)

(*
 * \begin{doc}
 * \twofuns{find-in-path}{find-in-path-optional}
 *
 * \begin{verbatim}
 *     $(find-in-path path, files) : File Array
 *        path : Dir Array
 *        files : String Array
 *     raises RuntimeException
 *
 *     $(find-in-path-optional path, files) : File Array
 * \end{verbatim}
 *
 * The \verb+find-in-path+ function searches for the files in a search
 * path.  Only the tail of the filename is significant.  The \verb+find-in-path+
 * function raises an exception if the file can't be found.
 * The \verb+find-in-path-optional+ function silently removes
 * files that can't be found.
 * \end{doc}
 *)
let search_path_aux fail venv pos loc args =
   let pos = string_pos "search-path" pos in
      match args with
         [dirs; arg] ->
            (* List the path *)
            let cache = venv_cache venv in
            let path = values_of_value venv pos dirs in
            let path = Omake_eval.path_of_values venv pos path "." in
            let listing = Omake_cache.ls_path cache path in

            (* Find each file *)
            let files = strings_of_value venv pos arg in
            let files =
               List.fold_left (fun files s ->
                     let s = Filename.basename s in
                        try
                           let file =
                              match Omake_cache.listing_find cache listing s with
                                 DirEntry dir ->
                                    ValDir dir
                               | NodeEntry node ->
                                    ValNode node
                           in
                              file :: files
                        with
                           Not_found ->
                              if fail then
                                 raise (OmakeException (loc_pos loc pos, StringStringError ("file not found", s)))
                              else
                                 files) [] files
            in
               concat_array (List.rev files)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let find_in_path = search_path_aux true
let find_in_path_optional = search_path_aux false

(*
 * \begin{doc}
 * \twofuns{digest-in-path}{digest-in-path-optional}
 *
 * \begin{verbatim}
 *     $(digest-in-path path, files) : String/File Array
 *        path : Dir Array
 *        files : String Array
 *     raises RuntimeException
 *
 *     $(digest-in-path-optional path, files) : String/File Array
 * \end{verbatim}
 *
 * The \verb+digest-in-path+ function searches for the files in a search
 * path and returns the file and digest for each file.  Only the tail of the
 * filename is significant.  The \verb+digest-in-path+ function raises an exception
 * if the file can't be found.  The \verb+digest-in-path-optional+
 * function silently removes elements that can't be found.
 * \end{doc}
 *)
let digest_path_aux fail venv pos loc args =
   let pos = string_pos "digest-path" pos in
      match args with
         [dirs; arg] ->
            (* List the path *)
            let cache = venv_cache venv in
            let path = values_of_value venv pos dirs in
            let path = Omake_eval.path_of_values venv pos path "." in
            let listing = Omake_cache.ls_path cache path in

            (* Find each file *)
            let files = strings_of_value venv pos arg in
            let files =
               List.fold_left (fun files s ->
                     let s = Filename.basename s in
                        try
                           let file =
                              match Omake_cache.listing_find cache listing s with
                                 DirEntry dir ->
                                    ValArray [ValDir dir; ValData "directory"]
                               | NodeEntry node ->
                                    match Omake_cache.stat cache node with
                                       Some digest ->
                                          ValArray [ValNode node; ValData (Lm_string_util.hexify digest)]
                                     | None ->
                                          raise Not_found
                           in
                              file :: files
                        with
                           Not_found ->
                              if fail then
                                 raise (OmakeException (loc_pos loc pos, StringStringError ("file not found", s)))
                              else
                                 files) [] files
            in
               concat_array (List.rev files)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let digest_in_path = digest_path_aux true
let digest_in_path_optional = digest_path_aux false

(*
 * Check if a file exists.
 *
 * \begin{doc}
 * \section{File stats}
 * \threefuns{file-exists}{target-exists}{target-is-proper}
 *
 * \begin{verbatim}
 *    $(file-exists files) : String
 *    $(target-exists files) : String
 *    $(target-is-proper files) : String
 *        files : File Sequence
 * \end{verbatim}
 *
 * The \verb+file-exists+ function checks whether the files listed exist.
 * The \verb+target-exists+ function is similar to the \verb+file-exists+ function.
 * However, it returns true if the file exists \emph{or} if it can be built
 * by the current project.  The \verb+target-is-proper+ returns true only
 * if the file can be generated in the current project.
 * \end{doc}
 *)
let node_exists node_exists venv pos loc args =
   let pos = string_pos "file-exists" pos in
      match args with
         [arg] ->
            let cache = venv_cache venv in
            let args = values_of_value venv pos arg in
            let b =
               List.for_all (fun arg ->
                     node_exists cache venv pos (file_of_value venv pos arg)) args
            in
               val_of_bool b
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{stat-reset}
 *
 * \begin{verbatim}
 *    $(stat-reset files) : String
 *        files : File Sequence
 * \end{verbatim}
 *
 * \OMake{} uses a stat-cache.  The \verb+stat-reset+ function reset the \verb+stat+
 * information for the given files, forcing the \verb+stat+ information to
 * be recomputed the next time it is requested.
 * \end{doc}
 *)
let stat_reset venv pos loc args =
   let pos = string_pos "stat-reset" pos in
      match args with
         [arg] ->
            let cache = venv_cache venv in
            let args = values_of_value venv pos arg in
               List.iter (fun arg -> Omake_cache.reset cache (file_of_value venv pos arg)) args;
               ValSequence []
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Filter out the files that don't exist.
 *
 * \begin{doc}
 * \threefuns{filter-exists}{filter-targets}{filter-proper-targets}
 *
 * \begin{verbatim}
 *    $(filter-exists files) : File Sequence
 *    $(filter-targets files) : File Sequence
 *    $(filter-proper-targets) : File Sequence
 *       files : File Sequence
 * \end{verbatim}
 *
 * The \verb+filter-exists+, \verb+filter-targets+, and \verb+filter-proper-targets+
 * functions remove files from a list of files.
 * \begin{itemize}
 * \item \verb+filter-exists+: the result is the list of files that exist.
 * \item \verb+filter-targets+: the result is the list of files either exist, or
 *    can be built by the current project.
 * \item \verb+filter-proper-targets+: the result is the list of files that can
 *    be built in the current project.
 * \end{itemize}
 *
 * \paragraph{Creating a ``distclean'' target}
 * \label{section:distclean}
 *
 * One way to create a simple ``\verb+distclean+'' rule that removes generated files from
 * the project is by removing all files that can be built in the current
 * project.
 *
 * \textbf{CAUTION:} you should be careful before you do this.  The rule
 * removes \emph{any} file that can \emph{potentially} be reconstructed.
 * There is no check to make sure that the commands to rebuild the file
 * would actually succeed.  Also, note that no file outside the
 * current project will be deleted.
 *
 * \begin{verbatim}
 *     .PHONY: distclean
 *
 *     distclean:
 *         rm $(filter-proper-targets $(ls R, .))
 * \end{verbatim}
 *
 * If you use CVS, you may wish to utilize the \verb+cvs_realclean+ program that
 * is distributed with \OMake{} in order to create a ``\verb+distclean+'' rule that would
 * delete all the files thare are not known to CVS. For example, if you already have a more traditional
 * ``\verb+clean+'' target defined in your project, and if you want the ``\verb+distclean+'' rule to
 * be interactive by default, you can write the following:
 *
 * \begin{verbatim}
 *     if $(not $(defined FORCE_REALCLEAN))
 *         FORCE_REALCLEAN = false
 *         export
 *
 *     distclean: clean
 *         cvs_realclean $(if $(FORCE_REALCLEAN), -f) -i .omakedb -i .omakedb.lock
 * \end{verbatim}
 *
 * You can add more files that you want to always keep (such as configuration files) with the -i option.
 *
 * Similarly, if you use Subversion, you utilize the \verb+build/svn_realclean.om+ script that comes with \OMake:
 *
 * \begin{verbatim}
 *     if $(not $(defined FORCE_REALCLEAN))
 *         FORCE_REALCLEAN = false
 *         export
 *
 *     open build/svn_realclean
 *
 *     distclean: clean
 *         svn_realclean $(if $(FORCE_REALCLEAN), -f) -i .omakedb -i .omakedb.lock
 * \end{verbatim}
 *
 * See also the \hyperfun{dependencies-proper} for an alternate method for removing intermediate files.
 * \end{doc}
 *)
let filter_nodes node_exists venv pos loc args =
   let pos = string_pos "filter-exists" pos in
      match args with
         [arg] ->
            let cache = venv_cache venv in
            let args  = values_of_value venv pos arg in
            let nodes = List.map (file_of_value venv pos) args in
            let nodes = List.filter (node_exists cache venv pos) nodes in
            let nodes = List.map (fun v -> ValNode v) nodes in
               ValArray nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let file_exists venv pos loc args =
   node_exists (fun cache venv _ node -> Omake_cache.exists cache node) venv pos loc args

let file_exists_force venv pos loc args =
   node_exists (fun cache venv _ node -> Omake_cache.exists cache node) venv pos loc args

let filter_exists venv pos loc args =
   filter_nodes (fun cache venv _ node -> Omake_cache.exists cache node) venv pos loc args

(* Catch UnbuildableException *)
let target_is_buildable cache venv pos node =
   try
      target_is_buildable cache venv pos node
   with
      RaiseException(_, obj) when venv_instanceof obj unbuildable_exception_sym ->
         false

let target_is_buildable_proper cache venv pos node =
   try
      target_is_buildable_proper cache venv pos node
   with
      RaiseException(_, obj) when venv_instanceof obj unbuildable_exception_sym ->
         false

let target_exists venv pos loc args =
   node_exists target_is_buildable venv pos loc args

let filter_targets venv pos loc args =
   filter_nodes target_is_buildable venv pos loc args

let target_is_proper venv pos loc args =
   node_exists target_is_buildable_proper venv pos loc args

let filter_proper_targets venv pos loc args =
   filter_nodes target_is_buildable_proper venv pos loc args

(*
 * \begin{doc}
 * \twofuns{find-targets-in-path}{find-targets-in-path-optional}
 *
 * \begin{verbatim}
 *     $(find-targets-in-path path files) : File Array
 *     $(find-targets-in-path-optional path, files) : File Array
 *         path : Dir Array
 *         files : File Sequence
 * \end{verbatim}
 *
 * The \verb+find-target-in-path+ function searches for targets in the
 * search path.  For each file \verb+file+ in the file list, the path is
 * searched sequentially for a directory \verb+dir+ such that the target
 * \verb+dir/file+ exists.  If so, the file \verb+dir/file+ is returned.
 *
 * For example, suppose you are building a C project, and project
 * contains a subdirectory \verb+src/+ containing only the files
 * \verb+fee.c+ and \verb+foo.c+.  The following expression
 * evaluates to the files \verb+src/fee.o+ \verb+src/foo.o+ even
 * if the files have not already been built.
 *
 * \begin{verbatim}
 *     $(find-targets-in-path lib src, fee.o foo.o)
 *
 *     # Evaluates to
 *     src/fee.o src/foo.o
 * \end{verbatim}
 *
 * The \verb+find-targets-in-path+
 * function raises an exception if the file can't be found.
 * The \verb+find-targets-in-path-optional+ function silently removes
 * targets that can't be found.
 *
 * \begin{verbatim}
 *     $(find-targets-in-path-optional lib src, fee.o foo.o fum.o)
 *
 *     # Evaluates to
 *     src/fee.o src/foo.o
 * \end{verbatim}
 *
 * \fun{find-ocaml-targets-in-path-optional}
 * The \verb+find-ocaml-targets-in-path-optional+ function is very similar to the
 * \hyperfunn{find-targets-in-path-optional} one, except an OCaml-style search
 * is used, where for every element of the search path and for every name being
 * searched for, first the uncapitalized version is tried and if it is not buildable,
 * then the capitalized version is tried next.
 * \end{doc}
 *)
let search_target_path_aux search venv pos loc args =
   let pos = string_pos "search-target-path" pos in
      match args with
         [dirs; arg] ->
            (* List the path *)
            let path = values_of_value venv pos dirs in
            let path = Omake_eval.path_of_values venv pos path "." in
            let path = List.rev (List.fold_left (fun path (_, entry) -> List.rev_append entry path) [] path) in
            let cache = venv_cache venv in

            (* Find each file *)
            let files = strings_of_value venv pos arg in
            let files = List.fold_left (search venv cache pos loc path) [] files in
               concat_array (List.rev files)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let rec search_target_in_path_aux fail venv cache pos loc path files name =
   match path with
      dir :: path ->
         let node = venv_intern_cd venv PhonyOK dir name in
            if target_is_buildable cache venv pos node then
               ValNode node :: files
            else
               search_target_in_path_aux fail venv cache pos loc path files name
    | [] ->
         if fail then
            raise (OmakeException (loc_pos loc pos, StringStringError ("target not found", name)))
         else
            files

let rec search_ocaml_target_in_path_aux venv cache pos loc path files name1 name2 =
   match path with
      dir :: path ->
         let node1 = venv_intern_cd venv PhonyProhibited dir name1 in
            if target_is_buildable cache venv pos node1 then
               ValNode node1 :: files
            else
               let node2 = venv_intern_cd venv PhonyProhibited dir name2 in
                  if target_is_buildable cache venv pos node2 then
                     ValNode node2 :: files
                  else
                     search_ocaml_target_in_path_aux venv cache pos loc path files name1 name2
    | [] ->
         files

let search_ocaml_target_in_path_aux venv cache pos loc path files name =
   search_ocaml_target_in_path_aux venv cache pos loc path files (String.uncapitalize name) (String.capitalize name)

let find_targets_in_path = search_target_path_aux (search_target_in_path_aux true)
let find_targets_in_path_optional = search_target_path_aux (search_target_in_path_aux false)
let find_ocaml_targets_in_path_optional = search_target_path_aux search_ocaml_target_in_path_aux

(*
 * Get the file from a string.
 *
 * \begin{doc}
 * \fun{file-sort}
 * \index{link-order sorting}
 * \index{sorting (link-order)}
 *
 * \begin{verbatim}
 *    $(file-sort order, files) : File Sequence
 *       order : String
 *       files : File Sequence
 * \end{verbatim}
 *
 * \targetref{.ORDER}%
 * \targetref{.BUILDORDER}%
 * The \verb+file-sort+ function sorts a list of filenames by
 * build order augmented by a set of sort rules.  Sort
 * rules are declared using the \verb+.ORDER+ target.
 * The \verb+.BUILDORDER+ defines the default order.
 *
 * \verb+$(file-sort <order>, <files>)+
 *
 * For example, suppose we have the following set of rules.
 *
 * \begin{verbatim}
 *    a: b c
 *    b: d
 *    c: d
 *
 *    .DEFAULT: a b c d
 *       echo $(file-sort .BUILDORDER, a b c d)
 * \end{verbatim}
 *
 * In the case, the sorter produces the result \verb+d b c a+.
 * That is, a target is sorted \emph{after} its dependencies.
 * The sorter is frequently used to sort files that are to be linked
 * by their dependencies (for languages where this matters).
 *
 * There are three important restrictions to the sorter:
 * \begin{itemize}
 * \item The sorter can be used only within a rule body.
 * The reason for this is that \emph{all} dependencies
 * must be known before the sort is performed.
 * \item The sorter can only sort files that are buildable
 * in the current project.
 * \item The sorter will fail if the dependencies are cyclic.
 * \end{itemize}
 *
 * \subsubsection{sort rule}
 *
 * It is possible to further constrain the sorter through the use of
 * sort rules.  A sort rule is declared in two steps.  The
 * target must be listed as an \verb+.ORDER+ target; and then
 * a set of sort rules must be given.  A sort rule defines
 * a pattern constraint.
 *
 * \begin{verbatim}
 *    .ORDER: .MYORDER
 *
 *    .MYORDER: %.foo: %.bar
 *    .MYORDER: %.bar: %.baz
 *
 *    .DEFAULT: a.foo b.bar c.baz d.baz
 *       echo $(sort .MYORDER, a.foo b.bar c.baz d.baz)
 * \end{verbatim}
 *
 * In this example, the \verb+.MYORDER+ sort rule specifies that any
 * file with a suffix \verb+.foo+ should be placed after any file with
 * suffix \verb+.bar+, and any file with suffix \verb+.bar+ should be
 * placed after a file with suffix \verb+.baz+.
 *
 * In this example, the result of the sort is \verb+d.baz c.baz b.bar a.foo+.
 * \end{doc}
 *)
let sort_aux sorter venv pos loc args =
   let pos = string_pos "file-sort" pos in
   let name, nodes =
      match args with
         [name; arg] ->
            let values = values_of_value venv pos arg in
            let nodes = List.map (file_of_value venv pos) values in
            let name = Lm_symbol.add (string_of_value venv pos name) in
               name, nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let env = get_env pos loc in
      sorter env venv pos name nodes

let sort venv pos loc args =
   let sorter env venv pos name nodes =
      let nodes = Omake_build_util.sort env venv pos name nodes in
         ValSequence (sequence_map (fun node -> ValNode node) nodes)
   in
      sort_aux sorter venv pos loc args

(*
 * \begin{doc}
 * \fun{file-check-sort}
 *
 * \begin{verbatim}
 *    file-check-sort(files)
 *       files : File Sequence
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+file-check-sort+ function checks whether a list of files
 * is in sort order.  If so, the list is returned unchanged.
 * If not, the function raises an exception.
 *
 * \verb+$(file-check-sort <order>, <files>)+
 * \end{doc}
 *)
let check_sort venv pos loc args =
   let sorter env venv pos name nodes =
      Omake_build_util.check_sort env venv pos name nodes;
      ValSequence (sequence_map (fun node -> ValNode node) nodes)
   in
      sort_aux sorter venv pos loc args

(************************************************************************
 * Listings.
 *)

(*
 * Comparisons for forting.
 *)
let compare_dir_node dir1 node =
   let dir2 = Node.dir node in
   let cmp = Dir.compare dir1 dir2 in
      if cmp = 0 then
         -1
      else
         cmp

let compare_val_nodes node1 node2 =
   match node1, node2 with
      ValDir dir, ValNode node ->
         compare_dir_node dir node
    | ValNode node, ValDir dir ->
         -(compare_dir_node dir node)
    | ValDir dir1, ValDir dir2 ->
         Dir.compare dir1 dir2
    | ValNode node1, ValNode node2 ->
         Node.compare node1 node2
    | _ ->
         0

let sort_val_nodes nodes =
   List.sort compare_val_nodes nodes

(*
 * \begin{doc}
 * \section{Globbing and file listings}
 * \label{section:globbing}
 *
 * \OMake{} commands are ``glob-expanded'' before being executed.  That is,
 * names may contain \emph{patterns} that are expanded to sequences of
 * file and directory names.  The syntax follows the standard bash(1), csh(1),
 * syntax, with the following rules.
 *
 * \begin{itemize}
 * \item A \emph{pathname} is a sequence of directory and file names separated by
 * one of the \verb+/+ or \verb+\+ characters.  For example, the following pathnames
 * refer to the same file: \verb+/home/jyh/OMakefile+ and \verb+/home\jyh/OMakefile+.
 *
 * \item Glob-expansion is performed on the components of a path.  If a path contains
 * occurrences of special characters (listed below), the path is viewed as a pattern
 * to be matched against the actual files in the system.  The expansion produces a
 * sequence of all file/directory names that match.
 *
 * For the following examples, suppose that a directory \verb+/dir+ contains files
 * named \verb+a+, \verb+-a+, \verb+a.b+, and \verb+b.c+.
 *
 * \begin{description}
 * \item[\texttt{*}] Matches any sequence of zero-or-more characters.  For example,
 * the pattern \verb+/dir/a*+ expands to \verb+/dir/a /dir/aa /dir/a.b+.
 *
 * \item[\texttt{?}] Matches exactly one character.  The pattern \verb+/dir/?a+ expands
 * the filename \verb+/dir/-a+.
 *
 * \item[\texttt{[...]}]  Square brackets denote character sets and ranges
 * in the ASCII character set.  The pattern may contain individual characters $c$
 * or character ranges \texttt{$c_1$-$c_2$}.  The pattern matches any of the
 * individual characters specified, or any characters in the range.  A leading ``hat''
 * inverts the send of the pattern.  To specify a pattern that contains the
 * literal characters \verb+-+, the \verb+-+ should occur as the first character in
 * the range.
 *
 * \begin{center}
 * \begin{tabular}{ll}
 * Pattern & Expansion\\
 * \hline
 * \verb+/dir/[a-b]*+ & \verb+/dir/a /dir/a.b /dir/b.c+\\
 * \verb+/dir/[-a-b]*+ & \verb+/dir/a /dir/-a /dir/a.b /dir/b.c+\\
 * \verb+/dir/[-a]*+   & \verb+/dir/a /dir/-a /dir/a.b+\\
 * \end{tabular}
 * \end{center}
 *
 * \item[\texttt{\{s1,...,sN\}}]  Braces indicate brace-expansion.
 * The braces delimit a sequence of strings separated by commas.
 * Given $N$ strings, the result produces $N$ copies of the pattern,
 * one for each of the strings $s_i$.
 *
 * \begin{center}
 * \begin{tabular}{ll}
 * Pattern & Expansion\\
 * \hline
 * \verb+a{b,c,d}+ & \verb+ab ac ad+\\
 * \verb+a{b{c,d},e}+ & \verb+abc abd ae+\\
 * \verb+a{?{[A-Z],d},*}+ & \verb+a?[A-Z] a?d a*+
 * \end{tabular}
 * \end{center}
 *
 * \item[\texttt{~}]  The tilde is used to specify home directories.
 * Depending on your system, these might be possible expansions.
 *
 * \begin{center}
 * \begin{tabular}{ll}
 * Pattern & Expansion\\
 * \hline
 * \verb+~jyh+ & \verb+/home/jyh+\\
 * \verb+~bob/*.c+ & \verb+c:\Documents and Settings\users\bob+
 * \end{tabular}
 * \end{center}
 *
 * \item[\\]  The \verb+\+ character is both a pathname separator
 * and an escape character.  If followed by a special glob character,
 * the \verb+\+ changes the sense of the following character to non-special
 * status.  Otherwise, \verb+\+ is viewed as a pathname separator.
 *
 * \begin{center}
 * \begin{tabular}{ll}
 * Pattern & Expansion\\
 * \hline
 * \verb+~jyh/\*+  & \verb+~jyh/*+ (\verb+*+ is literal)\\
 * \verb+/dir/\[a-z?+ & \verb+/dir/[a-z?+ (\verb+[+ is literal, \verb+?+ is a pattern).\\
 * \verb+c:\Program Files\[A-z]+ & \verb+c:\Program Files[A-z]*+
 * \end{tabular}
 * \end{center}
 *
 * Note that the final case might be considered to be ambiguous (where \verb+\+ should
 * be viewed as a pathname separator, not as an escape for the subsequent \verb+[+
 * character.  If you want to avoid this ambiguity on Win32, you should use the
 * forward slash \verb+/+ even for Win32 pathnames (the \verb+/+ is translated
 * to \verb+\+ in the output).
 *
 * \begin{center}
 * \begin{tabular}{ll}
 * Pattern & Expansion\\
 * \hline
 * \verb+c:/Program Files/[A-z]*+ & \verb+c:\Program Files\WindowsUpdate ...+
 * \end{tabular}
 * \end{center}
 * \end{description}
 * \end{itemize}
 *
 * \fun{glob}
 *
 * \begin{verbatim}
 *    $(glob strings) : Node Array
 *       strings : String Sequence
 *    $(glob options, strings) : Node Array
 *       options : String
 *       strings : String Sequence
 * \end{verbatim}
 *
 * The \verb+glob+ function performs glob-expansion.
 *
 * The . and .. entries are always ignored.
 *
 * The options are:
 * \begin{description}
 * \item[b] Do not perform \Cmd{csh}{1}-style brace expansion.
 * \item[e] The \verb+\+ character does not escape special characters.
 * \item[n] If an expansion fails, return the expansion literally instead of aborting.
 * \item[i] If an expansion fails, it expands to nothing.
 * \item[.] Allow wildcard patterns to match files beginning with a .
 * \item[A] Return all files, including files that begin with a .
 * \item[F] Match only normal files (any file that is not a directory).
 * \item[D] Match only directory files.
 * \item[C] Ignore files according to \Cmd{cvs}{1} rules.
 * \item[P] Include only proper subdirectories.
 * \end{description}
 *
 * In addition, the following variables may be defined that affect the
 * behavior of \verb+glob+.
 *
 * \begin{description}
 * \item[GLOB\_OPTIONS] A string containing default options.
 * \item[GLOB\_IGNORE] A list of shell patterns for filenames that \verb+glob+ should ignore.
 * \item[GLOB\_ALLOW] A list of shell patterns.  If a file does not match a pattern in
 *    \verb+GLOB_ALLOW+, it is ignored.
 * \end{description}
 *
 * The returned files are sorted by name.
 * \end{doc}
 *)
let glob venv pos loc args =
   let pos = string_pos "glob" pos in
   let option, arg =
      match args with
         [arg] ->
            ValString "", arg
       | [option; arg] ->
            option, arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let options = glob_options_of_env venv pos in
   let option = string_of_value venv pos option in
   let options = glob_options_of_string options option in
   let options = Lm_glob.create_options options in
   let dirs = strings_of_value venv pos arg in
   let root = Dir.cwd () in
   let cwd = venv_dir venv in
   let cwd_name = Dir.name root cwd in
   let dirs, names = glob options cwd_name dirs in
   let dirs = List.map (fun dir -> ValDir (Dir.chdir cwd dir)) dirs in
   let nodes = List.map (fun name -> ValNode (venv_intern venv PhonyProhibited name)) names in
   let nodes = dirs @ nodes in
      ValArray (sort_val_nodes nodes)

(*
 * ls function.
 *
 * \begin{doc}
 * \fun{ls}
 *
 * \begin{verbatim}
 *    $(ls files) : Node Array
 *       files : String Sequence
 *    $(ls options, files) : Node Array
 *       files : String Sequence
 * \end{verbatim}
 *
 * The \verb+ls+ function returns the filenames in a directory.
 *
 * The . and .. entries are always ignored.
 * The patterns are shell-style patterns, and are glob-expanded.
 *
 * The options include all of the options to the \verb+glob+ function,
 * plus the following.
 *
 * \begin{description}
 * \item[R] Perform a recursive listing.
 * \end{description}
 *
 * The \verb+GLOB_ALLOW+ and \verb+GLOB_IGNORE+ variables can be defined
 * to control the globbing behavior.
 * The returned files are sorted by name.
 * \end{doc}
 *)
let relative_filename_concat dir file =
   if Filename.is_relative file then
      Filename.concat dir file
   else
      file

let ls_fun_of_string s =
   let len = String.length s in
   let rec search i =
      if i = len then
         list_dirs
      else
         match s.[i] with
            'R' -> list_dirs_rec
          | _ -> search (succ i)
   in
      search 0

let ls venv pos loc args =
   let pos = string_pos "ls" pos in
   let option, arg =
      match args with
         [arg] ->
            ValString "", arg
       | [option; arg] ->
            option, arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let option = string_of_value venv pos option in
   let ls_fun  = ls_fun_of_string option in
   let options = glob_options_of_env venv pos in
   let options = glob_options_of_string options option in
   let options = Lm_glob.create_options options in
   let dirs = strings_of_value venv pos arg in
   let root = Dir.cwd () in
   let cwd  = Dir.name root (venv_dir venv) in
   let dirs, files1 = Lm_glob.glob options cwd dirs in
   let dirs = List.map (relative_filename_concat cwd) dirs in
   let files1 = List.map (relative_filename_concat cwd) files1 in
   let dirs, files2 = ls_fun options "" dirs in
   let dirs  = List.map (fun dir -> ValDir (Dir.chdir root dir)) dirs in
   let nodes = List.map (fun name -> ValNode (venv_intern_cd venv PhonyProhibited root name)) (files1 @ files2) in
   let nodes = dirs @ nodes in
      ValArray (sort_val_nodes nodes)

(*
 * The builtin function.
 *
 * \begin{doc}
 * \fun{subdirs}
 *
 * \begin{verbatim}
 *    $(subdirs dirs) : Dir Array
 *       dirs : String Sequence
 *    $(subdirs options, dirs) : Dir Array
 *       options : String
 *       dirs : String Sequence
 * \end{verbatim}
 *
 * The \verb+subdirs+ function returns all the subdirectories
 * of a list of directories, recursively.
 *
 * The possible options are the following:
 * \begin{description}
 * \item[A] Return directories that begin with a .
 * \item[C] Ignore files according to \File{.cvsignore} rules.
 * \item[P] Include only proper subdirectories.
 * \end{description}
 * \end{doc}
 *)
let subdirs venv pos loc args =
   let pos = string_pos "subdirs" pos in
   let option, arg =
      match args with
         [arg] ->
            ValString "", arg
       | [options; arg] ->
            options, arg
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let options = glob_options_of_env venv pos in
   let options = glob_options_of_string options (string_of_value venv pos option) in
   let options = Lm_glob.create_options options in
   let dirs = strings_of_value venv pos arg in
   let root = Dir.cwd () in
   let cwd = Dir.name root (venv_dir venv) in
   let dirs = List.map (relative_filename_concat cwd) dirs in
   let dirs = subdirs_of_dirs options "" dirs in
   let dirs = List.map (fun dir -> ValDir (Dir.chdir root dir)) dirs in
      ValArray dirs

(************************************************************************
 * Argument parsing.
 *)

(*
 * Modes can be specified in two forms:
 *    1. As octal numbers
 *    2. In the symbolic format
 *       u+rg-wx  etc
 *)
type mode_op =
   ModeSet
 | ModeAdd
 | ModeSub

type chmod_mode =
   ChmodInt of int
 | ChmodString of string
 | ChmodNone

(*
 * Mode bit operations.
 *)
let add_mode_bit mode mask op bits =
   let bits = ((bits lsl 6) lor (bits lsl 3) lor bits) land mask in
      match op with
         ModeSet ->
            (mode land (lnot mask)) lor bits
       | ModeAdd ->
            mode lor bits
       | ModeSub ->
            mode land (lnot bits)

let add_abs_mode_bit mode op bit =
   match op with
      ModeSet
    | ModeAdd ->
         mode lor bit
    | ModeSub ->
         mode land (lnot bit)

(*
 * Symbolic modes:
 *     [ugoa]*[+-=][rwxXstugo]*
 *)
let mode_of_symbolic_component mode s =
   let len = String.length s in
   let rec parse_bits mode mask op i =
      if i = len then
         mode
      else
         let mode =
            match s.[i] with
               'r' -> add_mode_bit mode mask op 0b100
             | 'w' -> add_mode_bit mode mask op 0b010
             | 'x' -> add_mode_bit mode mask op 0b001
             | 'X' ->
                  if (mode land 0o111) = 0 then
                     mode
                  else
                     add_mode_bit mode mask op 0b001
             | 's' ->
                  let bit =
                     if (mask land 0o770) <> 0 then
                        0o6000
                     else if (mask land 0o700) <> 0 then
                        0o4000
                     else if (mask land 0o070) <> 0 then
                        0o2000
                     else
                        raise (Failure "mode_of_string")
                  in
                     add_abs_mode_bit mode op bit
             | 't' ->
                  add_abs_mode_bit mode op 0o1000
             | 'u' ->
                  add_mode_bit mode mask op ((mode lsr 6) land 7)
             | 'g' ->
                  add_mode_bit mode mask op ((mode lsr 3) land 7)
             | 'o' ->
                  add_mode_bit mode mask op (mode land 7)
             | _ ->
                  raise (Failure "mode_of_string")
         in
            parse_bits mode mask op (succ i)
   in
   let rec parse_mode mode mask i =
      if i = len then
         mode
      else
         match s.[i] with
            'u' -> parse_mode mode (mask lor 0o700) (succ i)
          | 'g' -> parse_mode mode (mask lor 0o070) (succ i)
          | 'o' -> parse_mode mode (mask lor 0o007) (succ i)
          | 'a' -> parse_mode mode (mask lor 0o777) (succ i)
          | '+' -> parse_bits mode mask ModeAdd (succ i)
          | '-' -> parse_bits mode mask ModeSub (succ i)
          | '=' -> parse_bits mode mask ModeSet (succ i)
          | _ -> raise (Failure "mode_of_string")
   in
      parse_mode mode 0 0

let mode_of_symbolic_string mode s =
   List.fold_left mode_of_symbolic_component mode (Lm_string_util.split "," s)

let mode_of_string mode s =
   if s = "" then
      mode
   else
      match s.[0] with
         '0'..'7' ->
            int_of_string ("0o" ^ s)
       | _ ->
            mode_of_symbolic_string mode s

let mode_of_chmod mode = function
   ChmodNone ->
      mode
 | ChmodInt mode ->
      mode
 | ChmodString s ->
      mode_of_string mode s

(************************************************************************
 * Directories.
 *)

(*
 * \begin{doc}
 * \section{Filesystem operations}
 * \fun{mkdir}
 *
 * \begin{verbatim}
 *    mkdir(mode, node...)
 *       mode : Int
 *       node : Node
 *    raises RuntimeException
 *
 *    mkdir(node...)
 *       node : Node
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+mkdir+ function creates a directory, or a set of directories.
 * The following options are supported.
 * \begin{description}
 * \item[-m mode] Specify the permissions of the created directory.
 * \item[-p] Create parent directories if they do not exist.
 * \item[--] Interpret the remaining names literally.
 * \end{description}
 * \end{doc}
 *)
type mkdir_info =
   { mkdir_mode    : int;
     mkdir_parents : bool;
     mkdir_files   : string list
   }

let mkdir_default_info =
   { mkdir_mode    = 0o777;
     mkdir_parents = false;
     mkdir_files   = []
   }

let mkdir_spec =
   Lm_arg.MultiLetterOptions, (**)
      ["options", (**)
          ["-m", (**)
              StringFold (fun info s -> { info with mkdir_mode = mode_of_string info.mkdir_mode s }),
              "set permission mode";
           "-p", (**)
              UnitFold (fun info -> { info with mkdir_parents = true }),
              "make parents as needed";
           "--", (**)
              RestFold (fun info s -> { info with mkdir_files = s :: info.mkdir_files }),
              "the rest of the arguments are interpreted literally"]]

let mkdir_default info s =
   { info with mkdir_files = s :: info.mkdir_files }, false

let mkdir_usage = "Create a directory"

let mkdir venv pos loc args =
   let pos = string_pos "mkdir" pos in
   let info, nodes =
      match args with
         [mode; nodes] ->
            let info = { mkdir_default_info with mkdir_mode = int_of_value venv pos mode } in
               info, nodes
       | [nodes] ->
            mkdir_default_info, nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))
   in
   let argv = Array.of_list ("mkdir" :: strings_of_value venv pos nodes) in
   let info =
      try Lm_arg.fold_argv argv mkdir_spec info mkdir_default mkdir_usage with
         Failure s ->
            raise (OmakeException (loc_pos loc pos, StringError s))
   in
   let mode = info.mkdir_mode in
   let mkdir =
      if info.mkdir_parents then
         Lm_filename_util.mkdirhier
      else
         Unix.mkdir
   in
   let () =
      try
         List.iter (fun s ->
               let name = Dir.fullname (venv_intern_dir venv s) in
                  mkdir name mode) info.mkdir_files
      with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

(************************************************************************
 * Stat.
 *)

(*
 * \begin{doc}
 * \obj{Stat}
 *
 * The \verb+Stat+ object represents an information about a filesystem node,
 * as returned by the \verb+stat+ and \verb+lstat+ functions.
 * It contains the following fields.
 *
 * \begin{description}
 * \item[dev]: the device number.
 * \item[ino]: the inode number.
 * \item[kind]: the kind of the file, one of the following:
 *    \verb+REG+ (regular file),
 *    \verb+DIR+ (directory),
 *    \verb+CHR+ (character device),
 *    \verb+BLK+ (block device),
 *    \verb+LNK+ (symbolic link),
 *    \verb+FIFO+ (named pipe),
 *    \verb+SOCK+ (socket).
 * \item[perm]: access rights, represented as an integer.
 * \item[nlink]: number of links.
 * \item[uid]: user id of the owner.
 * \item[gid]: group id of the file's group.
 * \item[rdev]: device minor number.
 * \item[size]: size in bytes.
 * \item[atime]: last access time, as a floating point number.
 * \item[mtime]: last modification time, as a floating point number.
 * \item[ctime]: last status change time, as a floating point number.
 * \end{description}
 *
 * Not all of the fields will have meaning on all operating systems.
 *
 * \twofuns{stat}{lstat}
 *
 * \begin{verbatim}
 *     $(stat node...) : Stat
 *        node : Node or Channel
 *     $(lstat node...) : Stat
 *        node : Node or Channel
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+stat+ functions return file information.
 * If the file is a symbolic link, the \verb+stat+ function refers to the
 * destination of the link; the \verb+lstat+ function refers to the link
 * itself.
 * \end{doc}
 *)

(*
 * XXX: JYH: HACK: if the file size is too large,
 * represent it as a string.  We may want to make
 * 64-bit ints a representable type.
 *)
let max_file_size = Int64.of_int max_int
let clip_size i =
   if i > max_file_size then
      ValData (Int64.to_string i)
   else
      ValInt (Int64.to_int i)

let create_stat_obj obj stat =
   let obj = venv_add_field_internal obj st_dev_sym (ValInt stat.Unix.LargeFile.st_dev) in
   let obj = venv_add_field_internal obj st_ino_sym (ValInt stat.Unix.LargeFile.st_ino) in
   let kind =
      match stat.Unix.LargeFile.st_kind with
         Unix.S_REG -> "REG"
       | Unix.S_DIR -> "DIR"
       | Unix.S_CHR -> "CHR"
       | Unix.S_BLK -> "BLK"
       | Unix.S_LNK -> "LNK"
       | Unix.S_FIFO -> "FIFO"
       | Unix.S_SOCK -> "SOCK"
   in
   let obj = venv_add_field_internal obj st_kind_sym (ValString kind) in
   let obj = venv_add_field_internal obj st_perm_sym (ValInt stat.Unix.LargeFile.st_perm) in
   let obj = venv_add_field_internal obj st_nlink_sym (ValInt stat.Unix.LargeFile.st_nlink) in
   let obj = venv_add_field_internal obj st_uid_sym   (ValInt stat.Unix.LargeFile.st_uid) in
   let obj = venv_add_field_internal obj st_gid_sym   (ValInt stat.Unix.LargeFile.st_gid) in
   let obj = venv_add_field_internal obj st_rdev_sym  (ValInt stat.Unix.LargeFile.st_rdev) in
   let obj = venv_add_field_internal obj st_size_sym  (clip_size stat.Unix.LargeFile.st_size) in
   let obj = venv_add_field_internal obj st_atime_sym (ValFloat stat.Unix.LargeFile.st_atime) in
   let obj = venv_add_field_internal obj st_mtime_sym (ValFloat stat.Unix.LargeFile.st_mtime) in
   let obj = venv_add_field_internal obj st_ctime_sym (ValFloat stat.Unix.LargeFile.st_ctime) in
      ValObject obj

let stat_aux stat_fun venv pos loc args =
   let pos = string_pos "stat" pos in
   let obj = venv_find_object_or_empty venv stat_object_var in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let stats =
               List.map (fun arg ->
                     let file = filename_of_value venv pos arg in
                     let stat =
                        try stat_fun file with
                           Unix.Unix_error _ as exn ->
                              raise (UncaughtException (pos, exn))
                     in
                        create_stat_obj obj stat) args
            in
               concat_array stats
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let stat = stat_aux Unix.LargeFile.stat

let lstat =
   if Sys.os_type = "Win32" then
      stat_aux Unix.LargeFile.stat
   else
      stat_aux Unix.LargeFile.lstat

(************************************************************************
 * Links.
 *)

(*
 * \begin{doc}
 * \fun{unlink}
 *
 * \begin{verbatim}
 *    $(unlink file...)
 *       file : File
 *    #(rm file...)
 *       file : File
 *    $(rmdir dir...)
 *       dir : Dir
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+unlink+ and \verb+rm+ functions remove a file.
 * The \verb+rmdir+ function removes a directory.
 *
 * The following options are supported for \verb+rm+ and \verb+rmdir+.
 * \begin{description}
 * \item[-f] ignore nonexistent files, never prompt.
 * \item[-i] prompt before removal.
 * \item[-r] remove the contents of directories recursively.
 * \item[-v] explain what is going on.
 * \item[--] the rest of the values are interpreted literally.
 * \end{description}
 * \end{doc}
 *)
let unlink_aux rm_fun venv pos loc args =
   let pos = string_pos "unlink" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let cache = venv_cache venv in
            let () =
               try
                  List.iter (fun arg ->
                        let name = filename_of_value venv pos arg in
                        let node = venv_intern_cd venv PhonyProhibited (Dir.cwd ()) name in
                           rm_fun name;
                           ignore (Omake_cache.reset cache node)) args
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let unlink = unlink_aux Unix.unlink

(*
 * Command-line versions.
 *)
type rm_info =
   { rm_force       : bool;
     rm_recursive   : bool;
     rm_interactive : bool;
     rm_verbose     : bool;
     rm_files       : string list
   }

let rm_default_info =
   { rm_force       = false;
     rm_recursive   = false;
     rm_interactive = false;
     rm_verbose     = false;
     rm_files       = []
   }

let rm_spec =
   Lm_arg.MultiLetterOptions, (**)
      ["options", (**)
          ["-f", (**)
              UnitFold (fun info -> { info with rm_force = true }),
              "force removal, never prompt";
           "-i", (**)
              UnitFold (fun info -> { info with rm_interactive = true }),
              "prompt before removal";
           "-r", (**)
              UnitFold (fun info -> { info with rm_recursive = true }),
              "remove the contents of directories recursively";
           "-R", (**)
              UnitFold (fun info -> { info with rm_recursive = true }),
              "remove the contents of directories recursively";
           "--recursive", (**)
              UnitFold (fun info -> { info with rm_recursive = true }),
              "remove the contents of directories recursively";
           "-v", (**)
              UnitFold (fun info -> { info with rm_verbose = true }),
              "explain what is being done";
           "--", (**)
              RestFold (fun info s -> { info with rm_files = s :: info.rm_files }),
              "the rest of the arguments are interpreted literally"]]

let rm_default info s =
   { info with rm_files = s :: info.rm_files }, false

let rm_usage = "Remove files and directories"

(*
 * Remove a directory, with options.
 *)
let rm_aux unlink info filename =
   if info.rm_force then
      try unlink filename with
         Unix.Unix_error _ ->
            ()
   else
      let rm_flag =
         if info.rm_interactive then
            begin
               printf "Remove %s? " filename;
               flush stdout;
               match String.lowercase (Lm_string_util.trim (input_line stdin)) with
                  "y" | "yes" ->
                     true
                | _ ->
                     false
            end
         else
            true
      in
         if rm_flag then
            begin
               if info.rm_verbose then
                  printf "Removing %s@." filename;
               unlink filename
            end

(*
 * Remove a directory or file recursively.
 *)
let rec rm_rec info filename =
   if is_dir filename then begin
      let names = Lm_filename_util.lsdir filename in
      let names = List.map (fun name -> Filename.concat filename name) names in
         List.iter (rm_rec info) names;
         rm_aux Unix.rmdir info filename
   end else
      rm_aux Unix.unlink info filename

(*
 * Main command.
 *)
let rm_command rm_fun venv pos loc args =
   let pos = string_pos "rm" pos in
   let argv =
      match args with
         [arg] ->
            Array.of_list ("rm" :: strings_of_value venv pos arg)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let info =
      try Lm_arg.fold_argv argv rm_spec rm_default_info rm_default rm_usage with
         Failure s ->
            raise (OmakeException (loc_pos loc pos, StringError s))
   in
   let nodes = List.map (venv_intern venv PhonyProhibited) info.rm_files in
   let files = List.map Node.fullname nodes in
   let cache = venv_cache venv in
   let () =
      List.iter (fun node ->
            Omake_cache.reset cache node) nodes;

      try
         if info.rm_recursive then
            List.iter (rm_rec info) files
         else
            List.iter (rm_aux rm_fun info) files
      with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

let rmdir = rm_command Unix.rmdir
let rm = rm_command Unix.unlink

(*
 * \begin{doc}
 * \fun{rename}
 *
 * \begin{verbatim}
 *     rename(old, new)
 *        old : Node
 *        new : Node
 *     mv(nodes... dir)
 *        nodes : Node Sequence
 *        dir   : Dir
 *     cp(nodes... dir)
 *        nodes : Node Sequence
 *        dir   : Dir
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+rename+ function changes the name of a file or directory named \verb+old+
 * to \verb+new+.
 *
 * The \verb+mv+ function is similar, but if \verb+new+ is a directory, and it exists,
 * then the files specified by the sequence are moved into the directory.  If not,
 * the behavior of \verb+mv+ is identical to \verb+rename+.  The \verb+cp+ function
 * is similar, but the original file is not removed.
 *
 * The \verb+mv+ and \verb+cp+ functions take the following options.
 * \begin{description}
 * \item[-f] Do not prompt before overwriting.
 * \item[-i] Prompt before overwriting.
 * \item[-v] Explain what it happening.
 * \item[-r] Copy the contents of directories recursively.
 * \item[--] Interpret the remaining arguments literally.
 * \end{description}
 * \end{doc}
 *)
let rename venv pos loc args =
   let pos = string_pos "rename" pos in
      match args with
         [node1; node2] ->
            let name1 = filename_of_value venv pos node1 in
            let name2 = filename_of_value venv pos node2 in
            let () =
               try Unix.rename name1 name2 with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * Command-line versions.
 *)
let rec split_last l' l =
   match l with
      [h] ->
         List.rev l', h
    | h :: l ->
         split_last (h :: l') l
    | [] ->
         raise (Invalid_argument "split_last")

type mv_info =
   { mv_force       : bool;
     mv_interactive : bool;
     mv_verbose     : bool;
     mv_recursive   : bool;
     mv_mode        : string;
     mv_files       : string list
   }

let mv_default_info =
   { mv_force       = false;
     mv_interactive = false;
     mv_verbose     = false;
     mv_recursive   = false;
     mv_mode        = "";
     mv_files       = []
   }

let mv_spec =
   Lm_arg.MultiLetterOptions, (**)
      ["options", (**)
          ["-f", (**)
              UnitFold (fun info -> { info with mv_force = true }),
              "force removal, never prompt";
           "-i", (**)
              UnitFold (fun info -> { info with mv_interactive = true }),
              "prompt before removal";
           "-v", (**)
              UnitFold (fun info -> { info with mv_verbose = true }),
              "explain what is being done";
           "-r", (**)
              UnitFold (fun info -> { info with mv_recursive = true }),
              "copy contents recursively";
           "-m", (**)
              StringFold (fun info s -> { info with mv_mode = s }),
              "specify the permissions of the copied file";
           "--", (**)
              RestFold (fun info s -> { info with mv_files = s :: info.mv_files }),
              "the rest of the arguments are interpreted literally"]]

let mv_default info s =
   { info with mv_files = s :: info.mv_files }, false

let mv_usage = "Move files and directories"

(*
 * Prompter.
 *)
let mv_prompt info file1 file2 =
   if info.mv_force then
      true
   else
      let flag =
         if info.mv_interactive && Sys.file_exists file2 then
            begin
               printf "Remove %s? @?" file2;
               match String.lowercase (Lm_string_util.trim (input_line stdin)) with
                  "y" | "yes" ->
                     true
                | _ ->
                     false
            end
         else
            true
      in
         if flag && info.mv_verbose then
            printf "Copying %s to %s@." file1 file2;
         flag

(*
 * The main function.
 *)
let mv_aux mv venv pos loc args =
   let pos = string_pos "mv" pos in
   let argv =
      match args with
         [args] ->
            Array.of_list ("mv" :: strings_of_value venv pos args)
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let info =
      try Lm_arg.fold_argv argv mv_spec mv_default_info mv_default mv_usage with
         Failure s ->
            raise (OmakeException (loc_pos loc pos, StringError s))
   in
   let files =
      List.map (fun name ->
            Dir.fullname (venv_intern_dir venv name)) (List.rev info.mv_files)
   in
   let files, dir =
      match files with
         [] ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, 0)))
       | _ ->
            split_last [] files
   in
   let () =
      try
         if is_dir dir then
            List.iter (fun file ->
                  mv info file (Filename.concat dir (Filename.basename file))) files
         else
            match files with
               [file] ->
                  mv info file dir
             | _ ->
                  raise (OmakeException (loc_pos loc pos, StringStringError ("destination directory does not exist", dir)))
      with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

(*
 * Recursive copy.
 *)
let cp_file info file1 file2 =
   if mv_prompt info file1 file2 then
      let mode = (Unix.LargeFile.stat file1).Unix.LargeFile.st_perm in
      let mode = mode_of_string mode info.mv_mode in
      let fdr = Lm_unix_util.openfile file1 [Unix.O_RDONLY] 0 in
      let () =
         if info.mv_force then
            try Unix.unlink file2 with
               Unix.Unix_error _ ->
                  try Unix.chmod file2 0o200 with
                     Unix.Unix_error _ ->
                        ()
      in
      let fdw =
         try Lm_unix_util.openfile file2 [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o200 with
            exn ->
               Unix.close fdr;
               raise exn
      in
         try
            let max = 4096 in
            let buffer = String.create max in
            let rec loop () =
               let count = Unix.read fdr buffer 0 max in
                  if count <> 0 then
                     let rec write off =
                        if off < count then
                           write (off + Unix.write fdw buffer off (count - off))
                     in
                        write 0;
                        loop ()
            in
               loop ();
               Unix.close fdr;
               Unix.close fdw;
               try Unix.chmod file2 mode with
                  Unix.Unix_error _ ->
                     ()
         with
            exn ->
               Unix.close fdr;
               Unix.close fdw;
               raise exn

let rec cp_rec info file1 file2 =
   if is_dir file1 then
      let subnames = Lm_filename_util.lsdir file1 in
         Unix.mkdir file2 0o777;
         List.iter (fun name ->
               let file1 = Filename.concat file1 name in
               let file2 = Filename.concat file2 name in
                  cp_rec info file1 file2) subnames
   else
      cp_file info file1 file2

let cp_files info file1 file2 =
   if info.mv_recursive then
      cp_rec info file1 file2
   else
      cp_file info file1 file2

let cp = mv_aux cp_files

let mv_file info file1 file2 =
   if mv_prompt info file1 file2 then
      try Unix.rename file1 file2 with
         Unix.Unix_error _ ->
            cp_file info file1 file2;
            Unix.unlink file1

let mv = mv_aux mv_file

(*
 * \begin{doc}
 * \fun{link}
 *
 * \begin{verbatim}
 *    link(src, dst)
 *       src : Node
 *       dst : Node
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+link+ function creates a hard link named \verb+dst+ to the file
 * or directory \verb+src+.
 *
 * Hard links may work under Win32 when NTFS is used.
 *
 * Normally, only the superuser can create hard links to directories.
 * \end{doc}
 *)
let link venv pos loc args =
   let pos = string_pos "link" pos in
      match args with
         [src; dst] ->
            let src = filename_of_value venv pos src in
            let dst = filename_of_value venv pos dst in
            let () =
               try Unix.link src dst with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \twofuns{symlink}{symlink-raw}
 *
 * \begin{verbatim}
 *    symlink(src, dst)
 *       src : Node
 *       dst : Node
 *    symlink-raw(src, dst)
 *       src : String
 *       dst : Node
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+symlink+ function creates a symbolic link \verb+dst+ that
 * points to the \verb+src+ file.
 *
 * For \verb+symlink+, the link name is computed relative to
 * the target directory.  For example, the expression
 * \verb+$(symlink a/b, c/d)+ creates a link named
 * \verb+c/d -> ../a/b+.
 *
 * The function \verb+symlink-raw+ performs no translation.
 * The symbolic link is set to the \verb+src+ string.
 *
 * Symbolic links are not supported in Win32. Consider using the \verb+ln-or-cp+
 * \verb+Shell+ alias for cross-platform portable linking/copying.
 * \end{doc}
 *)
let symlink venv pos loc args =
   let pos = string_pos "symlink" pos in
      match args with
         [src; dst] ->
            let dst = file_of_value venv pos dst in
            let src = file_of_value venv pos src in
            let src = Node.name (Node.dir dst) src in
            let dst = Node.fullname dst in
            let () =
               try Unix.symlink src dst with
                  (Unix.Unix_error _ | Invalid_argument _) as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

let symlink_raw venv pos loc args =
   let pos = string_pos "symlink-raw" pos in
      match args with
         [src; dst] ->
            let dst = file_of_value venv pos dst in
            let src = string_of_value venv pos src in
            let dst = Node.fullname dst in
            let () =
               try Unix.symlink src dst with
                  (Unix.Unix_error _ | Invalid_argument _) as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))

(*
 * \begin{doc}
 * \twofuns{readlink}{readlink-raw}
 *
 * \begin{verbatim}
 *    $(readlink node...) : Node
 *       node : Node
 *    $(readlink-raw node...) : String
 *       node : Node
 * \end{verbatim}
 *
 * The \verb+readlink+ function reads the value of a symbolic link.
 * \end{doc}
 *)
let readlink venv pos loc args =
   let pos = string_pos "readlink" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let args =
               try
                  List.map (fun arg ->
                        let node = file_of_value venv pos arg in
                        let dir = Node.dir node in
                        let name = Node.fullname node in
                        let name = Unix.readlink name in
                           ValNode (venv_intern_cd venv PhonyProhibited dir name)) args
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let readlink_raw venv pos loc args =
   let pos = string_pos "readlink-raw" pos in
      match args with
         [arg] ->
            let args = values_of_value venv pos arg in
            let args =
               try
                  List.map (fun arg ->
                        let node = file_of_value venv pos arg in
                        let name = Node.fullname node in
                        let name = Unix.readlink name in
                           ValData name) args
               with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               concat_array args
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(************************************************************************
 * Permissions.
 *)

(*
 * \begin{doc}
 * \fun{chmod}
 *
 * \begin{verbatim}
 *    chmod(mode, dst...)
 *       mode : Int
 *       dst : Node or Channel
 *    chmod(mode dst...)
 *       mode : String
 *       dst : Node Sequence
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+chmod+ function changes the permissions of the targets.
 *
 * Options:
 * \begin{description}
 * \item[-v] Explain what is happening.
 * \item[-r] Change files and directories recursively.
 * \item[-f] Continue on errors.
 * \item[--] Interpret the remaining argument literally.
 * \end{description}
 * \end{doc}
 *)
type chmod_info =
   { chmod_mode    : chmod_mode;
     chmod_rec     : bool;
     chmod_force   : bool;
     chmod_verbose : bool;
     chmod_files   : string list
   }

let chmod_default_info =
   { chmod_mode    = ChmodNone;
     chmod_rec     = false;
     chmod_force   = false;
     chmod_verbose = false;
     chmod_files   = []
   }

let chmod_spec =
   Lm_arg.MultiLetterOptions, (**)
      ["options", (**)
          ["-m", (**)
              StringFold (fun info s -> { info with chmod_mode = ChmodString s }),
              "set permission mode";
           "-r", (**)
              UnitFold (fun info -> { info with chmod_rec = true }),
              "change permission recursively";
           "-f", (**)
              UnitFold (fun info -> { info with chmod_force = true }),
              "do not fail on errors";
           "-v", (**)
              UnitFold (fun info -> { info with chmod_verbose = true }),
              "explain what is happening";
           "--", (**)
              RestFold (fun info s -> { info with chmod_files = s :: info.chmod_files }),
              "the rest of the arguments are interpreted literally"]]

let chmod_default info s =
   { info with chmod_files = s :: info.chmod_files }, false

let chmod_usage = "Change file permissions"

(*
 * Actual chmod command.
 *)
let chmod info filename =
   if info.chmod_verbose then
      printf "Changing permissions on %s@." filename;
   let mode = (Unix.LargeFile.stat filename).Unix.LargeFile.st_perm in
   let mode = mode_of_chmod mode info.chmod_mode in
   if info.chmod_force then
      try Unix.chmod filename mode with
         Unix.Unix_error _ ->
            ()
   else
      Unix.chmod filename mode

(*
 * Recursive versions.
 *)
let rec chmod_rec info filename =
   let subnames =
      try Lm_filename_util.lsdir filename with
         Unix.Unix_error _ ->
            []
   in
      List.iter (fun name -> chmod_rec info (Filename.concat filename name)) subnames;
      chmod info filename

(*
 * The command-line version.
 *)
let chmod venv pos loc args =
   let pos = string_pos "chmod" pos in
   let info, nodes =
      match args with
         [mode; nodes] ->
            let mode =
               match mode with
                  ValInt mode ->
                     ChmodInt mode
                | _ ->
                     let s = string_of_value venv pos mode in
                        try ChmodInt (int_of_string s) with
                           Failure _ ->
                              ChmodString s
            in
            let info = { chmod_default_info with chmod_mode = mode } in
               info, nodes
       | [nodes] ->
            chmod_default_info, nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (1, 2), List.length args)))
   in
   let argv = Array.of_list ("chmod" :: strings_of_value venv pos nodes) in
   let info =
      try Lm_arg.fold_argv argv chmod_spec info chmod_default chmod_usage with
         Failure s ->
            raise (OmakeException (loc_pos loc pos, StringError s))
   in
   let info, files =
      if info.chmod_mode <> ChmodNone then
         info, info.chmod_files
      else
         match List.rev info.chmod_files with
            mode :: rest ->
               let info = { info with chmod_mode = ChmodString mode } in
                  info, rest
          | [] ->
               raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let files =
      List.map (fun name ->
            Dir.fullname (venv_intern_dir venv name)) files
   in
   let () =
      if info.chmod_rec then
         List.iter (chmod_rec info) files
      else
         List.iter (chmod info) files
   in
      ValNone

(*
 * \begin{doc}
 * \fun{chown}
 *
 * \begin{verbatim}
 *    chown(uid, gid, node...)
 *       uid : Int
 *       gid : Int
 *       node : Node or Channel
 *    chown(uid, node...)
 *       uid : Int
 *       node : Node or Channel
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+chown+ function changes the user and group id of the file.
 * If the \verb+gid+ is not specified, it is not changed.  If either
 * id is -1, that id is not changed.
 * \end{doc}
 *)
let chown venv pos loc args =
   let pos = string_pos "chown" pos in
   let uid, gid, nodes =
      match args with
         [uid; nodes] ->
            uid, None, nodes
       | [uid; gid; nodes] ->
            uid, Some gid, nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (2, 3), List.length args)))
   in
   let uid = int_of_value venv pos uid in
   let gid =
      match gid with
         Some gid ->
            int_of_value venv pos gid
       | None ->
            -1
   in
   let nodes = values_of_value venv pos nodes in
   let () =
      try
         List.iter (fun node ->
               Unix.chown (filename_of_value venv pos node) uid gid) nodes
      with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

(*
 * \begin{doc}
 * \fun{utimes}
 *
 * \begin{verbatim}
 *    utimes(atime, mtime, node...)
 *       atime : Float
 *       mtime : Float
 *       node : Node
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+utimes+ function changes the access and modification
 * times of the files.
 * \end{doc}
 *)
let utimes venv pos loc args =
   let pos = string_pos "utimes" pos in
   let atime, mtime, nodes =
      match args with
         [atime; mtime; nodes] ->
            atime, mtime, nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 3, List.length args)))
   in
   let atime = float_of_value venv pos atime in
   let mtime = float_of_value venv pos mtime in
   let nodes = values_of_value venv pos nodes in
   let () =
      try
         List.iter (fun node ->
               Unix.utimes (filename_of_value venv pos node) atime mtime) nodes
      with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

(*
 * \begin{doc}
 * \fun{truncate}
 *
 * \begin{verbatim}
 *    truncate(length, node...)
 *        length : Int
 *        node : Node or Channel
 *    raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+truncate+ function truncates a file to the given length.
 * \end{doc}
 *)
let truncate venv pos loc args =
   let pos = string_pos "truncate" pos in
   let len, nodes =
      match args with
         [len; nodes] ->
            len, nodes
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))
   in
   let len = int_of_value venv pos len in
   let nodes = values_of_value venv pos nodes in
   let () =
      try
         List.iter (fun node ->
               Unix.truncate (filename_of_value venv pos node) len) nodes
      with
         Unix.Unix_error _ as exn ->
            raise (UncaughtException (pos, exn))
   in
      ValNone

(*
 * \begin{doc}
 * \fun{umask}
 *
 * \begin{verbatim}
 *     $(umask mode) : Int
 *        mode : Int
 *     raises RuntimeException
 * \end{verbatim}
 *
 * Sets the file mode creation mask.
 * The previous mask is returned.
 * This value is not scoped, changes have global effect.
 * \end{doc}
 *)
let umask venv pos loc args =
   let pos = string_pos "umask" pos in
      match args with
         [arg] ->
            let mode = int_of_value venv pos arg in
            let mask =
               try Unix.umask mode with
                  Unix.Unix_error _ as exn ->
                     raise (UncaughtException (pos, exn))
            in
               ValInt mask
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \section{vmount}
 * \fun{vmount}
 *
 * \begin{verbatim}
 *     vmount(src, dst)
 *        src, dst : Dir
 *     vmount(flags, src, dst)
 *        flags : String
 *        src, dst : Dir
 * \end{verbatim}
 *
 * ``Mount'' the \verb+src+ directory on the \verb+dst+ directory.  This is
 * a virtual mount, changing the behavior of the \verb+$(file ...)+ function.
 * When the \verb+$(file str)+ function is used, the resulting file is taken
 * relative to the \verb+src+ directory if the file exists.  Otherwise, the
 * file is relative to the current directory.
 *
 * The main purpose of the \verb+vmount+ function is to support multiple
 * builds with separate configurations or architectures.
 *
 * The options are as follows.
 * \begin{description}
 * \item[l] Create symbolic links to files in the \verb+src+ directory.
 * \item[c] Copy files from the \verb+src+ directory.
 * \end{description}
 *
 * Mount operations are scoped.
 * \end{doc}
 *)
type vmount_flags =
   MountForce

let vmount_flags pos loc s =
   let pos = string_pos "vmount_flags" pos in
   let len = String.length s in
   let rec collect local_flags mount_flags i =
      if i = len then
         local_flags, mount_flags
      else
         let local_flags, mount_flags =
            match s.[i] with
               'l' ->
                  local_flags, MountLink :: mount_flags
             | 'c' ->
                  local_flags, MountCopy :: mount_flags
             | 'f' ->
                  MountForce :: local_flags, mount_flags
             | '-'
             | ' ' ->
                  local_flags, mount_flags
             | c ->
                  raise (OmakeException (loc_pos loc pos, StringStringError ("illegal vmount option", String.make 1 c)))
         in
            collect local_flags mount_flags (succ i)
   in
      collect [] [] 0

(*
 * Refer to all the files in the source directory, so that
 * they get copied.
 *)
let vmount_touch_files venv pos src dst =
   let _pos = string_pos "vmount_touch_files" pos in
   let src_name = Dir.fullname src in
   let dst_name = Dir.name src dst in
   let options = Lm_glob.create_options [GlobIgnore [dst_name]] in
   let _, files = list_dirs_rec options src_name ["."] in
      List.iter (fun name -> ignore (venv_intern_cd venv PhonyProhibited dst name)) files

(*
 * Add the mount.
 *)
let vmount venv pos loc args kargs =
   let pos = string_pos "vmount" pos in
   let flags, src, dst =
      match args, kargs with
         [src; dst], [] ->
            "", src, dst
       | [flags; src; dst], [] ->
            string_of_value venv pos flags, src, dst
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityRange (2, 3), List.length args)))
   in
   let local_flags, mount_flags = vmount_flags pos loc flags in
   let src = dir_of_value venv pos src in
   let dst = dir_of_value venv pos dst in
   let venv = venv_mount venv mount_flags src dst in
      if List.mem MountForce local_flags then
         vmount_touch_files venv pos src dst;
      venv, ValNone

(*
 * \begin{doc}
 * \fun{add-project-directories}
 *
 * \begin{verbatim}
 *     add-project-directories(dirs)
 *        dirs : Dir Array
 * \end{verbatim}
 *
 * Add the directories to the set of directories that omake considers to be part
 * of the project.  This is mainly used to avoid omake complaining that the
 * current directory is not part of the project.
 * \end{doc}
 *)
let add_project_directories venv pos loc args =
   let pos = string_pos "add-project-directories" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
               List.iter (fun v ->
                     venv_add_explicit_dir venv (dir_of_value venv pos v)) values;
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \fun{remove-project-directories}
 *
 * \begin{verbatim}
 *     remove-project-directories(dirs)
 *        dirs : Dir Array
 * \end{verbatim}
 *
 * Removed the directories from the set of directories that omake considers to be part
 * of the project.  This is mainly used to cancel a \verb+.SUBDIRS+ from including
 * a directory if it is determined that the directory does not need to be compiled.
 * \end{doc}
 *)
let remove_project_directories venv pos loc args =
   let pos = string_pos "add-project-directories" pos in
      match args with
         [arg] ->
            let values = values_of_value venv pos arg in
               List.iter (fun v ->
                     venv_remove_explicit_dir venv (dir_of_value venv pos v)) values;
               ValNone
       | _ ->
            raise (OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(************************************************************************
 * Tables.
 *)

let () =
   let builtin_funs =
      [true, "file-sort",               sort,                     ArityExact 2;
       true, "file-check-sort",         check_sort,               ArityExact 2;
       true, "ls",                      ls,                       ArityRange (1, 2);
       true, "glob",                    glob,                     ArityRange (1, 2);
       true, "subdirs",                 subdirs,                  ArityRange (1, 2);
       true, "basename",                basename,                 ArityExact 1;
       true, "dirname",                 dirname,                  ArityExact 1;
       true, "homename",                homename,                 ArityExact 1;
       true, "rootname",                rootname,                 ArityExact 1;
       true, "fullname",                fullname,                 ArityExact 1;
       true, "absname",                 absname,                  ArityExact 1;
       true, "suffix",                  suffix,                   ArityExact 1;
       true, "tmpfile",                 tmpfile,                  ArityRange (1, 2);
       true, "file",                    file,                     ArityExact 1;
       true, "dir",                     dir,                      ArityExact 1;
       true, "which",                   which,                    ArityExact 1;
       true, "where",                   where,                    ArityExact 1;
       true, "exists-in-path",          exists_in_path,           ArityExact 1;
       true, "in",                      ind,                      ArityExact 2;
       true, "dirof",                   dirof,                    ArityExact 1;
       true, "stat-reset",              stat_reset,               ArityExact 1;
       true, "file-exists",             file_exists,              ArityExact 1;
       true, "filter-exists",           filter_exists,            ArityExact 1;
       true, "target-exists",           target_exists,            ArityExact 1;
       true, "filter-targets",          filter_targets,           ArityExact 1;
       true, "target-is-proper",        target_is_proper,         ArityExact 1;
       true, "filter-proper-targets",   filter_proper_targets,    ArityExact 1;
       true, "stat",                    stat,                     ArityExact 1;
       true, "lstat",                   lstat,                    ArityExact 1;
       true, "unlink",                  unlink,                   ArityExact 1;
       true, "rename",                  rename,                   ArityExact 2;
       true, "readlink",                readlink,                 ArityExact 1;
       true, "readlink-raw",            readlink_raw,             ArityExact 1;
       true, "truncate",                truncate,                 ArityExact 2;

       true, "mkdir",                   mkdir,                    ArityRange (1, 2);
       true, "rmdir",                   rmdir,                    ArityExact 1;
       true, "rm",                      rm,                       ArityExact 1;
       true, "mv",                      mv,                       ArityExact 1;
       true, "cp",                      cp,                       ArityExact 1;
       true, "link",                    link,                     ArityExact 2;
       true, "symlink",                 symlink,                  ArityExact 2;
       true, "symlink-raw",             symlink_raw,              ArityExact 2;
       true, "chmod",                   chmod,                    ArityExact 2;
       true, "chown",                   chown,                    ArityRange (2, 3);
       true, "utimes",                  utimes,                   ArityExact 3;
       true, "umask",                   umask,                    ArityExact 1;
       true, "digest-string",           digest_string,            ArityExact 1;
       true, "digest",                  digest,                   ArityExact 1;
       true, "digest-optional",         digest_optional,          ArityExact 1;
       true, "find-in-path",            find_in_path,             ArityExact 2;
       true, "find-in-path-optional",   find_in_path_optional,    ArityExact 2;
       true, "digest-in-path",          digest_in_path,           ArityExact 2;
       true, "digest-in-path-optional", digest_in_path_optional,  ArityExact 2;
       true, "rehash",                  rehash,                   ArityExact 0;
       true, "find-targets-in-path",     find_targets_in_path,      ArityExact 2;
       true, "find-targets-in-path-optional", find_targets_in_path_optional, ArityExact 2;
       true, "find-ocaml-targets-in-path-optional", find_ocaml_targets_in_path_optional, ArityExact 2;

       true, "add-project-directories", add_project_directories,  ArityExact 1;
       true, "remove-project-directories", remove_project_directories,  ArityExact 1]
   in
   let builtin_kfuns =
      [true, "vmount",                  vmount,                   ArityRange (2, 3);
      ]
   in
   let pervasives_objects =
      ["Tm";
      ]
   in
   let builtin_info =
      { builtin_empty with builtin_funs = builtin_funs;
                           builtin_kfuns = builtin_kfuns;
                           pervasives_objects = pervasives_objects
      }
   in
      register_builtin builtin_info

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
