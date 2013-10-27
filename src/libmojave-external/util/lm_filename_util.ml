(*
 * Utilities on filenames.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2006 Mojave Group, Caltech
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
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)

type pathname = string list

(*
 * Tests for whether a file is executable.
 *)
let euid =
   try Unix.geteuid () with
      Unix.Unix_error _ ->
         0

let groups =
   try Array.to_list (Unix.getgroups ()) with
      Unix.Unix_error _ ->
         []

let unix_is_executable s =
   let flag =
      try
         let { Unix.LargeFile.st_kind = kind;
               Unix.LargeFile.st_perm = perm;
               Unix.LargeFile.st_uid = uid;
               Unix.LargeFile.st_gid = gid
             } = Unix.LargeFile.stat s
         in
            (kind = Unix.S_REG)
            && ((perm land 0o001) <> 0
                || (List.mem gid groups && (perm land 0o010) <> 0)
                || (uid = euid && (perm land 0o100) <> 0))
      with
         Unix.Unix_error _ ->
            false
   in
      if flag then
         Some s
      else
         None

(*
 * On Windows, the file does not have the be executable,
 * it just has to exist.
 *)
let win32_suffixes =
   [""; ".com"; ".exe"; ".bat"; ".cmd"]

let win32_is_executable =
   let rec search_win32 suffixes name =
      match suffixes with
         suffix :: suffixes ->
            let name' = name ^ suffix in
               if Sys.file_exists name' then
                  Some name'
               else
                  search_win32 suffixes name
       | [] ->
            None
   in
      search_win32 win32_suffixes

(*
 * Cygwin is weird. See http://cygwin.com/cygwin-ug-net/using-specialnames.html#id4745135
 * and http://bugzilla.metaprl.org/show_bug.cgi?id=496#c11
 *)
let cygwin_is_executable name =
   match unix_is_executable (name ^ ".exe") with
      Some _ as res -> res
    | None ->
         begin match unix_is_executable name with
            Some _ as res' -> res'
          | None -> None
         end

(*
 * System-dependent config.
 * On win32, use lowercase names, and watch for drive letters.
 *)
let has_drive_letters,
    normalize_string,
    normalize_path,
    separator_char,
    search_separator_char,
    is_executable =
   match Sys.os_type with
      "Win32" ->
         true, String.lowercase, List.map String.lowercase, '\\', ';', win32_is_executable
    | "Cygwin" ->
         false, String.lowercase, List.map String.lowercase, '/', ':', cygwin_is_executable
    | "Unix" ->
         false, (fun s -> s), (fun s -> s), '/', ':', unix_is_executable
    | s ->
         raise (Invalid_argument ("Omake_node: unknown system type " ^ s))

let separator_string = String.make 1 separator_char
let search_separator_string = String.make 1 search_separator_char

(*
 * Utilities for splitting paths.
 *)
(* %%MAGICBEGIN%% *)
type root =
   NullRoot
 | DriveRoot of char

type 'a path =
   RelativePath of 'a
 | AbsolutePath of root * 'a
(* %%MAGICEND%% *)

let null_root = NullRoot

(*
 * Windows sucks.  Here are some utilities to
 * analyze drve letters.
 *)
let is_drive_letter c =
   match c with
      'a'..'z'
    | 'A'..'Z' ->
         true
    | _ ->
         false

let drive_skip name =
   let len = String.length name in
      if has_drive_letters && len >= 2 && is_drive_letter name.[0] && name.[1] = ':' then
         2
      else
         0

let is_slash c =
   match c with
      '/'
    | '\\' ->
         true
    | _ ->
         false

let is_absolute name =
   let len = String.length name in
      (len >= 3 && is_drive_letter name.[0] && name.[1] = ':' && is_slash name.[2]) || (len >= 1 && is_slash name.[0])

(*
 * Print the drive letter.
 *)
let string_of_root = function
   NullRoot ->
      separator_string
 | DriveRoot c ->
      let s = String.make 3 c in
         s.[1] <- ':';
         s.[2] <- separator_char;
         s

(*
 * Unescape a possibly quoted filename.
 * The only things we unquote are quotations.
 *)
let unescape_string s =
   let len = String.length s in
   let rec start i =
      if i = len then
         s
      else
         match String.unsafe_get s i with
            '"' ->
               let buf = Buffer.create len in
                  Buffer.add_substring buf s 0 i;
                  dquote buf (succ i)
          | '\'' ->
               let buf = Buffer.create len in
                  Buffer.add_substring buf s 0 i;
                  squote buf (succ i)
          | _ ->
               start (succ i)

   and dquote buf i =
      if i = len then
         Buffer.contents buf
      else
         match String.unsafe_get s i with
            '"' ->
               normal buf (succ i)
          | '\\' when i < len - 1 ->
               (match s.[i + 1] with
                   '"' ->
                      Buffer.add_char buf '"';
                      dquote buf (i + 2)
                 | _ ->
                      Buffer.add_char buf '\\';
                      dquote buf (succ i))
          | c ->
               Buffer.add_char buf c;
               dquote buf (succ i)

   and squote buf i =
      if i = len then
         Buffer.contents buf
      else
         match String.unsafe_get s i with
            '\'' ->
               normal buf (succ i)
          | '\\' when i < len - 1 ->
               (match s.[i + 1] with
                   '\'' ->
                      Buffer.add_char buf '\'';
                      dquote buf (i + 2)
                 | _ ->
                      Buffer.add_char buf '\\';
                      dquote buf (succ i))
          | c ->
               Buffer.add_char buf c;
               dquote buf (succ i)

   and normal buf i =
      if i = len then
         Buffer.contents buf
      else
         match String.unsafe_get s i with
            '"' ->
               dquote buf (succ i)
          | '\'' ->
               squote buf (succ i)
          | c ->
               Buffer.add_char buf c;
               normal buf (succ i)
   in
      start 0

(*
 * Split the path into root part, and the rest.
 *)
let filename_string name =
   let len = String.length name in
      if has_drive_letters && len >= 3 && is_drive_letter name.[0] && name.[1] = ':' && (name.[2] = '/' || name.[2] = '\\') then
         let root = DriveRoot (Char.lowercase name.[0]) in
         let path = String.sub name 3 (len - 3) in
            AbsolutePath (root, path)
      else if len >= 1 && name.[0] = '/' then
         let root = NullRoot in
         let path = String.sub name 1 (len - 1) in
            AbsolutePath (root, path)
      else
         RelativePath name

(*
 * Be careful not to split on glob characters.
 *)
let is_glob_char s i =
   match String.unsafe_get s i with
      '{' | '}' | '*' | '?' | '[' | ']' ->
         true
    | _ ->
         false

let filename_split name =
   let len = String.length name in
   let rec collect path off i =
      if i = len then
         let path =
            if i <= succ off then
               path
            else
               String.sub name off (i - off) :: path
         in
            List.rev path
      else
         let c = String.unsafe_get name i in
            match c with
               '/' ->
                  if i <= succ off then
                     collect path i (succ i)
                  else
                     collect (String.sub name off (i - off) :: path) i (succ i)
             | '\\' ->
                  if i < len - 1 && is_glob_char name (succ i) then
                     collect path off (i + 2)
                  else if i <= succ off then
                     collect path i (succ i)
                  else
                     collect (String.sub name off (i - off) :: path) i (succ i)
             | _ ->
                  collect path off (succ i)
   in
      collect [] 0 0

(*
 * Split the rest into parts.
 *)
let filename_path name =
   match filename_string name with
      AbsolutePath (root, path) ->
         AbsolutePath (root, Lm_string_util.split "\\/" path)
    | RelativePath path ->
         RelativePath (Lm_string_util.split "\\/" path)

(*
 * Split a filename into root/suffix.
 *)
let split name =
   try
      let index = String.rindex name '.' in
      let len = String.length name in
      let root = String.sub name 0 index in
      let suffix = String.sub name index (len - index) in
         root, suffix
   with
      Not_found ->
         name, ""

(*
 * Separate this for efficiency.
 *)
let root name =
   try
      let index = String.rindex name '.' in
         String.sub name 0 index
   with
      Not_found ->
         name

let suffix name =
   try
      let index = String.rindex name '.' in
         String.sub name index (String.length name - index)
   with
      Not_found ->
         ""

let strip_suffixes name =
   let start =
      try String.rindex name '/' with
         Not_found ->
            try String.rindex name '\\' with
               Not_found ->
                  0
   in
      try
         let index = String.index_from name start '.' in
            String.sub name 0 index
      with
         Not_found ->
            name

(*
 * Pathname separator chars.
 *)
let separators = "/\\"

(*
 * Split a pathname.
 *)
let split_path = Lm_string_util.split separators

(*
 * Put it back together.
 *)
let concat_path = String.concat separator_string

(*
 * Basic file operations.
 *)
let basename s =
   try
      let i = Lm_string_util.rindex_set s separators + 1 in
         String.sub s i (String.length s - i)
   with
      Not_found ->
         s

let replace_basename s1 s2 =
   try
      let i = Lm_string_util.rindex_set s1 separators in
         Filename.concat (String.sub s1 0 i) s2
   with
      Not_found ->
         s2

(*
 * Simplify, remove leading directory.
 *)
let simplify_path path =
   let rec simplify path' = function
      dir::tl ->
         if dir = "" or dir = "." then
            simplify path' tl
         else if dir = ".." then
            match path' with
               [] ->
                  simplify path' tl
             | _::path'' ->
                  simplify path'' tl
         else
            simplify (dir :: path') tl
    | [] ->
         List.rev path'
   in
      simplify [] path

(*
 * Path searching.
 *)
let search_table = Hashtbl.create 19

(*
 * Get the system path.
 *)
let search_path =
   let path =
      try Sys.getenv "PATH" with
         Not_found ->
            "."
   in
   let path = Lm_string_util.split search_separator_string path in
      List.filter (fun name -> not (Filename.is_relative name)) path

(*
 * Search for the file in the path.
 * Win32 files do not need to be executable.
 *)
let search_command name =
   let rec search dirs name =
      match dirs with
         dir :: dirs ->
            let pathname = Filename.concat dir name in
               (match is_executable pathname with
                   Some pathname ->
                      pathname
                 | None ->
                      search dirs name)
       | [] ->
            raise Not_found
   in
      search search_path name

(*
 * Figure out where in the path the commands comes from.
 *)
let which name =
   if Filename.is_relative name then
      if Lm_string_util.contains_any name separators then
         let name = Filename.concat (Sys.getcwd ()) name in
            match is_executable name with
               Some fullname ->
                  fullname
             | None ->
                  raise Not_found
      else
         try Hashtbl.find search_table name with
            Not_found ->
               let fullname = search_command name in
                  Hashtbl.add search_table name fullname;
                  fullname
   else
      match is_executable name with
         Some fullname ->
            fullname
       | None ->
            raise Not_found

(*
 * Use the directory as the starting point for relative names.
 *)
let which_dir dir name =
   if Filename.is_relative name then
      if Lm_string_util.contains_any name separators then
         let name = Filename.concat dir name in
            match is_executable name with
               Some fullname ->
                  fullname
             | None ->
                  raise Not_found
      else
         try Hashtbl.find search_table name with
            Not_found ->
               let fullname = search_command name in
                  Hashtbl.add search_table name fullname;
                  fullname
   else
      match is_executable name with
         Some fullname ->
            fullname
       | None ->
            raise Not_found

(*
 * Figure out where in the path the commands comes from.
 * Return all matches in order.
 *)
let where name =
   if Lm_string_util.contains_any name separators then
      raise (Invalid_argument "Lm_filename_util.where");
   let path =
      try Sys.getenv "PATH" with
         Not_found ->
            "."
   in
   let path = Lm_string_util.split search_separator_string path in
   let find dir = is_executable (Filename.concat dir name) in
      Lm_list_util.some_map find path

(*
 * Make a directory hierarchy.
 *)
let mkdirhier dir mode =
   let rec mkdir dir path =
      match path with
         head :: path ->
            let dir = Filename.concat dir head in
            let () =
               try
                  let s = Unix.LargeFile.stat dir in
                     if s.Unix.LargeFile.st_kind <> Unix.S_DIR then
                        raise (Unix.Unix_error (Unix.ENOTDIR, "Lm_filename_util.mkdirhier", dir))
               with
                  Unix.Unix_error (Unix.ENOENT, _, _) ->
                     try Unix.mkdir dir mode with
                        Unix.Unix_error (Unix.EEXIST, _, _) ->
                           ()
            in
               mkdir dir path
       | [] ->
            ()
   in
   let path = filename_path dir in
   let top, path =
      match path with
         AbsolutePath (root, path) ->
            string_of_root root, path
       | RelativePath path ->
            ".", path
   in
      mkdir top path

(*
 * Get the names in a directory.
 *)
let lsdir dirname =
   let dir = Unix.opendir dirname in
   let rec loop names =
      let name =
         try Some (Unix.readdir dir) with
            End_of_file ->
               None
      in
         match name with
            Some "."
          | Some ".." ->
               loop names
          | Some name ->
               loop (name :: names)
          | None ->
               Unix.closedir dir;
               List.rev names
   in
      loop []

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
