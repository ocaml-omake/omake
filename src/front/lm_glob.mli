(* open Lm_lexer. *)

type glob_option =
  | GlobNoBraces         (* Do not perform csh-style brace expansion *)
  | GlobNoTilde          (* Do not perform tilde-expansion *)
  | GlobNoEscape         (* The \ character does not escape special characters *)
  | GlobNoCheck          (* If an expansion fails, return the expansion literally *)
  | GlobIgnoreCheck      (* If an expansion fails, it expands to nothing *)
  | GlobDot              (* Allow wildcards to match filenames with a leading . *)
  | GlobOnlyFiles        (* Return only non-directories in the result *)
  | GlobOnlyDirs         (* Return only directories in the result *)
  | GlobCVSIgnore        (* Ignore files as specified by .cvsignore files *)
  | GlobIgnore of string list  (* Ignore the files that match the pattern *)
  | GlobAllow of string list   (* Allow only files that match the pattern *)
  | GlobIgnoreFun of (string -> bool)  (* Ignore the files specified by the function *)
  | GlobAllowFun of (string -> bool)   (* Allow only the files specified by the function *)
  | GlobHomeDir of string              (* Home directory for ~ expansion *)
  | GlobProperSubdirs                  (* Include only proper subdirs in listing *)
    
type glob_options

val create_options : glob_option list -> glob_options
val default_glob_options : glob_options

(*
 * The initial home directory for tilde expansion.
 * The globber does its best to figure this out.
 *)
val home_dir : string

(*
 * Get a list of all the users in the system.
 * On non-unix systems, returns the empty list.
 *)
val getusers : unit -> string list

(*
 * Get the home directory for a user.
 *)
val gethomedir : string -> string

(*
 * Try to collapse a filename.
 * Tilde-expansion will invert this process.
 *)
val tilde_collapse : string -> string

(*
 * Glob detection and escaping.
 *)
val is_glob_string : glob_options -> string -> bool
val glob_add_escaped : glob_options -> Buffer.t -> string -> unit

(*
 * The glob function returns two lists:
 *    1. a list of directories
 *    2. a list of files of other types
 *
 * The second argument to the glob and the glob_argv functions is the directory
 * where to perform expansion. If the glob pattern is relative, the results are
 * left relative (to that directory) as well.
 *
 * Raises Failure if the syntax is ill-formed.
 *)
val glob : glob_options -> string -> string list -> string list * string list

(*
 * Glob a command line.
 * Preserves the argument ordering.
 *)
val glob_argv : glob_options -> string -> string list -> string list

(*
 * Get the entries in a directory.
 *
 *    list_dirs root dirs
 *       root: the directory prefix, not appended to the output strings
 *       dirs: the directories to list
 *)
val list_dirs : glob_options -> string -> string list -> string list * string list
val list_dirs_rec : glob_options -> string -> string list -> string list * string list
val subdirs_of_dirs : glob_options -> string -> string list -> string list

(*
 * Utilities.
 *)
val regex_of_shell_pattern : glob_options -> string -> Lm_lexer.LmStr.t
