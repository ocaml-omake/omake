
(*
 * Search for the index after the drive letter.
 *)
type root =
 | NullRoot
 | DriveRoot of char

type 'a path =
 | RelativePath of 'a
 | AbsolutePath of root * 'a

(**  Pathname separator chars. *)
val separators       : string
val pathsep : string
(*
 * Remove quotations from a string that represents a filename.
 *)
val unescape_string  : string -> string

(*
 * Normalize function will give the canonical
 * lowercase name on Windows.  It is a nop on
 * Unix.
 *)
val normalize_string : string -> string
val normalize_path   : string list -> string list

(*
 * A null root directory.
 *)
val null_root       : root

(*
 * Get the root string.
 *)
val string_of_root  : root -> string

(*
 * Skip the drive letter if it exists.
 *)
val drive_skip      : string -> int

(*
 * Is this an absolute filename?
 *)
val is_absolute     : string -> bool

(*
 * Parse filenames.
 *)
val filename_string : string -> string path
val filename_path   : string -> string list path

(*
 * Split into root, suffix.
 *)
val split : string -> string * string

(*
 * Get the name without suffix.
 *)
val root : string -> string
val suffix : string -> string
val strip_suffixes : string -> string

(*
 * Replace Filename. operations.
 *)
val basename : string -> string
val replace_basename : string -> string -> string

(** Path simplification.
  Remove . and .. entries. *)
type pathname = string list

val split_path    : string -> pathname
val simplify_path : pathname -> pathname
val concat_path   : pathname -> string

(*
 * Path searching.
 *)
val is_executable : string -> string option
val which         : string -> string
val which_dir     : string -> string -> string
val where         : string -> string list

(*
 * Make an entire hierarchy.
 *)
val mkdirhier : string -> int -> unit

