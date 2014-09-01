(*
 * A simple database.  This is a low-level ionterface.
 * See, for example, omake_db.ml to see a higher-level
 * interface.
 *)
val debug_db : bool ref

type t = Unix.file_descr

type tag = int
type magic = string
type digest = string
type hostname = string
type named_value = string * string

type entry_pred = tag -> named_value list -> hostname -> digest -> bool

(*
 * Some kinds of entries are host-independent.
 *)
type host =
   HostIndependent
 | HostDependent

(*
 * These functions assume that the file is locked.
 *    tag: the kind of entry
 *    magic: the magic number for this version
 *    digest: the source file digest (or use the empty string)
 *
 * These functions operate by side-effect, modifying the file.
 *    add: remove the old entry and add a new one
 *    find: find an existing entry, or raise Not_found if it doesn't exist
 *    remove: remove an old entry, does not fail.
 *)
val add    : t -> string -> tag * host -> magic -> digest -> 'a -> unit
val find   : t -> string -> tag * host -> magic -> digest -> 'a
val remove : t -> string -> tag * host -> magic -> unit

(*
 * Somewhat more general interface.
 *)
val first_entry_tag : tag

val append_entry : t -> string -> tag -> named_value list -> digest -> 'a -> unit
val find_entry   : t -> string -> entry_pred -> 'a
val remove_entry : t -> string -> entry_pred -> unit
