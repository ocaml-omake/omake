(*
 * Detection of filesystem case-sensitivity
 *)

val available : bool
val case_sensitive : string -> bool

(* toggle the name and compare return true if their stat is not equal
*)
val stat_with_toggle_case : string -> string -> bool

val check_already_lowercase : string -> int -> int -> unit



exception Already_lowercase




exception Not_a_usable_directory
val dir_case_sensitive : string -> bool
