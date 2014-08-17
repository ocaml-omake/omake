(*
 * Parsing command line arguments, MCC-style. Arguments to options
 * may be separated from the option by a space, or may be placed
 * immediately after the option (without space) IF the option is
 * not ambiguous.  Also, options may be abbreviated as long as the
 * short form is not ambiguous.
 *
 *)

type 'a poly_spec =
  (* Non-folding versions *)
  | Unit       of (unit -> unit)
  | Set        of bool ref
  | Clear      of bool ref
  | String     of (string -> unit)
  | Int        of (int -> unit)
  | Float      of (float -> unit)
  | Rest       of (string -> unit)

  (* Folding versions *)
  | UnitFold   of ('a -> 'a)
  | SetFold    of ('a -> bool -> 'a)
  | ClearFold  of ('a -> bool -> 'a)
  | StringFold of ('a -> string -> 'a)
  | IntFold    of ('a -> int -> 'a)
  | FloatFold  of ('a -> float -> 'a)
  | RestFold   of ('a -> string -> 'a)

  (* Usage message *)
  | Usage

(* spec_mode

   StrictOptions: options are processed literally, and may not be collapsed
      into multi-letter options.
   MultiLetterMode: single-letter options of the form -x may be collapsed
      into multi-letter options. *)
type spec_mode =
    StrictOptions
  | MultiLetterOptions

type 'a poly_section = (string * 'a poly_spec * string) list
type 'a poly_sections = spec_mode * (string * 'a poly_section) list

type spec = unit poly_spec
type section = unit poly_section
type sections = unit poly_sections

exception BogusArg of string
exception UsageError

(*
 * Folding versions.
 *)
val fold_argv : string array -> 'a poly_sections -> 'a -> ('a -> string -> 'a * bool) -> string -> 'a
val fold      : 'a poly_sections -> 'a -> ('a -> string -> 'a * bool) -> string -> 'a

(*
 * Non-folding versions.
 *)
val parse_argv : string array -> sections -> (string -> unit) -> string -> unit
val parse      : sections -> (string -> unit) -> string -> unit

(*
 * Usage string doesn't care.
 *)
val usage : 'a poly_sections -> string -> unit
