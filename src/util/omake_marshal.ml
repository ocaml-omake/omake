(*
 * Marshaling of messages.
 *)
(* open Fmarshal. *)

let version_number = Hashtbl.hash "$Id$"

type magic =
  | LocationMagic
  | IdMagic
  | NullRootMagic
  | DriveRootMagic
  | DirRootMagic
  | DirSubMagic
  | NodeFileMagic
  | NodePhonyGlobalMagic
  | NodePhonyDirMagic
  | NodePhonyFileMagic
  | NodeFlaggedMagic
  | NodeIsOptionalMagic
  | NodeIsExistingMagic
  | NodeIsSquashedMagic
  | NodeIsScannerMagic
  | QuietFlagMagic
  | AllowFailureFlagMagic
  | AllowOutputFlagMagic
  | CommandLineMagic
  | PrintEagerMagic
  | PrintLazyMagic
  | PrintExitMagic
  | RequestSpawnMagic
  | ResponseCreateMagic
  | ResponseExitedMagic
  | ResponseStdoutMagic
  | ResponseStderrMagic
  | ResponseStatusMagic
  | MaxMagic

type msg = magic Fmarshal.item

exception MarshalError

(*
 * Magic numbers.
 * We cheat a little here.
 *)
let int_of_magic magic =
   (Obj.magic magic : int)

let max_magic = int_of_magic MaxMagic

let magic_of_int i =
   if i < 0 || i >= max_magic then
      raise (Failure "magic_of_int");
   (Obj.magic i : magic)

(*
 * Some common marshalers.
 *)
let marshal_string_list l =
   Fmarshal.List (List.map (fun s -> Fmarshal.String s) l)

let unmarshal_string_list l =
  match l with
  | Fmarshal.List l ->
    List.map (function
      |  Fmarshal.String s -> s
      | _ -> raise MarshalError) l
  | _ ->
    raise MarshalError

(*
 * Locations.
 *)
let marshal_loc loc =
   let file, sline, schar, eline, echar = Lm_location.dest_loc loc in
   let file = Lm_symbol.to_string file in
      Fmarshal.List [Magic LocationMagic; String file; Int sline; Int schar; Int eline; Int echar]

let unmarshal_loc l =
  match l with
  |Fmarshal.List [Magic LocationMagic; String file; Int sline; Int schar; Int eline; Int echar] ->
    Lm_location.create_loc (Lm_symbol.add file) sline schar eline echar
  | _ ->
    raise MarshalError
