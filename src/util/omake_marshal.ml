(*
 * Marshaling of messages.
 *)


(* let version_number = Hashtbl.hash "$Id$" *)

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
  | MaxMagic
  | ResponseStatusMagic


type msg = magic Fmarshal.item

exception MarshalError


(*
 * Some common marshalers.
 *)
let marshal_string_list l =
   Fmarshal.List (List.map (fun s -> Fmarshal.String s) l)

let unmarshal_string_list (l : 'a Fmarshal.item) : string list  =
  match l with
  | Fmarshal.List l ->
    List.map (function
      |  Fmarshal.String s -> s
      | _ -> raise MarshalError) l
  | _ ->
    raise MarshalError


let marshal_loc (loc : Lm_location.loc) : magic Fmarshal.item =
   let file, sline, schar, eline, echar = Lm_location.dest_loc loc in
   let file = Lm_symbol.to_string file in
   List [Magic LocationMagic; String file; Int sline; Int schar; Int eline; Int echar]

let unmarshal_loc (l : magic Fmarshal.item) : Lm_location.loc =
  match l with
  | List [Magic LocationMagic; String file; Int sline; Int schar; Int eline; Int echar] ->
    Lm_location.create_loc (Lm_symbol.add file) sline schar eline echar
  | _ ->
    raise MarshalError
