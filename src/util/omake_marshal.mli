type 'a item =
  | Bool of bool
  | Char of char
  | Code of int
  | Symbol of int
  | Int of int
  | Magic of 'a
  | Float of float
  | String of string
  | List of 'a item list


type magic 
  =
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



type msg = magic item

exception MarshalError

val marshal_string_list :
  string list -> 'a item
val unmarshal_string_list :
  'a item -> string list

val marshal_loc :
  Lm_location.loc ->
  magic item
val unmarshal_loc :
  magic item ->
  Lm_location.loc
