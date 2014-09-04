

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



type msg = magic Fmarshal.item

exception MarshalError

val marshal_string_list :
  string list -> 'a Fmarshal.item
val unmarshal_string_list :
  'a Fmarshal.item -> string list

val marshal_loc :
  Lm_location.loc ->
  magic Fmarshal.item
val unmarshal_loc :
  magic Fmarshal.item ->
  Lm_location.loc
