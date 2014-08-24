(* Abstract syntax of OMakefiles. *)
type var = Lm_symbol.symbol

(*
 * Shell flags indicating whether a body needs to be read.
 *)
type body_flag =
  | NoBody
  | OptBody
  | ColonBody
  | ArrayBody

(*
 * Function applications can be tagged as Lazy or Eager.
 *)
type apply_strategy =
  | LazyApply
  | EagerApply
  | NormalApply
  | CommandApply

(*
 * When a variable is defined, these are additional flags.
 * The bool is true if this is an array operation.
 *)
type define_kind =
  | DefineString
  | DefineArray

type define_flag =
  | DefineNormal
  | DefineAppend

(*
 * Expressions.
 *
 * The String*Exp are all strings.  Normally, they are all interpreted
 * the same way.
 *)
type exp =
  | NullExp         of Lm_location.loc
  | IntExp          of int * Lm_location.loc
  | FloatExp        of float * Lm_location.loc
  | StringOpExp     of string * Lm_location.loc
  | StringIdExp     of string * Lm_location.loc
  | StringIntExp    of string * Lm_location.loc
  | StringFloatExp  of string * Lm_location.loc
  | StringWhiteExp  of string * Lm_location.loc
  | StringOtherExp  of string * Lm_location.loc
  | StringKeywordExp of string * Lm_location.loc
  | QuoteExp        of exp list * Lm_location.loc
  | QuoteStringExp  of char * exp list * Lm_location.loc
  | SequenceExp     of exp list * Lm_location.loc
  | ArrayExp        of exp list * Lm_location.loc
  | ApplyExp        of apply_strategy * var * arg list * Lm_location.loc
  | SuperApplyExp   of apply_strategy * var * var * arg list * Lm_location.loc
  | MethodApplyExp  of apply_strategy * var list * arg list * Lm_location.loc
  | CommandExp      of var * exp * exp list * Lm_location.loc
  | VarDefExp       of var list * define_kind * define_flag * exp * Lm_location.loc
  | VarDefBodyExp   of var list * define_kind * define_flag * exp list * Lm_location.loc
  | ObjectDefExp    of var list * define_flag * exp list * Lm_location.loc
  | FunDefExp       of var list * params * exp list * Lm_location.loc
  | RuleExp         of bool * exp * exp * exp Lm_symbol.SymbolTable.t * exp list * Lm_location.loc
  | BodyExp         of exp list * Lm_location.loc
  | ShellExp        of exp * Lm_location.loc
  | CatchExp        of var * var * exp list * Lm_location.loc
  | ClassExp        of Lm_symbol.symbol list * Lm_location.loc
  | KeyExp          of apply_strategy * string * Lm_location.loc
  | KeyDefExp       of string * define_kind * define_flag * exp * Lm_location.loc
  | KeyDefBodyExp   of string * define_kind * define_flag * exp list * Lm_location.loc

and params = param list

and param =
  | OptionalParam of var * exp * Lm_location.loc
  | RequiredParam of var * Lm_location.loc
  | NormalParam   of var * Lm_location.loc

and arg      =
  | KeyArg      of var * exp
  | ExpArg      of exp
  | ArrowArg    of param list * exp

and parse_arg =
  | IdArg       of string * (string * Lm_location.loc) option * Lm_location.loc   (* Second string is always whitespace *)
  | NormalArg   of arg

and args     = arg list

type prog = exp list
