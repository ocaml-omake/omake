(* Abstract syntax of OMakefiles. *)
type var = Lm_symbol.t

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
  | NullExp         of Lm_location.t
  | IntExp          of int * Lm_location.t
  | FloatExp        of float * Lm_location.t
  | StringOpExp     of string * Lm_location.t
  | StringIdExp     of string * Lm_location.t
  | StringIntExp    of string * Lm_location.t
  | StringFloatExp  of string * Lm_location.t
  | StringWhiteExp  of string * Lm_location.t
  | StringOtherExp  of string * Lm_location.t
  | StringKeywordExp of string * Lm_location.t
  | QuoteExp        of exp list * Lm_location.t
  | QuoteStringExp  of char * exp list * Lm_location.t
  | SequenceExp     of exp list * Lm_location.t
  | ArrayExp        of exp list * Lm_location.t
  | ApplyExp        of apply_strategy * var * arg list * Lm_location.t
  | SuperApplyExp   of apply_strategy * var * var * arg list * Lm_location.t
  | MethodApplyExp  of apply_strategy * var list * arg list * Lm_location.t
  | CommandExp      of var * exp * exp list * Lm_location.t
  | VarDefExp       of var list * define_kind * define_flag * exp * Lm_location.t
  | VarDefBodyExp   of var list * define_kind * define_flag * exp list * Lm_location.t
  | ObjectDefExp    of var list * define_flag * exp list * Lm_location.t
  | FunDefExp       of var list * params * exp list * Lm_location.t
  | RuleExp         of bool * exp * exp * exp Lm_symbol.SymbolTable.t * exp list * Lm_location.t
  | BodyExp         of exp list * Lm_location.t
  | ShellExp        of exp * Lm_location.t
  | CatchExp        of var * var * exp list * Lm_location.t
  | ClassExp        of Lm_symbol.t list * Lm_location.t
  | KeyExp          of apply_strategy * string * Lm_location.t
  | KeyDefExp       of string * define_kind * define_flag * exp * Lm_location.t
  | KeyDefBodyExp   of string * define_kind * define_flag * exp list * Lm_location.t

and params = param list

and param =
  | OptionalParam of var * exp * Lm_location.t
  | RequiredParam of var * Lm_location.t
  | NormalParam   of var * Lm_location.t

and arg      =
  | KeyArg      of var * exp
  | ExpArg      of exp
  | ArrowArg    of param list * exp

and parse_arg =
  | IdArg       of string * (string * Lm_location.t) option * Lm_location.t   (* Second string is always whitespace *)
  | NormalArg   of arg

and args     = arg list

type prog = exp list
