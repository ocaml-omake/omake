(* Abstract syntax of OMakefiles. *)
type var = Lm_symbol.t

(*
 * Shell flags indicating whether a body needs to be read.
 * GS: Many parsing functions return whether a body follows, and which:
   NoBody                  no body follows
   OptBody                 a body may follow (subexpression)
   ColonBody               a body will follow. In the interactive mode this
                           is indicated by a colon at the end of the line,
                           hence the name
   ArrayBody               a body interpreted as array will follow. Used
                           for "V[] =".
 *)
type body_flag =
  | NoBody
  | OptBody
  | ColonBody
  | ArrayBody

(*
 * Function applications can be tagged as Lazy or Eager.
 * GS. Also used for variables etc. LazyApply is an _explicitly_ lazy
 * application (e.g. $`x) and EagerApply an _explicitly_ eager application
 * (e.g. $,x). Both overriding any evaluation strategy that was recorded
 * with the variable.
 *
 * GS. CommandApply: seeing this where a command is run (notation Cmd(...)).
 * I guess it means that the result is discarded
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
 *
 * GS:
 * NullExp               sometimes used for missing expression, e.g. missing
 *                       argument
 * IntExp                an integer (only used when int parsing is obligatory)
 * FloatExp              a float (only used when int parsing is obligatory)
 * StringOpExp           string that parses as special char/string, e.g. "::"
 *                       or "=>"
 * StringIdExp           string that parses as name
 * StringIntExp          string that parses as integer
 * StringFloatExp        string that parses as float
 * StringWhiteExp        string that parses as whitespace
 * StringKeywordExp      string that parses as keyword
 * StringOtherExp        other string
 * QuoteExp              range quoted with $""
 * QuoteStringExp        range in double quotes
 * SequenceExp           string sequence (concatenation)
 * ArrayExp              an argument is tagged as ArrayExp if it came from an
 *                       array body (not returned by parser, later added by
 *                       Omake_ast_util.update_body_args)
 * ApplyExp(CommandApply,...)
 *                       call a command (syntax Cmd(...)). Args may be given
 *                       as body or inline
 * ApplyExp(_,...)       call a function ($(name arg) - resulting in a value)
 * ApplyExp(...,var,[])  look up a variable (like function call with empty args)
 * MethodApplyExp        call a method command (syntax obj.Cmd(...))
 * SuperApplyExp         call a super method command
 * CommandExp            the "var" is the symbol for the command to run.
 *                       Used for builtins like "if", "match" etc.
 * VarDefExp             variable assignment (inline w/o body, v = ...)
 * VarDefBodyExp         variable assignment (w/ body, v =)
 * ObjectDefExp          object definition (w/body, v. =)
 * FunDefExp             function definition (w/ body, v(args) =)
 * RuleExp               a rule (2- or 3-place)
 * ShellExp              an external command to run (new process)
 * BodyExp               an argument is tagged as BodyExp if it came from a
 *                       non-array body (not returned by parser, later added by
 *                       Omake_ast_util.update_body_args)
 * CatchExp              "catch" expression (unclear why this doesn't fit into
 *                       CommandExp)
 * ClassExp              "class" expression
 * KeyExp                key lookup (|name|)
 * KeyDefExp             key assignment (inline w/o body, |name| = ...)
 * KeyDefBodyExp         key assignment (w/body, |name| =)
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

(* GS. param = formal param of a lambda *)
and param =
  | OptionalParam of var * exp * Lm_location.t
  | RequiredParam of var * Lm_location.t
  | NormalParam   of var * Lm_location.t

(* GS. arg = actual argument of an application *)
and arg      =
  | KeyArg      of var * exp
  | ExpArg      of exp
  | ArrowArg    of param list * exp

and parse_arg =
  | IdArg       of string * (string * Lm_location.t) option * Lm_location.t   (* Second string is always whitespace *)
  | NormalArg   of arg

and args     = arg list

type prog = exp list
