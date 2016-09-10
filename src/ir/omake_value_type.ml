
(* %%MAGICBEGIN%% *)
(*
 * Various kinds of handles.
 *)
type handle_env = Lm_handle_table.handle

(*
 * I/O channels.
 *)
type channel_mode = Lm_channel.mode =
   InChannel
 | OutChannel
 | InOutChannel

type prim_channel = Lm_int_handle_table.handle

(*
 * Possible values.
 * Someday we may want to include rules and functions.
 * For the function, the obj is the static scope.
 *
 * GS:
 * ValQuote          This is just a concatenation of the inner stuff as string.
 *                   Unlike ValSequence this doesn't generate a list of words.
 * ValQuoteString    Concatenates the inner stuff and puts the quote char
 *                   around it. Keeping that info seems reasonable because
 *                   you can auto-escape the inner data.
 * ValBody/ValFun    These are now very similar - both can take parameters.
 *                   The difference is that a ValFun sets the static env
 *                   (i.e. private vars) to the scope of the time when the
 *                   function was defined. ValBody doesn't do this, and hence
 *                   the static env of the caller is used for the evaluation.
 *)
type t =
  | ValNone
  | ValInt         of int
  | ValFloat       of float
  | ValSequence    of t list
  | ValArray       of t list
  | ValWhite       of string
  | ValString      of string
  | ValData        of string
  | ValQuote       of t list
  | ValQuoteString of char * t list
  | ValRules       of Omake_node.Node.t list
  | ValNode        of Omake_node.Node.t
  | ValDir         of Omake_node.Dir.t
  | ValObject      of obj
  | ValMap         of map
  | ValChannel     of channel_mode * prim_channel
  | ValClass       of obj Lm_symbol.SymbolTable.t

  (* Raw expressions *)
  | ValStringExp   of env * Omake_ir.string_exp
  | ValBody        of keyword_param_value list * Omake_ir.param list * Omake_ir.exp list * Omake_ir.export
  | ValCases       of (Omake_ir.var * t * Omake_ir.exp list * Omake_ir.export) list

  (* Functions *)
  | ValFun         of env * keyword_param_value list * Omake_ir.param list * Omake_ir.exp list * Omake_ir.export
  | ValFunCurry    of env * param_value list * keyword_param_value list * Omake_ir.param list * Omake_ir.exp list * Omake_ir.export * keyword_value list

  (* Closed values *)
  | ValPrim        of Omake_ir.arity * bool * Omake_ir.apply_empty_strategy * prim_fun

  (* The args, kargs are kept in -reverse- order *)
  | ValPrimCurry   of Omake_ir.arity * bool * prim_fun * t list * keyword_value list

  (* Implicit t dependencies *)
  | ValMaybeApply  of Lm_location.t * Omake_ir.var_info

  (* Variables that are not applications *)
  | ValVar         of Lm_location.t * Omake_ir.var_info

  (* Other values *)
  | ValOther       of value_other

  (* Delayed values *)
  | ValDelayed     of value_delayed ref

(*
 * Put all the other stuff here, to keep the primary t type
 * smaller.
 *)
and value_other =
  | ValLexer       of Omake_lexer.Lexer.t
  | ValParser      of Omake_parser.Parser.t
  | ValLocation    of Lm_location.t
  | ValExitCode    of int
  | ValEnv         of handle_env * Omake_ir.export

and value_delayed =
  | ValValue of t

  (* Value in a static block *)
  | ValStaticApply of t * Omake_ir.var

(*
 * Arguments have an optional keyword.
 *)
and param_value = Omake_ir.param * t
and keyword_value = Omake_ir.var * t
and keyword_param_value = Omake_ir.var * Omake_ir.param * t option

(*
 * Primitives are option refs.
 * We do this so that we can marshal these values.
 * Just before marshaling, all the options are set to None.
 *)
and prim_fun = Lm_symbol.t

(*
 * An object is just an environment.
 *)
and obj = t Lm_symbol.SymbolTable.t
and env = t Lm_symbol.SymbolTable.t
and map = (t, t) Lm_map.tree
(* %%MAGICEND%% *)

(************************************************************************
 * Non-marshaled values.
*)

(*
 * A method path.
 *)
type path =
  | PathVar   of Omake_ir.var_info
  | PathField of path * obj * Omake_ir.var

(*
 * Command lists are used for rule bodies.
 * They have their environment, a list of sources,
 * and the actual body.  The body is polymorphic
 * for various kinds of commands.
 *)
type command =
  | CommandSection of t * Omake_ir_free_vars.free_vars * Omake_ir.exp list
  (* Name of the section, its free variables, and the expression *)
  | CommandValue of Lm_location.t * env * Omake_ir.string_exp

(*
 * Kinds of rules.
 *)
type rule_multiple =
  | RuleSingle
  | RuleMultiple
  | RuleScannerSingle
  | RuleScannerMultiple

type rule_kind =
  | RuleNormal
  | RuleScanner

(*
 * A target t that represents a node in a rule.
 *)
type target =
  | TargetNode of Omake_node.Node.t
  | TargetString of string

(*
 * A source is either
 *   1. A wild string
 *   2. A node
 *   3. An optional source
 *   4. A squashed source
 *)
type source_core =
  | SourceWild of Lm_wild.out_patt
  | SourceNode of Omake_node.Node.t

type 'a source = Omake_node_sig.node_kind * 'a

(************************************************************************
 * Exceptions.
 *)
type item =
  | Symbol        of Lm_symbol.t
  | String        of string
  | AstExp        of Omake_ast.exp
  | IrExp         of Omake_ir.exp
  | Location      of Lm_location.t
  | Value         of t
  | Error         of omake_error

and pos = item Lm_position.pos

and omake_error =
  | SyntaxError        of string
  | StringError        of string
  | StringAstError     of string * Omake_ast.exp
  | StringStringError  of string * string
  | StringDirError     of string * Omake_node.Dir.t
  | StringNodeError    of string * Omake_node.Node.t
  | StringVarError     of string * Omake_ir.var
  | StringIntError     of string * int
  | StringMethodError  of string * Omake_ir.var list
  | StringValueError   of string * t
  | StringTargetError  of string * target
  | LazyError          of (Format.formatter -> unit)
  | UnboundVar         of Omake_ir.var
  | UnboundVarInfo     of Omake_ir.var_info
  | UnboundFun         of Omake_ir.var
  | UnboundMethod      of Omake_ir.var list
  | UnboundFieldVar    of obj * Omake_ir.var
  | ArityMismatch      of Omake_ir.arity * int
  | NotImplemented     of string
  | UnboundKey         of string
  | UnboundValue       of t
  | NullCommand

(*
 * Standard exceptions.
 *)
exception OmakeException    of pos * omake_error
exception UncaughtException of pos * exn
exception RaiseException    of pos * obj
exception ExitException     of pos * int
exception ExitParentException     of pos * int
exception Return            of Lm_location.t * t * Omake_ir.return_id

(*
 * Omake's internal version of the Invalid_argument
 *)
exception OmakeFatal of string
exception OmakeFatalErr of pos * omake_error

