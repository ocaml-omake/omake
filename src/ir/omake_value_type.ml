
(* %%MAGICBEGIN%% *)
(*
 * Various kinds of handles.
 *)
type handle_env = Lm_handle_table.HandleTable.handle

(*
 * I/O channels.
 *)
type channel_mode = Lm_channel.mode =
   InChannel
 | OutChannel
 | InOutChannel

type prim_channel = Lm_handle_table.IntHandleTable.handle

(*
 * Possible values.
 * Someday we may want to include rules and functions.
 * For the function, the obj is the static scope.
 *)
type value =
  |ValNone
  | ValInt         of int
  | ValFloat       of float
  | ValSequence    of value list
  | ValArray       of value list
  | ValWhite       of string
  | ValString      of string
  | ValData        of string
  | ValQuote       of value list
  | ValQuoteString of char * value list
  | ValRules       of Omake_node.Node.t list
  | ValNode        of Omake_node.Node.t
  | ValDir         of Omake_node.Dir.t
  | ValObject      of obj
  | ValMap         of map
  | ValChannel     of channel_mode * prim_channel
  | ValClass       of obj Lm_symbol.SymbolTable.t

  (* Raw expressions *)
  | ValStringExp   of env * Omake_ir.string_exp
  | ValBody        of Omake_ir.exp list * Omake_ir.export
  | ValCases       of (Omake_ir.var * value * Omake_ir.exp list * Omake_ir.export) list

  (* Functions *)
  | ValFun         of env * keyword_param_value list * Omake_ir.param list * Omake_ir.exp list * Omake_ir.export
  | ValFunCurry    of env * param_value list * keyword_param_value list * Omake_ir.param list * Omake_ir.exp list * Omake_ir.export * keyword_value list

  (* Closed values *)
  | ValPrim        of Omake_ir.arity * bool * Omake_ir.apply_empty_strategy * prim_fun

  (* The args, kargs are kept in -reverse- order *)
  | ValPrimCurry   of Omake_ir.arity * bool * prim_fun * value list * keyword_value list

  (* Implicit value dependencies *)
  | ValMaybeApply  of Lm_location.loc * Omake_ir.var_info

  (* Variables that are not applications *)
  | ValVar         of Lm_location.loc * Omake_ir.var_info

  (* Other values *)
  | ValOther       of value_other

  (* Delayed values *)
  | ValDelayed     of value_delayed ref

(*
 * Put all the other stuff here, to keep the primary value type
 * smaller.
 *)
and value_other =
  | ValLexer       of Omake_lexer.Lexer.t
  | ValParser      of Omake_parser.Parser.t
  | ValLocation    of Lm_location.loc
  | ValExitCode    of int
  | ValEnv         of handle_env * Omake_ir.export

and value_delayed =
    ValValue of value

  (* Value in a static block *)
  | ValStaticApply of value * Omake_ir.var

(*
 * Arguments have an optional keyword.
 *)
and param_value = Omake_ir.param * value
and keyword_value = Omake_ir.var * value
and keyword_param_value = Omake_ir.var * Omake_ir.param * value option

(*
 * Primitives are option refs.
 * We do this so that we can marshal these values.
 * Just before marshaling, all the options are set to None.
 *)
and prim_fun = Lm_symbol.symbol

(*
 * An object is just an environment.
 *)
and obj = value Lm_symbol.SymbolTable.t
and env = value Lm_symbol.SymbolTable.t
and map = (value, value) Lm_map.tree
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
  | CommandSection of value * Omake_ir_free_vars.free_vars * Omake_ir.exp list
  (* Name of the section, its free variables, and the expression *)
  | CommandValue of Lm_location.loc * env * Omake_ir.string_exp

(*
 * Kinds of rules.
 *)
type rule_multiple =
   RuleSingle
 | RuleMultiple
 | RuleScannerSingle
 | RuleScannerMultiple

type rule_kind =
   RuleNormal
 | RuleScanner

(*
 * A target value that represents a node in a rule.
 *)
type target =
   TargetNode of Omake_node.Node.t
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
   Symbol        of Lm_symbol.symbol
 | String        of string
 | AstExp        of Omake_ast.exp
 | IrExp         of Omake_ir.exp
 | Location      of Lm_location.loc
 | Value         of value
 | Error         of omake_error

and pos = item Lm_position.pos

and omake_error =
  |  SyntaxError        of string
  | StringError        of string
  | StringAstError     of string * Omake_ast.exp
  | StringStringError  of string * string
  | StringDirError     of string * Omake_node.Dir.t
  | StringNodeError    of string * Omake_node.Node.t
  | StringVarError     of string * Omake_ir.var
  | StringIntError     of string * int
  | StringMethodError  of string * Omake_ir.var list
  | StringValueError   of string * value
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
  | UnboundValue       of value
  | NullCommand

(*
 * Standard exceptions.
 *)
exception OmakeException    of pos * omake_error
exception UncaughtException of pos * exn
exception RaiseException    of pos * obj
exception ExitException     of pos * int
exception ExitParentException     of pos * int
exception Return            of Lm_location.loc * value * Omake_ir.return_id

(*
 * Omake's internal version of the Invalid_argument
 *)
exception OmakeFatal of string
exception OmakeFatalErr of pos * omake_error

(*
 * Position printer.
 *)
module type PosSig =
sig
   val loc_exp_pos    : Lm_location.loc -> pos
   val loc_pos        : Lm_location.loc -> pos -> pos

   val ast_exp_pos    : Omake_ast.exp -> pos
   val ir_exp_pos     : Omake_ir.exp -> pos
   val var_exp_pos    : Omake_ir.var -> pos
   val string_exp_pos : string -> pos
   val value_exp_pos  : value -> pos

   val string_pos     : string -> pos -> pos
   val pos_pos        : pos -> pos -> pos
   val int_pos        : int -> pos -> pos
   val var_pos        : Omake_ir.var -> pos -> pos
   val error_pos      : omake_error -> pos -> pos

   val del_pos        : (Format.formatter -> unit) -> Lm_location.loc -> pos
   val del_exp_pos    : (Format.formatter -> unit) -> pos -> pos

   (* Utilities *)
   val loc_of_pos     : pos -> Lm_location.loc
   val pp_print_pos   : Format.formatter -> pos -> unit
end

(************************************************************************
 * Basic values and functions.
 *)

(*
 * Empty object.
 *)
let empty_obj = Lm_symbol.SymbolTable.empty

(*
 * Get the class identifiers from the object.
 *)
let class_sym = Lm_symbol.add "$class"

let venv_get_class obj =
   match Lm_symbol.SymbolTable.find obj class_sym with
   |   ValClass table -> table
   | _ -> Lm_symbol.SymbolTable.empty 
   | exception Not_found -> Lm_symbol.SymbolTable.empty 

(************************************************************************
 * Value table.
 *)
module ValueCompare =
struct
   type t = value

   (*
    * Check for simple values.
    * Arrays cannot be nested.
    *)
   let check_simple pos v =
      match v with
         ValNone
       | ValInt _
       | ValFloat _
       | ValData _
       | ValNode _
       | ValDir _
       | ValOther (ValLocation _)
       | ValOther (ValExitCode _)
       | ValVar _ ->
            ()
       | _ ->
            raise (OmakeException (pos, StringValueError ("illegal Map key", v)))

   let check pos v =
      (match v with
          ValArray vl ->
             List.iter (check_simple pos) vl
        | _ ->
             check_simple pos v);
      v

   (*
    * Compare two simple values.
    *)
   let tag = function
      ValNone                  -> 0
    | ValInt _                 -> 1
    | ValFloat _               -> 2
    | ValArray _               -> 3
    | ValData _                -> 4
    | ValNode _                -> 5
    | ValDir _                 -> 6
    | ValOther (ValExitCode _) -> 7
    | ValOther (ValLocation _) -> 8
    | ValVar _                 -> 9
    | _ ->
         raise (Invalid_argument "ValueCompare: value not supported")

   let rec compare v1 v2 =
      match v1, v2 with
         ValNone, ValNone ->
            0
       | ValInt i1, ValInt i2
       | ValOther (ValExitCode i1), ValOther (ValExitCode i2) ->
            if i1 < i2 then
               -1
            else if i1 > i2 then
               1
            else
               0
       | ValFloat x1, ValFloat x2 ->
            if x1 < x2 then
               -1
            else if x1 > x2 then
               1
            else
               0
       | ValArray a1, ValArray a2 ->
            compare_list a1 a2
       | ValData s1, ValData s2 ->
            Pervasives.compare s1 s2
       | ValNode node1, ValNode node2 ->
            Omake_node.Node.compare node1 node2
       | ValDir dir1, ValDir dir2 ->
            Omake_node.Dir.compare dir1 dir2
       | ValOther (ValLocation loc1), ValOther (ValLocation loc2) ->
            Lm_location.compare loc1 loc2
       | ValVar (_, v1), ValVar (_, v2) ->
            Omake_ir_util.VarInfoCompare.compare v1 v2
       | _ ->
            tag v1 - tag v2

   and compare_list l1 l2 =
      match l1, l2 with
         v1 :: l1, v2 :: l2 ->
            let cmp = compare v1 v2 in
               if cmp = 0 then
                  compare_list l1 l2
               else
                  cmp
       | [], [] ->
            0
       | [], _ :: _ ->
            -1
       | _ :: _, [] ->
            1
end;;

module ValueTable = Lm_map.LmMakeRec (ValueCompare);;

