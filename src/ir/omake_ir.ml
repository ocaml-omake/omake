(*
 * Define an intermediate representation that is a little
 * easier to work with than the AST.
 *)

(*
 * %%MAGICBEGIN%%
 * Last manual IR versioning: 12/09/07 by Aleksey Nogin
 *)
type var = Lm_symbol.symbol
type keyword = Lm_symbol.symbol
type curry_flag = bool

(*
 * Whether a function of zero arguments should be applied.
 *)
type apply_empty_strategy =
   ApplyEmpty
 | ApplyNonEmpty

(*
 * Arity of functions.
 *)
type arity =
   ArityRange of int * int
 | ArityExact of int
 | ArityNone
 | ArityAny

(*
 * Kinds of matches.
 *)
type match_kind =
   MatchWild
 | MatchRegex

(*
 * Variable definitions have several forms.
 *    VarDefNormal: normal definition
 *    VarDefAppend: append the text
 *)
type var_def_kind =
   VarDefNormal
 | VarDefAppend

(*
 * Simple version of variables includes the kind of
 * scope, the location, and the variable name.
 *)
type var_info =
   VarPrivate        of Lm_location.loc * var
 | VarThis           of Lm_location.loc * var
 | VarVirtual        of Lm_location.loc * var
 | VarGlobal         of Lm_location.loc * var

type param = var_info

(*
 * A symbol table maps variables to their info.
 *)
type senv = var_info Lm_symbol.SymbolTable.t

(*
 * Exporting.
 *)
type export_item =
   ExportRules
 | ExportPhonies
 | ExportVar of var_info

type export =
   ExportNone
 | ExportAll
 | ExportList of export_item list

(*
 * A return identifier is a unique id for the function to return from.
 * NOTE: this is a unique string, compared with pointer equality.
 *)
type return_id = Lm_location.loc * string

(*
 * Expression that results in a string.
 *
 * Functions: a function takes a triple:
 *    keyword_param list : the optional parameters
 *    keyword_set : the set of keywords (for checking against the keyword arguments)
 *    param list : the names of the required parameters
 *
 * The ordering of keyword arguments in the source is irrelevant.
 * Internally, we sort them by symbol name, for easy checking.
 *)
type string_exp =
   NoneString        of Lm_location.loc
 | IntString         of Lm_location.loc * int
 | FloatString       of Lm_location.loc * float
 | WhiteString       of Lm_location.loc * string
 | ConstString       of Lm_location.loc * string
 | FunString         of Lm_location.loc * keyword_param list * param list * exp list * export
 | ApplyString       of Lm_location.loc * var_info * string_exp list * keyword_arg list
 | SuperApplyString  of Lm_location.loc * var * var * string_exp list * keyword_arg list
 | MethodApplyString of Lm_location.loc * var_info * var list * string_exp list * keyword_arg list
 | SequenceString    of Lm_location.loc * string_exp list
 | ArrayString       of Lm_location.loc * string_exp list
 | ArrayOfString     of Lm_location.loc * string_exp
 | QuoteString       of Lm_location.loc * string_exp list
 | QuoteStringString of Lm_location.loc * char * string_exp list
 | ObjectString      of Lm_location.loc * exp list * export
 | BodyString        of Lm_location.loc * exp list * export
 | ExpString         of Lm_location.loc * exp list * export
 | CasesString       of Lm_location.loc * (var * string_exp * exp list * export) list
 | KeyApplyString    of Lm_location.loc * string
 | VarString         of Lm_location.loc * var_info
 | ThisString        of Lm_location.loc
 | LazyString        of Lm_location.loc * string_exp
 | LetVarString      of Lm_location.loc * var_info * string_exp * string_exp

and source_exp = Omake_node_sig.node_kind * string_exp

and source_table = string_exp Lm_symbol.SymbolTable.t

(*
 * Optional function arguments.
 *)
and keyword_param = var * param * string_exp option

(*
 * Arguments are a pair of normal arguments and keyword arguments.
 *)
and keyword_arg = var * string_exp

(*
 * Commands.
 *)
and rule_command =
   RuleSection of string_exp * exp
 | RuleString of string_exp

and exp =
   (* Definitions *)
   LetVarExp        of Lm_location.loc * var_info * var list * var_def_kind * string_exp
 | LetFunExp        of Lm_location.loc * var_info * var list * curry_flag * keyword_param list * param list * exp list * export
 | LetObjectExp     of Lm_location.loc * var_info * var list * string_exp * exp list * export
 | LetThisExp       of Lm_location.loc * string_exp
 | LetKeyExp        of Lm_location.loc * string * var_def_kind * string_exp

   (* Applications *)
 | ApplyExp         of Lm_location.loc * var_info * string_exp list * keyword_arg list
 | SuperApplyExp    of Lm_location.loc * var * var * string_exp list * keyword_arg list
 | MethodApplyExp   of Lm_location.loc * var_info * var list * string_exp list * keyword_arg list
 | KeyExp           of Lm_location.loc * string

   (* Sequences *)
 | SequenceExp      of Lm_location.loc * exp list
 | SectionExp       of Lm_location.loc * string_exp * exp list * export

   (* StaticExp (Lm_location.loc, filename, id, el) *)
 | StaticExp        of Lm_location.loc * Omake_node.Node.t * Lm_symbol.symbol * exp list

   (* Conditional *)
 | IfExp            of Lm_location.loc * (string_exp * exp list * export) list

   (* Shell command *)
 | ShellExp         of Lm_location.loc * string_exp

   (*
    * StringExp (loc, s)
    *    This is just an identity, evaluating to s
    * ReturnExp (loc, s)
    *    This is a control operation, branching to the innermost ReturnBodyExp
    * ReturnBodyExp (loc, e)
    *    Return to here.
    *)
 | StringExp        of Lm_location.loc * string_exp
 | ReturnExp        of Lm_location.loc * string_exp * return_id
 | ReturnBodyExp    of Lm_location.loc * exp list * return_id

   (*
    * LetOpenExp (loc, v, id, file, link)
    *    id    : the current object
    *    file  : name of the file/object to open
    *    link  : link information for the rest of the variables in scope.
    *)
 | OpenExp          of Lm_location.loc * Omake_node.Node.t list
 | IncludeExp       of Lm_location.loc * string_exp * string_exp list

   (* Return the current object *)
 | ReturnObjectExp  of Lm_location.loc * Lm_symbol.symbol list
 | ReturnSaveExp    of Lm_location.loc

(*
 * The IR stored in a file.
 *    ir_classnames   : class names of the file
 *    ir_vars         : variables defined by this file
 *    ir_exp          : the expression
 *)
type ir =
   { ir_classnames   : Lm_symbol.symbol list;
     ir_vars         : senv;
     ir_exp          : exp
   }
(* %%MAGICEND%% *)

(*
 * Variable classes.
 *    private: variables local to the file, statically scoped.
 *    this: object fields, dynamically scoped.
 *    virtual: file fields, dynamically scoped.
 *    global: search each of the scopes in order (ZZZ: 0.9.8 only)
 *)
type var_scope =
   VarScopePrivate
 | VarScopeThis
 | VarScopeVirtual
 | VarScopeGlobal

(************************************************************************
 * Simplified variables.
 *)
type simple_var_info = var_scope * var



(*  Path definitions. *)
type name_info =
   { name_static     : bool;
     name_curry      : bool;
     name_scope      : var_scope option
   }

type method_name =
  | NameEmpty   of name_info
  | NameMethod  of name_info * var * var list


