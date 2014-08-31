




module SimpleVarCompare =
struct
  type t = Omake_ir.simple_var_info

  let compare (s1, v1) (s2, v2) =
    match s1, s2 with
      Omake_ir.VarScopePrivate, Omake_ir.VarScopePrivate
    | VarScopeThis, VarScopeThis
    | VarScopeVirtual, VarScopeVirtual
    | VarScopeGlobal, VarScopeGlobal ->
      Lm_symbol.compare v1 v2
    | VarScopePrivate, VarScopeThis
    | VarScopePrivate, VarScopeVirtual
    | VarScopePrivate, VarScopeGlobal
    | VarScopeThis, VarScopeVirtual
    | VarScopeThis, VarScopeGlobal
    | VarScopeVirtual, VarScopeGlobal ->
      -1
    | VarScopeThis, VarScopePrivate
    | VarScopeVirtual, VarScopePrivate
    | VarScopeVirtual, VarScopeThis
    | VarScopeGlobal, VarScopePrivate
    | VarScopeGlobal, VarScopeThis
    | VarScopeGlobal, VarScopeVirtual ->
      1
end;;

module SimpleVarSet = Lm_set.LmMake (SimpleVarCompare);;
module SimpleVarTable = Lm_map.LmMake (SimpleVarCompare);;

(************************************************************************
 * Variable tables.  The const_flag and protected_flag are just
 * comments, and aren't part of the comparison.
 *)
module VarInfoCompare =
struct
  type t = Omake_ir.var_info

  let compare info1 info2 =
    match info1, info2 with
    |Omake_ir.VarPrivate   (_, v1), Omake_ir.VarPrivate   (_, v2)
    | VarThis (_, v1),      VarThis (_, v2)
    | VarVirtual (_, v1),   VarVirtual (_, v2)
    | VarGlobal (_, v1),    VarGlobal (_, v2) ->
      Lm_symbol.compare v1 v2
    | VarPrivate _,        VarThis _
    | VarPrivate _,        VarVirtual _
    | VarPrivate _,        VarGlobal _
    | VarThis _,           VarVirtual _
    | VarThis _,           VarGlobal _
    | VarVirtual _,        VarGlobal _ ->
      -1
    | VarThis _,           VarPrivate _
    | VarVirtual _,        VarPrivate _
    | VarVirtual _,        VarThis _
    | VarGlobal _,         VarPrivate _
    | VarGlobal _,         VarThis _
    | VarGlobal _,         VarVirtual _ ->
      1
end;;

module VarInfoSet = Lm_set.LmMake (VarInfoCompare);;
module VarInfoTable = Lm_map.LmMake (VarInfoCompare);;

let var_equal v1 v2 =
   VarInfoCompare.compare v1 v2 = 0

let var_of_var_info = function
   Omake_ir.VarPrivate (loc, v)
 | VarThis (loc, v)
 | VarVirtual (loc, v)
 | VarGlobal (loc, v) ->
      loc, v

let loc_of_exp e =
  match e with
  | Omake_ir.LetVarExp (loc, _, _, _, _)
  | KeyExp (loc, _)
  | LetKeyExp (loc, _, _, _)
  | LetFunExp (loc, _, _, _, _, _, _, _)
  | LetObjectExp (loc, _, _, _, _, _)
  | LetThisExp (loc, _)
  | ShellExp (loc, _)
  | IfExp (loc, _)
  | SequenceExp (loc, _)
  | SectionExp (loc, _, _, _)
  | OpenExp (loc, _)
  | IncludeExp (loc, _, _)
  | ApplyExp (loc, _, _, _)
  | SuperApplyExp (loc, _, _, _, _)
  | MethodApplyExp (loc, _, _, _, _)
  | StaticExp (loc, _, _, _)
  | ReturnBodyExp (loc, _, _)
  | StringExp (loc, _)
  | ReturnExp (loc, _, _)
  | ReturnObjectExp (loc, _)
  | ReturnSaveExp loc ->
    loc

