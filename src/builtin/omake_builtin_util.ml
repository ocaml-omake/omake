
module Pos = Omake_pos.Make (struct let name = "Omake_builtin" end)




 (*
 * Variable manipulations.
*)
let defined_sym venv pos loc s =
  let pos = Pos.string_pos "defined_sym" pos in
  let v, vl = Omake_build_util.parse_sym venv pos loc s in
  match vl with
    [] ->
    Omake_env.venv_defined venv v
  | _ ->
    Omake_eval.eval_defined_field venv pos loc v vl

let get_sym venv pos loc s =
  let pos = Pos.string_pos "get_sym" pos in
  let v, vl = Omake_build_util.parse_sym venv pos loc s in
  match vl with
  | [] ->
    Omake_env.venv_find_var venv pos loc v
  | _ ->
    snd (Omake_eval.eval_find_method venv pos loc v vl)

let add_sym venv pos loc s x =
  let pos = Pos.string_pos "add_sym" pos in
  let v, vl = Omake_build_util.parse_sym venv pos loc s in
  if vl <> [] then
    raise (Omake_value_type.OmakeException (Pos.loc_pos loc pos, StringError "name has too many components"));
  Omake_env.venv_add_var venv v x


 (*
 * Fold its in a sequence, and place separators between them.
*)
let sequence_map f sl =
  let white = Omake_value_type.ValString " " in
  let rec collect seq sl =
    match sl with
      s :: sl ->
      let s = f s in
      let seq =
        if seq = [] then
          [s]
        else
          s :: white :: seq
      in
      collect seq sl
    | [] ->
      List.rev seq
  in
  collect [] sl

 (*
 * Add separators to a list.
*)
let sequence_list sl =
  let white = Omake_value_type.ValString " " in
  let rec collect sl =
    match sl with
      [s] ->
      [s]
    | s :: sl ->
      s :: white :: collect sl
    | [] ->
      []
  in
  collect sl

 (*
 * Default Boolean values.
*)
let val_true  = Omake_value_type.ValData "true"
let val_false = Omake_value_type.ValData "false"

let val_of_bool b =
  if b then val_true else val_false




(*
 * Extend an object with another.
 * The argument may be a file or an object.
*)
  let object_of_file venv pos loc s : Omake_value_type.obj =
  let pos  = Pos.string_pos "extends" pos in
  let node = Omake_eval.find_include_file venv pos loc s in
    try Omake_env.venv_find_object_file_exn venv node with
    Not_found ->
    let obj = Omake_eval.eval_object_file venv pos loc node in
    Omake_env.venv_add_object_file venv node obj;
obj
