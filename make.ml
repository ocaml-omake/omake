(* This file is distributed under the terms and conditions of the GNU GPL
   (General Public License), as detailed in the file LICENSE.

   Copyright (C) 2016 by Gerd Stolpmann.
 *)

(* Run this as:

   ocaml make.ml <args>

   This is a little "make"-like utility. It is not fully implemented, and
   mainly intended to help building omake under Windows.
 *)

(* TODO:
    - globbing for sources
    - emulate cp, rm, ln, echo for Windows
    - suffix substitution
 *)

#warnings "-3";;
#load "str.cma";;
#load "unix.cma";;

open Printf

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

type exp_rule =
  { targets : string list;
    sources : string list;
    commands : string list;  (* reverse order *)
  }

type suffix_rule =
  { target_suffix : string;
    source_suffix : string;
    sfx_commands : string list;  (* reverse order *)
  }

type rule =
  | Explicit of exp_rule
  | DblColon of exp_rule
  | Suffix of suffix_rule

type rules =
  { exp_rules : exp_rule StrMap.t;             (* keyed by target *)
    dblcolon_rules : exp_rule list StrMap.t;   (* keyed by target; rev order *)
    suffix_rules : suffix_rule list;
    enabled_suffixes : StrSet.t;
    phonies : StrSet.t;
    default : string list;
  }

let empty_rules =
  { exp_rules = StrMap.empty;
    dblcolon_rules = StrMap.empty;
    suffix_rules = [];
    enabled_suffixes = StrSet.empty;
    phonies = StrSet.empty;
    default = [];
  }

type expr =
  | Concat of expr list
  | Literal of string
  | Variable of string
  | Shell of expr

type varvalue =
  | Expanded of string
  | Unexpanded of expr

type varmap = varvalue StrMap.t   (* keyed by var name *)

let debug_enabled = ref false

let debug s =
  if !debug_enabled then
    print_endline s

let catch_not_found  f arg ~on_result ~on_not_found =
  match
    try Some(f arg) with Not_found -> None
  with
    | Some r -> on_result r
    | None -> on_not_found()

let starts_with s u =
  let sl = String.length s in
  let ul = String.length u in
  sl >= ul && String.sub s 0 ul = u

let multi_index s k charlist =
  let l = String.length s in
  let rec search k =
    if k >= l then
      raise Not_found;
    let c = s.[k] in
    if List.mem c charlist then
      k
    else
      search (k+1) in
  search k

let read_data ch =
  (* textual data *)
  let buf = Buffer.create 1024 in
  try
    while true do
      let line = input_line ch in
      if Buffer.length buf > 0 then
        Buffer.add_char buf '\n';
      let n = String.length line in
      if n > 0 && line.[n-1] = '\r' then
        Buffer.add_substring buf line 0 (n-1)
      else
        Buffer.add_string buf line
    done;
    assert false
  with
    | End_of_file ->
        Buffer.contents buf

let ws_re = Str.regexp "[ \t\r\n]+"
let wildcard_re = Str.regexp "\\*\\(\\.[a-zA-Z0-9]+\\)"

let expand_command cmd =
  (* Windows shells do not expand wildcards, so we need to do it here.
     The following is super-primitive and only covers the form *.suffix.
     Quoting is not taken into account.
   *)
  if Sys.os_type = "Win32" then
    let words = Str.split ws_re cmd in
    if List.exists (fun w -> Str.string_match wildcard_re w 0) words then
      let words' =
        List.flatten
          (List.map
             (fun word ->
                if Str.string_match wildcard_re word 0 then
                  let suffix = Str.matched_group 1 word in
                  List.filter
                    (fun file -> Filename.check_suffix file suffix)
                    (Array.to_list(Sys.readdir "."))
                else
                  [word]
             )
             words
          ) in
      String.concat " " words'
    else
      cmd
  else
    cmd
  

let command_output cmd =
  let ch = Unix.open_process_in (expand_command cmd) in
  try
    let data = read_data ch in
    let status = Unix.close_process_in ch in
    match status with
      | Unix.WEXITED 0 ->
          data
      | Unix.WEXITED n ->
          failwith ("Command exited with error: " ^ cmd)
      | _ ->
          failwith ("Command exited with signal: " ^ cmd)
  with
    | Unix.Unix_error(code,_,name) ->
        ignore(Unix.close_process_in ch);
        let prefix =
          if name = "" then "" else name ^ ": " in
        raise (Sys_error(prefix ^ Unix.error_message code))
        
let rec eval_expr active env expr =
  match expr with
    | Concat l ->
        let l' = List.map (eval_expr active env) l in
        String.concat "" l'
    | Literal s ->
        s
    | Variable name ->
        if StrSet.mem name active then
          failwith ("Recursive variable: " ^ name);
        let value =
          try StrMap.find name env
          with Not_found ->
            try Expanded(Sys.getenv name)
            with Not_found ->
              Expanded "" in
        ( match value with
            | Expanded s -> s
            | Unexpanded expr -> eval_expr (StrSet.add name active) env expr
        )
    | Shell expr ->
        let cmd = eval_expr active env expr in
        command_output cmd


let rec parse_expr s =
  let l = String.length s in
  let literal_list k0 k1 =
    if k1 > k0 then
      [ Literal(String.sub s k0 (k1-k0)) ]
    else
      [] in
  let rec find_dollar delims onend acc k =
    let delimchars = List.map fst delims in
    catch_not_found
      (multi_index s k) ('$' :: delimchars)
      ~on_result:(
        fun d ->
          let c = s.[d] in
          if c = '$' then (
            if d+1 >= l then failwith "Bad expression: '$' at end";
            let litl = literal_list k d in
            let c = s.[d+1] in
            match c with
              | '$' ->
                  let acc' = [ Literal "$" ] @ litl @ acc in
                  find_dollar delims onend acc' (d+2)
              | '(' ->
                  let acc' = litl @ acc in
                  find_varend delims onend acc' ')' (d+2)
              | '{' ->
                  let acc' = litl @ acc in
                  find_varend delims onend acc' '}' (d+2)
              | _ ->
                  let name = String.make 1 c in
                  let acc' = [ Variable name ] @ litl @ acc in
                  find_dollar delims onend acc' (d+2)
          ) else
            let litl = literal_list k d in
            let f = List.assoc c delims in
            f (litl @ acc) (d+1)
      )
      ~on_not_found:(
        fun () -> 
          let acc' = literal_list k l @ acc in
          onend acc'
      )
  and find_varend old_delims old_onend acc cend k =
    catch_not_found
      (multi_index s k) [cend; ' '; '\t'; '('; '{'; '$']
      ~on_result:(
        fun e ->
          let c = s.[e] in
          if c = cend then
            let name = String.sub s k (e-k) in
            let acc' = [ Variable name ] @ acc in
            find_dollar old_delims old_onend acc' (e+1)
          else
            match c with
              | ' '
              | '\t' ->
                  let name = String.sub s k (e-k) in
                  if name = "shell" then
                    let new_delims =
                      [ cend, (fun subexpr p ->
                                 let acc' =
                                   Shell (Concat (List.rev subexpr)) :: acc in
                                 find_dollar old_delims old_onend acc' p
                              )
                      ] @ old_delims in
                    let new_onend _ =
                      failwith "Bad expression: unterminated shell command" in
                    find_dollar new_delims new_onend [] (e+1)
                  else
                    failwith ("Bad expression: unsupported function: " ^
                                name)
              | _ ->
Printf.eprintf "Char: %c\n%!" c;
                  failwith "Bad expression: variable ref contains bad char"
      )
      ~on_not_found:(fun () ->
                       failwith "Bad expression: unterminated variable ref"
                    )
  in
  find_dollar [] (fun l -> Concat(List.rev l)) [] 0

let expand env s =
  let expr = parse_expr s in
  eval_expr StrSet.empty env expr

let ws_re =
  Str.regexp "^[ \t\t\n]*$";;

let set_var_re =
  Str.regexp "^\\([^ +?=:]+\\)[ \t]*\\([:?+]?=\\)[ \t]*\\(.*\\)$"

let dblcolon_rule_re =
  Str.regexp "^\\([^:]+\\)::\\(.*\\)$"

let rule_re =
  Str.regexp "^\\([^:]+\\):\\(.*\\)$"

let include_re =
  Str.regexp "^include[ \t]+\\([^ \t].*\\)$"

let split_ws_re =
  Str.regexp "[ \t\r\n]+"


let rec fold_lines fh f number acc =
  match
    try Some(input_line fh)
    with End_of_file -> None
  with
    | Some line ->
        let acc' = f number line acc in
        fold_lines fh f (number+1) acc'
    | None ->
        acc


let update_env name op value env =
  match op with
    | "=" ->
        let expr = parse_expr value in
        StrMap.add name (Unexpanded expr) env
  | "?=" ->
      let expr = parse_expr value in
      if StrMap.mem name env then
        env
      else
        StrMap.add name (Unexpanded expr) env
  | "+=" ->
      let expr = parse_expr value in
      let old_value =
        match
          try StrMap.find name env
          with Not_found -> Unexpanded (Concat [])
        with
          | Unexpanded e -> e
          | Expanded s -> Literal s in
      let new_value =
        if old_value = Concat [] then
          expr
        else
          Concat [old_value; Literal " "; expr] in
      StrMap.add name (Unexpanded new_value) env
  | ":=" ->
      let expr = parse_expr value in
      let value = eval_expr StrSet.empty env expr in
      StrMap.add name (Expanded value) env
  | _ ->
      assert false

let is_empty line =
  Str.string_match ws_re line 0

let append_to_rule rule line =
  match rule with
    | Explicit r -> Explicit { r with commands = line :: r.commands }
    | DblColon r -> DblColon { r with commands = line :: r.commands }
    | Suffix r -> Suffix { r with sfx_commands = line :: r.sfx_commands }

let same_suffix_rule r1 r2 =
  r1.target_suffix = r2.target_suffix &&
    r1.source_suffix = r2.source_suffix

let enter_rule1 current env rules =
  match current with
    | None ->
        rules
    | Some (Explicit r) ->
        if r.commands = [] then
          List.fold_left
            (fun rules target ->
               if StrMap.mem target rules.dblcolon_rules then
                 failwith "Cannot have both normal and double-colon rules for \
                           the same target";
               if StrMap.mem target rules.exp_rules then
                 (* add further dependencies to existing explicit rule *)
                 let r1 = StrMap.find target rules.exp_rules in
                 let r2 = { r1 with sources = r1.sources @ r.sources } in
                 { rules with
                   exp_rules = StrMap.add target r2 rules.exp_rules
                 }
               else (
                 debug ("Enter " ^ target);
                 (* add command-less rule *)
                 { rules with
                   exp_rules = StrMap.add target r rules.exp_rules
                 }
               )
            )
            rules
            r.targets
        else (
          (* new explicit rule *)
          List.iter
            (fun target ->
               if StrMap.mem target rules.dblcolon_rules then
                 failwith "Cannot have both normal and double-colon rules for \
                           the same target";
               try
                 let r = StrMap.find target rules.exp_rules in
                 if r.commands <> [] then
                   failwith("Rule for target already exists: " ^ target);
               with Not_found -> ()
            )
            r.targets;
          List.fold_left
            (fun rules target ->
               debug ("Enter " ^ target);
               let r1 =
                 try
                   let r0 = StrMap.find target rules.exp_rules in
                   { r with sources = r0.sources @ r.sources }
                 with Not_found -> r in
               { rules with
                 exp_rules = StrMap.add target r1 rules.exp_rules
               }
            )
            rules
            r.targets
        )
    | Some (DblColon r) ->
        let target = List.hd r.targets in  (* only one target here *)
        if StrMap.mem target rules.exp_rules then
          failwith "Cannot have both normal and double-colon rules for the \
                    same target";
        ( try
            let l = StrMap.find target rules.dblcolon_rules in
            { rules with
              dblcolon_rules = StrMap.add target (r :: l) rules.dblcolon_rules
            }
          with
            | Not_found ->
                { rules with
                  dblcolon_rules = StrMap.add target [r] rules.dblcolon_rules
                }
        )
    | Some (Suffix r) ->
        if List.exists (same_suffix_rule r) rules.suffix_rules then
          failwith ("Suffix rule already exists: " ^ 
                      r.source_suffix ^ r.target_suffix);
        { rules with
          suffix_rules = r :: rules.suffix_rules
        }

let enter_rule current env rules =
  let rules = enter_rule1 current env rules in
  match current with
    | Some (Explicit r) when rules.default = [] ->
        { rules with
          default = r.targets
        }
    | _ ->
        rules


let is_suffix_comb suffixes word =
  StrSet.fold
    (fun source_suffix acc ->
       match acc with
         | None ->
             if starts_with word source_suffix then
               let l1 = String.length source_suffix in
               let l2 = String.length word in
               let target_suffix = String.sub word l1 (l2-l1) in
               if StrSet.mem target_suffix suffixes then
                 Some(target_suffix,source_suffix)
               else
                 None
             else
               None
         | Some _ ->
             acc
    )
    suffixes
    None

let match_rule text =
  if Str.string_match dblcolon_rule_re text 0 then
    let target_str = Str.matched_group 1 text in
    let source_str = Str.matched_group 2 text in
    Some(`DblColon, target_str, source_str)
  else
    if Str.string_match rule_re text 0 then
      let target_str = Str.matched_group 1 text in
      let source_str = Str.matched_group 2 text in
      Some(`Colon, target_str, source_str)
    else
      None

let process_rule env rules' ty target_str source_str =
  let target_words0 = Str.split split_ws_re target_str in
  (* not handled: glob expressions in sources *)
  ( match target_words0 with
      | [] ->
          failwith "Empty target"
      | [ ".PHONY" ] ->
          if ty = `DblColon then
            failwith ".PHONY not possible with double-colon rule";
          let source_words =
            Str.split split_ws_re (expand env source_str) in
          let phonies =
            List.fold_left
              (fun acc phony -> StrSet.add phony acc)
              rules'.phonies
              source_words in
          let rules'' = { rules' with phonies } in
          ([],None,env,rules'')
      | [ ".SUFFIXES" ] ->
          if ty = `DblColon then
            failwith ".SUFFIXES not possible with double-colon rule";
          let source_words =
            Str.split split_ws_re (expand env source_str) in
          let enabled_suffixes =
            List.fold_left
              (fun acc suffix -> StrSet.add suffix acc)
              rules'.enabled_suffixes
              source_words in
          let rules'' = { rules' with enabled_suffixes } in
          ([],None,env,rules'')
      | [ word ] when is_empty source_str ->
          ( match is_suffix_comb rules'.enabled_suffixes word
            with
              | Some(target_suffix, source_suffix) ->
                  if ty = `DblColon then
                    failwith "Suffix rule not possible with double-colon";
                  let r =
                    { target_suffix;
                      source_suffix;
                      sfx_commands = []
                    } in
                  ([],Some(Suffix r),env,rules')
              | None ->
                  let r =
                    { targets = Str.split
                                  split_ws_re
                                  (expand env word);
                      sources = [];
                      commands = []
                    } in
                  let rule =
                    match ty with
                      | `Colon -> Explicit r
                      | `DblColon -> DblColon r in
                  ([],Some rule,env,rules')
          )
      | _ ->
          let target_words =
            Str.split split_ws_re (expand env target_str) in
          let source_words =
            Str.split split_ws_re (expand env source_str) in
          if ty = `DblColon && List.length target_words > 1 then
            failwith "Only one target allowed for double-colon rule";
          let r =
            { targets = target_words;
              sources = source_words;
              commands = []
            } in
          let rule =
            match ty with
              | `Colon -> Explicit r
              | `DblColon -> DblColon r in
          ([],Some rule,env,rules')
  )

let rec parse_statements ?(exec = fun _ _ _ -> ()) env rules file =
  let fh = open_in file in
  let last_num = ref 0 in
  try
    let (preceding,current,env,rules) =
      fold_lines
        fh
        (fun number line (preceding,current,env,rules) ->
           last_num := number;
           let lline = String.length line in
           if preceding = [] && (is_empty line || line.[0] = '#') then
             (preceding,current,env,rules)
           else
             if line <> "" && line.[lline - 1] = '\\' then
               let data = String.sub line 0 (lline-1) in
               (data::preceding,current,env,rules)
             else
               let multiline =
                 String.concat "" (List.rev (line :: preceding)) in
               if multiline.[0] = '\t' then
                 match current with
                   | None ->
                       failwith "TAB found outside a rule definition"
                   | Some rule ->
                       let current' =
                         Some (append_to_rule rule multiline) in
                       ([],current',env,rules)
               else
                 let rules' =
                   enter_rule current env rules in
                 if Str.string_match set_var_re multiline 0 then
                   let name = Str.matched_group 1 multiline in
                   let op = Str.matched_group 2 multiline in
                   let value = Str.matched_group 3 multiline in
                   let env' = update_env name op value env in
                   ([],None,env',rules')
               else
                 match match_rule multiline with
                   | Some(ty, target_str, source_str) ->
                       process_rule env rules' ty target_str source_str
                   | None ->
                       if Str.string_match include_re multiline 0 then (
                         let target = Str.matched_group 1 multiline in
                         exec env rules target;
                         let (env',rules'') =
                           parse_statements ~exec env rules' target in
                         ([],None,env',rules'')
                       )
                       else
                         failwith "Cannot parse line"
        )
        1
        ([],None,env,rules) in
    if preceding <> [] then
      failwith "Last line ends with backslash";
    let rules =
      enter_rule current env rules in
    (env, rules)
  with
    | Failure msg ->
        close_in fh;
        failwith ("File " ^ file ^ ", line " ^ string_of_int !last_num ^ ": " ^ 
                    msg)
    | error ->
        close_in fh;
        raise error


let strip_spaces_re = Str.regexp "^[ \t]*\\(.*\\)$"


let run_command1 cmd =
  if is_empty cmd then
    ()
  else
    (* CHECK order: @- or -@ *)
    let no_echo, cmd1 =
      if cmd <> "" && cmd.[0] = '@' then
        true, String.sub cmd 1 (String.length cmd - 1)
      else
        false, cmd in
    let no_errcheck, cmd2 =
      if cmd1 <> "" && cmd1.[0] = '-' then
        true, String.sub cmd1 1 (String.length cmd1 - 1)
      else
        false, cmd1 in
    if not no_echo then
      print_endline cmd2;
    let code = Sys.command (expand_command cmd2) in
    if not no_errcheck && code <> 0 then
      failwith ("Command failed: " ^ cmd2)

               
let run_command s =
  if Str.string_match strip_spaces_re s 0 then
    let cmd = Str.matched_group 1 s in
    run_command1 cmd
  else
    run_command1 s


let exec made_targets env rules target =
  let rec make ancestors target =
    debug ("Make: " ^ target);
    if not (StrSet.mem target !made_targets) then (
      match search_rule ancestors target with
        | Some list ->
            List.iter
              (fun (r,local_env) ->
                 let env' =
                   List.fold_left
                     (fun acc (n,v) -> StrMap.add n v acc)
                     env
                     local_env in
                 let ancestors' =
                   StrSet.add target ancestors in
                 List.iter (make ancestors') r.sources;
                 let cmds1 = List.rev r.commands in
                 let cmds2 = List.map (expand env') cmds1 in
                 List.iter run_command cmds2;
                 List.iter
                   (fun tgt ->
                      made_targets := StrSet.add tgt !made_targets
                   )
                   r.targets
              )
              list
        | None ->
            if StrSet.mem target rules.phonies then
              failwith("No way to execute phony target: " ^ target);
            if not(Sys.file_exists target) then
              failwith("No way to make target: " ^ target)
    );
    debug ("Made: " ^ target);

  and search_rule ancestors target =
    if StrSet.mem target ancestors then
      None
    else
      try
        let r = StrMap.find target rules.exp_rules in
        if r.commands = [] then raise Not_found;
        let localenv =
          [ "@", (Expanded (String.concat " " r.targets));
            "<", (Expanded (String.concat " " r.sources));
          ] in
        debug ("  target: " ^ target ^ " = direct");
        Some [r, localenv]
      with
        | Not_found ->
            try
              (* Double-colon rules are always executed if r.sources=[].
                 Well, we always execute anyway, so this does not make a
                 difference.
               *)
              let rlist = StrMap.find target rules.dblcolon_rules in
              debug ("  target: " ^ target ^ " = double-colon");
              Some
                (List.map
                   (fun r ->
                      let localenv =
                        [ "@", (Expanded (String.concat " " r.targets));
                          "<", (Expanded (String.concat " " r.sources));
                        ] in
                      (r,localenv)
                   )
                   rlist
                )
            with
              | Not_found ->
                  search_implicit_rule ancestors target

  and search_implicit_rule ancestors target =
    debug ("  target: " ^ target ^ " = search");
    let r_opt =
      StrSet.fold
        (fun target_suffix rule_opt ->
           match rule_opt with
             | Some r -> Some r
             | None ->
                 if Filename.check_suffix target target_suffix then (
                   debug("  target_suffix: " ^ target_suffix);
                   let irules =
                     List.filter
                       (fun suffr ->
                          suffr.target_suffix = target_suffix
                       )
                       rules.suffix_rules in
                   if irules = [] then
                     None
                   else (
                     let target_base =
                       Filename.chop_suffix target target_suffix in
                     try
                       let ancestors' =
                         StrSet.add target ancestors in
                       let irule =
                         List.find
                           (fun suffr ->
                              debug("  source_suffix: " ^ suffr.source_suffix);
                              let source =
                                target_base ^ suffr.source_suffix in
                              search_rule ancestors' source <> None
                              || Sys.file_exists source
                           )
                           irules in
                       let source =
                         target_base ^ irule.source_suffix in
                       let deps =
                         try
                           let r = StrMap.find target rules.exp_rules in
                           r.sources
                         with
                           | Not_found -> [] in
                       Some
                         [ { targets = [target];
                             sources = source :: deps;
                             commands = irule.sfx_commands
                           },
                           [ "*", (Expanded target_base);
                             "@", (Expanded target);
                             "<", (Expanded source);
                           ]
                         ]
                     with Not_found ->
                       None
                   )
                 )
                 else
                   None
        )
        rules.enabled_suffixes
        None in
    r_opt in

  try
    make StrSet.empty target
  with
    | Failure msg ->
        let d = Sys.getcwd() in
        failwith ("*** Error in directory " ^ d ^ ": " ^ msg)


let set_re = Str.regexp "^\\([^=]+\\)=\\(.*\\)$"


let main() =
  let self = Sys.argv.(0) in
  let self = 
    if Filename.is_relative self then
      Filename.concat (Sys.getcwd()) self
    else
      self in
  let env = ref StrMap.empty in
  let targets = ref [] in
  let setvar name value =
    env := StrMap.add name (Expanded value) !env in
  Arg.parse
    [ "-C", Arg.String (fun s -> Sys.chdir s),
      "<dir>  Change to this directory";
      "-debug", Arg.Set debug_enabled,
      "   Enable debug logging";
    ]
    (fun s ->
       if Str.string_match set_re s 0 then
         let name = Str.matched_group 1 s in
         let value = Str.matched_group 2 s in
         setvar name value
       else
         targets := !targets @ [s]
    )
    "ocaml make.ml [options] (var=value | target) ...";

  setvar
    "MAKE"
    (sprintf "ocaml %s %s"
             self
             (if !debug_enabled then "-debug" else "")
    );
  
  let made_targets = ref StrSet.empty in
  let (env,rules) =
    parse_statements ~exec:(exec made_targets) !env empty_rules "Makefile" in
  debug("Suffixes: " ^ 
          String.concat "," (StrSet.elements rules.enabled_suffixes));
  if !targets = [] then
    targets := rules.default;
  List.iter
    (exec made_targets env rules)
    !targets


let () =
  try main()
  with
    | Failure msg
    | Arg.Bad msg
    | Sys_error msg ->
        flush stdout;
        prerr_endline msg;
        flush stderr;
        exit 1
