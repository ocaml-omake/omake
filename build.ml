(* This file is distributed under the terms and conditions of the GNU GPL
   (General Public License), as detailed in the file LICENSE.

   Copyright (C) 2016 by Gerd Stolpmann.
 *)

(* Bootstrap build driver for omake *)

(* You can override the commands ocaml, ocamlc, ocamlopt on the command-line,
   e.g. ocaml build.ml OCAML=myocaml
 *)

#warnings "-3";;
#load "str.cma";;
#load "unix.cma";;

open Printf
module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

let ocaml vars =
  try StrMap.find "OCAML" vars
  with Not_found -> "ocaml"

let ocamlc vars =
  try StrMap.find "OCAMLC" vars
  with Not_found -> "ocamlc"

let ocamlopt vars =
  try StrMap.find "OCAMLOPT" vars
  with Not_found -> "ocamlopt"

let rec restart f arg =
  try f arg
  with
    | Unix.Unix_error(Unix.EINTR,_,_) ->
        restart f arg

let rec fold_lines f number acc fh =
  match
    try
      let line = input_line fh in
      let n = String.length line in
      let line =
        if n > 0 && line.[n-1] = '\r' then String.sub line 0 (n-1) else line in
      Some line
    with End_of_file -> None
  with
    | Some line ->
        let acc' = f number line acc in
        fold_lines f (number+1) acc' fh
    | None ->
        acc

let with_command cmd parse =
  let ch = Unix.open_process_in cmd in
  try
    let data = parse ch in
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


let exec_command env prog args =
  try
    let cmd =
      String.concat
        " " (prog :: List.map Filename.quote (Array.to_list args)) in
    printf "%s\n%!" cmd;
    let env =
      Array.of_list
        (List.map (fun (n,v) -> n ^ "=" ^ v) env) in
    let env = Array.append env (Unix.environment()) in
    let args = Array.append [| prog |] args in
    let pid =
      Unix.create_process_env
        prog args env Unix.stdin Unix.stdout Unix.stderr in
    let _, st = Unix.waitpid [] pid in
    match st with
      | Unix.WEXITED 0 ->
          ()
      | Unix.WEXITED n ->
          failwith ("Command exited with error: " ^ cmd)
      | _ ->
          failwith ("Command exited with signal: " ^ cmd)
  with
    | Unix.Unix_error(code,_,name) ->
        let prefix =
          if name = "" then "" else name ^ ": " in
        raise (Sys_error(prefix ^ Unix.error_message code))

let copy src dest =
  (* only for small files... *)
  let f_src = open_in_bin src in
  let n = in_channel_length f_src in
  let data = Bytes.create n in
  really_input f_src data 0 n;
  close_in f_src;
  let f_dest = open_out_bin dest in
  output f_dest data 0 n;
  flush f_dest;
  close_out f_dest

let touch file =
  let f = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 file in
  close_out f


let is_dir dir =
  (* Sys.file_exists (Filename.concat dir ".") - doesn't work under Windows *)
  try
    let st = Unix.stat dir in
    Unit.(st.st_kind = S_DIR)
  with
    | Unix.Unix_error _ -> false


let rec find dir pattern =
  (* pattern: regexp of the files to match in this directory *)
  let re = Str.regexp pattern in
  let allfiles = Array.to_list (Sys.readdir dir) in
  let allpaths =
    List.map (Filename.concat dir) allfiles in
  let files =
    List.filter
      (fun n ->
         Str.string_match re n 0
      )
      allfiles in
  let paths =
    List.map (Filename.concat dir) files in
  let subdirs =
    List.filter is_dir allpaths in
  let subpaths =
    List.flatten
      (List.map (fun dir -> find dir pattern) subdirs) in
  paths @ subpaths


let safe_remove path =
  try Sys.remove path
  with Sys_error _ -> ()


let safe_remove_list l =
  List.iter safe_remove l


let system_re = Str.regexp "^system: \\(.*\\)$"

let get_system vars =
  let cmd =
    sprintf "%s -config" (ocamlc vars) in
  with_command
    cmd
    (fold_lines
       (fun i line acc ->
          if Str.string_match system_re line 0 then
            Str.matched_group 1 line
          else
            acc
       )
       1
       "unknown"
    )

let have_ocamlopt vars =
  let cmd = ocamlopt vars in
  Sys.command cmd = 0

let configure_bootstrap vars =
  copy "src/Makefile" "boot/Makefile";
  touch "boot/Makefile.dep";
  match get_system vars with
    | "mingw"
    | "mingw64" ->
        copy "mk/oscofig_mingw.mk" "boot/osconfig.mk"
    | "win32"
    | "win64" ->
        copy "mk/oscofig_msvc.mk" "boot/osconfig.mk"
    | _ ->
        copy "mk/osconfig_unix.mk" "boot/osconfig.mk"

let make self makedir vars target =
  let dir = Filename.dirname self in
  let args =
    StrMap.fold
      (fun n v acc -> (n ^ "=" ^ v) :: acc)
      vars
      [] in
  let args_a =
    Array.of_list
      (Filename.concat dir "make.ml" :: "-C" :: makedir :: target :: args) in
  exec_command [] (ocaml vars) args_a

let run_bootstrap self vars =
  configure_bootstrap vars;
  let vars =
    if have_ocamlopt vars then
      StrMap.add "PREFERRED" ".opt" vars
    else
      vars in
  make self "boot" vars "Makefile.dep";
  make self "boot" vars "omake"


let do_bootstrap self vars ty =
  match ty with
    | `Force ->
        run_bootstrap self vars;
        ("boot/omake", "src/main/omake")
    | `Disable ->
        ("omake", "omake")
    | `Auto ->
        if not (Sys.file_exists "boot/omake") then
          run_bootstrap self vars;
        ("boot/omake", "src/main/omake")

let run_omake omake env vars args =
  let args1 =
    StrMap.fold
      (fun n v acc -> (n ^ "=" ^ v) :: acc)
      vars
      [] in
  exec_command env omake (Array.of_list (args @ args1))

let do_action self vars action omake1 omake2 =
  let env =
    [ "OMAKEFLAGS", "";
      "OMAKEPATH", "lib"
    ] in
  match action with
    | `Build ->
        if not (Sys.file_exists ".preconfig") then
          failwith "Unconfigured. Run the configure script first!";
        touch ".config";
        run_omake
          omake1
          env
          vars
          [ "--dotomake"; ".omake"; "--force-dotomake"; "main" ];
        run_omake
          omake2
          env
          vars
          [ "--dotomake"; ".omake"; "--force-dotomake"; "all" ]
    | `Install ->
        run_omake
          omake2
          env
          vars
          [ "--dotomake"; ".omake"; "--force-dotomake"; "install" ]
    | `Clean ->
        safe_remove ".config";
        find "boot" ".*" |> safe_remove_list;
        find "."    ".*\\.omc" |> safe_remove_list;
        find "src"  ".*\\.cmi" |> safe_remove_list;
        find "src"  ".*\\.cmo" |> safe_remove_list;
        find "src"  ".*\\.cmx" |> safe_remove_list;
        find "src"  ".*\\.o" |> safe_remove_list;
        find "src"  ".*\\.cmxa" |> safe_remove_list;
        find "src"  ".*\\.a" |> safe_remove_list;
        find "src"  ".*\\.so" |> safe_remove_list;
        find "src"  ".*\\.dll" |> safe_remove_list;
        safe_remove "src/env/omake_ast_parse.mly";
        safe_remove "src/libmojave/lm_thread_core.ml";
        safe_remove "src/libmojave/lm_thread_pool.ml";
        safe_remove "src/magic/omake_magic.ml";
        safe_remove "src/shell/omake_shell_sys.ml";
        safe_remove ".omakedb";
        safe_remove ".omakedb.lock"


let set_re = Str.regexp "^\\([^=]+\\)=\\(.*\\)$"

let main() =
  let self = Sys.argv.(0) in
  let self = 
    if Filename.is_relative self then
      Filename.concat (Sys.getcwd()) self
    else
      self in
  let bootstrap = ref `Auto in
  let action = ref `Build in
  let vars = ref StrMap.empty in
  let setvar name value =
    vars := StrMap.add name value !vars in
  Arg.parse
    [ "-force-bootstrap", Arg.Unit (fun () -> bootstrap := `Force),
      "   do the bootstrap again";

      "-no-bootstrap", Arg.Unit (fun () -> bootstrap := `Disable),
      "   no bootstrap, instead use the omake from $PATH";

      "-auto-bootstrap", Arg.Unit (fun () -> bootstrap := `Auto),
      "   do the bootstrap if needed - this is the default";

      "-build", Arg.Unit (fun () -> action := `Build),
      "   action: build omake - this is the default";

      "-install", Arg.Unit (fun () -> action := `Install),
      "   action: install omake";

      "-clean", Arg.Unit (fun () -> action := `Clean),
      "   action: clean up";
    ]
    (fun s ->
       if Str.string_match set_re s 0 then
         let name = Str.matched_group 1 s in
         let value = Str.matched_group 2 s in
         setvar name value
       else
         raise(Arg.Bad ("Don't know what to do with: " ^ s))
    )
    "ocaml build.ml [options] (var=value) ...";

  let boot_omake, made_omake =
    if !action = `Clean then
      "", ""
    else
      do_bootstrap self !vars !bootstrap in
  do_action self !vars !action boot_omake made_omake


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
