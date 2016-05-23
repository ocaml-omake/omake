(* This file is distributed under the terms and conditions of the GNU GPL
   (General Public License), as detailed in the file LICENSE.

   Copyright (C) 2016 by Gerd Stolpmann.
 *)

(* Configuration script for omake *)

#warnings "-3";;
#load "str.cma";;

open Printf

let is_windows =
  Sys.os_type = "Win32"

let path =
  if is_windows then
    let re = Str.regexp ";" in
    Str.split re (Sys.getenv "PATH")
  else
    let re = Str.regexp ":" in
    Str.split re (Sys.getenv "PATH")

let find_in_path prog =
  let prog =
    if is_windows then prog ^ ".exe" else prog in
  List.find
    (fun p -> Sys.file_exists (Filename.concat p prog))
    path

let prefix = 
  try
    ref (Filename.dirname (find_in_path "ocamlc"))
  with
    | Not_found ->
        prerr_endline "ocamlc not found; aborting";
        exit 1

let gnu_re1 = Str.regexp "--\\([-A-Za-z0-9]+\\)=\\(.*\\)$"
let gnu_re2 = Str.regexp "--\\([-A-Za-z0-9]+\\)$"

let gnu_options argv =
  let argv = Array.to_list argv in
  let argv =
    List.mapi
      (fun i arg ->
         if i > 0 && Str.string_match gnu_re1 arg 0 then
           ["-" ^ Str.matched_group 1 arg; Str.matched_group 2 arg]
         else
           if i > 0 && Str.string_match gnu_re2 arg 0 then
             ["-" ^ Str.matched_group 1 arg ]
           else
             [arg]
      )
      argv in
  Array.of_list (List.flatten argv)


let main() =
  let no_readline = ref false in
  let no_ncurses = ref false in
  let no_fam = ref false in
  Arg.parse_argv
    (gnu_options Sys.argv)
    [ "-prefix", Arg.Set_string prefix,
      "<dir>    Install prefix";

      "-disable-readline", Arg.Set no_readline,
      "    Disable readline support";

      "-disable-ncurses", Arg.Set no_ncurses,
      "    Disable ncurses support";

      "-disable-fam", Arg.Set no_fam,
      "    Disable FAM support";
    ]
    (fun s ->
       raise(Arg.Bad ("Unexpected: " ^ s))
    )
    "usage: configure [options]";
  
  let f = open_out ".preconfig" in
  fprintf f "public.PREFIX = %s\n\n" !prefix;

  List.iter
    (fun (disabled, name) ->
       fprintf f "# %s: %s\n" name (if disabled then "disabled" else "auto");
       fprintf f "%spublic.%s = false\n\n" (if disabled then "" else "#") name;
    )
    [ !no_readline,   "READLINE_ENABLED";
      !no_ncurses,    "NCURSES_ENABLED";
      !no_fam,        "FAM_ENABLED";
    ];
  close_out f;

  printf "Wrote .preconfig\n%!";
  
  (try Sys.remove ".config"
   with Sys_error _ -> ()
  )


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
