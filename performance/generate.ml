(* Generate a performance test for omake

   The test involves the generation of DR * DC directories, the
   "directory matrix", and each directory contains MR * MC modules,
   the "module matrix". The module in row r and column c of the module
   matrix depends on all modules in the previous row of the same
   directory. The first row of modules in a directory depends on 
   all modules in the preceding row of directories.

   The difficulty for omake is the "iterative nature" of this test:
   omake can only find out the dependencies row by row, so the
   dynamic discovery of dependencies is the stressed feature here.

   The test setup permits a lot of parallelism for actually executing
   the rules: the modules in the same row can be compiled in parallel,
   as well as the directories in the same row.

   Every module includes a big comment, so that the size of the files
   is not super-small. This comment will stress the omake digestion
   function.
 *)

(* Flags: -package unix,xstrp4 -syntax camlp4o *)

open Printf

let dir_rows = ref 1
let dir_cols = ref 1
let mod_rows = ref 1
let mod_cols = ref 1
let comment_size = ref 5000


let count n =
  Array.to_list (Array.init n (fun k -> k+1))


let write_directory basedir dir_row dir_col =
  let dirname = sprintf "%s/dir_%d_%d" basedir dir_row dir_col in
  Unix.mkdir dirname 0o777;

  for row = 1 to !mod_rows do
    for col = 1 to !mod_cols do
      let deps =
        if row = 1 then
          if dir_row = 1 then
            []
          else
            List.flatten
              (List.map 
                 (fun k -> 
                    List.map
                      (fun j ->
                         sprintf "M_%d_%d_%d_%d.f()" (dir_row-1) j !mod_rows k
                      )
                      (count !dir_cols)
                 )
                 (count !mod_cols)
              )
        else
          List.map 
            (fun k -> sprintf "M_%d_%d_%d_%d.f()" dir_row dir_col (row-1) k)
            (count !mod_cols) in

      let deps =
        List.rev ("()" :: (List.rev deps)) in

      let str_deps = String.concat ";\n  " deps in
      let comment = String.make !comment_size 'X' in
      let mod_text = sprintf "(* %s *)
let f() =
  %s
" 
        comment
        str_deps in
      let f = open_out
                (sprintf "%s/m_%d_%d_%d_%d.ml" dirname
                         dir_row dir_col row col) in
      output_string f mod_text;
      close_out f
    done
  done;

  let includes_l =
    List.flatten
      (List.map
         (fun dr ->
            let l1 = count !dir_cols in
            List.map
              (fun dc ->
                 sprintf "../dir_%d_%d" dr dc
              )
              l1
         )
         (count (dir_row-1))
      ) in
  let includes =
    String.concat "\n    " includes_l in

  let files_l =
    List.flatten
      (List.map
         (fun mr ->
            let l1 = count !mod_cols in
            List.map
              (fun mc ->
                 sprintf "m_%d_%d_%d_%d" dir_row dir_col mr mc
              )
              l1
         )
         (count !mod_rows)
      ) in
  let files =
    String.concat "\n    " files_l in

  let libname = sprintf "lib_%d_%d" dir_row dir_col in

  let omakefile = sprintf "
OCAMLINCLUDES[] +=
    %s

FILES[] =
    %s

.DEFAULT: $(OCamlLibrary %s, $(FILES))

"
    includes
    files
    libname in

  let f = open_out (sprintf "%s/OMakefile" dirname) in
  output_string f omakefile;
  close_out f


let write basedir =
  for row = 1 to !dir_rows do
    for col = 1 to !dir_cols do
      write_directory basedir row col
    done
  done;

  let subdirs_l =
    List.flatten
      (List.map
         (fun dr ->
            let l1 = count !dir_cols in
            List.map
              (fun dc ->
                 sprintf "dir_%d_%d" dr dc
              )
              l1
         )
         (count !dir_rows)
      ) in
  let subdirs =
    String.concat " " subdirs_l in
  
  let omakefile = sprintf "
.SUBDIRS: %s
" subdirs in

  let f = open_out (sprintf "%s/OMakefile" basedir) in
  output_string f omakefile;
  close_out f;

  let omakeroot = sprintf "
open build/OCaml
DefineCommandVars()
.SUBDIRS: .
" in

  let f = open_out (sprintf "%s/OMakeroot" basedir) in
  output_string f omakeroot;
  close_out f


let () =
  let basedir = ref "." in
  Arg.parse
    [ "-dir-rows", Arg.Set_int dir_rows,
      "<n>  number of rows in the directory matrix";
      
      "-dir-cols", Arg.Set_int dir_cols,
      "<n>  number of columns in the directory matrix";

      "-mod-rows", Arg.Set_int mod_rows,
      "<n>  number of rows in the module matrix";
      
      "-mod-cols", Arg.Set_int mod_cols,
      "<n>  number of columns in the module matrix";

      "-n", Arg.Int (fun n -> dir_rows := n;
                              dir_cols := n;
                              mod_rows := n;
                              mod_cols := n),
      "<n>  set all of -dir-rows, -dir-cols, -mod-rows, -mod-cols to the same value";

      "-comment-size", Arg.Set_int comment_size,
      "<n>  size of the module comment";
    ]
    (fun d ->
       basedir := d
    )
    (sprintf "usage: %s [basedir]" (Filename.basename Sys.argv.(0)));

  write !basedir

