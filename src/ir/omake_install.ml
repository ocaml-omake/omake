(*
 * Functions to install OMakefiles into a project.
 *)



(*
 * Copy a file into a directory, but prompt the user if
 * the file already exists.
 *)
let copy_file force src dst =
   let prompt () =
      Lm_printf.printf "%s already exists, overwrite (yes/no)? " dst;
      Lm_printf.flush Lm_printf.std_formatter;
      String.lowercase_ascii (input_line stdin) = "yes"
   in
      if force || not (Sys.file_exists dst) || prompt () then
         let () = Lm_printf.printf "*** omake: creating %s@." dst in
         let inx = open_in src in
         let outx = open_out dst in
         let rec copy () =
            output_char outx (input_char inx);
            copy ()
         in
         let () =
            try copy () with
               End_of_file ->
                  ()
         in
            close_in inx;
            close_out outx
      else
         Lm_printf.printf "*** omake: skipping %s@." dst

(*
 * Names of the standard files.
 *)
let omakeroot = Omake_node.Node.fullname 
    (Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty 
       Omake_node.Dir.lib "OMakeroot.default")

let omakefile = 
  Omake_node.Node.fullname
    (Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty 
       Omake_node.Dir.lib "OMakefile.default")

(*
 * Install just into the current directory.
 *)
let install_current force =
   copy_file force omakeroot "OMakeroot";
   copy_file force omakefile "OMakefile";
   Lm_printf.printf "*** omake: project files OMakefile and OMakeroot have been installed\n";
   Lm_printf.printf "*** omake: you should edit these files before continuing@."

(*
 * Install into all subdirectories.
 *)
let glob_cvs_ignore = Lm_glob.create_options [GlobCVSIgnore]

let install_subdirs force =
   let dirs = Lm_glob.subdirs_of_dirs glob_cvs_ignore "" ["."] in
      copy_file force omakeroot "OMakeroot";
      List.iter (fun dir -> copy_file force omakefile (Filename.concat dir "OMakefile")) dirs;
      Lm_printf.printf "*** omake: project files OMakefile and OMakeroot have been installed\n";
      Lm_printf.printf "*** omake: OMakefiles have been installed into all subdirectories\n";
      Lm_printf.printf "*** omake: you should edit these files before continuing@."

