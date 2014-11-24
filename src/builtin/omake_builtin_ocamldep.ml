(* Experimental builtins for accelerating ocamldep postprocessing *)

include Omake_pos.Make (struct let name = "Omake_builtin_ocamldep" end)
open Printf
open Omake_ir

(*
 * \begin{doc}
 * \fun{ocamldep-print-buildable-deps}
 *
 * \begin{verbatim}
 *    $(ocamldep-print-buildable-deps path, target, deps, enable_cmx)
 *        path : Dir array
 *        target : File
 *        deps : String sequence
 *        enable_cmx : Bool
 * \end{verbatim}
 *
 * Prints several lines to stdout with an OCaml dependency: If \verb+target+ 
 * has the suffix ".mli", the single line looks like
 *
 * \begin{verbatim}
 * target.cmi: module1.cmi module2.cmi ...
 * \end{verbatim}
 *
 * where the module names are from \verb+deps+. Only those cmi files of the
 * modules are printed that can be found in \verb+path+.
 *
 * If \verb+target+ has the suffix ".ml", the lines look like
 *
 * \begin{verbatim}
 * target.cmo: module1.cmi module2.cmi ...
 * target.cmx target.o: module1.cmi module2.cmi ...
 * \end{verbatim}
 *
 * and if \verb+enable_cmx+ is true, the block looks like
 *
 * \begin{verbatim}
 * target.cmo: module1.cmi module2.cmi ...
 * target.cmx target.o: module1.cmi module2.cmi ... module1.cmx module2.cmx ...
 * \end{verbatim}
 *
 * Only those cmi and cmx files are included as dependencies that are actually
 * buildable.
 *
 * Accesses the global \verb+EXT_OBJ+ for getting the ".o" suffix.
 * \end{doc}
 *)

let target_is_buildable cache venv pos node =
  try
    Omake_target.target_is_buildable cache venv pos node
  with
    Omake_value_type.RaiseException(_, obj) when Omake_env.venv_instanceof obj Omake_symbol.unbuildable_exception_sym ->
    false

let ocamldep_print_buildable_deps venv pos loc args =
  let cache = Omake_env.venv_cache venv in
  let rec search_in_path path name =
    match path with
      | dir :: path ->
           let name1 = String.uncapitalize name in
           let name2 = String.capitalize name in
           let node1 = Omake_env.venv_intern_cd venv PhonyOK dir name1 in
           if target_is_buildable cache venv pos node1 then
             Some(Omake_env.venv_nodename venv node1)
           else
             let node2 = Omake_env.venv_intern_cd venv PhonyOK dir name2 in
             if target_is_buildable cache venv pos node2 then
             Some(Omake_env.venv_nodename venv node2)
             else
               search_in_path path name
      | [] ->
           None in
  let accumulate_over_path path deps suffix =
    List.rev
      (List.fold_left
         (fun acc dep ->
            match search_in_path path (dep ^ suffix) with
              | Some node -> node :: acc
              | None -> acc
         )
         []
         deps
      ) in

  let pos = string_pos "ocamldep-print-buildable-deps" pos in
  let stdout_var = Omake_env.venv_find_var venv pos loc Omake_var.stdout_var in
  let stdout_prim, close_flag = 
    Omake_value.out_channel_of_any_value venv pos stdout_var in
  let stdout_fd = Omake_env.venv_find_channel venv pos stdout_prim in
  let ext_obj_val = Omake_builtin_util.get_sym venv pos loc "EXT_OBJ" in
  let ext_obj = Omake_value.string_of_value venv pos ext_obj_val in
  match args with
    | [path; target; deps; enable_cmx] ->
         let path = Omake_value.values_of_value venv pos path in
         let path = Omake_eval.path_of_values venv pos path "." in
         let path = List.flatten (List.map snd path) in
         let target = Omake_value.string_of_value venv pos target in
         let deps = Omake_value.strings_of_value venv pos deps in
         let enable_cmx = Omake_value.bool_of_value venv pos enable_cmx in

         let cmideps = accumulate_over_path path deps ".cmi" in
         let cmideps_str = 
           String.concat " " (List.map String.escaped cmideps) in
         if Filename.check_suffix target ".mli" then (
           let targetbase = Filename.chop_suffix target ".mli" in
           if cmideps <> [] then
             Lm_channel.output_string
               stdout_fd
               (sprintf
                  "%s.cmi: %s\n"
                  (String.escaped targetbase)
                  cmideps_str
               )
         )
         else
           if Filename.check_suffix target ".ml" then (
             let targetbase = Filename.chop_suffix target ".ml" in
             let targetbase_esc = String.escaped targetbase in
             let cmxdeps =
               if enable_cmx then
                 accumulate_over_path path deps ".cmx"
               else
                 [] in
             let alldeps = cmideps @ cmxdeps in
             let alldeps_str = 
               String.concat " " (List.map String.escaped alldeps) in
             if cmideps <> [] || cmxdeps <> [] then (
               if cmideps <> [] then
                 Lm_channel.output_string
                   stdout_fd
                   (sprintf
                      "%s.cmo: %s\n"
                      targetbase_esc
                      cmideps_str
                   );
               Lm_channel.output_string
                 stdout_fd
                 (sprintf
                    "%s.cmx %s.%s: %s\n"
                    targetbase_esc
                    targetbase_esc
                    ext_obj
                    alldeps_str
                 )
             )
           )
           else
             raise (Omake_value_type.OmakeException (loc_pos loc pos, StringError "illegal filename suffix"));
         if close_flag then
           Omake_env.venv_close_channel venv pos stdout_prim;
         Omake_value_type.ValNone

    | _ ->
         raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 4, List.length args)))



(* register *)

let () =
  let builtin_funs =
    [true, "ocamldep-print-buildable-deps", ocamldep_print_buildable_deps,
     ArityExact 4;
    ] in
  let builtin_info =
    { Omake_builtin_type.builtin_empty with
      builtin_funs = builtin_funs;
    }
  in
  Omake_builtin.register_builtin builtin_info
