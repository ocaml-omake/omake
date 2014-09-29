(*
 * Glob expansion.
 *
 * There is a dilemma here for Win32.
 * Since \ is the pathname separator, we will
 * often see names like "dir\*.c".  This is not
 * and escape sequence in the DOS shell, and in
 * fact * is not a valid character in Win32
 * filenames.
 *
 * So, we could turn off escape sequences in Win32,
 * but doing this globally is a bad idea.  For example,
 * what about the sequence dir\[a-z]*.c?  In this case,
 * the [ and ] characters _are_ valid in Win32 filenames.
 *
 * For now, we punt.  The \ character is considered to
 * be an escape character.  If you want globbing in
 * Win32, use forward slashes.  The result will still
 * use backslashes, so no worries.
 *
 *)




(************************************************************************
 * Tilde expansion.
*)

(*
 * Keep a table of entries.
 * Whenever we look a value in the passwd file,
 * add it to the table.
 *)
let tilde_table = ref [||]

(*
 * Keep the table sorted for quick lookup.
 *)
let tilde_insert (dir : string) (name : string) =
  let table = !tilde_table in
  let len = Array.length table in
  if len = 0 then
    tilde_table := [|dir, name|]
  else
    (* Binary search *)
    let rec search i j (dir : string) table =
      if i < j - 1 then
        let k = (i + j) / 2 in
        let dir', _ = table.(k) in
        if dir' > dir then
          search i k dir table 
        else
          search k j dir table
      else
        i
    in
    let i = search (-1) len dir table in
    if i >= 0 && fst table.(i) = dir then
      (if snd table.(i) <> "" then
         table.(i) <- dir, name)
    else
      let i = succ i in
      let ntable = Array.create (len + 1) table.(0) in
      Array.blit table 0 ntable 0 i;
      ntable.(i) <- dir, name;
      Array.blit table i ntable (i + 1) (len - i);
      tilde_table := ntable

(*
 * Find an entry in the table.
 *)
let rec tilde_matches dir1 dir2 len i =
  i = len || dir1.[i] = dir2.[i] && tilde_matches dir1 dir2 len (succ i)

let tilde_collapse dir =
  let table = !tilde_table in
  let len = Array.length table in
  let rec search i j =
    if i < j - 1 then
      let k = (i + j) / 2 in
      let dir', _ = table.(k) in
      if dir' > dir then
        search i k
      else
        search k j
    else
      i
  in
  let i = search (-1) len in
  if i < 0 then
    dir
  else
    let dir', name = table.(i) in
    let len' = String.length dir' in
    let len = String.length dir in
    if len' <= len && tilde_matches dir' dir len' 0 then
      let namelen = String.length name in
      let length = len - len' + namelen + 1 in
      let s = String.make length ' ' in
      s.[0] <- '~';
      String.blit name 0 s 1 namelen;
      String.blit dir len' s (namelen + 1) (len - len');
      s
    else
      dir

(*
 * Here is the caching getpwnam.
 *)
let getpwnam user =
  let passwd = Unix.getpwnam user in
  let dir = passwd.Unix.pw_dir in
  tilde_insert dir user;
  dir

let gethomedir = getpwnam

(*
 * Try to figure out the home directory as best as possible.
 *)
let home_dir =
  let home = Lm_unix_util.home_dir in
  tilde_insert home "";
  home

(*
 * Get a list of all the users.
 *)
let getusers () =
  let users = Lm_unix_util.getpwents () in
  List.map (fun entry ->
      let { Unix.pw_name = name;
            Unix.pw_dir  = dir;
            _
          } = entry
      in
      tilde_insert dir name;
      name) users

(************************************************************************
 * Glob expansion.
*)
type glob_option =
    GlobNoBraces                       (* Do not perform csh-style brace expansion *)
  | GlobNoTilde                        (* Do not perform tilde-expansion *)
  | GlobNoEscape                       (* The \ character does not escape special characters *)
  | GlobNoCheck                        (* If an expansion fails, return the expansion literally *)
  | GlobIgnoreCheck                    (* If an expansion fails, it expands to nothing *)
  | GlobDot                            (* Allow wildcards to match filenames with a leading . *)
  | GlobOnlyFiles                      (* Return only non-directories in the result *)
  | GlobOnlyDirs                       (* Return only directories in the result *)
  | GlobCVSIgnore                      (* Ignore files as specified by .cvsignore files *)
  | GlobIgnore of string list          (* Ignore the files that match the pattern *)
  | GlobAllow of string list           (* Allow only files that match the pattern *)
  | GlobIgnoreFun of (string -> bool)  (* Ignore the files specified by the function *)
  | GlobAllowFun of (string -> bool)   (* Allow only the files specified by the function *)
  | GlobHomeDir of string              (* Home directory for ~ expansion *)
  | GlobProperSubdirs                  (* Include only proper subdirs in listing *)

type glob_check =
  | NoMatchError
  | NoMatchPreserve
  | NoMatchIgnore

type glob_options =
  { glob_braces    : bool;
    glob_tilde     : bool;
    glob_escape    : bool;
    glob_check     : glob_check;
    glob_dot       : bool;
    glob_files     : bool;
    glob_dirs      : bool;
    glob_cvs       : bool;
    glob_ignore    : (string -> bool);
    glob_allow     : (string -> bool);
    glob_cvsignore : (string -> bool);
    glob_home      : string;
    glob_proper    : bool
  }

let default_glob_options =
  { glob_braces    = true;
    glob_tilde     = true;
    glob_escape    = true;
    glob_check     = NoMatchError;
    glob_dot       = false;
    glob_files     = false;
    glob_dirs      = false;
    glob_cvs       = false;
    glob_ignore    = (fun _ -> false);
    glob_allow     = (fun _ -> true);
    glob_cvsignore = (fun _ -> false);
    glob_home      = home_dir;
    glob_proper    = false
  }

(************************************************************************
 * Utilities.
*)

(*
 * Determine if a string contains glob characters.
 *)
let is_glob_string options name =
  let len = String.length name in
  let rec search lbrack i =
    if i >= len then
      false
    else
      match name.[i] with
        '*' | '?' ->
        true
      | '~' when i = 0 ->
        true
      | '[' ->
        search true (succ i)
      | ']' ->
        lbrack || search lbrack (succ i)
      | '\\' when options.glob_escape ->
        search lbrack (i + 2)
      | _ ->
        search lbrack (succ i)
  in
  search false 0

let glob_add_escaped options buf s =
  let len = String.length s in
  let rec collect i =
    if i < len then
      let c = String.unsafe_get s i in
      match c with
        '*' | '?' | '[' | ']' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        collect (succ i)
      | '~' when i = 0 ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        collect (succ i)
      | '{' | '}' when options.glob_braces ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        collect (succ i)
      | '\\' when options.glob_escape ->
        Buffer.add_char buf '\\';
        if i < len - 1 then begin
          Buffer.add_char buf s.[i + 1];
          collect (i + 2)
        end
      | c ->
        Buffer.add_char buf c;
        collect (succ i)
  in
  collect 0

(*
 * Unescape a name.
 *)
let unescape options s =
  if options.glob_escape then
    let len = String.length s in
    let buf = Buffer.create len in
    let rec collect i =
      if i = len then
        Buffer.contents buf
      else
        let c = s.[i] in
        if c = '\\' && i < len - 1 then
          let c = s.[i + 1] in
          match c with
            '*' | '?' | '[' | ']' | '~' | '{' | '}' ->
            Buffer.add_char buf c;
            collect (i + 2)
          | _ ->
            Buffer.add_char buf '\\';
            collect (i + 1)
        else
          begin
            Buffer.add_char buf c;
            collect (i + 1)
          end
    in
    collect 0
  else
    s

(*
 * Don't add unnecessary separators.
 *)
let filename_concat dir name =
  match dir, name with
    "", _ ->
    name
  | _, "" ->
    dir
  | _ ->
    Filename.concat dir name

(*
 * Split the path into root part, and the rest.
 * If escaping is enabled, do not split at escape sequences,
 * but split everywhere else.
 *)
let filename_split options s =
  let len = String.length s in
  let add_name names start i =
    if start < i then
      String.sub s start (i - start) :: names
    else
      names
  in
  let rec collect names start i =
    if i = len then
      add_name names start i
    else
      let c = s.[i] in
      match c with
        '/' ->
        collect (add_name names start i) (succ i) (succ i)
      | '\\' ->
        if options.glob_escape && i < len - 1 then
          let c = s.[i + 1] in
          match c with
            '*' | '?' | '[' | ']' | '~' ->
            collect names start (i + 2)
          | _ ->
            collect (add_name names start i) (succ i) (succ i)
        else
          collect (add_name names start i) (succ i) (succ i)
      | _ ->
        collect names start (succ i)
  in
  let names = collect [] 0 0 in
  List.rev names

(*
 * Split the rest into parts.
 *)
let filename_path options name : string list Lm_filename_util.path =
  match Lm_filename_util.filename_string name with
    AbsolutePath (root, path) ->
    Lm_filename_util.AbsolutePath (root, filename_split options path)
  | RelativePath path ->
    RelativePath (filename_split options path)

(************************************************************************
 * Shell regular expressions.
 *
*)
let add_shell_pattern options buf s =
  let len = String.length s in
  let rec collect i =
    if i >= len then
      Buffer.add_char buf '$'
    else
      let c = s.[i] in
      match s.[i] with
        '*' ->
        Buffer.add_string buf ".*";
        collect (succ i)
      | '?' ->
        Buffer.add_string buf ".";
        collect (succ i)
      | '.'
      | '+'
      | '^'
      | '$'
      | '|'
      | '('
      | ')'
      | '{'
      | '}' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        collect (succ i)
      | '\\' ->
        if options.glob_escape && i < len - 1 then
          let c = s.[i + 1] in
          match c with
            '*' | '?' | '[' | ']' | '~' ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf c;
            collect (i + 2)
          | _ ->
            Buffer.add_string buf "\\\\";
            collect (succ i)
        else
          begin
            Buffer.add_string buf "\\\\";
            collect (succ i)
          end
      | _ ->
        Buffer.add_char buf c;
        collect (succ i)
  in
  collect 0

let add_shell_disjunct options buf s =
  Buffer.add_string buf "|";
  add_shell_pattern options buf s

let regexp_of_shell_pattern options s =
  let buf = Buffer.create 32 in
  add_shell_pattern options buf s;
  Lm_lexer.LmStr.regexp (Buffer.contents buf)

let make_filter options sl default =
  let buf = Buffer.create 32 in
  match sl with
    s :: sl ->
    add_shell_pattern options buf s;
    List.iter (add_shell_disjunct options buf) sl;
    let pattern = Lm_lexer.LmStr.regexp (Buffer.contents buf) in
    (fun name -> Lm_lexer.LmStr.string_match pattern name 0)
  | [] ->
    (fun _ -> default)

(*
 * These are the files that CVS ignores by default.
 * https://www.cvshome.org/docs/manual/cvs-1.11.16/cvs_18.html#IDX266
 *)
let default_patterns =
  ["RCS";
   "SCCS";
   "CVS";
   "CVS.adm";
   "RCSLOG";
   "cvslog.*";
   "tags";
   "TAGS";
   ".make.state";
   ".nse_depinfo";
   ".svn";
   "*~";
   "#*";
   ".#*";
   ",*";
   "_$*";
   "*$";
   "*.old";
   "*.bak";
   "*.BAK";
   "*.orig";
   "*.rej";
   ".del-*";
   "*.a";
   "*.olb";
   "*.o";
   "*.obj";
   "*.so";
   "*.exe";
   "*.Z";
   "*.elc";
   "*.ln";
   "core.*"]

let stdignore =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "^\\.cvsignore$";
  List.iter (add_shell_disjunct default_glob_options buf) default_patterns;
  Lm_lexer.LmStr.regexp (Buffer.contents buf)

(*
 * Load the ignore expression from .cvsignore.
 *)
let load_cvsignore dirname =
  let filename = filename_concat dirname ".cvsignore" in

  (* Get the patterns from the file *)
  let inx = open_in filename in
  let rec collect patterns =
    try collect (Lm_string_util.tokens_std (input_line inx) @ patterns) with
      End_of_file ->
      patterns
  in
  let patterns = collect [] in
  let () = close_in inx in

  (* Concatenate them into a large regular expression *)
  let buf = Buffer.create 256 in
  Buffer.add_string buf "^\\.cvsignore$";
  List.iter (add_shell_disjunct default_glob_options buf) default_patterns;
  List.iter (add_shell_disjunct default_glob_options buf) patterns;
  Lm_lexer.LmStr.regexp (Buffer.contents buf)

let load_cvsignore dirname =
  let pattern =
    try load_cvsignore dirname with
      Sys_error _ ->
      stdignore
  in
  (fun name -> Lm_lexer.LmStr.string_match pattern name 0)

(*
 * Check if a filename refers to a directory.
 *)
let is_dir filename =
  try (Unix.lstat filename).Unix.st_kind = Unix.S_DIR with
    Unix.Unix_error _ ->
    false

(************************************************************************
 * Globbing.
*)

(*
 * Collect glob options.
 *)
let create_options l =
  let rec collect options l =
    match l with
      option :: l ->
      let options =
        match option with
          GlobNoBraces    -> { options with glob_braces = false }
        | GlobNoTilde     -> { options with glob_tilde = false }
        | GlobNoEscape    -> { options with glob_escape = false }
        | GlobNoCheck     -> { options with glob_check = NoMatchPreserve }
        | GlobIgnoreCheck -> { options with glob_check = NoMatchIgnore }
        | GlobDot         -> { options with glob_dot = true }
        | GlobOnlyFiles   -> { options with glob_files = true }
        | GlobOnlyDirs    -> { options with glob_dirs = true }
        | GlobCVSIgnore   -> { options with glob_cvs = true }
        | GlobIgnoreFun f -> { options with glob_ignore = f }
        | GlobAllowFun f  -> { options with glob_allow = f }
        | GlobIgnore sl   -> { options with glob_ignore = make_filter options sl false }
        | GlobAllow sl    -> { options with glob_allow = make_filter options sl true }
        | GlobHomeDir dir -> { options with glob_home = dir }
        | GlobProperSubdirs -> { options with glob_proper = true }
      in
      collect options l
    | [] ->
      options
  in
  collect default_glob_options l

(*
 * Perform brace expansion.
 *)
let rec expand_braces options expanded_names unexpanded_names =
  match unexpanded_names with
    name :: unexpanded_names ->
    let len = String.length name in
    let expanded_names, unexpanded_names =
      (* Search for the first brace *)
      let rec search_brace i =
        if i >= len then
          name :: expanded_names, unexpanded_names
        else
          match name.[i] with
            '\\' when options.glob_escape ->
            search_brace (i + 2)
          | '{' ->
            search_found 0 i (i + 1) [] (i + 1)
          | _ ->
            search_brace (i + 1)

      (* Found a brace, search for the parts *)
      and search_found level start last names i =
        if i >= len then
          raise (Failure (name ^ ": brace mismatch"));

        match name.[i] with
          '\\' when options.glob_escape ->
          search_found level start last names (i + 2)
        | ',' when level = 0 ->
          let name = String.sub name last (i - last) in
          search_found level start (i + 1) (name :: names) (i + 1)
        | '{' ->
          search_found (succ level) start last names (i + 1)
        | '}' when level = 0 ->
          let pref = String.sub name 0 start in
          let suf = String.sub name (i + 1) (len - i - 1) in
          let name = String.sub name last (i - last) in
          let names = name :: names in
          let names = List.map (fun s -> pref ^ s ^ suf) names in
          expanded_names, List.append names unexpanded_names
        | '}' ->
          search_found (pred level) start last names (i + 1)
        | _ ->
          search_found level start last names (i + 1)
      in
      search_brace 0
    in
    expand_braces options expanded_names unexpanded_names
  | [] ->
    expanded_names

let glob_braces options names =
  if options.glob_braces then
    expand_braces options [] (List.rev names)
  else
    names

(*
 * Expand a glob pattern.
 * The dir is a fully-expanded directory name.
 *)
let glob_dir_pattern options root dirs names dir pattern =
  let options =
    if options.glob_cvs then
      { options with glob_cvsignore = load_cvsignore dir }
    else
      options
  in
  let root_dir = filename_concat root dir in
  let dirx = Unix.opendir root_dir in
  let rec collect dirs names =
    let name =
      try Some (Unix.readdir dirx) with
        End_of_file ->
        None
    in
    match name with
      None ->
      dirs, names
    | Some ""
    | Some "."
    | Some ".." ->
      collect dirs names
    | Some name ->
      let root_name = filename_concat root_dir name in
      let file_name = filename_concat dir name in
      let dir_flag = is_dir root_name in
      let dirs, names =
        if (options.glob_dot || name.[0] <> '.')
        && (not options.glob_files || not dir_flag)
        && (not options.glob_dirs || dir_flag)
        && Lm_lexer.LmStr.string_match pattern name 0
        && not (options.glob_ignore name)
        && (options.glob_allow name)
        && not (options.glob_cvsignore name)
        then
          if dir_flag then
            file_name :: dirs, names
          else
            dirs, file_name :: names
        else
          dirs, names
      in
      collect dirs names
  in
  let dirs_names = collect dirs names in
  Unix.closedir dirx;
  dirs_names

let glob_dirs_pattern options root dirs pattern =
  let rec collect dirs' names' dirs =
    match dirs with
      dir :: dirs ->
      let dirs', names' = glob_dir_pattern options root dirs' names' dir pattern in
      collect dirs' names' dirs
    | [] ->
      dirs', names'
  in
  collect [] [] dirs

let glob_dirs_name options root dirs name =
  if is_glob_string options name then
    let options =
      if name <> "" && name.[0] = '.' then
        { options with glob_dot = true }
      else
        options
    in
    let pattern = regexp_of_shell_pattern options name in
    glob_dirs_pattern options root dirs pattern
  else
    let name = unescape options name in
    List.fold_left (fun (dirs, names) dir ->
        let root_dir  = filename_concat root dir in
        let root_name = filename_concat root_dir name in
        let file_name = filename_concat dir name in
        try
          let stat = Unix.LargeFile.stat root_name in
          if stat.Unix.LargeFile.st_kind = Unix.S_DIR then
            file_name :: dirs, names
          else
            dirs, file_name :: names
        with
          Unix.Unix_error _ ->
          dirs, names) ([], []) dirs

(*
 * Perform tilde expansion.
 *)
let null_root = ""

let glob_tilde options root dir path =
  if options.glob_tilde then
    match path with
      name :: rest ->
      let len = String.length name in
      if len > 0 && name.[0] = '~' then
        if len = 1 then
          null_root, options.glob_home, rest
        else
          let user = String.sub name 1 (len - 1) in
          let dir =
            try getpwnam user with
              Not_found ->
              raise (Failure ("Unknown user: " ^ user))
          in
          null_root, dir, rest
      else if len > 1 && name.[0] = '\\' && name.[1] = '~' then
        root, dir, String.sub name 1 (len - 1) :: rest
      else
        root, dir, path
    | [] ->
      root, dir, path
  else
    root, dir, path

(*
 * Perform a glob expansion on a single path.
 *)
let glob_match options root dir name =
  (* Split the path into components *)
  let root, dir, path =
    match filename_path options name with
      RelativePath path ->
      root, dir, path
    | AbsolutePath (root, path) ->
      null_root, Lm_filename_util.string_of_root root, path
  in

  (* Do ~ expansion *)
  let root, dir, path = glob_tilde options root dir path in

  (* Walk through the path *)
  let rec glob dirs path =
    match path with
      [] ->
      dirs, []
    | [name] ->
      glob_dirs_name options root dirs name
    | name :: path ->
      let options = { options with glob_dirs = true } in
      let dirs, _ = glob_dirs_name options root dirs name in
      glob dirs path
  in
  glob [dir] path

(*
 * Don't glob-expand unless it is a glob pattern.
 *)
let glob_name options root dir name =
  if is_glob_string options name then
    let dirs, names = glob_match options root dir name in
    if dirs = [] && names = [] then
      match options.glob_check with
        NoMatchError ->
        raise (Failure (name ^ ": bad match"))
      | NoMatchPreserve ->
        [], [name]
      | NoMatchIgnore ->
        [], []
    else
      dirs, names
  else if Filename.is_relative name then
    let name      = unescape options name in
    let root_dir  = filename_concat root dir in
    let root_name = filename_concat root_dir name in
    let file_name = filename_concat dir name in
    if is_dir root_name then
      [file_name], []
    else
      [], [file_name]
  else
    let file_name = unescape options name in
    if is_dir file_name then
      [file_name], []
    else
      [], [file_name]

(*
 * Perform the actual glob.
 *)
let glob options dir names =
  let names = glob_braces options names in
  List.fold_left (fun (dirs, names) name ->
      let dirs', names' = glob_name options dir "" name in
      let dirs = List.rev_append dirs' dirs in
      let names = List.rev_append names' names in
      dirs, names) ([], []) names

(*
 * Don't glob-expand unless it is a glob pattern.
 * For argv expansion, we don't care about what is a directory
 * and what is not.
 *)
let glob_argv_name options root dir name =
  if is_glob_string options name then
    let dirs, names = glob_match options root dir name in
    if dirs = [] then
      if names = [] then
        match options.glob_check with
          NoMatchError ->
          raise (Failure (name ^ ": bad match"))
        | NoMatchPreserve ->
          [name]
        | NoMatchIgnore ->
          []
      else
        names
    else if names = [] then
      dirs
    else
      let names = List.append dirs names in
      List.sort Pervasives.compare names
  else
    let name      = unescape options name in
    let file_name = filename_concat dir name in
    [file_name]

(*
 * Glob an argv list.
 * We have to be a little more careful to preserve the order.
 *)
let glob_argv options dir names =
  let names = glob_braces options names in
  let names =
    List.fold_left (fun names name ->
        let names' = glob_argv_name options dir "" name in
        List.rev_append names' names) [] names
  in
  List.rev names

(************************************************************************
 * Directory listings.
*)

(*
 * Get all the names in the directory.
 *)
let list_dir_exn options root hidden_dirs dirs names dirname =
  let inx = Unix.opendir (filename_concat root dirname) in
  let rec read hidden_dirs dirs names =
    let name =
      try Some (Unix.readdir inx) with
        End_of_file ->
        None
    in
    match name with
      Some "."
    | Some ".." ->
      read hidden_dirs dirs names
    | None ->
      hidden_dirs, dirs, names
    | Some name ->
      let hidden_dirs, dirs, names =
        let filename = filename_concat dirname name in
        let dir_flag = is_dir filename in
        if (options.glob_dot || name.[0] <> '.')
        && (dir_flag || not options.glob_dirs)
        && not (options.glob_ignore name)
        && not (options.glob_cvsignore name)
        then
          if dir_flag then
            if options.glob_allow name then
              hidden_dirs, filename :: dirs, names
            else
              filename :: hidden_dirs, dirs, names
          else if options.glob_allow name then
            hidden_dirs, dirs, filename :: names
          else
            hidden_dirs, dirs, names
        else
          hidden_dirs, dirs, names
      in
      read hidden_dirs dirs names
  in
  let hidden_dirs_names = read hidden_dirs dirs names in
  Unix.closedir inx;
  hidden_dirs_names

let list_dir_aux options root hidden_dirs dirs names dirname =
  let options =
    if options.glob_cvs then
      { options with glob_cvsignore = load_cvsignore (filename_concat root dirname) }
    else
      options
  in
  try list_dir_exn options root hidden_dirs dirs names dirname with
    Unix.Unix_error _
  | Sys_error _
  | Failure _ ->
    hidden_dirs, dirs, names

(*
 * Perform a directory listing.
 *)
let list_dirs options root dirs =
  let rec collect dirs names l =
    match l with
      dir :: l ->
      let _, dirs, names = list_dir_aux options root [] dirs names dir in
      collect dirs names l
    | [] ->
      dirs, names
  in
  collect [] [] dirs

(*
 * Recursive directory listing.
 *)
let list_dirs_rec options root dirs =
  let rec collect examined_dirs hidden_dirs unexamined_dirs names =
    match hidden_dirs, unexamined_dirs with
      dir :: hidden_dirs, _ ->
      let hidden_dirs, unexamined_dirs, names =
        list_dir_aux options root hidden_dirs unexamined_dirs names dir
      in
      collect examined_dirs hidden_dirs unexamined_dirs names
    | [], dir :: unexamined_dirs ->
      let examined_dirs = dir :: examined_dirs in
      let hidden_dirs, unexamined_dirs, names =
        list_dir_aux options root hidden_dirs unexamined_dirs names dir
      in
      collect examined_dirs hidden_dirs unexamined_dirs names
    | [], [] ->
      examined_dirs, names
  in
  let hidden_dirs, unexamined_dirs =
    List.fold_left (fun (hidden_dirs, unexamined_dirs) dir ->
        if options.glob_allow dir then
          hidden_dirs, dir :: unexamined_dirs
        else
          dir :: hidden_dirs, unexamined_dirs) ([], []) dirs
  in
  collect [] hidden_dirs unexamined_dirs []

(*
 * Recursively expand all subdirectories.
 *)
let subdirs_of_dirs options root dirs =
  let options = { options with glob_dirs = true } in
  let rec collect listing hidden_dirs dirs =
    match hidden_dirs, dirs with
      dir :: hidden_dirs, _ ->
      let hidden_dirs, dirs, _ = list_dir_aux options root hidden_dirs dirs [] dir in
      collect listing hidden_dirs dirs
    | [], dir :: dirs ->
      let listing = dir :: listing in
      let hidden_dirs, dirs, _ = list_dir_aux options root hidden_dirs dirs [] dir in
      collect listing hidden_dirs dirs
    | [], [] ->
      listing
  in
  let hidden_dirs, dirs =
    if options.glob_proper then
      dirs, []
    else
      [], dirs
  in
  collect [] hidden_dirs dirs

(*
 * Regular expression export.
 *)
let regex_of_shell_pattern = regexp_of_shell_pattern
