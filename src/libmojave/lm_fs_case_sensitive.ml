
external case_sensitive_available : unit -> bool   = 
   "lm_fs_case_sensitive_available"
external case_sensitive           : string -> bool =
   "lm_fs_case_sensitive"

let available = case_sensitive_available ()

(* Test whether stats are equal enough that we think that it's the same file.*)
let stats_equal (stat1 : Unix.LargeFile.stats) (stat2 : Unix.LargeFile.stats) =
  stat1.st_dev = stat2.st_dev
  && stat1.st_ino = stat2.st_ino
  && stat1.st_kind = stat2.st_kind
  && stat1.st_rdev = stat2.st_rdev
  && stat1.st_nlink = stat2.st_nlink
  && stat1.st_size = stat2.st_size
  && stat1.st_mtime = stat2.st_mtime
  && stat1.st_ctime = stat2.st_ctime


(* Stat, does not fail. *)
let do_stat absname =
  try Some (Unix.LargeFile.lstat absname) with
    Unix.Unix_error _ ->
    None

(* Unlink, does not fail. *)
let do_unlink absname =
  try Unix.unlink absname with
    Unix.Unix_error _ ->
    ()

(* Create a file, raising Unix_error if the file can't be created. *)
let do_create absname =
  Unix.close (Unix.openfile absname [O_WRONLY; O_CREAT; O_EXCL] 0o600)

(*
Given two filenames that differ only in case,
stat them both.  If the stats are different,
the directory is on a case-sensitive fs.

XXX: try to detect race conditions by performing
a stat on the first file before and after.
Raise Not_found if a race condition is detected.
*)
let stats_not_equal name1 name2 =
  let stat1 = do_stat name1 in
  let stat2 = do_stat name2 in
  let stat3 = do_stat name1 in
  match stat1, stat3 with
    Some s1, Some s3 when stats_equal s1 s3 ->
    (match stat2 with
       Some s2 when stats_equal s2 s1 -> false
     | _ -> true)
  | _ ->
    raise Not_found

(* If we have an alphabetic name, just toggle the case.*)
let stat_with_toggle_case absdir name : bool =
  (* Toggle the case of the name.
     Raises Not_found if the name contains no alphabetic letters.
  *)
  let rec toggle_name_case name len i : string  =
    if i = len then
      raise Not_found
    else
      match name.[i] with
      | 'A'..'Z'
      | '\192' .. '\214'
      | '\216' .. '\222' ->
        String.lowercase name
      | 'a'..'z'
      | '\224' .. '\246'
      | '\248' .. '\254' ->
        String.uppercase name
      | _ ->
        toggle_name_case name len (i+ 1)
  in
  let alternate_name = toggle_name_case name (String.length name) 0 in
  stats_not_equal (Filename.concat absdir name) (Filename.concat absdir alternate_name)

exception Already_lowercase
let rec check_already_lowercase name len i =
  if i = len then
    raise Already_lowercase
  else
    match name.[i] with
    | 'A'..'Z'
    | '\192' .. '\214'
    | '\216' .. '\222' -> ()
    | _ -> check_already_lowercase name len (i+1)



(**
 Look through the entire directory for a name with alphabetic characters.
 A check for case-sensitivity base on that.
Raises Not_found if there are no such filenames.
*)
let rec dir_test_all_entries_exn absdir dir_handle =
  match Unix.readdir dir_handle
  with
  | exception (Unix.Unix_error _ | End_of_file as exn) -> 
    Unix.closedir dir_handle ;
    raise exn 
  | "."
  | ".." ->
    dir_test_all_entries_exn absdir dir_handle
  | name ->
    match 
      stat_with_toggle_case absdir name
    with 
    | exception Not_found ->     
      dir_test_all_entries_exn absdir dir_handle
    | result ->
      Unix.closedir dir_handle ; 
      result 


let fs_random = Random.State.make_self_init () 

(* Check for sensativity by creating a dummy file.
*)
let dir_test_new_entry_exn absdir =
  Unix.access absdir [W_OK];
  let name = Lm_printf.sprintf "OM%06x.tmp" (Random.State.bits fs_random land 0xFFFFFF) in
  let absname = Filename.concat absdir name  in
  begin 
    do_create absname ;
    match stat_with_toggle_case absdir name with
    | exception (Not_found as exn) -> 
      do_unlink absname;
      raise exn
    | flag -> do_unlink absname; flag 
  end


exception Not_a_usable_directory
let dir_case_sensitive absdir = 
  match Unix.opendir absdir with 
  | exception Unix.Unix_error ((ENOENT | ENOTDIR |ELOOP |ENAMETOOLONG), _, _) -> 
    raise Not_a_usable_directory
  | dir_handle -> 
    try dir_test_all_entries_exn absdir dir_handle
    with Unix.Unix_error _ | Not_found | End_of_file ->
      dir_test_new_entry_exn absdir


