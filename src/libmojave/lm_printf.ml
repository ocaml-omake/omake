
include Format
type 'a t = Format.formatter -> 'a  -> unit
(**
 * Redirect formatter's output to both a channel and a log file.
 * The log file will be appended (not truncated), lockf-mutexed,
 * and all the entries will be annotated with the PID of the logger:
 * [|PID1: string1|][|PID2: string2|]...
 * (with consequetive non-NL strings from the same PID merged together).
*)


let extra_formatting file chan formatter =
  let log = open_out_gen [Open_wronly; Open_append; Open_creat] 0o600 file in
  let pid_file = file ^ ".pid" in
  let pid_fd = Unix.openfile pid_file [O_RDWR; O_CREAT] 0o666 in
    let get_pid () =
    ignore (Unix.lseek pid_fd 0 Unix.SEEK_SET);
    Unix.lockf pid_fd Unix.F_LOCK 0;
      try
      let s = String.make 10 ' ' in
      let i = Unix.read pid_fd s 0 10 in
      let s = String.sub s 0 i in
    int_of_string s
      with _ ->
  0   in
    let write_pid i =
    ignore (Unix.lseek pid_fd 0 SEEK_SET);
    let () = Unix.ftruncate pid_fd 0 in
    let s = string_of_int i in
    ignore (Unix.single_write pid_fd s 0 (String.length s));
    ignore (Unix.lseek pid_fd 0 SEEK_SET);
  Unix.lockf pid_fd F_ULOCK 0 in
    let flush () =
    flush chan;
  flush log
  in
    let do_newline nl =
    let old_pid = get_pid () in
    if old_pid <> 0 then output_string log "|]";
    output_string chan nl;
    output_string log "\n";
    flush ();
  write_pid 0
  in
    let rec out s i len =
      if len > 0 then begin
      let s = String.sub s i len in
        if String.contains s '\r' then begin
        let i = String.index s '\r' in
        out s 0 i;
          if (i < len - 1) && s.[i+1] = '\n' then begin
          do_newline "\n";
        out s (i+2) (len - i - 2)
          end else begin
          do_newline "\r";
        out s (i+1) (len - i - 1)
      end
        end else if String.contains s '\n' then begin
        let i = String.index s '\n' in
        out s 0 i;
        do_newline "\n";
      out s (i+1) (len - i - 1)
        end else begin
        output_string chan s;
        let old_pid = get_pid () in
        let pid = Unix.getpid () in
          if pid <> old_pid then begin
          if old_pid <> 0 then output_string log "|]";
        output_string log (sprintf "[|%i: " pid)
        end;
        output_string log s;
      write_pid pid
    end
  end
  in
  let newline () = do_newline "\n" in
    let spaces i =
    let s = String.make i ' ' in
  out s 0 i;
  in
  pp_set_all_formatter_output_functions formatter ~out ~flush ~newline ~spaces
 
 (*
 * Copy the std_formatter and err_formatter outputs to log files.
 *
* XXX: TODO: for now this is commented out, we might consider doing
* this based on an environment variable.
   
   let () =
extra_formatting "/tmp/mylog.out" stdout std_formatter;
extra_formatting "/tmp/mylog.err" stderr err_formatter

*)

type out_channel = formatter
 
 (*
* Standard channels.
*)
let stdout = std_formatter
let stderr = err_formatter
let stdstr = str_formatter
 
 (*
* Get the string from the string formatter.
*)
let flush_stdstr = flush_str_formatter
 
 (*
* Open new output channels.
*)
let open_out name =
  formatter_of_out_channel (open_out name)

let open_out_bin name =
  formatter_of_out_channel (open_out_bin name)
 
 (*
* Output.
*)
let output_char       = pp_print_char
let output_string     = pp_print_string
 
 (*
* Normal printing.
*)
let print_char    = pp_print_char std_formatter
let print_int     = pp_print_int std_formatter
let print_string  = pp_print_string std_formatter

let prerr_char    = pp_print_char err_formatter
let prerr_int     = pp_print_int err_formatter
let prerr_string  = pp_print_string err_formatter
 
 (*
* Print a newline and flush.
*)
let flush buf  = pp_print_flush buf ()
let eflush buf = pp_print_newline buf ()
 
 (*
* Printing functions.
*)
let printf  = printf
let eprintf = eprintf
let sprintf = sprintf
let fprintf = fprintf
let bprintf = bprintf
 
(*
* Formatting functions.
*)
let set_all_formatter_output_functions out flush newline spaces =
  set_all_formatter_output_functions ~out ~flush ~newline ~spaces

  let pp_set_all_formatter_output_functions buf out flush newline spaces =
pp_set_all_formatter_output_functions buf ~out ~flush ~newline ~spaces
 
(*
* List separated by semicolons.
*)
let rec print_any_list print out l =
    match l with
    | [h] ->
      print out h
    | h::t ->
      print out h;
      output_string out "; ";
      print_any_list print out t
    | [] -> ()

let print_string_list = print_any_list pp_print_string

let print_int_list = print_any_list pp_print_int
 
 (*
* Get a formatter.
*)
let out_channel_of_formatter out = out

let rec pp_print_any_list print buf = function
  | [] ->
    ()
  | [a] ->
    print buf a
  | a::rest ->
    print buf a;
    pp_print_string buf ";";
    pp_print_space buf ();
    pp_print_any_list print buf rest
  
