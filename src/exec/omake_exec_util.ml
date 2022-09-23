(*
 * Utilities for execution.
 *)
(* open Lm_printf *)

(*
 * Build debugging.
 *)
let debug_exec =
   Lm_debug.create_debug (**)
      { debug_name = "exec";
        debug_description = "Display execution debugging";
        debug_value = false
      }


module IntTable = Lm_map.LmMake (struct
   type t = int
   let compare = (-)
end)

module FdTable = Lm_map.LmMake (struct
   type t = Unix.file_descr
   let compare = compare
end
);;

let unix_close fd =
   try Unix.close fd with
      Unix.Unix_error _ -> ()

let with_pipe f =
   let read, write = Unix.pipe () in
   try f read write with
     exn ->
       unix_close read;
       unix_close write;
       raise exn

(*
 * Write the data in the buffer to the channel.
 *)
let  write_all name fd _id buf  off len =
   if len <> 0 then
     match Unix.write fd buf off len with 
     | amount when amount <> len 
       -> 
         Format.eprintf "Writing to %s was only partially successful (%i out of %i written)@."
          name amount len;
         invalid_arg "Omake_exec_util.write_all"
     | exception Unix.Unix_error (err1, err2, err3) -> 
         Format.eprintf "Writing to %s resulted in an error: %s: %s: %s@." name err2 err3 
                (Unix.error_message err1);
         invalid_arg "Omake_exec_util.write_all"
     | _ -> 
         ()
(*
 * Copy output to a file.
 *)
let copy_file name =
   let fd_out = Lm_unix_util.openfile name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
   let () = Unix.set_close_on_exec fd_out in
   let copy id buf off len =
      if len = 0 then
         Unix.close fd_out
      else
         write_all name fd_out id buf off len
   in
      copy

(*
 * Tee the output to a file if any occurs.
 * The files are created only if there is output.
 *)
type tee_info =
 | TeeChannel of string * out_channel
 | TeeFile of string
 | TeeMaybe
 | TeeNever

type tee = tee_info ref

let tee_file tee =
   match !tee with
   |  TeeChannel (name, _)
   | TeeFile name ->
       Some name
   | TeeMaybe
   | TeeNever -> None

let tee_channel tee =
   match !tee with
   | TeeChannel (_, outx) ->
       Some outx
   | TeeMaybe ->
       let filename, outx =
         Filename.open_temp_file ~mode:[Open_binary;Open_append] "omake" ".divert" in
       tee := TeeChannel (filename, outx);
       Some outx
   | TeeFile _
   | TeeNever ->
         None

let tee_close tee =
   match !tee with
   | TeeChannel (name, outx) ->
       close_out outx;
       tee := TeeFile name
   | TeeFile _
   | TeeMaybe
   | TeeNever -> ()

let tee_none = ref TeeNever

let tee_create (b : bool) : tee =
   if b then
      ref TeeMaybe
   else
      tee_none

let tee_copy name fd flush_flag tee tee_only id buf off len =
   if len = 0 then begin
      if not tee_only then
         flush_flag := true;
      match !tee with
      |  TeeChannel (_, outx) ->
          flush outx
      | _ ->
            ()
   end else begin
      if not tee_only then begin
         if !flush_flag then begin
            Omake_exec_print.progress_flush ();
            flush_flag := false;
         end;
         write_all name fd id buf off len;
      end;
      match tee_channel tee with
         Some outx ->
            output outx buf off len
       | None ->
            ()
   end

let tee_file_descr tee =
  match tee_channel tee with
   | Some outx ->
        Some(Unix.descr_of_out_channel outx)
   | None ->
        None


let tee_stdout = tee_copy "Unix.stdout" Unix.stdout (ref true)
let tee_stderr = tee_copy "Unix.stderr" Unix.stderr (ref true)
