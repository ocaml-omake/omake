
(************************************************************************
 * Argument collapsing.
 *)

type arg_buffer = Omake_command_type.arg_string list

let arg_buffer_empty =
   []

let arg_buffer_add_string buf s =
   Omake_command_type.ArgString s :: buf

let arg_buffer_add_data buf s =
   Omake_command_type.ArgData s :: buf

let rec collect_string buf args =
   match args with
      Omake_command_type.ArgString s :: args ->
         Buffer.add_string buf s;
         collect_string buf args
    | _ ->
         args

let rec collect_data buf args =
   match args with
      Omake_command_type.ArgData s :: args ->
         Buffer.add_string buf s;
         collect_data buf args
    | _ ->
         args

let arg_buffer_contents args =
   let buf = Buffer.create 32 in
   let rec collect args' args =
      match args with
         Omake_command_type.ArgString s :: ((ArgString _ :: _) as tl) ->
            Buffer.add_string buf s;
            let args = collect_string buf tl in
            let s = Buffer.contents buf in
               Buffer.clear buf;
               collect (Omake_command_type.ArgString s :: args') args
       | ArgData s :: ((ArgData _ :: _) as tl) ->
            Buffer.add_string buf s;
            let args = collect_data buf tl in
            let s = Buffer.contents buf in
               Buffer.clear buf;
               collect (ArgData s :: args') args
       | h :: args ->
            collect (h :: args') args
       | [] ->
            List.rev args'
   in
      collect [] (List.rev args)

(************************************************************************
 * Command utilities
 *)

(*
 * Parse the command lines from the strings.
 *)
let parse_command venv dir target loc flags line =
   { Omake_command_type.command_loc    = loc;
     command_dir    = dir;
     command_target = target;
     command_flags  = flags;
     command_venv   = venv;
     command_inst   = line
   }

let parse_commands venv dir target loc lines =
   List.map (fun (flags, line) -> parse_command venv dir target loc flags line) lines

(*
 * Allow output in the command.
 *)
let command_allow_output command =
   { command with Omake_command_type.command_flags = AllowOutputFlag :: command.Omake_command_type.command_flags }
