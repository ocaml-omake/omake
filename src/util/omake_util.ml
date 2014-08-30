
(*  Test a string for being false. *)
let bool_of_string s =
   match String.lowercase s with
      ""
    | "0"
    | "no"
    | "nil"
    | "false"
    | "undefined" ->
         false
    | _ ->
         true

(*  Path separator. *)
let pathsep =
   if Sys.os_type = "Win32" then
      ";"
   else
      ":"




let pp_time buf secs =
   if secs < 60. then
      Format.fprintf buf "%0.2f sec" secs
   else
      let subsec, sec = modf secs in
      let sec = int_of_float sec in
      let h = sec / 3600 in
      let m = (sec / 60) mod 60 in
      let s = sec mod 60 in
         if h > 0 then
            Format.fprintf buf "%d hrs %02d min %05.2f sec" h m (float s +. subsec)
         else
            Format.fprintf buf "%d min %05.2f sec" m (float s +. subsec)
