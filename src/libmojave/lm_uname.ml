
type uname =
   { sysname  : string;
     nodename : string;
     release  : string;
     version  : string;
     machine  : string
   }

external lm_uname : unit -> uname = "lm_uname"

let uname = lm_uname ()

let sysname  = uname.sysname
let nodename = uname.nodename
let release  = uname.release
let version  = uname.version
let machine  = uname.machine

