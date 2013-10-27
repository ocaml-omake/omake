(*
 * Debugging utilities.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
open Printf

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Info needed for a debug variable.
 *)
type debug_info =
   { debug_name : string;
     debug_description : string;
     debug_value : bool
   }

(*
 * Info about variables.
 *)
type info =
   { info_name : string;
     mutable info_info : string option;
     info_flag : bool ref
   }

(************************************************************************
 * BASIC
 ************************************************************************)

(*
 * Perform debugging?
 * Set this function to false to disable all debugging.
 *)
let debug_enabled =
   true

let debug flag =
   debug_enabled && !flag

let debug_level flag i =
   debug_enabled && !flag >= i

(************************************************************************
 * DEBUG                                                                *
 ************************************************************************)

(*
 * Initial info is empty.
 *)
let info = ref []

(*
 * Description of debug flags added from the command line.
 *)
let default_description = "Unitialized debug flag"

(*
 * C debugging functions.
external ml_debug : string -> bool -> unit = "ml_debug"
external ml_get_debug : string -> string * bool = "ml_get_debug"
external ml_debuggers : unit -> (string * string * bool) array = "ml_debuggers"
 *)

(*
 * List all the debug flags.
 *)
let debuggers () =
   let collect { info_name = name; info_info = info; info_flag = flag } =
      let info =
         match info with
            Some info ->
               info
          | None ->
               default_description
      in
         { debug_name = name; debug_description = info; debug_value = !flag }
   in
      Array.append (**)
         (Array.of_list (List.map collect !info))
(*
         (Array.map  (**)
            (fun name, info, flag -> { debug_name = name; debug_description = info; debug_value = flag })
            (ml_debuggers ()))
 *)
         [||]

(*
 * Print a usage argument.
 *)
let debug_usage () =
   let usage { debug_name = name; debug_description = desc; debug_value = flag } =
      eprintf "\t%s: %s: %b\n" name desc flag
   in
      eprintf "Debugging flags:\n";
      eprintf "You can specify these as a colon-separated list\n";
      Array.iter usage (debuggers ());
      flush stderr

(*
 * Create a debugging variable.
 *)
let create_debug
    { debug_name = name;
      debug_description = desc;
      debug_value = flag
    } =
   let vinfo = !info in
   let rec search = function
      info :: t ->
         let { info_name = name'; info_info = desc'; info_flag = flag' } = info in
            if name' = name then
               match desc' with
                  None ->
                     info.info_info <- Some desc;
                     flag'
                | Some desc' ->
                     if desc <> desc' then
                        raise (Failure (sprintf "Lm_debug.create_debug: variable '%s' is already created with a different description" name))
                     else
                        flag'
            else
               search t
    | [] ->
         let flag' = ref flag in
         let ninfo =
            { info_name = name;
              info_info = Some desc;
              info_flag = flag'
            }
         in
            info := ninfo :: vinfo;
            flag'
   in
      search vinfo

(*
 * Get the value of a debugging variable.
 *)
let load_debug name =
   let rec search = function
      { info_name = name'; info_flag = flag } :: t ->
         if name' = name then
            flag
         else
            search t
    | [] ->
         raise (Failure (sprintf "Lm_debug.load_debug: variable '%s' has not been created" name))
   in
      search !info

(*
 * Modify a debugging flag.
 *)
let set_debug name flag =
   let rec search = function
      h :: t ->
         let { info_name = name'; info_flag = flag' } = h in
            if name' = name then
               flag' := flag
            else
               search t
    | [] ->
(*
         (* Try a C function *)
         try ml_debug name flag with
            Failure "ml_debug" ->
*)
               raise (Failure "set_debug")
   in
      search !info

(*
 * Possible debug flag.
 * Try setting the flag first.
 *)
let set_possible_debug name flag =
   try set_debug name flag with
      Failure "set_debug" ->
         let flag' = ref flag in
         let ninfo =
            { info_name = name;
              info_info = None;
              info_flag = flag'
            }
         in
            info := ninfo :: !info

(*
 * Get the value of a debugging flag.
 *)
let get_debug name =
   let rec search = function
      h :: t ->
         if h.info_name = name then
            let { info_info = description; info_flag = flag } = h in
            let description =
               match description with
                  Some desc ->
                     desc
                | None ->
                     default_description
            in
               { debug_name = name;
                 debug_description = description;
                 debug_value = !flag
               }
         else
            search t
    | [] ->
         (* Try a C function *)
(*
         try
            let info, flag = ml_get_debug name in
               { debug_name = name;
                 debug_description = info;
                 debug_value = flag
               }
         with
            Failure "ml_get_debug" ->
*)
               eprintf "Lm_debug.get_debug: no such variable: %s\n%t" name flush;
               raise (Failure "get_debug")
   in
      search !info

(*
 * Check for no remaining possible debug flags.
 *)
let check_debug () =
   ()
(*
   if List.exists (fun info -> info.info_info = None) !info then
      begin
         debug_usage ();
         raise (Failure "check_debug")
      end
*)

(************************************************************************
 * PARTICULAR DEBUG                                                     *
 ************************************************************************)

(*
 * File loading.
 *)
let debug_load =
   create_debug (**)
      { debug_name = "load";
        debug_description = "Print file names as they load";
        debug_value = false
      }

let eflush outx =
   output_char outx '\n';
   flush outx

let show_loading s =
   if !debug_load then
      eprintf s eflush

(*
 * Split a string at a particular char.
 *)
let split c s =
   let len = String.length s in
   let rec loop i j =
      if j = len then
         if i = j then
            []
         else
            [String.sub s i (j - i)]
      else if String.contains c s.[j] then
         if i = j then
            loop (succ j) (succ j)
         else
            (String.sub s i (j - i)) :: (loop (succ j) (succ j))
      else
         loop i (succ j)
   in
      loop 0 0

(*
 * Set debug flags from the environment.
 *)
let set_possible_debug_flags _ _ flags =
   List.iter (fun name -> set_possible_debug name true) (split ":" flags)

let set_debug_flags flags =
   let names = split ":" flags in
      try List.iter (fun name -> set_debug name true) names with
         Failure _ ->
            debug_usage ();
            exit 1

(*************************************************************************
 * AD-HOC PROFILING
 *)

open Unix

type times = {
   mutable calls : int;
   mutable wtime : float;
   mutable utime : float;
   mutable stime : float
}

type profile = {
   ok : times;
   exn : times
}

type 'a res =
   Ok of 'a
 | Exn of exn

let tbl = Hashtbl.create 19

let compare (_,t1) (_,t2) =
   (t1.ok.wtime +. t1.exn.wtime) <= (t2.ok.wtime +. t2.exn.wtime)

let report1 s t =
   let calls_f = float_of_int t.calls in
      eprintf "\t%s:\n\t\tCalls: %i;\n\t\tTime elapsed: %0.3fs (%0.6fs/call);\n\t\tSystem time: %0.2fs (%0.6fs/call);\n\t\tUser time: %0.2fs (%0.6fs/call).\n" (**)
         s t.calls t.wtime (t.wtime /. calls_f) t.stime (t.stime /. calls_f) t.utime (t.utime /. calls_f)

let report (s, t) =
   eprintf "Timing information for %s (%0.3fs total):\n" s (t.ok.wtime +. t.exn.wtime);
   if t.ok.calls <> 0 then report1 (if t.exn.calls <> 0 then "Successful calls" else "All calls succeeded") t.ok;
   if t.exn.calls <> 0 then report1 (if t.ok.calls <> 0 then "Failed calls" else "All calls failed") t.exn;
   if t.ok.calls <> 0 && t.exn.calls <> 0 then report1 "Total calls" {
      calls = t.ok.calls + t.exn.calls;
      wtime = t.ok.wtime +. t.exn.wtime;
      utime = t.ok.utime +.  t.exn.utime;
      stime = t.ok.stime +.  t.exn.stime
   }

let add s t l = (s,t) :: l

let report_timing () =
   if Hashtbl.length tbl > 0 then
      List.iter report (Sort.list compare (Hashtbl.fold add tbl []))

let () = at_exit report_timing

let timing_wrap s f arg =
   let start_f = gettimeofday () in
   let start_p = times () in
   let res =
      try Ok (f arg)
      with exn -> Exn exn
   in
   let end_f = gettimeofday () in
   let end_p = times () in
   let times =
      try Hashtbl.find tbl s
      with Not_found ->
         let times = {
            ok = {calls = 0; wtime = 0.0; utime = 0.0; stime = 0.0};
            exn = {calls = 0; wtime = 0.0; utime = 0.0; stime = 0.0}}
         in
            Hashtbl.add tbl s times;
            times
   in
   let times =
      match res with
         Ok _ -> times.ok
       | Exn _ -> times.exn
   in
      times.calls <- times.calls + 1;
      times.wtime <- times.wtime +. end_f -. start_f;
      times.utime <- times.utime +. end_p.tms_utime -. start_p.tms_utime;
      times.stime <- times.stime +. end_p.tms_stime -. start_p.tms_stime;
      match res with
         Ok res -> res
       | Exn exn -> raise exn

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
