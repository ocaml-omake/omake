(*
 * Timers for low-level profiling.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2001 Mojave Group, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

open Lm_profile

(*
 * We will keep track of all clocks by their names.
 *)
module StringBase =
struct
   type t = string
   let compare = compare
end

module StringSet = Lm_map.LmMake (StringBase)

type clock_data_type =
   { clock_active : bool;
     clock_starttime : Unix.process_times;
     clock_ttime : float;
     clock_tcalls : int
   }

let clock_add = StringSet.add
let clock_find = StringSet.find

(*
 * We store clocks here.
 *)
let clocks = ref StringSet.empty

(*
 * Start given clock.
 * Raise an exception if clock does not exist.
 *)
let clock_start name =
   try
      let clock = clock_find !clocks name in
      if clock.clock_active then
         raise (Invalid_argument (Lm_printf.sprintf "clock %s is already active" name));
      let clock =
         { clock_active = true;
           clock_starttime = Unix.times ();
           clock_ttime = clock.clock_ttime;
           clock_tcalls = clock.clock_tcalls
         }
      in
         clocks := clock_add !clocks name clock
   with
      Not_found ->
         let clock =
            { clock_active = true;
              clock_starttime = Unix.times ();
              clock_ttime = 0.0;
              clock_tcalls = 0
            }
         in
            clocks := clock_add !clocks name clock

(*
 * Stop given clock.
 * Raise an exception if clock does not exist.
 *)
let clock_stop name =
   try
      let clock = clock_find !clocks name in
      if not clock.clock_active then
         raise (Invalid_argument (Lm_printf.sprintf "clock %s is not set" name));
      let clock =
         { clock_active = false;
           clock_starttime = clock.clock_starttime;
           clock_ttime = clock.clock_ttime +. (difference_of (Unix.times ()) clock.clock_starttime);
           clock_tcalls = clock.clock_tcalls + 1
         }
      in
         clocks := clock_add !clocks name clock
   with
      Not_found ->
         raise (Invalid_argument (Lm_printf.sprintf "clock %s does not exist" name))

(*
 * The total execution time when clock was active.
 * Raise an exception if clock does not exist.
 *)
let clock_ttime name =
   try
      let clock = clock_find !clocks name in
         clock.clock_ttime
   with
      Not_found ->
         raise (Invalid_argument (Lm_printf.sprintf "clock_ttime: clock %s does not exist" name))

(*
 * The total number of calls (start-stop sequences).
 * Raise an exception if clock does not exist.
 *)
let clock_tcalls name =
   try
      let clock = clock_find !clocks name in
         clock.clock_tcalls
   with
      Not_found ->
         raise (Invalid_argument (Lm_printf.sprintf "clock_tcalls: clock %s does not exist" name))

(*
 * Reset clock.
 * If clock does not exist, create it. Also, we ignore
 * the fact that the clock may be active.
 *)
let clock_reset name =
   let null_time =
      { Unix.tms_utime = 0.0;
        Unix.tms_stime = 0.0;
        Unix.tms_cutime = 0.0;
        Unix.tms_cstime = 0.0
      }
   in
   let clock =
      { clock_active = false;
        clock_starttime = null_time;
        clock_ttime = 0.0;
        clock_tcalls = 0
      }
   in
      clocks := clock_add !clocks name clock

(*
 * Report on given clock.
 * Raise an exception if clock does not exist.
 *)
let clock_report msg name =
   let ttime = clock_ttime name in
   let tcalls = clock_tcalls name in
   Lm_printf.print_string (Lm_printf.sprintf "%sTotal time in %s = %.2f sec, %d calls, %.5f sec/call\n" (**)
      msg name ttime tcalls (ttime /. (float_of_int tcalls)))
