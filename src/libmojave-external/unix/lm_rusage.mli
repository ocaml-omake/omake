(*
 * Simple rusage interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
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
 *)


(* rusage
   Data type that contains information on process resource usage.  Currently
   a limited form of the full data structure returned by libc.  This contains
   the user and system runtimes, broken down into seconds and microseconds,
   for the requested processes.  *)
type rusage_short =
   { ru_utime_sec  : int;
     ru_utime_usec : int;
     ru_stime_sec  : int;
     ru_stime_usec : int;
   }

(*
 * A more complete version of rusage.
 *)
type rusage =
   { ru_utime : float;
     ru_stime : float;
     ru_maxrss : int;
     ru_ixrss : int;
     ru_idrss : int;
     ru_isrss : int;
     ru_minflt : int;
     ru_majflt : int;
     ru_nswap : int;
     ru_inblock : int;
     ru_oublock : int;
     ru_msgsnd : int;
     ru_msgrcv : int;
     ru_nsignals : int;
     ru_nvcsw : int;
     ru_nivcsw : int
   }

(* rusage_who
   Who to report statistics on: the current process, or the aggregate of all
   its children.  *)
type rusage_who =
   RUSAGE_SELF
 | RUSAGE_CHILDREN


(***  Interface  ***)


(* getrusage_time who
   Returns the resource usage of the indicated process.  *)
val getrusage_time : rusage_who -> rusage_short

(*
 * A more complete version of rusage.
 *)
val getrusage : unit -> rusage

(* setrlimit_time time
   Sets the resource CPU limit for the current process to the indicated
   time interval (in seconds).  Note that this number can only be decreased
   from the current resource limit; it cannot be increased.  *)
val setrlimit_time : int -> unit


(* total_rusage rusage
   Takes the rusage structure returned by getrusage_time, and computes the
   total compute time of the process.  Returns the pair (sec, usec) which
   indicate the process total running time.  *)
val total_rusage : rusage_short -> int * int
