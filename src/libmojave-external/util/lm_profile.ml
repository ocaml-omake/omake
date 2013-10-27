(*
   Profiling code
   Copyright (C) 2001 Justin David Smith, Caltech

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation,
   version 2.1 of the License.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
   
   Additional permission is given to link this library with the
   OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
   and you may distribute the linked executables.  See the file
   LICENSE.libmojave for more details.
 *)
let profile_level = ref 0

(*
 * Saving profile information to a file.
 *)
let profile_file = ref ""
let profile_file_opened = ref false
let profile_out = ref stdout

let difference_of tm1 tm2 =
      tm1.Unix.tms_utime -. tm2.Unix.tms_utime

let profile msg f arg =
   if !profile_file <> "" && not (!profile_file_opened) then
      begin
         profile_out := open_out (!profile_file);
         profile_file_opened := true
      end;

   let padding    = String.make (!profile_level * 2) ' ' in
   let _          = profile_level := succ !profile_level in
   let _          = Lm_printf.eprintf "profile: %s%s entered\n" padding msg in
   let _          = flush stderr in
   let starttime  = Unix.times () in
   let result     = f arg in
   let stoptime   = Unix.times () in
   let _          = profile_level := pred !profile_level in
   let deltatime  = difference_of stoptime starttime in
   let seconds    = int_of_float deltatime in
   let hours      = seconds / 3600 in
   let minutes    = (seconds / 60) mod 60 in
   let seconds    = seconds mod 60 in
   let millisec   = (int_of_float (deltatime *. 1000.0)) mod 1000 in
   let total = int_of_float (deltatime *. 1000.0) in
      Lm_printf.eprintf "profile: %s%s took %d:%02d:%02d.%03d\n" padding msg hours minutes seconds millisec;
      if !profile_file <> "" then
         begin
            let s = Lm_printf.sprintf "%s%s #%d\n"
                  padding msg total in
               output_string !profile_out s;
               flush !profile_out
         end;
      flush stderr;
      result

