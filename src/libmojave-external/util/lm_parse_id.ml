(*
   Parse a CVS ID string
   Copyright (C) 2002 Justin David Smith, Caltech

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


let parse_id id =
   let parts = Lm_string_util.split " " id in
      match parts with
         [_; name; rev; date; time; user; ty; _] ->
            name, rev, date, time, user, ty
       | _ ->
            raise (Failure "parse_id:  Malformed ID string")


let parse_id_revision id =
   try
      let _, rev, _, _, _, _ = parse_id id in
         rev
   with
      Failure _ ->
         "unknown"
