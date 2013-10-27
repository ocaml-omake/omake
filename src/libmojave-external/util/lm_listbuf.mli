(*
   Simple list buffer utility (used for instruction buffers)
   Copyright (C) 2002,2001 Justin David Smith, Caltech

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


type 'a t

val empty : 'a t

val of_elt : 'a -> 'a t

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val add : 'a t -> 'a -> 'a t

val add_list : 'a t -> 'a list -> 'a t

val add_rev_list : 'a t -> 'a list -> 'a t

val add_listbuf : 'a t -> 'a t -> 'a t

