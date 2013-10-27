(*
 * Parsing command line arguments, MCC-style. Arguments to options
 * may be separated from the option by a space, or may be placed
 * immediately after the option (without space) IF the option is
 * not ambiguous.  Also, options may be abbreviated as long as the
 * short form is not ambiguous.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2006 Mojave Group, Caltech
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
 * Authors: Jason Hickey <jyh@cs.caltech.edu>
 *          Justin David Smith
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_printf


(***  Basic Specifications  ***)


(* spec
   Argument specification.  Each option uses this specification to indicate
   what type of argument (if any) the option takes.  The following option
   specifications are defined.
      Unit f:     Call an arbitrary function f ()
      Set b:      Set the boolean (reference) value b to true
      Clear b:    Set the boolean (reference) value b to false
      String f:   Takes one argument:  call function f <string>
      Int f:      Takes one argument:  call function f <integer>
      Float f:    Takes one argument:  call function f <float>
      Rest f:     Call function f <arg>, for all remaining arguments

   section = (name, spec, desc) list
   Used to define a group of related arguments.  (name, spec) indicate the
   option name and option specification.  desc gives a textual description
   of the option.

   sections = (desc, section) list
   Used to define all option groups.  Each option group is prefixed by desc
   which briefly describes the section.
 *)
type 'a poly_spec =
   (* Imperative versions *)
   Unit       of (unit -> unit)
 | Set        of bool ref
 | Clear      of bool ref
 | String     of (string -> unit)
 | Int        of (int -> unit)
 | Float      of (float -> unit)
 | Rest       of (string -> unit)

   (* Functional versions *)
 | UnitFold   of ('a -> 'a)
 | SetFold    of ('a -> bool -> 'a)
 | ClearFold  of ('a -> bool -> 'a)
 | StringFold of ('a -> string -> 'a)
 | IntFold    of ('a -> int -> 'a)
 | FloatFold  of ('a -> float -> 'a)
 | RestFold   of ('a -> string -> 'a)

   (* Usage message *)
 | Usage

(* spec_mode

   StrictOptions: options are processed literally, and may not be collapsed
      into multi-letter options.
   MultiLetterMode: single-letter options of the form -x may be collapsed
      into multi-letter options. *)
type spec_mode =
   StrictOptions
 | MultiLetterOptions

type 'a poly_section = (string * 'a poly_spec * string) list
type 'a poly_sections = spec_mode * (string * 'a poly_section) list

type spec = unit poly_spec
type section = unit poly_section
type sections = unit poly_sections

(* parsing mode

   StrictMode: options are processed literally, and may not be collapsed
      into multi-letter options.
   MultiLetterMode: single-letter options may be collapsed.
   MultiLetterPending: processing a multi-letter option *)
type mode =
   StrictMode
 | MultiLetterMode
 | MultiLetterPending of string * int

(* BogusArg
   Thrown by option processing when something goes wrong...  *)
exception BogusArg of string

(* UsageError
   Thrown on --help *)
exception UsageError


(***  Option Table  ***)


(* CharCompare, CharTable
   Defines a table indexed by individual characters.  *)
module CharCompare = struct
   type t = char
   let compare (c1 : char) (c2 : char) =
      if c1 < c2 then
         -1
      else if c1 > c2 then
         1
      else
         0
end (* CharCompare *)

module CharTable = Lm_map.LmMake (CharCompare);;


(* options
   The option table is a tree, where each edge is labelled by a character.
   To lookup the specification for an option, we walk the tree using the
   characters of the option until we reach a node that has a specification
   associated with it.  This tree is used to help us identify unambiguous
   prefixes, and also to determine where an option name ends and its value
   begins (when the name and value are not space-delimited).

   option_node
   The type of a node in the options tree.  Each node contains a spec if
   the node matches an option name, and may contain a subtree if there is
   at least one longer option that has this prefix.
      SpecNode spec:    Leaf node; this branch corresponds to the spec.
      NameNode tree:    No option corresponds to this branch, but there
                        are options in the subtree.
      SpecOrName (spec, tree):
                        This branch corresponds to an option with the
                        indicated spec; there are also suboptions in the
                        indicated subtree.
 *)
type 'a option_node =
   SpecNode of 'a poly_spec
 | NameNode of 'a option_node CharTable.t
 | SpecOrName of 'a poly_spec * 'a option_node CharTable.t

type 'a options = 'a option_node CharTable.t

(* is_alnum

   test if a letter is a letter or number *)
let is_alnum = function
   'a'..'z'
 | 'A'..'Z'
 | '0'..'9' ->
      true
 | _ ->
      false


(* char_table_lookup
   Lookup an entry in the char table.  If no entry exists in the table,
   then None is returned (instead of raising an exception).  *)
let char_table_lookup table ch =
   try
      Some (CharTable.find table ch)
   with
      Not_found ->
         (* If the character is '_', try looking it up as '-'. This is a
            hack to accomodate both '_' and '-' in option names (proper
            GCC style uses hyphen, but our old options used underscores). *)
         if ch = '_' then
            try
               Some (CharTable.find table '-')
            with
               Not_found ->
                  None
         else
            None


(* lookup_option
   We also allow --no-* prefixes on Boolean options.

   JYH: this is perhaps not the simplest way to deal
   with inversion, but the implementation is simple.  *)
let is_invert_prefix name =
   String.length name > 5
   && String.unsafe_get name 0 = '-'
   && String.unsafe_get name 1 = '-'
   && String.unsafe_get name 2 = 'n'
   && String.unsafe_get name 3 = 'o'
   && String.unsafe_get name 4 = '-'

let strip_invert_prefix name =
   String.sub name 4 (String.length name - 4)

let is_invertable_option opt = function
   Set _
 | Clear _
 | SetFold _
 | ClearFold _ ->
      String.length opt > 1 && opt.[0] = '-'
 | _ ->
      false


(* add_option
   Add a new option name to the option tree.  If the exact option already
   exists, then an exception is thrown.  If a prefix or suffix of this
   option is already defined, then no error occurs.  *)
let add_option options name spec =
   if is_invert_prefix name then
      raise (BogusArg ("Option contains an invertion prefix: " ^ name));
   let length = String.length name in

   (* deconstruct_name
      Updates the subtree rooted at options, based on the substring
      of name beginning with offset.  *)
   let rec deconstruct_name options offset =
      let ch = name.[offset] in
      let offset = offset + 1 in
      let entry =
         if offset < length then

            (* This is NOT the last character of the option; we
               need to build a subtree and recurse on ourself.  *)
            match char_table_lookup options ch with
               None ->
                  NameNode (deconstruct_name CharTable.empty offset)
             | Some (SpecNode spec') ->
                  SpecOrName (spec', deconstruct_name CharTable.empty offset)
             | Some (NameNode options) ->
                  NameNode  (deconstruct_name options offset)
             | Some (SpecOrName (spec', options)) ->
                  SpecOrName (spec', deconstruct_name options offset)
         else

            (* This is the last character of the option; this is
               where we might have a duplicate hit, and where we
               need to drop our specification.  *)
            match char_table_lookup options ch with
               None ->
                  SpecNode spec
             | Some (NameNode options) ->
                  SpecOrName (spec, options)
             | Some _ ->
                  raise (BogusArg ("Duplicate option defined: " ^ name))
      in
         (* Update this node in the tree *)
         CharTable.add options ch entry
   in
      deconstruct_name options 0


(* lookup_option_core
   Lookup the option with the indicated name in the options tree.  If there
   is an exact option match in the tree, we return the option spec and an
   empty string.  If we hit end up at a node without a spec, but we are an
   UNAMBIGUOUS prefix of an option in the tree, then we return that option's
   spec, and an empty string.

   The final case is more interesting:  when we end up at a leaf, then we
   split the ``name'' we were given into a name/value pair at that point,
   and return the excess characters as the option's value.  This is how we
   determine when the value associated with an option is not delimited by a
   space.  Note that any option that is a prefix of another option cannot
   take a value in this way.
 *)
let lookup_option_core options name =
   let length = String.length name in

   (* find_branch
      Checks to see if the subtree rooted at options is a linear branch.
      If so, return the spec at the end of the branch; otherwise, raise an
      exception (assuming the option was ambiguous if the branch splits,
      or that the option is unbound if there is no branch).  *)
   let rec find_branch options =
      CharTable.fold (fun spec _ options ->
            match spec, options with
               None, SpecNode spec ->
                  Some spec
             | None, NameNode options ->
                  find_branch options
             | _ ->
                  raise (BogusArg ("Ambiguous option specified: " ^ name))) None options
   in
   let find_branch options =
      match find_branch options with
         None ->
            raise (BogusArg ("No such option: " ^ name))
       | Some spec ->
            spec
   in

   (* lookup_name
      Lookup an option in the subtree rooted at options, based on the
      substring of name beginning at offset.  *)
   let rec lookup_name options offset =
      let ch = name.[offset] in
      let offset = offset + 1 in
         if offset < length then

            (* We're not at the end of the name we're searching for
               yet; it is possible that we are looking at a name/value
               pair. *)
            match char_table_lookup options ch with
               None ->
                  (* No option with this prefix was defined *)
                  raise (BogusArg ("No such option: " ^ name))
             | Some (SpecNode (Unit _ | Set _ | Clear _ | UnitFold _ | SetFold _ | ClearFold _ | Usage )) ->
                  (* Name was too long; can not assume a name/value pair *)
                  raise (BogusArg ("No such option: " ^ name ^ " (option " ^ (String.sub name 0 offset) ^ " does not take arguments)"))
             | Some (SpecNode spec) ->
                  (* Name was too long; assume it was a name/value pair *)
                  spec, String.sub name offset (length - offset)
             | Some (NameNode options)
             | Some (SpecOrName (_, options)) ->
                  (* Still searching... *)
                  lookup_name options offset
         else

            (* Last character in the name we were given; this is either
               an exact match, or (hopefully) an unambiguous prefix of
               an option in the tree. *)
            match char_table_lookup options ch with
               None ->
                  (* Last char of name, not no option matches *)
                  raise (BogusArg ("No such option: " ^ name))
             | Some (SpecNode spec)
             | Some (SpecOrName (spec, _)) ->
                  (* Exact match to an option in the tree. *)
                  spec, ""
             | Some (NameNode options) ->
                  (* Inexact match; try to find a branch. *)
                  find_branch options, ""
   in
      lookup_name options 0

let lookup_option options name =
   if is_invert_prefix name then
      let orig_name = strip_invert_prefix name in
         try
            match lookup_option_core options orig_name with
               Set f, "" ->
                  Clear f, ""
             | SetFold f, "" ->
                  ClearFold f, ""
             | Clear f, "" ->
                  Set f, ""
             | ClearFold f, "" ->
                  SetFold f, ""
             | _ ->
                  raise (Failure "invert")
         with
            BogusArg _
          | Not_found ->
               raise (BogusArg ("No such option: " ^ orig_name ^ " (extracted from inverted: " ^ name ^ ")"))
          | Failure "invert" ->
               raise (BogusArg ("Not an invertable option: " ^ orig_name ^ " (extracted from inverted: " ^ name ^ ")"))
   else
      lookup_option_core options name


(* compute_option_tree
   Convert a sections spec into an option tree.  Can raise an exception
   if the sections spec contains duplicate options.  *)
let compute_option_tree spec =
   let options = CharTable.empty in
   let options = List.fold_left (fun options (_, spec_block) ->
      List.fold_left (fun options (name, spec, _) ->
         add_option options name spec) options spec_block) options spec
   in
      options


(***  Help System  ***)

(* Wraps at terminal width *)
let rec print_doc_string opt_width s =
   let width = Lm_termsize.stdout_width - opt_width in
   let margin = String.make (opt_width + 1) ' ' in
   let () = margin.[0] <- '\n' in
   let len = String.length s in
      if len <= width then
         print_string s
      else
         if String.rcontains_from s width ' ' then begin
            let i = String.rindex_from s width ' ' in
               print_string (String.sub s 0 i);
               print_string margin;
               print_doc_string opt_width (String.sub s (i+1) (len - i - 1))
         end else begin
            print_string (String.sub s 0 width);
            print_string margin;
            print_doc_string opt_width (String.sub s width (len - width))
         end

let usage_arg = function
   Unit _
 | Set _
 | Clear _
 | UnitFold _
 | SetFold _
 | ClearFold _
 | Usage ->
      ""
 | String _
 | StringFold _ ->
      " <string>"
 | Int _
 | IntFold _ ->
      " <number>"
 | Float _
 | FloatFold _ ->
      " <float>"
 | Rest _
 | RestFold _ ->
      " ..."

(* usage
   Display the usage message and help text for the options.  *)
let usage opt_width spec =
   List.iter (fun (opt, spec, doc) ->
         (* Descriptive text for the option argument *)
         let opt = opt ^ (usage_arg spec) in

            (* Display information on a single option. *)
            (if String.length opt > opt_width then
               (* option name too long to fit on one line *)
               printf "@ %s@ %*s" opt opt_width ""
            else
               printf "@ %-*s" opt_width opt);
            (if is_invertable_option opt spec then
               printf "*:  "
            else
               printf " :  ");
            print_doc_string (opt_width + 7) doc) spec

let usage_length (opt, spec, _) =
   String.length opt + String.length (usage_arg spec)

let usage (mode, spec) usage_msg =
   (* Display help for all sections. *)
   let opt_max_length =
      List.fold_left (fun i (_, spec) ->
         (List.fold_left (fun i opt -> max i (usage_length opt))) i spec)
      0 spec
   in
   let opt_width = min opt_max_length ((max 80 Lm_termsize.stdout_width) / 3 - 7) in
   printf "@[<v 0>%s." usage_msg;
   List.iter (fun (section, spec) ->
      printf "@ @ @[<v 3>%s:" section;
      usage opt_width spec;
      printf "@]") spec;
   (match mode with
       StrictOptions ->
          ()
     | MultiLetterOptions ->
          printf "@ Single-letter options may be concatenated as part of a single option.");
   (if List.exists (fun (_, spec) -> List.exists (fun (opt, spec, _) -> is_invertable_option opt spec) spec) spec then
      printf "@ @ (*) Prefix the option with \"--no\" to disable.");
   printf "@]@."


(***  Option Processing  ***)

(* pending_arguments
   Query for pending arguments or options.  Advances the parser for
   the current mode, and returns a pair (mode, found), where found
   is true iff there are options or arguments left to process.  *)
let advance_options mode _argv argv_length current =
   match mode with
      StrictMode
    | MultiLetterMode ->
         mode, current < argv_length
    | MultiLetterPending (opt, i) when i = String.length opt ->
         MultiLetterMode, current < argv_length
    | MultiLetterPending _ ->
         mode, true


(* get_next_arg
   Get the next argument in the argument stream.  Returns
   the argument string, as well as the new current marker.  *)
let get_next_arg opt argv argv_length current =
   if current < argv_length then
      argv.(current), current + 1
   else
      if (opt <> "") then
         raise (BogusArg ("Option " ^ opt ^ " requires an argument"))
      else
         raise (Invalid_argument "Lm_arg: internal error")

(* get_next_option
   In StrictMode, this is the same as get_next_arg.
   In MultiLetterMode, this walks letter-by-letter through
   simple options. *)
let rec get_next_option mode argv argv_length current =
   match mode with
      StrictMode ->
         let opt, current = get_next_arg "" argv argv_length current in
            opt, current, mode
    | MultiLetterMode ->
         (* See if the next argument is an option *)
         let opt, current = get_next_arg "" argv argv_length current in
            if String.length opt >= 2 && opt.[0] = '-' && is_alnum opt.[1] then
               get_next_option (MultiLetterPending (opt, 1)) argv argv_length current
            else
               opt, current, mode
    | MultiLetterPending (opt, i) ->
         let s = String.make 2 opt.[i] in
         let mode = MultiLetterPending (opt, succ i) in
            s.[0] <- '-';
            s, current, mode


(* parse
   Parses the program arguments, using a sections specification.  Any
   non-option argument is passed to the default function, in order; if
   -help or --help is intercepted on the argument stream, then the
   usage message is displayed.  *)
let fold_argv argv (mode_info, spec_info) arg default usage_msg =
   (* Always add the --help flag *)
   let spec_info = ("Help flags", ["--help", Usage, "Display a help message"]) :: spec_info in

   (* Set the current mode *)
   let mode =
      match mode_info with
         StrictOptions ->
            StrictMode
       | MultiLetterOptions ->
            MultiLetterMode
   in


   (* Convert spec into an options tree, for easier parsing *)
   let options = compute_option_tree spec_info in
   let argv_length = Array.length argv in

   (*
    * Parse a single option.
    *    arg: the fold value being computed
    *    current: the current index into argv
    *)
   let rec parse_option mode arg current =
      let mode, pending = advance_options mode argv argv_length current in
         if pending then
            (* Get the name of the option *)
            let opt, current, mode = get_next_option mode argv argv_length current in
            let current, arg =
               if String.length opt > 0 && opt.[0] = '-' then
                  (* Get information on the option *)
                  let spec, s = lookup_option options opt in

                  (* If no value was embedded in the option, but the option
                     requires a value, then grab the next argument for its
                     value.  *)
                  let s, current, arg =
                     match spec, s with
                        String _,     ""
                      | Int _,        ""
                      | Float _,      ""
                      | StringFold _, ""
                      | IntFold _,    ""
                      | FloatFold _,  "" ->
                           let s, current = get_next_arg opt argv argv_length current in
                              s, current, arg

                      | Unit _,       ""
                      | Set _,        ""
                      | Clear _,      ""
                      | Usage,        ""
                      | UnitFold _,   ""
                      | SetFold _,    ""
                      | ClearFold _,  ""

                      | String _,     _
                      | Int _,        _
                      | Float _,      _
                      | StringFold _, _
                      | IntFold _,    _
                      | FloatFold _,  _ ->
                           s, current, arg

                      | Rest f,     "" ->
                           let rec rest_function current =
                              if current < argv_length then begin
                                 f argv.(current);
                                 rest_function (current + 1)
                              end
                              else
                                 "", current, arg
                           in
                              rest_function current
                      | RestFold f,     "" ->
                           let rec rest_function arg current =
                              if current < argv_length then
                                 rest_function (f arg argv.(current)) (current + 1)
                              else
                                 "", current, arg
                           in
                              rest_function arg current
                      | _ ->
                           raise (Invalid_argument "Lm_arg: internal error")
                  in

                  (* Actually process the option. *)
                  let arg =
                     match spec with
                        Unit f ->
                           f ();
                           arg
                      | UnitFold f ->
                           f arg
                      | Set x ->
                           x := true;
                           arg
                      | SetFold f ->
                           f arg true;
                      | Clear x ->
                           x := false;
                           arg
                      | ClearFold f ->
                           f arg false
                      | String f ->
                           f s;
                           arg
                      | StringFold f ->
                           f arg s
                      | Int f ->
                           f (int_of_string s);
                           arg
                      | IntFold f ->
                           f arg (int_of_string s)
                      | Float f ->
                           f (float_of_string s);
                           arg
                      | FloatFold f ->
                           f arg (float_of_string s)
                      | Rest _
                      | RestFold _ ->
                           arg
                      | Usage ->
                           usage (mode_info, spec_info) usage_msg;
                           raise UsageError
                  in
                     current, arg
               else
                  (* Not an option; pass to the default function *)
                  let arg, rest = default arg opt in
                     if rest then
                        let rec rest_function arg current =
                           if current < argv_length then
                              let arg, _ = default arg argv.(current) in
                                 rest_function arg (current + 1)
                           else
                              current, arg
                        in
                           rest_function arg current
                     else
                        current, arg
            in
               (* We're done with this option, advance to next *)
               parse_option mode arg current
         else
            current, arg
   in
   let _, arg = parse_option mode arg 1 in
      arg

let fold spec arg default usage_msg =
   fold_argv Sys.argv spec arg default usage_msg

let parse_argv argv spec default usage_msg =
   fold_argv argv spec () (fun () opt -> default opt, false) usage_msg

let parse spec default usage_msg =
   fold spec () (fun () opt -> default opt, false) usage_msg
