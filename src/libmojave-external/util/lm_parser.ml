(*
 * Generic parser generator.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_debug
open Lm_hash
open Lm_printf
open Lm_location
open Lm_int_set

let debug_parse =
   create_debug (**)
      { debug_name = "parse";
        debug_description = "Debug the parseer";
        debug_value = false
      }

let debug_parsegen =
   create_debug (**)
      { debug_name = "parsegen";
        debug_description = "Debug the parser generator";
        debug_value = false
      }

let debug_parsetiming =
   create_debug (**)
      { debug_name = "parsetiming";
        debug_description = "Display timing statistics for the parser generator";
        debug_value = false
      }

let debug_parse_conflict_is_warning =
   create_debug (**)
      { debug_name = "parse_conflict_is_warning";
        debug_description = "Do not abort on grammar conflicts";
        debug_value = false
      }

(*
 * A precedence directive is left-associative, right-associative,
 * or nonassociative.
 *)
(* %%MAGICBEGIN%% *)
type assoc =
   LeftAssoc
 | RightAssoc
 | NonAssoc
 | NoneAssoc
(* %%MAGICEND%% *)

let pp_print_assoc buf assoc =
   let s =
      match assoc with
         LeftAssoc ->
            "left"
       | RightAssoc ->
            "right"
       | NonAssoc ->
            "nona"
       | NoneAssoc ->
            "none"
   in
      pp_print_string buf s

(************************************************************************
 * Tools for profiling.
 *)
let time_start () =
   Unix.gettimeofday(), Unix.times ()

let time_print debug t1 t2 =
   if !debug_parsetiming then
      let now1, t1 = t1 in
      let now2, t2 = t2 in
      let now3 = Unix.gettimeofday () in
      let t3 = Unix.times () in
      let total = now3 -. now1 in
      let utime = t3.Unix.tms_utime -. t1.Unix.tms_utime in
      let stime = t3.Unix.tms_stime -. t1.Unix.tms_stime in
      let diff_total = now3 -. now2 in
      let diff_utime = t3.Unix.tms_utime -. t2.Unix.tms_utime in
      let diff_stime = t3.Unix.tms_stime -. t2.Unix.tms_stime in
         eprintf "Time: %2.2f real %2.2f user %2.2f sys; %2.2f real %2.2f user %2.2f sys (%s)@." (**)
            diff_total diff_utime diff_stime total utime stime debug;
         now3, t3
   else
      t1

(************************************************************************
 * Precedences.
 *)
module type PrecedenceArg =
sig
   type t
   type precedence

   (* Precedence control *)
   val prec_min       : precedence
   val prec_max       : precedence

   (* Precedence tables *)
   val empty          : t
   val create_prec_lt : t -> precedence -> assoc  -> t * precedence
   val create_prec_gt : t -> precedence -> assoc  -> t * precedence

   (* Print a precedence *)
   val pp_print_prec  : t -> out_channel -> precedence -> unit

   (* Comparison *)
   val add_assoc      : t -> precedence -> assoc -> t
   val assoc          : t -> precedence -> assoc
   val compare        : t -> precedence -> precedence -> int

   (* Tables and sets *)
   module PrecTable   : Lm_map_sig.LmMap with type key = precedence
end

exception ParseError of loc * string

(*
 * The parser is parameterized over symbol and action names.
 *)
module type ParserArg =
sig
   (* Variable names: the names of terminals and nonterminals *)
   type symbol

   (* A symbol to represent eof *)
   val eof : symbol

   (* For debugging *)
   val to_string : symbol -> string
   val pp_print_symbol : out_channel -> symbol -> unit

   (* Sets and tables *)
   val hash_symbol : symbol -> int
   val compare_symbol : symbol -> symbol -> int

   (* Names of semantic actions *)
   type action

   (* For debugging *)
   val pp_print_action : out_channel -> action -> unit

   (* For set and table building *)
   val hash_action : action -> int
   val compare_action : action -> action -> int
end

module MakeParser (Arg : ParserArg) (Precedence : PrecedenceArg) =
struct
   open Precedence

   (************************************************************************
    * Types.
    *)

   (*
    * Type of lexing tokens.
    *)
   type ('a, 'b) lexer = 'a -> Arg.symbol * loc * 'a * 'b
   type ('a, 'b) eval =
      'a ->                     (* The argument *)
      Arg.action ->             (* The name of the action *)
      loc ->                    (* Location of the production *)
      'b list ->                (* The arguments to the action *)
      'a * 'b                   (* The result of the semantic action *)

   (************************************************************************
    * Internal versions of types.
    *)
   module VarArg =
   struct
      type t = Arg.symbol

      let debug = "Var"
      let hash = Arg.hash_symbol
      let compare = Arg.compare_symbol
   end;;

   module Var = MakeHash (VarArg);;
   module VarSet    = Lm_set.LmMake (Var);;
   module VarTable  = Lm_map.LmMake (Var);;
   module VarMTable = Lm_map.LmMakeList (Var);;

   module IVar = MakeHashCons (VarArg);;
   module IVarSet = Lm_set.LmMake (IVar);;
   module IVarTable = Lm_map.LmMake (IVar);;
   module IVarMTable = Lm_map.LmMakeList (IVar);;

   type var = Var.t
   type ivar = IVar.t

   (*
    * Also hash the actions.
    *)
   module ActionArg =
   struct
      type t = Arg.action

      let debug = "Action"
      let hash = Arg.hash_action
      let compare = Arg.compare_action
   end;;

   module Action = MakeHash (ActionArg);;
   module ActionSet = Lm_set.LmMake (Action);;

   module IAction = MakeHashCons (ActionArg);;
   module IActionSet = Lm_set.LmMake (IAction);;

   type action = Action.t
   type iaction = IAction.t

   (*
    * A production item is represents a production with
    * a current position.
    *)
   (* %%MAGICBEGIN%% *)
   type prod_item_core =
      { prod_item_name   : ivar;
        prod_item_left   : ivar list;       (* Reverse order *)
        prod_item_right  : ivar list;
        prod_item_action : iaction;
        prod_item_prec   : precedence
      }
   (* %%MAGICEND%% *)

   (*
    * Hash utilities.
    *)
   let ivar_list_hash hash vars =
      List.fold_left (fun hash v ->
            hash_combine hash (IVar.hash v)) hash vars

   let rec ivar_list_compare vars1 vars2 =
      match vars1, vars2 with
         v1 :: vars1, v2 :: vars2 ->
            let cmp = IVar.compare v1 v2 in
               if cmp = 0 then
                  ivar_list_compare vars1 vars2
               else
                  cmp
       | [], [] ->
            0
       | _ :: _, [] ->
            1
       | [], _ :: _ ->
            -1

   module ProdItemArg =
   struct
      type t = prod_item_core

      let debug = "ProdItem"

      let hash item =
         let { prod_item_name   = name;
               prod_item_left   = left;
               prod_item_right  = right;
               prod_item_action = action
             } = item
         in
         let hash = hash_combine (IVar.hash name) (IAction.hash action) in
         let hash = ivar_list_hash hash left in
         let hash = ivar_list_hash hash right in
            hash

      let compare item1 item2 =
         let { prod_item_name   = name1;
               prod_item_left   = left1;
               prod_item_right  = right1;
               prod_item_action = action1;
               prod_item_prec   = prec1
             } = item1
         in
         let { prod_item_name   = name2;
               prod_item_left   = left2;
               prod_item_right  = right2;
               prod_item_action = action2;
               prod_item_prec   = prec2
             } = item2
         in
         let cmp = IVar.compare name1 name2 in
            if cmp = 0 then
               let cmp = IAction.compare action1 action2 in
                  if cmp = 0 then
                     let cmp = ivar_list_compare left1 left2 in
                        if cmp = 0 then
                           let cmp = ivar_list_compare right1 right2 in
                              if cmp = 0 then
                                 Pervasives.compare prec1 prec2
                              else
                                 cmp
                        else
                           cmp
                  else
                     cmp
            else
               cmp
   end

   module ProdItem      = MakeHashCons (ProdItemArg);;
   module ProdItemSet   = Lm_set.LmMake (ProdItem);;
   module ProdItemTable = Lm_map.LmMake (ProdItem);;

   type prod_item = ProdItem.t

   (*
    * An LR(0) state is a set of ProdItems, and
    * a closure, which is a set of nonterminals.
    *)
   (* %%MAGICBEGIN%% *)
   type info_state =
      { info_state_items   : ProdItemSet.t;
        info_state_closure : IVarSet.t
      }
   (* %%MAGICEND%% *)

   module StateArg =
   struct
      type t = info_state

      let debug = "State"

      (*
       * We don't need to hash or compare the closure,
       * because it is uniquely determined by the items.
       *)
      let hash state =
         ProdItemSet.fold (fun hash item ->
               hash_combine hash (ProdItem.hash item)) 0 state.info_state_items

      let compare state1 state2 =
         ProdItemSet.compare state1.info_state_items state2.info_state_items
   end;;

   module State = MakeHashCons (StateArg);;
   module StateSet = Lm_set.LmMake (State);;
   module StateTable = Lm_map.LmMake (State);;

   (*
    * A StateItem is a pair of the state and the prod_item.
    *)
   module StateItemArg =
   struct
      type t = State.t * ProdItem.t

      let debug = "StateItem"

      let hash (state, item) =
         hash_combine (State.hash state) (ProdItem.hash item)

      let compare (state1, item1) (state2, item2) =
         let cmp = ProdItem.compare item1 item2 in
            if cmp = 0 then
               State.compare state1 state2
            else
               cmp
   end;;

   module StateItem = MakeHashCons (StateItemArg);;
   module StateItemSet = Lm_set.LmMake (StateItem);;
   module StateItemTable = Lm_map.LmMake (StateItem);;

   (************************************************
    * The grammar.
    *)

   (*
    * A production item is the production with position.
    * It does not include the lookahead.
    *
    * name ::= left . right
    *
    * We also keep the precedence of the production,
    * and its semantic action name.
    *)

   (* %%MAGICBEGIN%% *)

   (*
    * A single production.
    *)
   type prod =
      { prod_name   : var;
        prod_right  : var list;
        prod_action : action;
        prod_prec   : precedence
      }

   (*
    * A grammar has a set of symbols, productions,
    * and precedences.
    *)
   type grammar =
      { gram_prod          : prod VarMTable.t;
        gram_prec          : precedence VarTable.t;
        gram_prec_table    : Precedence.t;
        gram_start_symbols : VarSet.t
      }

   (************************************************
    * The PDA.
    *)

   (*
    * An action is shift, reduce, or accept.
    *)
   type 'a pda_action =
      ReduceAction of iaction * ivar * int  (* semantic action, production name, #args *)
    | GotoAction   of 'a
    | AcceptAction
    | ErrorAction

   (*
    * We may reduce states without lookahead,
    * and we may accept.
    *)
   type pda_reduce =
      ReduceNone
    | ReduceNow    of iaction * ivar * int
    | ReduceAccept of iaction * ivar * int

   (*
    * The PDA transition table.
    *
    * The pda_info is *purely* for debugging, so access
    * does not have to be fast.
    *)
   type pda_item =
      { pda_item_left      : ivar list;  (* Reverse order *)
        pda_item_right     : ivar list
      }

   type pda_state_info =
      { pda_items     : pda_item list;
        pda_next      : IVarSet.t
      }

   type pda_state =
      { pda_delta   : int pda_action IVarTable.t;
        pda_reduce  : pda_reduce;
        pda_info    : pda_state_info
      }

   type hash_state =
      { hash_ivar_state          : IVar.state;
        hash_iaction_state       : IAction.state;
        hash_prod_item_state     : ProdItem.state;
        hash_state_state         : State.state
      }

   type pda =
      { pda_start_states    : int IVarTable.t;
        pda_states          : pda_state array;
        pda_hash            : hash_state
      }

   (*
    * The actual machine has a grammar and an optional pda.
    *)
   type t =
      { parse_grammar     : grammar;
        mutable parse_pda : pda option
      }
   (* %%MAGICEND%% *)

   (*
    * Run time info.
    *)
   type ('a, 'b) run =
      { run_states        : pda_state array;
        run_lexer         : ('a, 'b) lexer;
        run_eval          : ('a, 'b) eval
      }

   (************************************************
    * Building the PDA.
    *)

   (*
    * Lookahead expressions.
    * LookAheadConst vars: the vars are spontaneously generated
    * LoadAheadProp vars: the vars are spontaneously generated, and the item vars are propagated.
    *)
   type lookahead =
      LookAheadConst of IVarSet.t
    | LookAheadProp of IVarSet.t

   (*
    * The info for constructing the PDA.
    *    info_gram     : the grammar
    *    info_nullable : the nonterminals that derive epsilon
    *    info_first    : the terminals that may start a production
    *)
   type info =
      { info_grammar             : grammar;
        info_prod                : ProdItem.t list IVarTable.t;
        info_start_symbols       : IVarSet.t;
        info_prec                : precedence IVarTable.t;
        info_nullable            : IVarSet.t;
        info_first               : IVarSet.t IVarTable.t;
        info_head_delta          : ProdItemSet.t IVarTable.t IVarTable.t;
        info_head_lookahead      : lookahead IVarTable.t IVarTable.t;
        info_eof                 : IVar.t;
        info_hash                : hash_state;
        info_hash_state_item     : StateItem.state
      }

   (*
    * A prop_edge is used to specify that we should
    * propagate from one item to another.
    *)
   type prop_edge =
      { prop_edge_src   : StateItem.t;          (* state, item *)
        prop_edge_dst   : StateItemSet.t        (* state, item *)
      }

   (*
    * The prop_entry is the lookahead we are computing.
    *)
   type prop_entry =
      { prop_state_item      : StateItem.t;
        mutable prop_changed : bool;
        mutable prop_vars    : IVarSet.t
      }

   (*
    * A state element is a set of items, with lookaheads for each.
    *)
   type info_item =
      { info_item_index   : int;
        info_item_empties : prop_entry list;
        info_item_closure : prop_entry list;
        info_item_entries : prop_entry array
      }

   (************************************************************************
    * Printing and errors.
    *)
   let string_of_var v =
      Arg.to_string (Var.get v)

   let pp_print_var buf v =
      Arg.pp_print_symbol buf (Var.get v)

   let rec pp_print_vars buf vl =
      List.iter (fun v -> fprintf buf " %a" pp_print_var v) vl

   let pp_print_var_set buf s =
      VarSet.iter (fun v ->
            fprintf buf "@ %a" pp_print_var v) s

   let pp_print_var_table buf table =
      VarTable.iter (fun v s ->
            fprintf buf "@ @[<b 3>%a:%a@]" (**)
               pp_print_var v
               pp_print_var_set s) table

   let pp_print_action buf action =
      Arg.pp_print_action buf (Action.get action)

   let string_of_ivar hash v =
      Arg.to_string (IVar.get hash.hash_ivar_state v)

   let pp_print_ivar hash buf v =
      Arg.pp_print_symbol buf (IVar.get hash.hash_ivar_state v)

   let rec pp_print_ivars hash buf vl =
      List.iter (fun v -> fprintf buf " %a" (pp_print_ivar hash) v) vl

   let pp_print_ivar_set hash buf s =
      IVarSet.iter (fun v ->
            fprintf buf "@ %a" (pp_print_ivar hash) v) s

   let pp_print_ivar_table hash buf table =
      IVarTable.iter (fun v s ->
            fprintf buf "@ @[<b 3>%a:%a@]" (**)
               (pp_print_ivar hash) v
               (pp_print_ivar_set hash) s) table

   let pp_print_iaction hash buf action =
      Arg.pp_print_action buf (IAction.get hash.hash_iaction_state action)

   let pp_print_prod gram buf item =
      let { prod_action = action;
            prod_prec   = pre;
            prod_name   = name;
            prod_right  = right
          } = item
      in
         fprintf buf "@[<v 3>%a ::=%a [%a, %a]@]" (**)
            pp_print_var name
            pp_print_vars right
            pp_print_action action
            (Precedence.pp_print_prec gram.gram_prec_table) pre

   let pp_print_grammar buf gram =
      let { gram_prod = prods;
            gram_prec = precs;
            gram_prec_table = prec_table;
            gram_start_symbols = starts
          } = gram
      in
         fprintf buf "@[<v 3>Grammar:";
         VarTable.iter (fun v pre ->
               fprintf buf "@ prec %a = %a" (**)
                  pp_print_var v
                  (Precedence.pp_print_prec prec_table) pre) precs;
         VarSet.iter (fun v ->
               fprintf buf "@ start %a" pp_print_var v) starts;
         VarMTable.iter_all (fun _ prods ->
               List.iter (fun prod -> fprintf buf "@ %a" (pp_print_prod gram) prod) prods) prods;
         fprintf buf "@]"

   let pp_print_pda_action hash buf action =
      match action with
         ReduceAction (action, _, _) ->
            fprintf buf "reduce %a" (pp_print_iaction hash) action
       | GotoAction state ->
            fprintf buf "goto %d" state
       | ErrorAction ->
            pp_print_string buf "error"
       | AcceptAction  ->
            pp_print_string buf "accept"

   let pp_print_pda_actions info buf actions =
      IVarTable.iter (fun v action ->
            fprintf buf "@ %a: %a" (pp_print_ivar info) v (pp_print_pda_action info) action) actions

   let pp_print_prod_item_core info buf item =
      let { prod_item_action = action;
            prod_item_name   = name;
            prod_item_left   = left;
            prod_item_right  = right
          } = item
      in
      let hash = info.info_hash in
         fprintf buf "%a ::=%a .%a (%a)" (**)
            (pp_print_ivar hash) name
            (pp_print_ivars hash) (List.rev left)
            (pp_print_ivars hash) right
            (pp_print_iaction hash) action

   let pp_print_prod_item info buf item =
      pp_print_prod_item_core info buf (ProdItem.get info.info_hash.hash_prod_item_state item)

   let pp_print_prod_item_set info buf items =
      ProdItemSet.iter (fun item ->
            fprintf buf "@ %a" (pp_print_prod_item info) item) items

   let pp_print_state info buf state =
      let { info_state_items = items } = State.get info.info_hash.hash_state_state state in
         eprintf "@[<v 3>State %d" (State.hash state);
         pp_print_prod_item_set info buf items;
         eprintf "@]"

   let pp_print_info_item info buf info_item =
      let { info_hash = hash;
            info_hash_state_item = hash_state_item
          } = info
      in
      let { info_item_index = index;
            info_item_entries = entries
          } = info_item
      in
         fprintf buf "@[<v 3>State %d:" index;
         Array.iter (fun entry ->
               let { prop_state_item = state_item;
                     prop_vars = lookahead
                   } = entry
               in
               let _, prod_item = StateItem.get hash_state_item state_item in
                  fprintf buf "@ @[<hv 3>%a@ @[<b 2>#%a@]@]" (pp_print_prod_item info) prod_item (pp_print_ivar_set hash) lookahead) entries;
         fprintf buf "@]"

   let pp_print_info buf info =
      let { info_grammar = gram;
            info_nullable = nullable;
            info_first = first;
            info_hash = hash
          } = info
      in
         fprintf buf "@[<v 0>%a" pp_print_grammar gram;
         fprintf buf "@ @[<b 3>Nullable:%a@]" (pp_print_ivar_set hash) nullable;
         fprintf buf "@ @[<v 3>First:%a@]" (pp_print_ivar_table hash) first;
         fprintf buf "@]"

   let pp_print_lookahead hash buf look =
      match look with
         LookAheadConst set ->
            fprintf buf "@[<b 3>const%a@]" (pp_print_ivar_set hash) set
       | LookAheadProp set ->
            fprintf buf "@[<b 3>prop%a@]" (pp_print_ivar_set hash) set

   (*
    * Print a transition table.
    *)
   let pp_print_delta info buf delta =
      let pp_print_ivar = pp_print_ivar info.info_hash in
      let pp_print_prod_item_set = pp_print_prod_item_set info in
         IVarTable.iter (fun v delta ->
               fprintf buf "@ @[<v 3>%a ->" pp_print_ivar v;
               IVarTable.iter (fun v item ->
                     fprintf buf "@ @[<v 3>%a ->%a@]" pp_print_ivar v pp_print_prod_item_set item) delta;
               fprintf buf "@]") delta

   (*
    * Print the lookahead table.
    *)
   let pp_print_look_table info buf table =
      let hash = info.info_hash in
      let pp_print_ivar = pp_print_ivar hash in
      let pp_print_lookahead = pp_print_lookahead hash in
         IVarTable.iter (fun v table ->
            fprintf buf "@ @[<v 3>%a ->" pp_print_ivar v;
               IVarTable.iter (fun v look ->
                     fprintf buf "@ %a -> %a" pp_print_ivar v pp_print_lookahead look) table;
               fprintf buf "@]") table

   (************************************************************************
    * Grammar construction.
    *)

   (*
    * Empty grammar has the basic precedences.
    *)
   let empty_grammar =
      { gram_prod          = VarMTable.empty;
        gram_prec          = VarTable.empty;
        gram_prec_table    = Precedence.empty;
        gram_start_symbols = VarSet.empty
      }

   (*
    * Add a start symbol.
    *)
   let add_start gram sym =
      { gram with gram_start_symbols = VarSet.add gram.gram_start_symbols (Var.create sym) }

   (*
    * Add a symbol at a given precedence level.
    *)
   let add_prec gram pre v =
      { gram with gram_prec = VarTable.add gram.gram_prec v pre }

   (*
    * Find the precedence level for a symbol.
    *)
   let find_prec gram v =
      VarTable.find gram.gram_prec v

   (*
    * Add a production.
    * If the precedence is not specified, it is the precedence
    * of the rightmost variable that has a precedence.
    *)
   let add_production gram action v rhs pre =
      let pre =
         match pre with
            Some sym ->
               find_prec gram sym
          | None ->
               List.fold_left (fun pre v ->
                     try VarTable.find gram.gram_prec v with
                        Not_found ->
                           pre) prec_min rhs
      in
      let prod =
         { prod_action  = action;
           prod_name    = v;
           prod_right   = rhs;
           prod_prec    = pre
         }
      in
         { gram with gram_prod = VarMTable.add gram.gram_prod v prod }

   (*
    * Remove a production.
    * We don't index by production name, so this takes linear time
    * in the number of productions.
    *)
   let remove_production gram action =
      let table =
         VarMTable.mapi_all (fun _ prods ->
               List.filter (fun prod -> prod.prod_action <> action) prods) gram.gram_prod
      in
         { gram with gram_prod = table }

   (*
    * Precedence union is a little hard.
    * Suppose the second grammar contains some precedence
    * levels that do not occur in the first grammar.  We
    * have to insert some levels, and we have to figure out
    * where to put them.
    *
    * The basic idea is to build an inverse table for the
    * second grammar.  Then sort this grammar, and walk
    * through each level.  If it exists in the first grammar,
    * keep it.  Otherwise add a new level, and continue.
    *)
   let rec find_existing_prec precs vars =
      match vars with
         [] ->
            None
       | v :: vars ->
            try Some (VarTable.find precs v) with
               Not_found ->
                  find_existing_prec precs vars

   let add_precs precs vars pre =
      List.fold_left (fun precs v ->
            VarTable.add precs v pre) precs vars

   let union_prec prec1 table1 prec2 table2 =
      (* Build an inverse precedence table for grammar2 *)
      let inv_table =
         VarTable.fold (fun inv_table v pre ->
               PrecTable.filter_add inv_table pre (fun vars ->
                     let vars =
                        match vars with
                           Some vars ->
                              vars
                         | None ->
                              []
                     in
                        v :: vars)) PrecTable.empty prec2
      in

      (* Sort the precedences in grammar2 *)
      let prec_list =
         PrecTable.fold (fun prec_list pre _ ->
               pre :: prec_list) [] inv_table
      in
      let prec_list = List.sort (Precedence.compare table2) prec_list in

      (* Initial translation *)
      let translate = PrecTable.empty in
      let translate = PrecTable.add translate prec_min prec_min in
      let translate = PrecTable.add translate prec_max prec_max in

      (* Walk through each level, and create it if it doesn't already exist *)
      let translate, precs, table, _ =
         List.fold_left (fun (translate, precs, table, prev_prec) pre ->
               let vars = PrecTable.find inv_table pre in
               let table, current_prec =
                  match find_existing_prec precs vars with
                     Some current_prec ->
                        table, current_prec
                   | None ->
                        let assoc = Precedence.assoc table2 pre in
                           Precedence.create_prec_gt table prev_prec assoc
               in
               let translate = PrecTable.add translate pre current_prec in
               let precs = add_precs precs vars current_prec in
                  translate, precs, table, current_prec) (translate, prec1, table1, Precedence.prec_min) prec_list
      in
         translate, precs, table

   (*
    * Union of two grammars.
    *)
   let union_grammar gram1 gram2 =
      let { gram_prod          = prod1;
            gram_prec          = prec1;
            gram_prec_table    = prec_table1;
            gram_start_symbols = start1
          } = gram1
      in
      let { gram_prod          = prod2;
            gram_prec          = prec2;
            gram_prec_table    = prec_table2;
            gram_start_symbols = start2
          } = gram2
      in

      (* Compute the new precedence table *)
      let prec_translate, precs, prec_table = union_prec prec1 prec_table1 prec2 prec_table2 in

      (* Get the complete set of actions for the first parser *)
      let actions =
         VarMTable.fold_all (fun actions _ prods ->
               List.fold_left (fun actions prod ->
                     let action = prod.prod_action in
                        ActionSet.add actions action) actions prods) ActionSet.empty prod1
      in

      (* Take the union of the productions *)
      let changed, prods =
         VarMTable.fold_all (fun (changed, prods) _ prodlist ->
               List.fold_left (fun (changed, prods) prod ->
                     let { prod_action = action;
                           prod_name   = name;
                           prod_prec   = pre
                         } = prod
                     in
                        if ActionSet.mem actions action then
                           changed, prods
                        else
                           let prod = { prod with prod_prec = PrecTable.find prec_translate pre } in
                              true, VarMTable.add prods name prod) (changed, prods) prodlist) (false, prod1) prod2
      in

      (* Union of the start symbols *)
      let start = VarSet.union start1 start2 in

      (* Has anything changed? *)
      let changed =
         changed
         || (VarTable.cardinal precs <> VarTable.cardinal prec1)
         || (VarSet.cardinal start <> VarSet.cardinal start1)
      in

      (* New grammar *)
      let gram =
         { gram_prod          = prods;
           gram_prec          = precs;
           gram_prec_table    = prec_table;
           gram_start_symbols = start
         }
      in
         changed, gram

   (*
    * Debugging version.
    *)
   let union_grammar gram1 gram2 =
      if !debug_parsegen then
         eprintf "@[<v 3>Grammar union:@ @[<hv 3>Grammar1:@ %a@]@ @[<hv 3>Grammar2:@ %a@]@]@." (**)
            pp_print_grammar gram1
            pp_print_grammar gram2;
      let changed, gram = union_grammar gram1 gram2 in
         if !debug_parsegen then
            eprintf "@[<v 3>Grammar union %b:@ %a@]@." (**)
               changed pp_print_grammar gram;
         changed, gram

   (************************************************************************
    * Initial info for LALR(1) construction.
    *)

   (*
    * A nonterminal is nullable if all variables on the rhs are nullable.
    *)
   let nullable hash prods =
      let prod_state = hash.hash_prod_item_state in
      let step nullable prods =
         IVarTable.fold (fun nullable v prods ->
               if IVarSet.mem nullable v then
                  nullable
               else if List.exists (fun prod ->
                             List.for_all (IVarSet.mem nullable) (**)
                                (ProdItem.get prod_state prod).prod_item_right) prods
               then
                  IVarSet.add nullable v
               else
                  nullable) nullable prods
      in
      let rec fixpoint nullable prods =
         let nullable' = step nullable prods in
            if IVarSet.cardinal nullable' <> IVarSet.cardinal nullable then
               fixpoint nullable' prods
            else
               nullable
      in
         fixpoint IVarSet.empty prods

   (*
    * Find the sets of first symbols that can start productions.
    *)
   let rec first_rhs nullable first set rhs =
      match rhs with
         v :: rhs ->
            let set = IVarSet.union set (IVarTable.find first v) in
               if IVarSet.mem nullable v then
                  first_rhs nullable first set rhs
               else
                  set
       | [] ->
            set

   let first hash prods nullable =
      let prod_state = hash.hash_prod_item_state in
      let step first prods =
         IVarTable.fold (fun (first, changed) _ prods ->
               List.fold_left (fun (first, changed) prod ->
                     let { prod_item_name = x;
                           prod_item_right = rhs
                         } = ProdItem.get prod_state prod
                     in
                     let set = IVarTable.find first x in
                     let set' = first_rhs nullable first set rhs in
                     let set, changed =
                        if changed || IVarSet.cardinal set' <> IVarSet.cardinal set then
                           set', true
                        else
                           set, false
                     in
                     let first = IVarTable.add first x set in
                        first, changed) (first, changed) prods) (first, false) prods
      in
      let rec fixpoint first prods =
         let first, changed = step first prods in
            if changed then
               fixpoint first prods
            else
               first
      in

      (* Initialize with the terminals *)
      let vars =
         IVarTable.fold (fun vars v prods ->
               let vars = IVarSet.add vars v in
                  List.fold_left (fun vars prod ->
                        List.fold_left IVarSet.add vars (ProdItem.get prod_state prod).prod_item_right) vars prods) IVarSet.empty prods
      in
      let first =
         IVarSet.fold (fun first v ->
               if IVarTable.mem prods v then
                  IVarTable.add first v IVarSet.empty
               else
                  IVarTable.add first v (IVarSet.singleton v)) IVarTable.empty vars
      in
         fixpoint first prods

   (************************************************************************
    * LR(0) construction.
    *)

   (*
    * Get the set of first symbols that can begin a list.
    *)
   let lookahead info rhs =
      let { info_first = first;
            info_nullable = nullable
          } = info
      in
      let rec search set rhs =
         match rhs with
            v :: rhs ->
               let set = IVarSet.union (IVarTable.find first v) set in
                  if IVarSet.mem nullable v then
                     search set rhs
                  else
                     LookAheadConst set
          | [] ->
               LookAheadProp set
      in
         search IVarSet.empty rhs

   (*
    * Concatenate lookahead sets.
    *)
   let lookahead_concat look1 look2 =
      match look1, look2 with
         LookAheadConst _, _ ->
            look1
       | LookAheadProp set1, LookAheadConst set2 ->
            LookAheadConst (IVarSet.union set1 set2)
       | LookAheadProp set1, LookAheadProp set2 ->
            LookAheadProp (IVarSet.union set1 set2)

   (*
    * Two different paths for lookahead.
    *)
   let lookahead_union look1 look2 =
      match look1, look2 with
         LookAheadConst set1, LookAheadConst set2 ->
            LookAheadConst (IVarSet.union set1 set2)
       | LookAheadProp set1, LookAheadConst set2
       | LookAheadConst set1, LookAheadProp set2
       | LookAheadProp set1, LookAheadProp set2 ->
            LookAheadProp (IVarSet.union set1 set2)

   (*
    * Comparison.
    *)
   let lookahead_equal look1 look2 =
      match look1, look2 with
         LookAheadConst set1, LookAheadConst set2
       | LookAheadProp set1, LookAheadProp set2 ->
            IVarSet.equal set1 set2
       | LookAheadConst _, LookAheadProp _
       | LookAheadProp _, LookAheadConst _ ->
            false

   (*
    * Split into a pair.
    *)
   let lookahead_pair look =
      match look with
         LookAheadConst set ->
            false, set
       | LookAheadProp set ->
            true, set

   let lookahead_set look =
      match look with
         LookAheadConst set
       | LookAheadProp set ->
            set

   (************************************************
    * Produce the derivation table for items where
    * the dot is at the head.
    *
    * We want a transition table, as well as lookaheads.
    *
    * The transition table gives a set of transitions
    * for nonterminal (symbol -> symbol -> ProdItemSet.t),
    * where and entry (v1 -> v2 -> items) states that:
    *    if looking at v1,
    *    you can goto on v2,
    *    with the resulting items.
    *
    * The pre-lookahead gives a similar table.  The entry
    * (v1 -> v2 -> look) means:
    *    For nonterminal v1, there is an item
    *       . v2 right_2
    *    where v2 is a nonterminal and
    *       look = LOOKAHEAD(right_2)
    *)

   (*
    * Compute the transition function and lookahead table
    * for an item.
    *)
   let build_head_item info delta lookaheads item =
      let core = ProdItem.get info.info_hash.hash_prod_item_state item in
         match core.prod_item_right with
            v :: right ->
               let core =
                  { core with prod_item_left = [v];
                              prod_item_right = right
                  }
               in
               let item = ProdItem.create info.info_hash.hash_prod_item_state core in
               let delta =
                  IVarTable.filter_add delta v (fun items ->
                        match items with
                           Some items ->
                              ProdItemSet.add items item
                         | None ->
                              ProdItemSet.singleton item)
               in
               let look1 = lookahead info right in
               let lookaheads =
                  if IVarTable.mem info.info_prod v then
                     IVarTable.filter_add lookaheads v (fun look2 ->
                           match look2 with
                              Some look2 ->
                                 lookahead_union look1 look2
                            | None ->
                                 look1)
                  else
                     lookaheads
               in
                  delta, lookaheads
          | [] ->
               delta, lookaheads

   let build_head_items info items =
      List.fold_left (fun (delta, lookaheads) item ->
            build_head_item info delta lookaheads item) (IVarTable.empty, IVarTable.empty) items

   (*
    * Solve the lookahead functions.
    * This is a fixpoint, but it should be pretty cheap.
    *
    * This flatten lookahead is defined as follows.
    * The table has an entry (v1 -> v2 -> look) iff:
    *    1. There is a derivation (v1 --> v2 right2)
    *    2. v2 is a nonterminal
    *    3. look = LOOKAHEAD(right2)
    * The main issue is that right2 may be constructed from
    * the right-hand-sides of several productions.
    *)
   let build_lookahead_item _info table v =
      (* Fixpoint *)
      let step venv =
         IVarTable.fold (fun (venv, changed) v e1 ->
               let next = IVarTable.find table v in
                  IVarTable.fold (fun (venv, changed) v e2 ->
                        let e = lookahead_concat e2 e1 in
                           try
                              let e_old = IVarTable.find venv v in
                              let e_new = lookahead_union e_old e in
                                 if lookahead_equal e_old e_new then
                                    venv, changed
                                 else
                                    IVarTable.add venv v e_new, true
                           with
                              Not_found ->
                                 IVarTable.add venv v e, true) (venv, changed) next) (venv, false) venv
      in
      let rec fixpoint venv =
         let venv, changed = step venv in
            if changed then
               fixpoint venv
            else
               venv
      in
         fixpoint (IVarTable.add IVarTable.empty v (LookAheadProp IVarSet.empty))

   let build_lookaheads info table =
      IVarTable.mapi (fun v _ ->
            build_lookahead_item info table v) table

   (*
    * Main function.
    *)
   let build_head_table info start now =
      let delta_table, look_table =
         IVarTable.fold (fun (delta_table, look_table) v items ->
               let delta, lookaheads = build_head_items info items in
               let delta_table = IVarTable.add delta_table v delta in
               let look_table = IVarTable.add look_table v lookaheads in
                  delta_table, look_table) (IVarTable.empty, IVarTable.empty) info.info_prod
      in
      let () =
         if !debug_parsegen then
            eprintf "@[<v 3>Head table:@ @[<v 3>Delta:%a@]@ @[<v 3>Lookahead:%a@]@]@." (**)
               (pp_print_delta info) delta_table
               (pp_print_look_table info) look_table
      in
      let now = time_print "Head items" start now in
      let look_table = build_lookaheads info look_table in
      let () =
         if !debug_parsegen then
            eprintf "@[<v 3>Closed lookahead:%a@]@." (**)
               (pp_print_look_table info) look_table
      in
      let now = time_print "Lookaheads" start now in
         now, delta_table, look_table

   (************************************************
    * Producing the state table.
    *)

   (*
    * Produce a transition table by shifting.
    * We take a set of items, and produce a IVarTable
    * containing the next states.
    *)
   let shift_items info items =
      let hash = info.info_hash.hash_prod_item_state in
         ProdItemSet.fold (fun delta prod_item ->
               let core = ProdItem.get hash prod_item in
               let { prod_item_left = left;
                     prod_item_right = right
                   } = core
               in
                  match right with
                     v :: right ->
                        let core =
                           { core with prod_item_left = v :: left;
                                       prod_item_right = right
                           }
                        in
                        let item = ProdItem.create hash core in
                           IVarTable.filter_add delta v (fun items ->
                                 match items with
                                    Some items ->
                                       ProdItemSet.add items item
                                  | None ->
                                       ProdItemSet.singleton item)
                   | [] ->
                        delta) IVarTable.empty items

   (*
    * Shift a closure, given the current state.
    * This produces a IVarTable that defines the next
    * state for each symbol.
    *)
   let shift_state info state =
      let core = State.get info.info_hash.hash_state_state state in
      let { info_state_items = items;
            info_state_closure = next
          } = core
      in
      let head_table = info.info_head_delta in
      let delta = shift_items info items in
         IVarSet.fold (fun delta v ->
               let head_delta = IVarTable.find head_table v in
                  IVarTable.fold (fun delta v items ->
                        IVarTable.filter_add delta v (fun current_items ->
                              match current_items with
                                 Some current_items ->
                                    ProdItemSet.union current_items items
                               | None ->
                                    items)) delta head_delta) delta next

   (*
    * A closure is represented by its kernel (all the
    * items where the dot is not at the front), plus
    * the names of all the productions where the dot
    * is at the front.
    *)
   let closure info items =
      let look_table = info.info_head_lookahead in
      let closure =
         ProdItemSet.fold (fun closure item ->
               let core = ProdItem.get info.info_hash.hash_prod_item_state item in
                  match core.prod_item_right with
                     v :: _ when IVarTable.mem look_table v ->
                        let look = IVarTable.find look_table v in
                           IVarTable.fold (fun closure v _ ->
                                 IVarSet.add closure v) (IVarSet.add closure v) look
                   | _ ->
                        closure) IVarSet.empty items
      in
      let state =
         { info_state_items = items;
           info_state_closure = closure
         }
      in
         State.create info.info_hash.hash_state_state state

   (*
    * Compute the transition table, only for shift operations.
    *)
   let build_delta info unexamined =
      (* Perform the closure *)
      let rec build shift_table examined unexamined =
         if StateSet.is_empty unexamined then
            shift_table, examined
         else
            (* Move an item from unexamined to examined *)
            let state = StateSet.choose unexamined in
            let examined = StateSet.add examined state in
            let unexamined = StateSet.remove unexamined state in

            (* Compute the goto states *)
            let delta = shift_state info state in
            let goto_table, unexamined =
               IVarTable.fold (fun (goto_table, unexamined) v items ->
                     let state = closure info items in
                     let unexamined =
                        if StateSet.mem examined state then
                           unexamined
                        else
                           StateSet.add unexamined state
                     in
                     let goto_table = IVarTable.add goto_table v state in
                        goto_table, unexamined) (IVarTable.empty, unexamined) delta
            in
            let shift_table = StateTable.add shift_table state goto_table in
               build shift_table examined unexamined
      in
         build StateTable.empty StateSet.empty unexamined

   let build_start_state info start_table unexamined start =
      let prods =
         try IVarTable.find info.info_prod start with
            Not_found ->
               raise (Failure ("no such production: " ^ string_of_ivar info.info_hash start))
      in
      let set = List.fold_left ProdItemSet.add ProdItemSet.empty prods in
      let state = closure info set in
      let unexamined = StateSet.add unexamined state in
      let start_table = IVarTable.add start_table start state in
         start_table, unexamined

   let build_state_table info =
      let () =
         if !debug_parsegen then
            eprintf "@[<hv 3>Grammar:@ %a@]@." pp_print_info info
      in
      let start_table, unexamined =
         IVarSet.fold (fun (start_table, unexamined) start ->
               build_start_state info start_table unexamined start) (**)
            (IVarTable.empty, StateSet.empty) info.info_start_symbols
      in
      let shift_table, states = build_delta info unexamined in
         start_table, shift_table, states

   (************************************************************************
    * LALR(1) construction.
    *
    * Once we have the set of LR(0) states, we need to propagate lookahead
    * sets.  For each item in a state, figure out what symbols are propagated
    * and which are spontaneously generated, then perform a fixpoint.
    *)

   (*
    * Build the empty propagation table.
    * It has an entry for each StateItem.
    *)
   let build_prop_empty info states =
      (* First, construct each StateItem *)
      let state_hash = info.info_hash.hash_state_state in
      let state_item_hash = info.info_hash_state_item in
         StateSet.iter (fun state ->
               let core = State.get state_hash state in
               let items = core.info_state_items in
                  ProdItemSet.iter (fun item ->
                        ignore (StateItem.create state_item_hash (state, item))) items) states;

         (* The prop table is an array from the hash code to the prop_entry *)
         StateItem.map_array (fun item _ ->
               { prop_state_item = item;
                 prop_changed    = true;
                 prop_vars       = IVarSet.empty
               }) state_item_hash

   (*
    * Add the propagation info for the initial items.
    *
    * We are looking at an item.
    *     item = left . v1 right
    *
    * Suppose v1 is a nonterminal, and we have some derivation
    * with head nonterminal v2 (perhaps v1 = v2, and right_1
    * is empty).
    *
    *     v1 --> . v2 right_1
    *
    * Suppose v2 has the following production.
    *
    *     v2 = v3 right_2
    *
    * Then the goto(X) state contains an item:
    *
    *     v3 . right_2
    *
    * We need to propagate lookaheads to this item.
    * Propagate FIRST(right_2 right_1) as spontaneous
    * lookaheads.  If NULLABLE(right_2 right_1), then
    * propagate lookaheads from this item.
    *
    * By definition, the lookahead entry for v1 contains
    * and entry (v2 -> look) for each nonterminal v2
    * for which (v1 --> v2 right2), and "look" is the
    * lookahead to be used in such a case.
    *
    * So the algorithm works as follows:
    * Let look1 be LOOKAHEAD(right1):
    *    For each entry (v2, look2) in the lookahead table:
    *       For each transition (v2 -X-> X . right):
    *          Add lookaheads (look2 look1) to the item "X . right"
    *)
   let build_prop_head info prop_table goto_table v1 right1 =
      (* Look up the derivations for v *)
      let delta_table = info.info_head_delta in
      let look_table = IVarTable.find info.info_head_lookahead v1 in
      let look1 = lookahead info right1 in
      let hash_state_item = info.info_hash_state_item in
         IVarTable.fold (fun prop_items v2 look2 ->
               let look = lookahead_concat look2 look1 in
               let prop, vars = lookahead_pair look in
               let delta = IVarTable.find delta_table v2 in
                  IVarTable.fold (fun prop_items v3 items ->
                        ProdItemSet.fold (fun prop_items next_item ->
                              (* Add the edge *)
                              let next_state = IVarTable.find goto_table v3 in
                              let next = StateItem.create hash_state_item (next_state, next_item) in

                              (* Add the edge if we need to propagate *)
                              let prop_items =
                                 if prop then
                                    StateItemSet.add prop_items next
                                 else
                                    prop_items
                              in

                              (* Initial propagation *)
                              let prop_entry = prop_table.(StateItem.hash next) in
                                 prop_entry.prop_vars <- IVarSet.union prop_entry.prop_vars vars;
                                 prop_items) prop_items items) prop_items delta) StateItemSet.empty look_table

   (*
    * Add the propagation info for a state_item.
    *
    * Propagate initial items.
    *
    * In addition, if the item is:
    *
    *   item = left . X right
    *
    * then goto(X) contains the item
    *
    *   left v . right
    *
    * Propagate lookaheads directly to this item.
    *)
   let build_prop_state info prop_table shift_table prop_edges state_item =
      let state_item_hash = info.info_hash_state_item in
      let state, prod_item = StateItem.get state_item_hash state_item in
      let goto_table = StateTable.find shift_table state in
      let prod_item_hash = info.info_hash.hash_prod_item_state in
      let prod_item_core = ProdItem.get prod_item_hash prod_item in
      let { prod_item_left = left;
            prod_item_right = right
          } = prod_item_core
      in
         match right with
            v :: right ->
               (* If v is a nonterminal, then also propagate to initial items *)
               let prop_items =
                  if IVarTable.mem info.info_prod v then
                     build_prop_head info prop_table goto_table v right
                  else
                     StateItemSet.empty
               in

               (* Propagate directly to the next state *)
               let next_state = IVarTable.find goto_table v in
               let next_item_core =
                  { prod_item_core with prod_item_left = v :: left;
                                        prod_item_right = right
                  }
               in
               let next_item = ProdItem.create prod_item_hash next_item_core in
               let next = StateItem.create state_item_hash (next_state, next_item) in

               (* Add the edges, but remove any self-edge (because it is useless) *)
               let prop_items = StateItemSet.add prop_items next in
               let prop_items = StateItemSet.remove prop_items state_item in
               let prop_edge =
                  { prop_edge_src = state_item;
                    prop_edge_dst = prop_items
                  }
               in
                  prop_edge :: prop_edges
          | [] ->
               prop_edges

   (*
    * Now construct a propagation network.
    * Each state is represented as an array of production indices,
    * each with a propagation entry to another item identified
    * by (state, index).
    *)
   let build_prop_table info shift_table states =
      let prop_table = build_prop_empty info states in
      let prop_edges = StateItem.fold (build_prop_state info prop_table shift_table) [] info.info_hash_state_item in
         prop_table, prop_edges

   (*
    * Add the eof symbol for the start states.
    *)
   let set_start_lookahead info prop_table start_table =
      let eof_set = IVarSet.singleton info.info_eof in
      let hash_state = info.info_hash.hash_state_state in
      let hash_state_item = info.info_hash_state_item in
         IVarTable.iter (fun _ state ->
               let core = State.get hash_state state in
               let items = core.info_state_items in
                  ProdItemSet.iter (fun item ->
                        let item = StateItem.create hash_state_item (state, item) in
                        let prop_entry = prop_table.(StateItem.hash item) in
                           prop_entry.prop_vars <- IVarSet.union prop_entry.prop_vars eof_set) (**)
                     items) start_table

   (*
    * The fixpoint is a forward-dataflow problem.
    * Try to order the states so that dependencies are in
    * order.  Use depth-first-search to find an approximate
    * order.
    *)
   let propagate_order info prop_edges =
      (*
       * Build an array of the edges.
       *)
      let length = StateItem.length info.info_hash_state_item in
      let marked = Array.create length false in
      let graph =
         match prop_edges with
            [] ->
               [||]
          | edge :: _ ->
               let graph = Array.create length edge in
                  List.iter (fun edge ->
                        graph.(StateItem.hash edge.prop_edge_src) <- edge) prop_edges;
                  graph
      in

      (*
       * Find the roots if there are any.
       * If there are none, just pick a node at random.
       *)
      let roots nodes =
         let roots =
            StateItemSet.fold (fun roots node ->
                  StateItemSet.diff roots graph.(StateItem.hash node).prop_edge_dst) nodes nodes
         in
            (* If the graph is cyclic, just choose the first node *)
            if StateItemSet.is_empty roots then
               StateItemSet.singleton (StateItemSet.choose nodes)
            else
               roots
      in

      (*
       * Produce a sort in DFS order.
       *)
      let rec dfs_sort_node (items, next) node =
         let next = StateItemSet.remove next node in
         let items, next = dfs_sort_nodes items next graph.(StateItem.hash node).prop_edge_dst in
            node :: items, next

      and dfs_sort_nodes items next nodes =
         StateItemSet.fold (fun items_next node ->
               if marked.(StateItem.hash node) then
                  items_next
               else begin
                  marked.(StateItem.hash node) <- true;
                  dfs_sort_node items_next node
               end) (items, next) nodes
      in

      (*
       * The tree may have disconnected components,
       * so repeat until done.
       *)
      let rec dfs_sort items nodes =
         if StateItemSet.is_empty nodes then
            items
         else
            let roots = roots nodes in
            let items, nodes = dfs_sort_nodes items nodes roots in
               dfs_sort items nodes
      in

      (*
       * Main sort functions.
       *)
      let nodes =
         List.fold_left (fun nodes node ->
               StateItemSet.add nodes node.prop_edge_src) StateItemSet.empty prop_edges
      in
      let items = dfs_sort [] nodes in
         List.map (fun item ->
               graph.(StateItem.hash item)) items

   (*
    * Now solve the lookahead fixpoint.
    *)
   let fixpoint_count = ref 0

   let propagate_lookahead prop_table prop_edges =
      let step () =
         List.fold_left (fun changed prop_edge ->
               let { prop_edge_src = src;
                     prop_edge_dst = dst
                   } = prop_edge
               in
               let item1 = prop_table.(StateItem.hash src) in
                  if item1.prop_changed then
                     let _ = item1.prop_changed <- false in
                        StateItemSet.fold (fun changed dst ->
                              let item2 = prop_table.(StateItem.hash dst) in
                              let vars2 = item2.prop_vars in
                              let vars2' = IVarSet.union vars2 item1.prop_vars in
                                 if IVarSet.cardinal vars2' = IVarSet.cardinal vars2 then
                                    changed
                                 else begin
                                    item2.prop_changed <- true;
                                    item2.prop_vars <- vars2';
                                    true
                                 end) changed dst
                  else
                     changed) false prop_edges
      in
      let rec fixpoint () =
         incr fixpoint_count;
         if step () then
            fixpoint ()
      in
         fixpoint ()

   (*
    * Rebuild the transition table.
    *)
   let rebuild_trans_table shift_table =
      StateTable.map (fun goto_table ->
            IVarTable.map (fun state ->
                  GotoAction state) goto_table) shift_table

   (*
    * Construct the LALR(1) table from the LR(0) table.
    *)
   let build_lalr_table info start now =
      let now = time_print "Starting LALR construction" start now in
      let start_table, shift_table, states = build_state_table info in
      let now = time_print "State table" start now in
      let prop_table, prop_edges = build_prop_table info shift_table states in
      let now = time_print "Propagation table" start now in
      let () =
         if !debug_parsetiming then
            eprintf "Propagate: %d entries, %d edges@." (Array.length prop_table) (List.length prop_edges)
      in
      let () = set_start_lookahead info prop_table start_table in
      let now = time_print "Start state lookaheads" start now in
      let prop_edges = propagate_order info prop_edges in
      let now = time_print "Propagation ordering" start now in

      (* Take the fixpoint *)
      let () = propagate_lookahead prop_table prop_edges in
      let now = time_print "Fixpoint" start now in
      let () =
         if !debug_parsetiming then
            eprintf "Fixpoint in %d iterations@." !fixpoint_count
      in

      (* Reconstruct the tables *)
      let trans_table = rebuild_trans_table shift_table in
      let now = time_print "LALR reconstruction" start now in
         now, start_table, trans_table, prop_table

   (************************************************************************
    * The info needed to build the grammar.
    *)
   let ivar_of_var hash v =
      IVar.icreate hash.hash_ivar_state v

   let ivar_list_of_var_list hash vars =
      List.map (ivar_of_var hash) vars

   let iaction_of_action hash action =
      IAction.icreate hash.hash_iaction_state action

   let prod_item_of_prod hash prod =
      let { prod_name   = name;
            prod_action = action;
            prod_right  = right;
            prod_prec   = pre
          } = prod
      in
      let core =
         { prod_item_name   = ivar_of_var hash name;
           prod_item_left   = [];
           prod_item_right  = ivar_list_of_var_list hash right;
           prod_item_action = iaction_of_action hash action;
           prod_item_prec   = pre
         }
      in
         ProdItem.create hash.hash_prod_item_state core

   let info_of_grammar gram start now =
      (* First and nullable *)
      let hash =
         { hash_ivar_state = IVar.create_state ();
           hash_iaction_state = IAction.create_state ();
           hash_prod_item_state = ProdItem.create_state ();
           hash_state_state =  State.create_state ()
         }
      in
      let prods =
         VarMTable.fold_all (fun prods v items ->
               let v = ivar_of_var hash v in
               let items = List.map (prod_item_of_prod hash) items in
                  IVarTable.add prods v items) IVarTable.empty gram.gram_prod
      in
      let nullable = nullable hash prods in
      let first = first hash prods nullable in
      let now = time_print "First and nullable sets" start now in

      (* Initial info *)
      let start_symbols =
         VarSet.fold (fun vars v ->
               IVarSet.add vars (ivar_of_var hash v)) IVarSet.empty gram.gram_start_symbols
      in
      let prec_table =
         VarTable.fold (fun precs v pre ->
            IVarTable.add precs (ivar_of_var hash v) pre) IVarTable.empty gram.gram_prec
      in
      let info =
         { info_grammar         = gram;
           info_prod            = prods;
           info_start_symbols   = start_symbols;
           info_prec            = prec_table;
           info_nullable        = nullable;
           info_first           = first;
           info_eof             = IVar.create hash.hash_ivar_state Arg.eof;
           info_hash            = hash;
           info_hash_state_item = StateItem.create_state ();

           (* Temporary placeholders *)
           info_head_delta      = IVarTable.empty;
           info_head_lookahead  = IVarTable.empty
         }
      in
      let now, head_delta, head_lookahead = build_head_table info start now in
      let now = time_print "Head table" start now in
      let info =
         { info with info_head_delta     = head_delta;
                     info_head_lookahead = head_lookahead
         }
      in
         now, info

   (************************************************************************
    * Building the parser actions.
    *)

   (*
    * Create the set of nonterminals that have empty production.
    *)
   let empty_productions info =
      let hash = info.info_hash.hash_prod_item_state in
         IVarTable.fold (fun empties v items ->
               let rec search items =
                  match items with
                     item :: items ->
                        let core = ProdItem.get hash item in
                        let empty_flag =
                           match core with
                              { prod_item_left = []; prod_item_right = [] } ->
                                 true
                            | _ ->
                                 false
                        in
                           if empty_flag then
                              IVarTable.add empties v item
                           else
                              search items
                   | [] ->
                        empties
               in
                  search items) IVarTable.empty info.info_prod

   (*
    * Get all the reduce productions.
    * The result is a table
    *     state_item -> lookahead
    * containing only the reduce items.
    *)
   let add_empty_action info actions empties state v look =
      let item = IVarTable.find empties v in
      let item = StateItem.create info.info_hash_state_item (state, item) in
         StateItemTable.filter_add actions item (fun current_look ->
               match current_look with
                  Some current_look ->
                     lookahead_union current_look look
                | None ->
                     look)

   let reduce_actions info empties prop_table =
      let { info_head_lookahead = look_table } = info in
      let hash = info.info_hash.hash_prod_item_state in
      let hash_state_item = info.info_hash_state_item in
         Array.fold_left (fun actions entry ->
               let { prop_state_item = state_item;
                     prop_vars = look3
                   } = entry
               in
               let state, item = StateItem.get hash_state_item state_item in
               let core = ProdItem.get hash item in
                  match core.prod_item_right with
                     v :: right when IVarTable.mem look_table v ->
                        (* Add all empty productions *)
                        let look_table = IVarTable.find look_table v in
                        let look2 = lookahead_concat (lookahead info right) (LookAheadConst look3) in
                        let actions =
                           if IVarTable.mem empties v then
                              add_empty_action info actions empties state v look2
                           else
                              actions
                        in
                           IVarTable.fold (fun actions v look1 ->
                                 if IVarTable.mem empties v then
                                    let look = lookahead_concat look1 look2 in
                                       add_empty_action info actions empties state v look
                                 else
                                    actions) actions look_table
                   | [] ->
                        (* This production calls for a reduce *)
                        StateItemTable.add actions state_item (LookAheadConst look3)
                   | _ :: _ ->
                        actions) StateItemTable.empty prop_table

   (*
    * Error messages.
    *)
   let shift_reduce_conflict info state v shift_state reduce_item =
      let { info_hash = hash } = info in
      let { hash_prod_item_state = hash_prod_item } = hash in
      let pp_print_ivar = pp_print_ivar hash in
      let pp_print_iaction = pp_print_iaction hash in
      let reduce_core = ProdItem.get hash_prod_item reduce_item in
         eprintf "shift/reduce conflict on %a: shift %d, reduce %a@." (**)
            pp_print_ivar v
            (State.hash shift_state)
            pp_print_iaction reduce_core.prod_item_action;
         if not !debug_parsegen then
            eprintf "%a@." (pp_print_state info) state;
         if not !debug_parse_conflict_is_warning then
            raise (Invalid_argument "Lm_parser.shift_reduce_conflict\n\tset MP_DEBUG=parse_conflict_is_warning to ignore this error")

   let reduce_reduce_conflict info state v reduce_item action =
      let { info_hash = hash } = info in
      let { hash_prod_item_state = hash_prod_item } = hash in
      let pp_print_ivar = pp_print_ivar hash in
      let pp_print_iaction = pp_print_iaction hash in
      let reduce_core = ProdItem.get hash_prod_item reduce_item in
         eprintf "reduce/reduce conflict on %a: reduce %a, reduce %a@." (**)
            pp_print_ivar v
            pp_print_iaction reduce_core.prod_item_action
            pp_print_iaction action;
         if not !debug_parsegen then
            eprintf "%a@." (pp_print_state info) state;
         if not !debug_parse_conflict_is_warning then
            raise (Invalid_argument "Lm_parser.reduce_reduce_conflict:\n\tset MP_DEBUG=parse_conflict_is_warning to ignore this error")

   (*
    * Process all the reduce actions.
    * This is finally the stage where we check for conflicts.
    *)
   let process_reduce_actions info reduce_actions action_table =
      let { info_grammar         = gram;
            info_prec            = var_prec_table;
            info_hash = { hash_prod_item_state = hash_prod_item }
          } = info
      in
      let { gram_prec_table = prec_table } = gram in
      let state_item_hash = info.info_hash_state_item in
         StateItemTable.fold (fun action_table state_item look ->
               let look = lookahead_set look in
               let state, item = StateItem.get state_item_hash state_item in
               let { prod_item_name   = name;
                     prod_item_action = action;
                     prod_item_left   = left;
                     prod_item_prec   = prec_name
                   } = ProdItem.get hash_prod_item item
               in
               let assoc = Precedence.assoc prec_table prec_name in
               let reduce = ReduceAction (action, name, List.length left) in
               let actions = StateTable.find action_table state in
               let actions =
                  IVarSet.fold (fun actions v ->
                        try
                           match IVarTable.find actions v with
                              GotoAction id ->
                                 (* Shift/reduce conflict *)
                                 let cmp =
                                    try Precedence.compare prec_table prec_name (IVarTable.find var_prec_table v) with
                                       Not_found ->
                                          0
                                 in
                                    if cmp < 0 then
                                       actions
                                    else if cmp = 0 then
                                       match assoc with
                                          LeftAssoc ->
                                             IVarTable.add actions v reduce
                                        | RightAssoc ->
                                             actions
                                        | NonAssoc ->
                                             IVarTable.add actions v ErrorAction
                                        | NoneAssoc ->
                                             shift_reduce_conflict info state v id item;
                                             actions
                                    else
                                       IVarTable.add actions v reduce
                            | ReduceAction (action2, _, _) ->
                                 (* Reduce/reduce conflict *)
                                 reduce_reduce_conflict info state v item action2;
                                 actions
                            | ErrorAction
                            | AcceptAction ->
                                 raise (Invalid_argument "reduce_action")
                        with
                           Not_found ->
                              IVarTable.add actions v reduce) actions look
               in
                  StateTable.add action_table state actions) action_table reduce_actions

   (*
    * If a state has only one production,
    * and that is a reduce production, we can do
    * the reduce without lookahead.
    *)
   let reduce_early info prop_table state items =
      if ProdItemSet.cardinal items = 1 then
         let item = ProdItemSet.choose items in
            match ProdItem.get info.info_hash.hash_prod_item_state item with
               { prod_item_right = [];
                 prod_item_action = action;
                 prod_item_name = name;
                 prod_item_left = left
               } ->
                  let state_item = StateItem.create info.info_hash_state_item (state, item) in
                  let lookahead = prop_table.(StateItem.hash state_item).prop_vars in
                     if IVarSet.cardinal lookahead = 1 && IVarSet.choose lookahead = info.info_eof then
                        ReduceAccept (action, name, List.length left)
                     else
                        ReduceNow (action, name, List.length left)
             | _ ->
                  ReduceNone
      else
         ReduceNone

   (************************************************************************
    * Constructing the PDA.
    *)

   (*
    * Flatten a production state to a pda description.
    *)
   let pda_info_of_items info prop_table state items =
      let { info_first = first;
            info_hash_state_item = hash_state_item;
            info_hash = { hash_prod_item_state = hash_prod_item }
          } = info
      in
      let items, next =
         ProdItemSet.fold (fun (items, next) prod_item ->
               let core = ProdItem.get hash_prod_item prod_item in
               let { prod_item_left  = left;
                     prod_item_right = right
                   } = core
               in
               let item =
                  { pda_item_left  = left;
                    pda_item_right = right
                  }
               in
               let items = item :: items in
               let next =
                  match right with
                     v :: _ ->
                        let next2 =
                           try IVarTable.find first v with
                              Not_found ->
                                 IVarSet.singleton v
                        in
                           IVarSet.union next next2
                   | [] ->
                        let state_item = StateItem.create hash_state_item (state, prod_item) in
                        let lookahead = prop_table.(StateItem.hash state_item).prop_vars in
                           IVarSet.union next lookahead
               in
                  items, next) ([], IVarSet.empty) items
      in
         { pda_items     = items;
           pda_next      = next
         }

   let pda_action action =
      match action with
         GotoAction state ->
            GotoAction (State.hash state)
       | ReduceAction _
       | AcceptAction
       | ErrorAction as action ->
            action

   let pda_delta table =
      IVarTable.map pda_action table

   (*
    * Find the start state for a production.
    *)
   let create_core gram =
      let start = time_start () in
      let now = start in
      let now, info = info_of_grammar gram start now in
      let now, start_table, trans_table, prop_table = build_lalr_table info start now in
      let empty_table = empty_productions info in
      let reduce_actions = reduce_actions info empty_table prop_table in
      let now = time_print "Reduce productions" start now in
      let trans_table = process_reduce_actions info reduce_actions trans_table in
      let now = time_print "Shift/reduce table" start now in

      (* Build the PDA states *)
      let table =
         State.map_array (fun state core ->
               let { info_state_items = items } = core in
                  { pda_delta  = pda_delta (StateTable.find trans_table state);
                    pda_reduce = reduce_early info prop_table state items;
                    pda_info   = pda_info_of_items info prop_table state items
                  }) info.info_hash.hash_state_state
      in
      let start_table = IVarTable.map State.hash start_table in
      let _now = time_print "PDA construction" start now in
         { pda_start_states    = start_table;
           pda_states          = table;
           pda_hash            = info.info_hash
         }

   let create gram =
      let start = time_start () in
      let pda = create_core gram in
      let _ = time_print "Grammar total" start start in
         pda

   (************************************************************************
    * PDA execution.
    *)

   (*
    * Execute a semantic action.
    *)
   let loc_of_stack stack =
      match stack with
         (_, loc, _) :: _ ->
            loc
       | [] ->
            bogus_loc "null"

   let rec collect_args state args loc1 stack i =
      if i = 0 then
         state, loc1, args, stack
      else
         match stack with
            (state, loc2, arg) :: stack ->
               collect_args state (arg :: args) (union_loc loc1 loc2) stack (pred i)
          | [] ->
               raise (Invalid_argument "semantic_action: stack is empty")

   let semantic_action hash eval arg action stack state tokens =
      let loc = loc_of_stack stack in
      let state, loc, args, stack = collect_args state [] loc stack tokens in
      let () =
         if !debug_parse then
            eprintf "Calling action %a@." (pp_print_iaction hash) action
      in
      let arg, value = eval arg (IAction.get hash.hash_iaction_state action) loc args in
      let () =
         if !debug_parse then
            eprintf "Called action %a@." (pp_print_iaction hash) action
      in
         state, arg, loc, value, stack

   (*
    * Exceptions.
    *)
   let parse_error loc hash run _stack state (v : ivar) =
      let { pda_info = { pda_items = items; pda_next = next } } = run.run_states.(state) in
      let pp_print_ivar = pp_print_ivar hash in
      let buf = stdstr in
         fprintf buf "@[<v 0>Syntax error on token %a" pp_print_ivar v;
         fprintf buf "@ @[<v 3>Current state:";
         List.iter (fun item ->
               let { pda_item_left = left;
                     pda_item_right = right
                   } = item
               in
                  fprintf buf "@ @[<b 3>";
                  Lm_list_util.rev_iter (fun v -> fprintf buf "@ %a" pp_print_ivar v) left;
                  fprintf buf "@ .";
                  List.iter (fun v -> fprintf buf "@ %a" pp_print_ivar v) right;
                  fprintf buf "@]") items;
         fprintf buf "@ @[<b 3>The next possible tokens are:";
         IVarSet.iter (fun v -> fprintf buf "@ %a" pp_print_ivar v) next;
         fprintf buf "@]@]";
         raise (ParseError (loc, flush_stdstr ()))

   (*
    * Execution.
    *
    * The stack contains (state * value) pairs, where the
    * state is the state of the machine when that token was pushed.
    *
    * !!!CAUTION!!!  Keep the number of arguments 6 or less so
    * that these functions can be tail recursive.
    *)
   let fst3 (v, _, _) = v

   let pda_loop hash run arg start =
      let rec pda_lookahead arg stack state tok =
         let { pda_delta = delta } = run.run_states.(state) in
         let v, loc, x = tok in
            match
               (try IVarTable.find delta v with
                   Not_found ->
                      parse_error loc hash run stack state v)
            with
               GotoAction new_state ->
                  if !debug_parse then
                     eprintf "State %d: token %a: shift %d@." state (pp_print_ivar hash) v new_state;
                  pda_no_lookahead arg ((state, loc, x) :: stack) new_state
             | ReduceAction (action, name, tokens) ->
                  if !debug_parse then
                     eprintf "State %d: reduce %a@." state (pp_print_iaction hash) action;
                  let state, arg, loc, x, stack = semantic_action hash run.run_eval arg action stack state tokens in
                     pda_goto_lookahead arg stack (state, loc, x) name tok
             | ErrorAction ->
                  parse_error loc hash run stack state v
             | AcceptAction ->
                  match stack with
                     [_, _, x] ->
                        arg, x
                   | _ ->
                        raise (Invalid_argument "pda_lookahead")

      and pda_goto_lookahead arg stack state_loc_x name tok =
         let state, loc, _x = state_loc_x in
         let () =
            if !debug_parse then
               eprintf "State %d: Goto lookahead: production %a@." (**)
                  state (pp_print_ivar hash) name
         in
         let action =
            try IVarTable.find run.run_states.(state).pda_delta name with
               Not_found ->
                  parse_error loc hash run stack state name
         in
            match action with
               GotoAction new_state ->
                  if !debug_parse then
                     eprintf "State %d: production %a: goto %d (lookahead %a)@." (**)
                        state (pp_print_ivar hash) name
                        new_state (pp_print_ivar hash) (fst3 tok);
                  let stack = state_loc_x :: stack in
                     pda_lookahead arg stack new_state tok
             | ErrorAction
             | ReduceAction _
             | AcceptAction ->
                  eprintf "pda_goto_no_lookahead: illegal action: %a@." (pp_print_pda_action hash) action;
                  raise (Invalid_argument "pda_goto_lookahead: illegal action")

      and pda_no_lookahead arg stack state =
         match run.run_states.(state).pda_reduce with
            ReduceNow (action, name, tokens) ->
               if !debug_parse then
                  eprintf "State %d: ReduceNow: %a@." state (pp_print_iaction hash) action;
               let state, arg, loc, x, stack = semantic_action hash run.run_eval arg action stack state tokens in
                  pda_goto_no_lookahead arg stack (state, loc, x) name
          | ReduceAccept (action, _, tokens) ->
               if !debug_parse then
                  eprintf "State %d: ReduceAccept: %a@." state (pp_print_iaction hash) action;
               let _, arg, _, x, _ = semantic_action hash run.run_eval arg action stack state tokens in
                  arg, x
          | ReduceNone ->
               let v, loc, arg, x = run.run_lexer arg in
               let v = IVar.create hash.hash_ivar_state v in
               let () =
                  if !debug_parse then
                     eprintf "State %d: Read token: %a@." state (pp_print_ivar hash) v
               in
                  pda_lookahead arg stack state (v, loc, x)

      and pda_goto_no_lookahead arg stack state_loc_x name =
         let state, loc, x = state_loc_x in
         let action =
            try IVarTable.find run.run_states.(state).pda_delta name with
               Not_found ->
                  parse_error loc hash run stack state name
         in
            match action with
               GotoAction new_state ->
                  if !debug_parse then
                     eprintf "State %d: production %a: goto %d (no lookahead)@." (**)
                        state (pp_print_ivar hash) name new_state;
                  let stack = (state, loc, x) :: stack in
                     pda_no_lookahead arg stack new_state
             | ErrorAction
             | ReduceAction _
             | AcceptAction ->
                  eprintf "pda_goto_no_lookahead: illegal action: %a@." (pp_print_pda_action hash) action;
                  raise (Invalid_argument "pda_goto_no_lookahead")
      in
         pda_no_lookahead arg [] start

   let parse pda start lexer eval arg =
      let { pda_states        = states;
            pda_start_states  = start_states;
            pda_hash          = hash
          } = pda
      in
      let run =
         { run_states        = states;
           run_lexer         = lexer;
           run_eval          = eval
         }
      in
      let start =
         try IVarTable.find start_states start with
            Not_found ->
               raise (Failure ("not a start symbol: " ^ string_of_ivar hash start))
      in
         try pda_loop hash run arg start with
            Not_found ->
               raise (Failure "syntax error")

   (************************************************************************
    * Wrappers.
    *)
   let empty =
      { parse_grammar = empty_grammar;
        parse_pda     = None
      }

   let add_start info sym =
      let gram = add_start info.parse_grammar sym in
         { parse_grammar = gram; parse_pda = None }

   let get_start info =
      VarSet.fold (fun vars v ->
            Var.get v :: vars) [] (info.parse_grammar.gram_start_symbols)

   let prec_min = Precedence.prec_min
   let prec_max = Precedence.prec_max

   let add_assoc info pre assoc =
      let { parse_grammar = gram } = info in
      let { gram_prec_table = prec_table } = gram in
      let prec_table = Precedence.add_assoc prec_table pre assoc in
      let gram = { gram with gram_prec_table = prec_table } in
      let info = { parse_grammar = gram; parse_pda = None } in
         info

   let create_prec_lt info pre assoc =
      let { parse_grammar = gram } = info in
      let { gram_prec_table = prec_table } = gram in
      let prec_table, pre = Precedence.create_prec_lt prec_table pre assoc in
      let gram = { gram with gram_prec_table = prec_table } in
      let info = { parse_grammar = gram; parse_pda = None } in
         info, pre

   let create_prec_gt info pre assoc =
      let { parse_grammar = gram } = info in
      let { gram_prec_table = prec_table } = gram in
      let prec_table, pre = Precedence.create_prec_gt prec_table pre assoc in
      let gram = { gram with gram_prec_table = prec_table } in
      let info = { parse_grammar = gram; parse_pda = None } in
         info, pre

   let add_prec info pre v =
      let gram = add_prec info.parse_grammar pre (Var.create v) in
         { parse_grammar = gram; parse_pda = None }

   let find_prec info v =
      find_prec info.parse_grammar (Var.create v)

   let add_production info action name rhs pre =
      let action = Action.create action in
      let name = Var.create name in
      let rhs = List.map Var.create rhs in
      let pre =
         match pre with
            Some v ->
               Some (Var.create v)
          | None ->
               None
      in
      let gram = add_production info.parse_grammar action name rhs pre in
         { parse_grammar = gram; parse_pda = None }

   let remove_production info action =
      let action = Action.create action in
      let gram = remove_production info.parse_grammar action in
         { parse_grammar = gram; parse_pda = None }

   let union info1 info2 =
      let changed, gram = union_grammar info1.parse_grammar info2.parse_grammar in
         if changed then
            { parse_grammar = gram; parse_pda = None }
         else
            info1

   let pda_of_info info =
      match info.parse_pda with
         Some pda ->
            pda
       | None ->
            let pda = create info.parse_grammar in
               info.parse_pda <- Some pda;
               pda

   let parse info start lexer eval =
      let pda = pda_of_info info in
      let start = IVar.create pda.pda_hash.hash_ivar_state start in
         parse pda start lexer eval

   let compile info =
      ignore (pda_of_info info)

   let build info debug =
      let prev_debug = !debug_parse in
      let () = debug_parse := debug in
      let pda = create info.parse_grammar in
         debug_parse := prev_debug;
         info.parse_pda <- Some pda

   let pp_print_parser buf info =
      pp_print_grammar buf info.parse_grammar

   let hash info =
      Hashtbl.hash_param max_int max_int info.parse_grammar
end

(*
 * Default precedence module.
 *)
module ParserPrecedence : PrecedenceArg =
struct
   (*
    * A precedence has a name and associativity.
    * The integer gives the *name* of a precedence,
    * not the actual priority.
    *)
   type precedence = int

   module PrecTable = IntTable;;
   type t = (assoc * int) PrecTable.t

   (*
    * Degenerate precedences.
    *)
   let prec_min    = 0
   let prec_max    = 1

   let empty =
      let prec_table = PrecTable.empty in
      let prec_table = PrecTable.add prec_table prec_min (NoneAssoc, 0) in
      let prec_table = PrecTable.add prec_table prec_max (NoneAssoc, 1) in
         prec_table

   (*
    * Check that the associativity matches.
    *)
   let add_assoc table pre assoc =
      let () =
         try
            let assoc', _ = PrecTable.find table pre in
               if assoc' <> assoc then
                  raise (Failure "ParserPrecedence.add_assoc: associativities do not match")
         with
            Not_found ->
               raise (Failure "ParserPrecedence.add_assoc: precedence is not defined")
      in
         table

   (*
    * Shift all the precedence levels at least the given level
    * up by one.
    *)
   let prec_shift table prio =
      PrecTable.map (fun (assoc, prio2) ->
            let prio =
               if prio2 >= prio then
                  succ prio2
               else
                  prio2
            in
               assoc, prio) table

   (*
    * Create a new precedence level after the given one.
    *)
   let create_prec_lt table pre assoc =
      let index = PrecTable.cardinal table in
      let _, prio = PrecTable.find table pre in
      let table = prec_shift table prio in
      let table = PrecTable.add table index (assoc, prio) in
         table, index

   let create_prec_gt table pre assoc =
      let index = PrecTable.cardinal table in
      let _, prio = PrecTable.find table pre in
      let table = prec_shift table (succ prio) in
      let table = PrecTable.add table index (assoc, succ prio) in
         table, index

   (*
    * Get the associativity of a precedence operator.
    *)
   let assoc table pre =
      fst (PrecTable.find table pre)

   (*
    * Compare two precedences.
    *)
   let compare table pre1 pre2 =
      let _, prio1 = PrecTable.find table pre1 in
      let _, prio2 = PrecTable.find table pre2 in
         prio1 - prio2

   (*
    * Print the precedence.
    *)
   let pp_print_prec table buf pre =
      let assoc, prio = PrecTable.find table pre in
         fprintf buf "%a, %d" pp_print_assoc assoc prio
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
