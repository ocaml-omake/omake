(*
 * Right now the symbol table is just a representation of strings.
 *)


let debug_symbol = ref false

(*
 * Hash-cons the symbols.
 *)
(* %%MAGICBEGIN%% *)
module SymbolHashArg =
struct
   type t = int * string

   let debug = "Symbol"

   let hash = Hashtbl.hash

   let compare (i1, s1) (i2, s2) =
      if i1 < i2 then
         -1
      else if i1 > i2 then
         1
      else
         Lm_string_util.string_compare s1 s2

   let reintern s =
      s
end;;

module SymbolHash = Lm_hash.MakeCoarse (SymbolHashArg);;

type t = SymbolHash.t
(* %%MAGICEND%% *)

(*
 * We no longer use a hashtable.
 * Symbols with a 0 index are interned.
 *)
let eq = SymbolHash.equal 
let compare = SymbolHash.compare
let hash = SymbolHash.hash

(* An "empty" variable name *)
let empty_var = SymbolHash.create (0, "")

let new_number, make =
  let count = ref 100 in
  let lock = Lm_thread.Mutex.create "Lm_symbol_hash" in
  (fun () ->
     Lm_thread.Mutex.lock lock;
     let i = !count in
     count := succ i;
     Lm_thread.Mutex.unlock lock;
     i),
  (fun s i ->
     if i >= !count then begin
       Lm_thread.Mutex.lock lock;
       count := max (!count) (i + 1);
       Lm_thread.Mutex.unlock lock
     end;
     SymbolHash.create (i, s))
(*
 * Create a new symbol.
 * Don't add it to the table.
 *)
let new_symbol_string s =
   SymbolHash.create (new_number (), s)

(*
 * Get the integer prefix.
 *)
let to_int v =
   fst (SymbolHash.get v)

(*
 * Get the string suffix.
 *)
let to_string v =
   snd (SymbolHash.get v)


let char0 = Char.code '0'

let rec zeros s i =
   (i < 0) || match s.[i] with
   | '1'..'9' -> false
   | '0' -> zeros s (i - 1)
   | _ -> true

let rec all_digits s i =
   (i<0) || match s.[i] with
   | '0' .. '9' -> all_digits s (i - 1)
   | _ -> false

let rec pad_with_underscore n s i  : bool  =
  if i <= 0 then
    n > 0
  else
    let i =  i - 1 in
    match s.[i] with
    | '_' -> pad_with_underscore n s i
    | '0' -> not (zeros s (i - 1)) && ((n>0) || not (all_digits s (i - 1)))
    | '1' .. '9' -> (n>0) || not (all_digits s (i - 1))
    | _ -> false


let rec loop s fact n i : t =
  if i < 0 then
    SymbolHash.create (0, s)
  else
    match s.[i] with
      '_' ->
      make (String.sub s 0 (if pad_with_underscore n s i then i else i + 1)) n
    | '0' when zeros s (i - 1) ->
      make (String.sub s 0 (succ i)) n
    | '0'..'9' as c ->
      loop s (fact * 10) (n + fact * (Char.code c - char0)) (i - 1)
    | _ ->
      make (String.sub s 0 (i + 1)) n

let add s =
  loop s 1 0 (String.length s - 1)


let is_numeric_symbol v =
   match SymbolHash.get v with
   | (0, s) -> all_digits s (String.length s - 1)
   | _ -> false


let new_symbol v =
   new_symbol_string (to_string v)

let new_symbol_pre pre v =
   let v = to_string v in
   let s =
      if Lm_debug.debug debug_symbol then
         v ^ "/" ^ pre
      else
         v
   in
      new_symbol_string s

(*
 * Create a new symbol, avoiding the ones defined by the predicate.
 *)
let new_name v pred =
   let v = to_string v in
   let rec search i =
     let nv = make v i in
     if pred nv then
       search (succ i)
     else
       nv in
      search 0

(*
 * Create a new symbol, calling the function f until it
 * returns non-nil.
 *)
let new_name_gen v f =
  let v = to_string v in
  let rec search i =
    let nv = make v i in
    match f nv with
    | Some x ->
      x
    | None ->
      search (i + 1)
  in
  search 0

(*
 * Check if the symbol is in the table.
 *)
let is_interned v =
   to_int v = 0

let dump_symbol fmt v = 
  let i, s = SymbolHash.get v in
  Format.fprintf fmt "(%d,%s)" i s


exception Has;;

(*
 * Printer.
 * If the symbol is not a defined symbol,
 * print the index.
 *)
let string_of_symbol v =
   let i, s = SymbolHash.get v in
   let len = String.length s in
   let s = if pad_with_underscore i s len then s ^ "_" else s in
      if i = 0 then
         s
      else
         s ^ string_of_int i

let string_of_ext_symbol v =
  let i, s = SymbolHash.get v in
  let has_special_char s =
    try
      for i = 0 to String.length s - 1 do
        let c = Char.lowercase_ascii (String.get s i) in
        if not ((Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z')
                || (Char.code c >= Char.code '0' && Char.code c <= Char.code '9')
                || c = '_')
        then
          raise Has
      done;
      false
    with Has -> true  in
  let s =
    if i = 0 then
      s
    else
      Lm_printf.sprintf "%s%d" s i
  in
  if has_special_char s then
    Lm_printf.sprintf "`\"%s\"" s
  else
    s

let pp_print_ext_symbol buf v =
   Lm_printf.pp_print_string buf (string_of_ext_symbol v)

let pp_print_symbol buf v =
   Lm_printf.pp_print_string buf (string_of_symbol v)
let output_symbol out v =
   Lm_printf.pp_print_string out (string_of_symbol v)

let rec output_symbol_list out vl =
   match vl with
      [v] ->
         output_symbol out v
    | v :: vl ->
         Format.fprintf out "%a, %a" output_symbol v output_symbol_list vl
    | [] ->
         ()

let rec pp_print_symbol_list buf vl =
   match vl with
      [v] ->
         pp_print_symbol buf v
    | v :: vl ->
         Format.fprintf buf "%a, %a" pp_print_symbol v pp_print_symbol_list vl
    | [] ->
         ()

(*
 * Method name.
 *)
let rec pp_print_method_name buf vl =
  match vl with
  |[v] ->
    pp_print_symbol buf v
  | v :: vl ->
    Format.fprintf buf "%a.%a" pp_print_symbol v pp_print_method_name vl 
  | [] -> ()



(*
 * Build sets, tables, indices where the keys are symbols,
 * ordered symbol pairs, or orderd symbol triples.
 *)
module Base =
struct
   type t = SymbolHash.t
   let compare = compare
end


module SymbolSet = Lm_set.LmMake (Base)
module SymbolTable = Lm_map.LmMake (Base)
module SymbolMTable = Lm_map.LmMakeList (Base)
module SymbolIndex = Lm_index.LmMake (Base)


(*
 * Symbol lists are also useful.
 *)
let output_symbol_set out s =
   output_symbol_list out (SymbolSet.to_list s)

let pp_print_symbol_set buf s =
   pp_print_symbol_list buf (SymbolSet.to_list s)
