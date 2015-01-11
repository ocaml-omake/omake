let wild_char   = '%'
let wild_string = "%"

(*
 * We have very simple regular expressions of the form, where
 * a single % is a wildcard.
 *)
type in_patt = int * string * int * string

type out_patt = string list

type subst = int * string



(*
 * Printing.
 *)
let pp_print_wild_in buf (_, s1, _, s2) =
   Format.fprintf buf "%s%c%s" s1 wild_char s2

let pp_print_wild_out buf strs =
   Lm_printf.pp_print_string buf (String.concat wild_string strs)


let is_wild s =   String.contains s wild_char

(*
 * Compile a pattern to make searching easier.
 *)
let compile_in s =
  let len = String.length s in
  match String.index s wild_char 
  with
  | index -> 
    let prefix = String.sub s 0 index in
    let slen = len - index - 1 in
    let suffix = String.sub s (index + 1) slen in
    if String.contains suffix wild_char then
      raise (Failure "Only one wildcard symbol % allowed in a match pattern");
    index, prefix, slen, suffix
  | exception Not_found -> 
    raise (Invalid_argument "Lm_wild.wild_compile")

let compile_out s =
   Lm_string_util.split wild_string s


(*
 * Match the wild pattern, and return a subst.
 *)
let wild_match (plen, prefix, slen, suffix)  s =
   let len = String.length s in
   if len >= plen + slen then 
     try 
     begin 
     let soffs = len-slen in
     for i = slen - 1 downto 0 do 
       if String.unsafe_get suffix i = String.unsafe_get s (soffs + i) 
       then 
         ()
       else 
         raise Not_found
     done;
     for i = 0 to plen - 1 do 
       if String.unsafe_get prefix i = String.unsafe_get s i then
         ()
       else 
         raise Not_found
     done
     end; 
     let len = len - plen - slen in
     Some (len, String.sub s plen len)
     with Not_found -> None 
   else 
     None
(*
 * Get the substitution value.
 *)
let core (_, s) =
   s

let of_core s =
   String.length s, s

(*
 * Perform a substitution.
 *)
let subst_in (slen, s) (plen, prefix, sflen, suffix) =
  let res = String.create (slen + plen + sflen) in
  String.blit prefix 0 res 0 plen;
  String.blit s 0 res plen slen;
  String.blit suffix 0 res (plen + slen) sflen;
  res

let subst (_, s) strs =
   String.concat s strs
     
