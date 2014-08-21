

(*
 * A job identifier is just an integer.
 *)
type t = int

module IdTable = Lm_map.LmMake (struct
   type t = int
   let compare = (-)
end)


let pp_print_pid = Format.pp_print_int

(* Id allocation. *)
let null_id = 0
 
let index = ref 1

let create () =
   let id = !index in
      index := succ id;
      id

(*
 * Marshaling.
 *)
let marshal_id id : Omake_marshal.msg =
   List [Magic IdMagic; Int id]

let unmarshal_id (l : Omake_marshal.msg) =
   match l with
   |  List [Magic IdMagic; Int id] ->
       id
   | _ ->
         raise Omake_marshal.MarshalError
