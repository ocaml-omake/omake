type t = int array

let bits_per_int =
  if Sys.word_size = 64 then
    62
  else
    30

let create() =
  [| 0 |]

let is_set bitset b =
  b >= 0 && (
    let j = b / bits_per_int in
    j < Array.length bitset && (
      let k = b mod bits_per_int in
      let x = bitset.(j) in
      ((x lsr k) land 1) <> 0
    )
  )

let set bitset b =
  if b < 0 then invalid_arg "Lm_bitset.set";
  let j = b / bits_per_int in
  let bitset' =
    if j < Array.length bitset then
      Array.copy bitset
    else
      let bitset' = Array.make (j+1) 0 in
      Array.blit bitset 0 bitset' 0 (Array.length bitset);
      bitset' in
  let k = b mod bits_per_int in
  let x = bitset'.(j) in
  let x' = x lor (1 lsl k) in
  bitset'.(j) <- x';
  bitset'

let set_multiple bitset b_list =
  let m = List.fold_left ( max ) 0 b_list in
  let mj = m / bits_per_int in
  let bitset' =
    if mj < Array.length bitset then
      Array.copy bitset
    else
      let bitset' = Array.make (mj+1) 0 in
      Array.blit bitset 0 bitset' 0 (Array.length bitset);
      bitset' in
  List.iter
    (fun b ->
       if b < 0 then invalid_arg "Lm_bitset.set_multiple";
       let j = b / bits_per_int in
       let k = b mod bits_per_int in
       let x = bitset'.(j) in
       let x' = x lor (1 lsl k) in
       bitset'.(j) <- x';
    )
    b_list;
  bitset'
    
