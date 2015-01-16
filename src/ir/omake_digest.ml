(* A faster alternative to MD5 - but this one is NOT cryptographically secure
 *)


let merge sbox =
  let smallbox = Array.make 16 0 in
  Array.blit sbox 0 smallbox 0 16;
  for k = 16 to 255 do
    let p = sbox.(k) in
    let q = (p lsr 4) lor ((p lsl 4) land 0xf0) in
    let i1 = (k + q) land 15 in
    smallbox.(i1) <- smallbox.(i1) lxor p;
    let i2 = (k + p) land 15 in
    smallbox.(i2) <- smallbox.(i2) lxor q;
  done;
  smallbox
                   

type digester =
    { sbox : int array;
      mutable pos : int;
      mutable acc : int;
      wrap_around : Buffer.t;
    }


let init() =
  let sbox = Array.make 256 0 in
  for i = 0 to 255 do
    sbox.(i) <- i
  done;
  let wrap_around = Buffer.create 16 in
  { sbox; pos = 0; acc = 0; wrap_around }


let add dg p =
  let sbox = dg.sbox in
  let pos = dg.pos in
  let acc = ref dg.acc in
  let l = String.length p in
  for k = 0 to l-1 do
    let i1 = (pos+k) land 0xff in
    let c = Array.unsafe_get sbox i1 in
    acc := (!acc + c + Char.code (String.unsafe_get p k)) land 0xff;
    Array.unsafe_set sbox i1 (Array.unsafe_get sbox !acc);
    Array.unsafe_set sbox !acc c;
  done;
  let bl = Buffer.length dg.wrap_around in
  if bl < 16 then
    Buffer.add_string dg.wrap_around (String.sub p 0 (min l (16-bl)));
  dg.pos <- dg.pos + l;
  ()

let finish dg =
  let w = Buffer.contents dg.wrap_around in
  let l = String.length w in
  if l > 0 then (
    while dg.pos < 16 do
      let p = String.sub w 0 (min l (16 - dg.pos)) in
      add dg p
    done
  );
  (* Condense the 256 bytes in sbox to just 16: *)
  let smallbox = merge dg.sbox in
  let d = String.create 16 in
  Array.iteri
    (fun i x -> d.[i] <- Char.chr x)
    smallbox;
  d

let string s =
  let dg = init() in
  add dg s;
  finish dg


let chunk_len = 65536

let file name =
  let f = open_in_bin name in
  try
    let dg = init() in
    let n = LargeFile.in_channel_length f in
    let s = String.create chunk_len in
    let chunks = Int64.to_int (Int64.div n (Int64.of_int chunk_len)) in
    for _i = 1 to chunks do
      really_input f s 0 chunk_len;
      add dg s
    done;
    let m = Int64.to_int (Int64.rem n (Int64.of_int chunk_len)) in
    if m > 0 then (
      really_input f s 0 m;
      add dg (String.sub s 0 m);
    );
    close_in f;
    finish dg
  with
    | error ->
         close_in f;
         raise error

  
  
