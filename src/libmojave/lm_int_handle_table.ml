

module Table = Lm_int_set.IntTable

type handle = int ref

type 'a t =
  { mutable table_index   : int;
    mutable table_entries : (handle * 'a) list Table.t
  }

 (************************************************
* Association lists.
*)
  let replaceq entries key x =
    let rec loop entries1 entries2 =
      match entries2 with
      (key', _) :: entries2 when key' == key ->
    List.rev_append entries1 ((key, x) :: entries2)
      | h :: entries2 ->
    loop (h :: entries1) entries2
      | [] ->
  (key, x) :: entries
  in
loop [] entries

  let rec assq_value entries x =
    match entries with
  (key, x') :: _ when x' == x -> key
  | _ :: _ -> assq_value entries x
| [] -> raise Not_found

 (************************************************
* Handle operations.
*)
  let create_handle table index =
  let index2 = table.table_index in
  table.table_index <- max (index + 1) index2;
ref index

  let new_handle table =
  let index = table.table_index in
  table.table_index <- succ index;
ref index

let int_of_handle = (!)

 (************************************************
* Table operations.
*)
  let create () =
    { table_index   = 0;
  table_entries = Table.empty
}

  let add table hand x =
    let entries =
        Table.filter_add table.table_entries !hand (fun entries ->
          let entries =
            match entries with
          Some entries -> entries
        | None -> []
        in
  replaceq entries hand x)
  in
table.table_entries <- entries

  let remove table hand =
    let entries =
        Table.filter_remove table.table_entries !hand (fun entries ->
        let entries = List.remove_assq hand entries in
          match entries with
        [] -> None
  | _ :: _ -> Some entries)
  in
table.table_entries <- entries

  let find table hand =
List.assq hand (Table.find table.table_entries !hand)

  let find_any table hand =
    match Table.find table.table_entries !hand with
  (_, x) :: _ -> x
| [] -> raise Not_found

  let find_any_handle table index =
    match Table.find table.table_entries index with
  (hand, _) :: _ -> hand
| [] -> raise Not_found

  let find_value table index x =
assq_value (Table.find table.table_entries index) x
