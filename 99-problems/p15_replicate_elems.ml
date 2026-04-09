let replicate lst n =
  let rec repeat n el acc =
    if n = 0 then acc else repeat (n - 1) el (el :: acc)
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (repeat n h acc) t
  in
  List.rev (aux [] lst)

let replicate' lst n =
  let rec repeat n acc el =
    if n = 0 then acc else repeat (n - 1) (el :: acc) el
  in
  List.rev (List.fold_left (repeat n) [] lst)
