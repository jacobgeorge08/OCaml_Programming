let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

let duplicate' lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in
  aux [] (List.rev lst)

let duplicate'' lst = List.rev (List.fold_left (fun acc x -> x :: x :: acc) [] lst)
