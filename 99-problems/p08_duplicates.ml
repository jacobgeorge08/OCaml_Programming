let compress lst =
  let rec aux acc = function
    | [] -> acc
    | h1 :: (h2 :: _ as t) -> if h1 = h2 then aux acc t else aux (h1 :: acc) t
    | h1 :: [] -> h1 :: acc
  in
  List.rev (aux [] lst)
