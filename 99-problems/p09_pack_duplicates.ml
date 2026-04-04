let pack lst =
  let rec aux current acc = function
    | [] -> acc
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: current) acc t else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] lst)
