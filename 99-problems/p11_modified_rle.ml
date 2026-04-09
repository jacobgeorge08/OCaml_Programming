type 'a rle = One of 'a | Many of int * 'a

let rle lst =
  let rec aux count acc = function
    | [] -> acc
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.map
    (fun x ->
      match x with
      | 1, e -> One e
      | c, e -> Many (c, e))
    (List.rev (aux 0 [] lst))

let rle' lst =
  let create_tuple count elem = if count = 1 then One elem else Many (count, elem) in
  let rec aux count acc = function
    | [] -> acc
    | [ x ] -> create_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 (create_tuple (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] lst)
