type 'a rle = One of 'a | Many of int * 'a

let encoding lst =
  let rle count elem = if count = 0 then One elem else Many (count + 1, elem) in
  let rec aux count acc = function
    | [] -> acc
    | [ x ] -> rle count x :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t else aux 0 (rle count a :: acc) t
  in
  List.rev (aux 0 [] lst)
