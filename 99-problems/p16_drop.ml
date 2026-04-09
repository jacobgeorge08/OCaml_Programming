let drop lst n =
  let rec aux count acc = function
    | [] -> acc
    | h :: t ->
      if count mod n = 0 then aux (count + 1) acc t
      else aux (count + 1) (h :: acc) t
  in
  List.rev (aux 1 [] lst)

let drop' lst n =
  let rec aux counter acc = function
    | [] -> acc
    | h :: t ->
      if counter = n then aux 1 acc t
      else aux (counter + 1) (h :: acc) t
  in
  List.rev (aux 1 [] lst)
