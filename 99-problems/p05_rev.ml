let rev lst =
  let rec helper acc lst =
    match lst with
    | [] -> acc
    | h :: t -> helper (h :: acc) t
  in
  helper [] lst
(* 'a list -> 'a list *)

let rec reverse_bad = function
  | [] -> []
  | h :: t -> reverse_bad t @ [ h ]
(* 'a list -> 'a list *)
