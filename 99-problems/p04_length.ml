let rec length = function
  | [] -> 0
  | h :: t -> 1 + length t

let length_tr lst =
  let rec helper acc lst =
    match lst with
    | [] -> acc
    | h :: t -> helper (acc + 1) t
  in
  helper 0 lst

(* 'a list -> int *)
