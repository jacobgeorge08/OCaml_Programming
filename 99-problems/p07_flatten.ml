(* Flatten a nested list structure *)
type 'a node = One of 'a | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One a :: t -> a :: flatten t
  | Many b :: t -> flatten b @ flatten t

let flatten_improved lst =
  let rec aux acc = function
    | [] -> acc
    | One a :: t -> aux (a :: acc) t
    | Many b :: t -> aux (aux acc b) t
  in
  List.rev (aux [] lst)
