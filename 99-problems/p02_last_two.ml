let rec last_two (lst : 'a list) : ('a * 'a) option =
  match lst with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t
