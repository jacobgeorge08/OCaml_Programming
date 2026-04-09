type 'a rle = One of 'a | Many of int * 'a

let decode lst =
  let rec spread num x = if num = 0 then [] else x :: spread (num - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (num, x) :: t -> aux (spread num x @ acc) t
  in
  List.rev (aux [] lst)

let decode' lst =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (num, x) :: t -> aux (many acc num x) t
  in
  aux [] (List.rev lst)

(* The first version is not very optimal because of the use of append. *)
(* In the second version we get rid of append and we also reverse the input list since its always 
 smaller than the output list *)
