(* Construct a list that has the integers 1 through 5 in it.  *)
let lst1 = [ 1; 2; 3; 4; 5 ]

(* Construct the same list, but use :: notation *)
(* let lst2 = 1 :: 2 :: 3 :: 4 :: 5 :: [] *)
(* Commented out because ocamlformat is being annoying *)

(* Now the following expression must appear in your answer: [2; 3; 4]  *)
(* Use the @ operator, and do not use :: *)
let lst3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Write a function product that returns the product of all the elements in a list.  *)
(* The product of all the elements of an empty list is 1. *)
let rec lst_prod = function [] -> 1 | h :: t -> h * lst_prod t

let prod_tr =
  let rec prod_aux acc = function
    | [] -> acc
    | h :: t -> prod_aux (acc * h) t
  in
  prod_aux 1

(* Write a function that concatenates all the strings in a list.  *)
(* The concatenation of all the strings in an empty list is the empty string "". *)
let rec concat = function [] -> "" | h :: t -> h ^ concat t

(* Unit test the function product that you wrote in an exercise above. *)
(* Added test cases in test.ml file *)

(* the list’s first element is "bigred" *)
let bigred = function [] -> false | h :: _ -> h = "bigred"

(* the list has exactly two or four elements; do not use the length function *)
let two_or_four_elements = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

(* • the first two elements of the list are equal  *)
let equal_first_elements = function a :: b :: _ -> a = b | _ -> false

(* Write a function that takes an int list and returns the fifth element of that list, *)
(* if such an element exists. If the list has fewer than five elements, return 0 *)
let fifth_element (lst : int list) : int =
  if List.length lst >= 5 then List.nth lst 4 else 0

(* A function that takes an int list and returns the list sorted in descending order.  *)
(* Hint: List.sort with Stdlib.compare as its first argument, and List.rev *)
let rev_list (lst : int list) : int list =
  List.rev (List.sort Stdlib.compare lst)

let rev_list' (lst : int list) : int list =
  lst |> List.sort Stdlib.compare |> List.rev

(* Write a function that returns the last element of a list. Assume that the list is non-empty *)
(* Use two library functions, and do not write any pattern matching code of your own. *)
let last_el (lst : 'a list) : 'a = List.nth lst (List.length lst - 1)
let last_el' (lst : 'a list) : 'a = lst |> List.rev |> List.hd

(* Write a function any_zeros : int list -> bool that returns true  *)
(* if and only if the input list contains at least one 0.  *)
(* Do not write any pattern matching code of your own. *)
let any_zeros (lst : int list) : bool = List.exists (fun a -> a = 0) lst

(* Write a function take : int -> 'a list -> 'a list such that  *)
(* take n lst returns the first n elements of lst.  *)
(* If lst has fewer than n elements, return all of them *)
let rec take (n : int) (lst : 'a list) : 'a list =
  if n = 0 then []
  else match lst with [] -> [] | h :: t -> h :: take (n - 1) t

(*  Write a function drop : int -> 'a list -> 'a list  *)
(* such that drop n lst returns all but the first n elements of lst.  *)
(* If lst has fewer than n elements, return the empty list. *)
let rec drop (n : int) (lst : 'a list) : 'a list =
  if n = 0 then lst else match lst with [] -> [] | _ :: t -> drop (n - 1) t

(* Revise your solutions for take and drop to be tail recursive.  *)
(* Test them on long lists with large values of n to see whether they run out of stack space.  *)
(* To construct long lists, use the -- operator from the lists section *)
(* Drop is already tail-recursive *)
let take_tr (n : int) (lst : 'a list) : 'a list =
  let rec aux acc n lst =
    if n = 0 then List.rev acc
    else
      match lst with [] -> List.rev acc | h :: t -> aux (h :: acc) (n - 1) t
  in
  aux [] n lst

(* Write a function is_unimodal : int list -> bool that takes an integer list and  *)
(* returns whether that list is unimodal. A unimodal list is a list that monotonically  *)
(* increases to some maximum value then monotonically decreases after that value.  *)
(* Either or both segments (increasing or decreasing) may be empty.  *)
(* A constant list is unimodal, as is the empty list. *)
let my_unimodal (lst : int list) : bool =
  let rec aux lst decreasing =
    if decreasing = false then
      match lst with
      | [] | [ _ ] -> true
      | h1 :: (h2 :: _ as t) -> if h1 <= h2 then aux t false else aux t true
    else
      match lst with
      | [] | [ _ ] -> true
      | h1 :: (h2 :: _ as t) -> if h1 < h2 then false else aux t true
  in
  aux lst false

let is_unimodal (lst : int list) : bool =
  let rec asc = function
    | [] | [ _ ] -> true
    | h1 :: (h2 :: _ as t) -> if h1 <= h2 then asc t else dsc t
  and dsc = function
    | [] | [ _ ] -> true
    | h1 :: (h2 :: _ as t) -> if h1 < h2 then false else dsc t
  in
  asc lst

(* Write a function powerset : int list -> int list list  *)
(* that takes a set S represented as a list and returns the set of all subsets of S.  *)
(* The order of subsets in the powerset and the order of elements in the subsets do not matter. *)
(* Hint: Consider the recursive structure of this problem.  *)
(* Suppose you already have p, such that p = powerset s.  *)
(* How could you use p to compute powerset (x :: s)? *)
let powerset s =
  let rec aux prefix = function
    | [] -> [ prefix ]
    | h :: t ->
        let with_h = aux (h :: prefix) t in
        let without_h = aux prefix t in
        with_h @ without_h
  in
  aux [] s

(* Write a function print_int_list that prints its input list, one number per line.  *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      print_int_list t

(* Write a function print_int_list' whose specification is the same as print_int_list.  *)
(* Do not use the keyword rec in your solution, instead List.iter *)
let print_int_list' (lst : int list) : unit =
  List.iter (fun x -> print_endline (string_of_int x)) lst

(* Assume the following type definition: *)
type student = { first_name : string; last_name : string; gpa : float }

(* Give OCaml expressions that have the following types: *)
(* • student *)
let cody = { first_name = "Cody"; last_name = "Ko"; gpa = 4.0 }

(* • student -> string * string (a function that extracts the student’s name) *)
let full_name std = (std.first_name, std.last_name)

(* • string -> string -> float -> student (a function that creates a student record) *)
let make_student (first_name : string) (last_name : string) (gpa : float) :
    student =
  { first_name; last_name; gpa }

(* Here is a variant that represents a few Pokémon types: *)
type poketype = Normal | Fire | Water

(* Define the type pokemon as record with fields name, hp (an integer), and ptype (a poketype). *)
type pokemon = { name : string; hp : int; ptype : poketype }

(* Create a record named charizard of type pokemon : a Pokémon with 78 HP and Fire type. *)
let charizard = { name = "Charizard"; hp = 78; ptype = Fire }

(* Create a record named squirtle of type pokemon : a Pokémon with 44 HP and Water type. *)
let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

(* Write a function safe_hd : 'a list -> 'a option that returns  *)
(* Some x if the head of the input list is x, and None if the input list is empty. *)
let safe_hd = function [] -> None | h :: _ -> Some h

(* Also write a function safe_tl  *)
let safe_tl = function [] -> None | _ :: t -> Some t

(* Write a function max_hp : pokemon list -> pokemon option that,  *)
(* given a list of pokemon, finds the Pokémon with the highest HP *)
let rec max_hp = function
  | [] -> None
  | poke1 :: t -> begin
      match max_hp t with
      | None -> Some poke1
      | Some poke2 -> if poke1.hp >= poke2.hp then Some poke1 else Some poke2
    end

let max_hp' lst =
  let rec helper acc = function
    | [] -> acc
    | poke1 :: t -> begin
        match acc with
        | None -> helper (Some poke1) t
        | Some poke2 ->
            if poke1.hp >= poke2.hp then helper (Some poke1) t
            else helper (Some poke2) t
      end
  in
  helper None lst

(* Define a date-like triple to be a value of type int * int * int *)
type date_like = int * int * int

(* Write a function is_before that takes two dates and returns true or false.  *)
(* It is true if the first argument is a date that comes before the second argument. *)
let is_before (date1 : date_like) (date2 : date_like) : bool =
  let y1, m1, d1 = date1 in
  let y2, m2, d2 = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)
