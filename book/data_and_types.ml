(* Construct a list that has the integers 1 through 5 in it *)
let _ = [ 1; 2; 3; 4; 5 ]

(* Construct the same list, but use :: notation *)
(* let lst2 = 1 :: 2 :: 3 :: 4 :: 5 :: [] *)
(* Commented out because ocamlformat is being annoying *)

(* Now the following expression must appear in your answer: [2; 3; 4] *)
(* Use the @ operator, and do not use :: *)
let _ = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Write a function product that returns the product of all the elements in a list *)
(* The product of all the elements of an empty list is 1 *)
let rec lst_prod = function
  | [] -> 1
  | h :: t -> h * lst_prod t

let prod_tr =
  let rec prod_aux acc = function
    | [] -> acc
    | h :: t -> prod_aux (acc * h) t
  in
  prod_aux 1

(* Write a function that concatenates all the strings in a list *)
(* The concatenation of all the strings in an empty list is the empty string "" *)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

(* Unit test the function product that you wrote in an exercise above *)
(* Added test cases in test.ml file *)

(* The list’s first element is "bigred" *)
let bigred = function
  | [] -> false
  | h :: _ -> h = "bigred"

(* The list has exactly two or four elements; do not use the length function *)
let two_or_four_elements = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

(* The first two elements of the list are equal *)
let equal_first_elements = function
  | a :: b :: _ -> a = b
  | _ -> false

(* Write a function that takes an int list and returns the fifth element of that list, *)
(* if such an element exists. If the list has fewer than five elements, return 0 *)
let fifth_element (lst : int list) : int = if List.length lst >= 5 then List.nth lst 4 else 0

(* A function that takes an int list and returns the list sorted in descending order *)
(* Hint: List.sort with Stdlib.compare as its first argument, and List.rev *)
let rev_list (lst : int list) : int list = List.rev (List.sort Stdlib.compare lst)
let rev_list' (lst : int list) : int list = lst |> List.sort Stdlib.compare |> List.rev

(* Write a function that returns the last element of a list. Assume that the list is non-empty *)
(* Use two library functions, and do not write any pattern matching code of your own *)
let last_el (lst : 'a list) : 'a = List.nth lst (List.length lst - 1)
let last_el' (lst : 'a list) : 'a = lst |> List.rev |> List.hd

(* Write a function any_zeros : int list -> bool that returns true *)
(* if and only if the input list contains at least one 0 *)
(* Do not write any pattern matching code of your own *)
let any_zeros (lst : int list) : bool = List.exists (fun a -> a = 0) lst

(* Write a function take : int -> 'a list -> 'a list such that *)
(* take n lst returns the first n elements of lst *)
(* If lst has fewer than n elements, return all of them *)
let rec take (n : int) (lst : 'a list) : 'a list =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t

(* Write a function drop : int -> 'a list -> 'a list *)
(* such that drop n lst returns all but the first n elements of lst *)
(* If lst has fewer than n elements, return the empty list *)
let rec drop (n : int) (lst : 'a list) : 'a list =
  if n = 0 then lst
  else
    match lst with
    | [] -> []
    | _ :: t -> drop (n - 1) t

(* Revise your solutions for take and drop to be tail recursive *)
(* Test them on long lists with large values of n to see whether they run out of stack space *)
(* To construct long lists, use the -- operator from the lists section *)
(* Drop is already tail-recursive *)
let take_tr (n : int) (lst : 'a list) : 'a list =
  let rec aux acc n lst =
    if n = 0 then List.rev acc
    else
      match lst with
      | [] -> List.rev acc
      | h :: t -> aux (h :: acc) (n - 1) t
  in
  aux [] n lst

(* Write a function is_unimodal : int list -> bool that takes an integer list and *)
(* returns whether that list is unimodal. A unimodal list is a list that monotonically *)
(* increases to some maximum value then monotonically decreases after that value *)
(* Either or both segments (increasing or decreasing) may be empty *)
(* A constant list is unimodal, as is the empty list *)
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

(* Write a function powerset : int list -> int list list *)
(* that takes a set S represented as a list and returns the set of all subsets of S *)
(* The order of subsets in the powerset and the order of elements in the subsets do not matter *)
(* Hint: Consider the recursive structure of this problem *)
(* Suppose you already have p, such that p = powerset s *)
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

(* Write a function print_int_list that prints its input list, one number per line *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
    print_endline (string_of_int h);
    print_int_list t

(* Write a function print_int_list' whose specification is the same as print_int_list *)
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
let make_student (first_name : string) (last_name : string) (gpa : float) : student =
  { first_name; last_name; gpa }

(* Here is a variant that represents a few Pokémon types: *)
type poketype = Normal | Fire | Water

(* Define the type pokemon as record with fields name, hp (an integer), and ptype (a poketype) *)
type pokemon = { name : string; hp : int; ptype : poketype }

(* Create a record named charizard of type pokemon : a Pokémon with 78 HP and Fire type *)
let charizard = { name = "Charizard"; hp = 78; ptype = Fire }

(* Create a record named squirtle of type pokemon : a Pokémon with 44 HP and Water type *)
let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

(* Write a function safe_hd : 'a list -> 'a option that returns *)
(* Some x if the head of the input list is x, and None if the input list is empty *)
let safe_hd = function
  | [] -> None
  | h :: _ -> Some h

(* Also write a function safe_tl *)
let safe_tl = function
  | [] -> None
  | _ :: t -> Some t

(* Write a function max_hp : pokemon list -> pokemon option that, *)
(* given a list of pokemon, finds the Pokémon with the highest HP *)
let rec max_hp = function
  | [] -> None
  | poke1 :: t -> (
    match max_hp t with
    | None -> Some poke1
    | Some poke2 -> if poke1.hp >= poke2.hp then Some poke1 else Some poke2)

let max_hp' lst =
  let rec helper acc = function
    | [] -> acc
    | poke1 :: t -> (
      match acc with
      | None -> helper (Some poke1) t
      | Some poke2 -> if poke1.hp >= poke2.hp then helper (Some poke1) t else helper (Some poke2) t)
  in
  helper None lst

(* Define a date-like triple to be a value of type int * int * int *)
type date_like = int * int * int

(* Write a function is_before that takes two dates and returns true or false *)
(* It is true if the first argument is a date that comes before the second argument *)
let is_before (date1 : date_like) (date2 : date_like) : bool =
  let y1, m1, d1 = date1 in
  let y2, m2, d2 = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

(* Write a function earliest : (int*int*int) list -> (int * int * int) option *)
(* It evaluates to None if the input list is empty, *)
(* and to Some d if date d is the earliest date in the list *)
let rec earliest (lst : date_like list) : date_like option =
  match lst with
  | [] -> None
  | d1 :: t -> (
    match earliest t with
    | None -> Some d1
    | Some d2 -> if is_before d1 d2 then Some d1 else Some d2)

let earliest' (lst : date_like list) : date_like option =
  let rec helper acc lst =
    match lst with
    | [] -> acc
    | d1 :: t -> (
      match acc with
      | None -> helper (Some d1) t
      | Some d2 -> if is_before d1 d2 then helper (Some d1) t else helper (Some d2) t)
  in
  helper None lst

(* Use the functions insert and lookup to construct an association list that maps the integer 1 to
   the string “one”, 2 to “two”, and 3 to “three”. Lookup the key 2. Lookup the key 4 *)
let insert k v d = (k, v) :: d

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let assoc_lst = insert 3 "three" (insert 2 "two" (insert 1 "one" []))
let some_2 = lookup 2 assoc_lst
let none_4 = lookup 4 assoc_lst

(* Define a variant type suit that represents the four suits ♣ ♦ ♥ ♠, in a standard deck *)
type suit = Clubs | Diamonds | Hearts | Spades

(* Define a type rank that represents the possible ranks of a card: *)
type rank = Num of int | Jack | Queen | King | Ace

(* Define a type card that represents the suit and rank of a single card *)
(* Make it a record with two fields *)
type card = { suit : suit; rank : rank }

(* Define a few values of type card: *)
(* the Ace of Clubs, the Queen of Hearts, the Two of Diamonds, the Seven of Spades *)
let ace_of_clubs = { suit = Clubs; rank = Ace }
let queen_of_hearts = { suit = Hearts; rank = Queen }
let two_of_diamonds = { suit = Diamonds; rank = Num 2 }
let two_of_spades = { suit = Spades; rank = Num 7 }

(* For each pattern in the list below, give a value of type int option list *)
(* that does not match the pattern and is not the empty list, or explain why that’s impossible *)
(* Some x :: tl *)
let lst1 : int option list = [ None; Some 4 ]

(* [Some 3110; None] *)
let lst2 : int option list = [ Some 45; None ]

(* [Some x; _] *)
let lst3 : int option list = [ None; None ]

(* h1 :: h2 :: tl *)
let lst4 : int option list = [ Some 1 ]

(* h :: tl *)
(* Impossible because it matches every non empty list *)
(* Can only be NOT matched with an empty list *)

(* Complete the quadrant function , which should return the quadrant of the given x, y point *)
(* Points that lie on an axis do not belong to any quadrant *)
(* Hints: (a) define a helper function for the sign of an integer, (b) match against a pair *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign = if x > 0 then Pos else if x < 0 then Neg else Zero

let quadrant ((x, y) : int * int) : quad option =
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | Zero, _ | _, Zero -> None

(* Rewrite the quadrant function to use the when syntax *)
let quadrant' = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None

(* Write a function depth : 'a tree -> int that returns the number of nodes in the *)
(* longest path from the root to a leaf *)
(* For example, the depth of an empty tree (simply Leaf) is 0, *)
(* and the depth of tree t above is 3 *)
type 'a tree = Leaf | Node of int * 'a tree * 'a tree

let rec depth (t : 'a tree) : int =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (depth l) (depth r)

(* Write a function same_shape : 'a tree -> 'b tree -> bool that determines *)
(* whether two trees have the same shape, regardless of whether the values they carry *)
let rec same_shape (t1 : 'a tree) (t2 : 'a tree) : bool =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2
  | _, _ -> false

(* Write a function list_max : int list -> int that returns the maximum integer in a list, *)
(* or raises Failure "empty" if the list is empty *)
let rec list_max (lst : int list) : int =
  match lst with
  | [] -> failwith "empty"
  | [ x ] -> x
  | h :: t -> max h (list_max t)

(* Write a function list_max_string : int list -> string that returns a string *)
(* containing the maximum integer in a list, or the string "empty" if the list is empty *)
let list_max_string (lst : int list) : string =
  try string_of_int (list_max lst) with Failure _ -> "empty"

(* Write two OUnit tests to determine whether your solution to list_max *)
(*  correctly raises an exception when its input is the empty list *)
(* tests added to test.ml *)

(* Modify your definition of quadrant to use polymorphic variants *)
(* The types of your functions should become these *)
let poly_sign (x : int) = if x > 0 then `Pos else if x < 0 then `Neg else `Zero

let poly_quadrant (x, y) =
  match (poly_sign x, poly_sign y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | `Zero, _ | _, `Zero -> None

(* Write a function is_bst : ('a * 'b) tree -> bool that returns true *)
(* if and the given tree satisfies the binary search tree invariant *)
(* An efficient version of the function that visits each node at most once is tricky to write *)

(* Hint: Write a recursive helper function that takes a tree and either gives you *)
(* (i) the minimum and maximum value in the tree, *)
(* or (ii) tells you that the tree is empty, *)
(* or (iii) tells you that the tree does not satisfy the invariant *)
(* Your is_bst function will not be recursive, but will call your helper function *)
(* and pattern match on the result *)
(* You will need to define a new variant type for the return type of your helper function *)

type ('a, 'b) bst_tree = Leaf | Node of ('a * 'b) * ('a, 'b) bst_tree * ('a, 'b) bst_tree
type 'a bst_helper = Empty | Invalid | Min_Max of ('a * 'a)

let rec check t =
  match t with
  | Leaf -> Empty
  | Node ((k, _), left, right) -> begin
    match (check left, check right) with
    | Invalid, _ | _, Invalid -> Invalid
    | Empty, Empty -> Min_Max (k, k)
    | Min_Max (l_min, l_max), Empty -> if l_max < k then Min_Max (l_min, k) else Invalid
    | Empty, Min_Max (r_min, r_max) -> if k < r_min then Min_Max (k, r_max) else Invalid
    | Min_Max (l_min, l_max), Min_Max (r_min, r_max) ->
      if l_max < k && k < r_min then Min_Max (l_min, r_max) else Invalid
  end

let is_bst t =
  match check t with
  | Invalid -> false
  | Empty | Min_Max _ -> true
