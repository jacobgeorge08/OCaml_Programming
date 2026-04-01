(* Consider the following definitions: *)
let double x = 2 * x
let square x = x * x
let twice f x = f (f x)

(* Determine what the types of quad and fourth are *)
let quad = twice double
(* int -> int *)

let fourth = twice square
(* int -> int *)

(* Explain how quad and fourth are not syntactically written as a function yet its type 
   shows that it is in fact a function *)
(* This is because of partical application. square expects an integer which has not
   been provided yet so we have a partially applied function. quad and fourth
   accept this integer and they are functions that will return an integer *)

(* What does the following operator do? *)
let ( $ ) f x = f x

(* It is the apply operator that applies f to x. *)
(* Also it has the weaker precedence than operators like +, - etc *)

(* What does the following operator do? *)
let ( @@ ) f g x = x |> g |> f
(* It is the function composition operator *)

(* Generalize twice to a repeat, such that repeat f n x applies f to x a total of n times *)
let rec repeat f n x = if n = 0 then x else repeat f (n - 1) (f x)

(* Use fold_left to write a function product_left that computes the product of a list of floats *)
(* The product of the empty list is 1.0.  *)
let product_left' lst = List.fold_left (fun acc elt -> acc *. elt) 1.0 lst

(* Use fold_right to write a function that computes the product of a list of floats. *)
let product_right' lst = List.fold_right (fun elt acc -> elt *. acc) lst 1.0

(* How terse can you make your solutions to the product exercise? *)
let product_left = List.fold_left ( *. ) 1.0
let product_right = ListLabels.fold_right ~f:( *. ) ~init:1.0

(* Write a function sum_cube_odd n that computes the sum of the cubes of all the odd numbers  *)
(* between 0 and n inclusive.  *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)
let cube x = x * x * x
let odd n = n mod 2 <> 0

let sum_cube_odd n =
  let nums = 0 -- n in
  let filtered = List.filter odd nums in
  let cubed = List.map cube filtered in
  List.fold_left ( + ) 0 cubed

(* Rewrite the function sum_cube_odd to use the pipeline operator |>. *)
let sum_cube_odd' n =
  0 -- n
  |> List.filter (fun i -> i mod 2 <> 0)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

(* Consider writing a function exists that returns whether at least one element of the  *)
(* list satisfies the predicate p. When applied to an empty list, it evaluates to false. *)
let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t

(* Its better to write acc || p x rather than p x || acc because true values will short circuit *)
(* the || chain and we wont have to check the rest of the values *)
let exists_fold p lst = List.fold_left (fun acc x -> acc || p x) false lst
let exists_lib p lst = if List.filter p lst <> [] then true else false
let exists_lib' = List.exists
(* Okay apparently the short circuiting doesnt work for List.fold but it does work for List.exists*)

(* Write a function which, given a list of numbers representing debits, deducts them  *)
(* from an account balance, and finally returns the remaining amount in the balance. *)
let rec deduct balance debits =
  match debits with
  | [] -> balance
  | h :: t -> deduct (balance -. h) t

let deduct_fl balance = List.fold_left (fun acc x -> acc -. x) balance
let deduct_fl' = List.fold_left ( -. )
let deduct_fr balance debits = List.fold_right (fun d b -> b -. d) debits balance

(* Here is an uncurried version of List.nth *)
let uncurried_nth (lst, n) = List.nth lst n

(* Write uncurried versions of these library functions *)
(* List.append *)
let uncurried_append (lst1, lst2) = List.append lst1 lst2

(* Char.compare *)
let uncurried_compare (c1, c2) = Char.compare c1 c2

(* Stdlib.max *)
let uncurried_max (val1, val2) = Stdlib.max val1 val2

(* Show how to replace an expression of the form List.map f (List.map g lst)  *)
(* with an equivalent expression that calls List.map only once. *)
let single_map lst f g = List.map (fun x -> f (g x)) lst
let composed_map lst f g = List.map (f @@ g) lst

(* Write functions that perform the following computations *)
(* Find those elements of a list of strings whose length is strictly greater than 3 *)
let more_than_3 lst = List.filter (fun x -> String.length x > 3) lst

(* Add 1.0 to every element of a list of floats *)
let add_one lst = List.map (fun x -> x +. 1.) lst

(* Given a list of strings strs and another string sep, produce the string that *)
(*  contains every element of strs separated by sep *)
let combined sep lst =
  match lst with
  | [] -> ""
  | h :: t -> List.fold_left (fun combined s -> combined ^ sep ^ s) h t

(* An association list is an implementation of a dictionary in terms of a list of pairs *)
(* where the first component of each pair is a key and the second component as a value. *)
(* Write a function keys: ('a * 'b) list -> 'a list that returns a list of the unique keys  *)
let keys1 lst =
  let rec get_keys = function
    | [] -> []
    | (k, _) :: t -> k :: get_keys t
  in
  get_keys lst |> List.sort_uniq Stdlib.compare

let keys2 lst =
  lst |> List.fold_left (fun acc p -> fst p :: acc) [] |> List.sort_uniq Stdlib.compare

let keys3 lst = lst |> List.map fst |> List.sort_uniq Stdlib.compare

(* Implement a function is_valid_matrix: int list list -> bool that returns whether  *)
(* the input matrix is valid. Unit test the function. *)
let is_valid_matrix (matrix : int list list) : bool =
  match matrix with
  | [] -> false
  | r1 :: r ->
    let r_len = List.length r1 in
    r_len > 0 && List.fold_left (fun acc row -> acc && r_len = List.length row) true r

let is_valid_matrix' = function
  | [] -> false
  | r1 :: r ->
    let r_len = List.length r1 in
    r_len > 0 && List.for_all (fun r' -> r_len = List.length r') r

(* Implement a function add_row_vectors for the element-wise addition of two row vectors *)
let add_row_vectors = List.map2 ( + )

(* Implement matrix addition.*)
let add_matrices = List.map2 add_row_vectors

(* Implement a function multiply_matrices  *)
(* Unit test the function. Hint: define functions for matrix transposition and row vector dot product. *)
let dot = List.fold_left2 (fun acc x y -> acc + (x * y)) 0

let transpose ls =
  let rec transpose' acc = function
    | [] | [] :: _ -> List.rev acc
    | ls -> transpose' (List.map List.hd ls :: acc) (List.map List.tl ls)
  in
  transpose' [] ls

let multiply_matrices m1 m2 = List.map (fun row -> List.map (dot row) (transpose m2)) m1
