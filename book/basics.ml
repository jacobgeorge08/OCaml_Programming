(* What is the type and value of each of the following OCaml expressions? *)

let _ = 7 * (1 + 2 + 3)
(* 42 *)

let _ = "CS " ^ string_of_int 3110
(* CS3110 *)

(* Write an expression that multiplies 42 by 10 *)
let _ = 42 * 10

(* Write an expression that divides 3.14 by 2.0 *)
let _ = 3.14 /. 2.

(* Write an expression that computes 4.2 raised to the seventh power *)
let _ = 4.2 ** 7.

(* Write an if expression that evaluates to 42 if 2 is greater than 1 else evaluates to 7 *)
let _ = if 2 > 1 then 42 else 7

(* Write an expression that compares 42 to 42 using structural equality. *)
let x = 42 = 42

(* Write an expression that compares "hi" to "hi" using structural equality. What is the result? *)
let y = "hi" = "hi"
(* The result was true *)

(* Write an expression that compares "hi" to "hi" using physical equality. What is the result? *)
let z = "hi" == "hi"
(* The result was false since they are not the same objects in memory *)

(* Using the increment function from above as a guide, define a function double that multiplies  *)
(* its input by 2. Test your function by applying it to a few inputs.  *)
(* Turn those test cases into assertions. *)
let double x = x * 2

let _ =
  assert (double 45 = 90);
  assert (double 85 = 170);
  assert (double 0 = 0);
  assert (double (-1) = -2)

(* Enter assert true;; into utop and see what happens. *)
(* - : unit = () *)

(* Enter assert false;; into utop and see what happens. *)
(* Exception: Assert_failure ("//toplevel//", 1, 0) *)

(* Write an expression that asserts 2110 is not (structurally) equal to 3110. *)
let _ = assert (2110 <> 3110)

(* Define a function that computes the cube of a float.  *)
(* Test your function by applying it to a few inputs. *)

let cube x = x ** 3.0

let _ =
  print_endline "cube func";
  print_endline (string_of_float (cube 5.0));
  print_endline (string_of_float (cube 2.0));
  print_endline (string_of_float (cube 10.0))

(* Define a function that computes the sign (1, 0, or -1) of an integer. *)
(* Use a nested if expression. Test your function by applying it to a few inputs. *)

let sign x = if x > 0 then "+" else if x < 0 then "-" else "0"

let _ =
  print_newline ();
  print_endline "sign of func";
  print_endline (sign 1);
  print_endline (sign (-234));
  print_endline (sign 0)

(* Define a function that computes the area of a circle given its radius.  *)
(* Test your function with assert. *)

let area_circle r = Float.pi *. (r ** 2.)
let close_enough x y = Float.abs (x -. y) < 1e-5
let _ = assert (close_enough (area_circle 1.) Float.pi)
let _ = assert (close_enough (area_circle (Float.sqrt (1. /. Float.pi))) 1.)

(* Define a function that computes the root mean square of two numbers i.e. √(𝑥2 + 𝑦2 )/2.  *)
(* Test your function with assert. *)

let rms x y = sqrt (((x ** 2.) +. (y ** 2.)) /. 2.)
let _ = assert (close_enough (rms 2. 2.) 2.)
let _ = assert (close_enough (rms 1. 1.) 1.)

(* Define a recursive function fib such that fib n is the nth number in the Fibonacci sequence *)
let rec fib n =
  if n < 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)

(* Write a tail recursive version of fib *)
let rec fib_aux n pp p = if n = 1 then p else fib_aux (n - 1) p (pp + p)
let fib_fast n = if n = 0 then 0 else fib_aux n 0 1

(* Using utop I found fib_fast 125 is where the integer overflow happens *)

(* Type the following in utop: *)
(* • print_endline "Hello world!";; *)
(* • print_string "Hello world!";; *)
(* Notice the difference in output from each. *)
(* print_enline prints the output and ends the line, print_string doesnt end the line *)

(* Define an infix operator +/. to compute the average of two floating-point numbers *)
let ( +/. ) x y = (x +. y) /. 2.

(* If we have let add x y = x + y.  *)
(* Which of the following produces an integer, function or error  *)
(* • add 5 1 *)
(* Produces an integer *)
(* • add 5 *)
(* Produces a function *)
(* • (add 5) 1 *)
(* Produces an integer *)
(* • add (5 1) *)
(* Produces an error *)

(* Write a function divide : numerator:float -> denominator:float -> float.  *)
let divide_dot ~numerator:x ~denominator:y = x /. y
let _ = divide_dot ~numerator:4. ~denominator:3.

(* What is the type of each of the functions below?  *)
(* let f x = if x then x else x *)
(* bool -> bool *)

(* let g x y = if y then x else x *)
(* 'a -> bool -> 'a *)

(* let h x y z = if x then y else z *)
(* bool -> a' -> a' -> a' *)

(* let i x y z = if x then y else y *)
(* bool -> a' -> b' -> a' *)

(* Write a function that takes an integer and string as input and returns true when they form a valid date *)
(* A valid date has a month that is one of the following :  *)
(* Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec.  *)
(* And the day must be a number that is between 1 and the minimum number of days in that month *)

let valid_date d m =
  if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov" then d >= 1 && d <= 30
  else if
    m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct"
    || m = "Dec"
  then d >= 1 && d <= 31
  else if m = "Feb" then d >= 1 && d <= 28
  else false
