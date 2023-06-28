(*exe1*)
(*
Write a function which checks if the elements of a 2-tuple are the same
• Write a (non-recursive) function which in a single call checks if the first and
third element of a list are the same
*)
  let checkTuple (a, b) = a = b
let checkFirstThird list =
  match list with
  | x :: _ :: y :: _ -> x = y
  | _ -> false
(*exe2*)
(*
Write a function which checks if the elements of a 2-tuple are the same
let same x =
match x with
| (a, b) when (a = b) -> true
| _ -> false;;
• Write a (non-recursive) function which in a single call checks if the first and
third element of a list are the same
let firstEqualThird x =
match x with
| h1::(_::(h3::_)) when (h1 = h3) -> true
| _ -> false;;  
*)
  let same (a, b) =
    match (a, b) with
    | (x, y) when x = y -> true
    | _ -> false
  let firstEqualThird list =
    match list with
    | h1 :: (_ :: h3 :: _) when h1 = h3 -> true
    | _ -> false
(*exe3*)
(*
Write a function which “compresses” a list by removing consecutive
copies
compress [1; 1; 1; 2; 2; 1] → [1; 2; 1]
We can simplify our code by using the “as” keyword
let rec compress l =
match l with
| a::(b::_ as tail) when a = b -> compress tail
| a::(b::_ as tail) -> a::compress tail
| remainder -> remainder;;   
*)  
let rec compress l =
  match l with
  | a :: (b :: _ as tail) when a = b -> compress tail
  | a :: (b :: _ as tail) -> a :: compress tail
  | remainder -> remainder
(*exe4*)
(*
Write a function that takes a list l and an integer n, and returns a list
containing all the elements in l greater than n.
• Write a function that takes a list and an integer n and returns the n-th
element of the list. If n is greater than the length of the list, the
function returns 0.   
*)
let filterGreaterThan l n =
  List.filter (fun x -> x > n) l
let getNthElement l n =
  match List.nth_opt l n with
  | Some element -> element
  | None -> 0
(*exe5*)
(*
   Write a function that takes a list l and an integer n, and returns a list
containing all the elements in l greater than n.
let rec getGreaterThan l n =
match l with
| h::t when (h > n) -> h :: getGreaterThan h t
| h::t -> greaterThan t n
| [] -> [];;
*)
let rec getGreaterThan l n =
  match l with
  | h :: t when h > n -> h :: getGreaterThan t n
  | _ :: t -> getGreaterThan t n
  | [] -> []
(*exe6*)
(*
   Write a function that takes a list and an integer n and returns the n-th
element of the list. If n is greater than the length of the list, the
function returns 0.
let rec nthElement l n =
match l with
| [] -> 0
| _ when (n <= 0) -> 0
| h::t when (n = 1) -> h
| h::t -> nthElement t (n - 1);;
*)
let rec nthElement l n =
  match l with
  | [] -> 0
  | _ when n <= 0 -> 0
  | h :: _ when n = 1 -> h
  | _ :: t -> nthElement t (n - 1)
(*exe7*)
(*
   Write a function that takes a function f and value v, and returns f (f
(v)).
• Write a function that takes a function f and a list l, and applies f to
each element in l.
*)
let compose_twice f v = f (f v)
let apply_to_each f l = List.map f l
(*exe8*)
(*
   Write a function that takes a function f and value v, and returns f (f
(v)).
let applyTwice f v =
f (f v);;
• Write a function that takes a function f and a list l, and applies f to
each element in l.
let rec myMap f l =
match l with
| [] -> []
| h::t -> f h :: myMap f t;;
*)
let applyTwice f v =
  f (f v)

let rec myMap f l =
  match l with
  | [] -> []
  | h :: t -> f h :: myMap f t
(*exe9*)
(*
   Write a function that multiplies each element of a list with a given
number n.
• Write a function that takes a list l and an integer n, and returns a list
containing all the elements in l greater than n.
• Use both fold functions separately to apply logical implication to a
given list. Apply both functions on the list [true;true;true] and an initial
value of false.
*)
let multiply_with_n n list =
  List.map (fun x -> n * x) list
let get_greater_than_n n list =
  List.filter (fun x -> x > n) list
let logical_implication_fold_left list initial_value =
  List.fold_left (fun acc x -> acc && x) initial_value list
let logical_implication_fold_right list initial_value =
  List.fold_right (fun x acc -> x && acc) list initial_value
(*exe10*)
(*
Write a function that multiplies each element of a list with a given
number n.
let multAll l n =
List.map (fun x -> x*n) l;;
• Write a function that takes a list l and an integer n, and returns a list
containing all the elements in l greater than n.
let greaterThan l n =
List.filter (fun x -> x > n) l;;   
*)
let multAll l n =
  List.map (fun x -> x * n) l
let greaterThan l n =
  List.filter (fun x -> x > n) l
(*exe11*)
(*
Use both fold functions separately to apply logical implication to a
given list. Apply both functions on the list [true;true;true] and an initial
value of false.
let implies a b =
match (a,b) with
| (true,false) -> false;
| _ -> true;;
List.fold_left implies false [true; true; true];;
List.fold_right implies [true; true; true] false;;   
*)
let implies a b =
  match (a, b) with
  | (true, false) -> false
  | _ -> true
let result_fold_left = List.fold_left implies false [true; true; true]
let result_fold_right = List.fold_right implies [true; true; true] false
let () =
  print_endline ("Result of fold_left implies [true; true; true] false: " ^ string_of_bool result_fold_left);
  print_endline ("Result of fold_right implies [true; true; true] false: " ^ string_of_bool result_fold_right)
