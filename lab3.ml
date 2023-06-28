(*examples*)
fun x -> x + 1;;
fun x -> x;;
(fun x -> x + 1) 1;;
let f = fun x -> x;;
let x = x;;
val f: 'a -> 'a = <fun>
f1;;
f(-1);;
let sub a b = a - b;;
val sub: int -> int -> int = <fun>
sub 5 4;;
let subFrom5 = sub 5;;
val subFrom5: int -> int = <fun>
subFrom5 4;; 
(*exe1*)
(*
Write a function that:
• takes x and returns the square of x
• takes a 2-tuple and returns its values in a list
• takes two numbers and returns the larger one (e.g. by using the "if"
statement
*)
let square x = x * x1let tuple_to_list (x,y) = [x;y]
let find_larger x y = 
  if x > y then
    x
  else
    y
(*exe2*)
(*
Write a function that:
• takes x and returns the square of x
let square x = x*x;;
• takes a 2-tuple and returns its values in a list
let tupleToList (c, d) = [c; d];;
• takes two numbers and returns the larger one (e.g. by using the "if"
statement
let max a b =
if a > b then a else b;;
*)
let square x = x * x
let tuple_to_list (c, d) = [c; d]
let max a b =
  if a > b then
    a
  else
    b
let result1 = square
let result2 = tuple_to_list (3, 7) 
let result3 = max 10 8 
let () =
  print_endline ("Square: " ^ string_of_int result1);
  print_endline ("Tuple to List: " ^ (string_of_list string_of_int result2));
  print_endline ("Max: " ^ string_of_int result3)
(*exe3 -> recursion*)
(*
• let rec sumToN n =
if (n = 0) then 0
else n + sumToN (n - 1);;
val sumToN : int -> int = <fun>
sumToN 4;;
- : int = 10
et rec createList n =
if n = 1 then [1]
else createList (n-1) @ [n];;
sumToN 5;;
- : int list = [1; 2; 3; 4; 5]
Execution of sumToN 4:
sumToN 4 = 4 + sumToN 3 = 4 + (3 + sumToN 2) = 4 + (3 + (2 + sumToN 1))
= 4 + (3 + (2 + (1 + sumToN 0))) = 4 + (3 + (2 + (1 + 0)))
= 4 + (3 + (2 + 1)) = 4 + (3 + 3) = 4 + 7 = 10
*)
let rec sumToN n =
  if n = 0 then
    0
  else
    n + sumToN (n - 1)

    let rec createList n =
      if n = 1 then
        [1]
      else
        createList (n - 1) @ [n]
(*examples -> recursion*)
(*handling lists*)
List.tl [1];;
List.tl [1; 2];;
List.hd [3];;
let rec sumList list =
  if list = [] then 0
  else(List.hd list) + sumList(List.tl list);;
(*exe4*)
(*
Write a function that:
• computes n! (n factorial) for integer n
• reverses a list
*)
let rec factorial n =
  if n = 0 then
    1
  else
    n * factorial (n - 1)
let rec reverse_list lst =
  match lst with
  | [] -> []
  | head :: tail -> (reverse_list tail) @ [head]
(*exe5*)
(*
Write a function that:
• computes n! (i.e., n factorial) for integer n
let rec factorial n =
if n = 1 then 1
else n * factorial (n-1);;
• reverses a list
let rec reverseList list =
if list = [] then []
else reverseList (List.tl list) @ [List.hd list];;
*)
let rec factorial n =
  if n = 1 then
    1
  else
    n * factorial (n - 1)
let rec reverseList list =
  if list = [] then
    []
  else
    reverseList (List.tl list) @ [List.hd list]
(*examples -> pattern matching*)
let isBetween0and5 n =
  match n with
  | 0 | 1 | 2 | 3 | 4 | 5 -> true
  | _ -> false;;

let rec sumList l =
    match l with
    | [] -> 0
    | h::t -> h + sum t;;
(*exe6*)
(*
Write a function that:
• checks if a given character is a vowel (‘a’, ‘e’, ‘i’, ‘o’, or ‘u’)
• finds the length of a list
• reverses a list using pattern matching
• returns the last element for a list of integers and 0 for an emtpy list
*)
  let is_vowel c =
    match c with
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
  let rec length_of_list lst =
    match lst with
    | [] -> 0
    | _ :: tail -> 1 + length_of_list tail
  let rec reverse_list lst =
    match lst with
    | [] -> []
    | head :: tail -> reverse_list tail @ [head]
  let rec last_element lst =
    match lst with
    | [] -> 0
    | [x] -> x
    | _ :: tail -> last_element tail
(*exe7*)
(*
Write a function that:
• checks if a given character is a vowel (‘a’, ‘e’, ‘i’, ‘o’, or ‘u’)
let isVowel c = match c with
| ’a’ | ’e’ | ’i’ | ’o’ | ’u’ -> true
| _ -> false;;
• finds the length of a list
let rec getListLength list = match list with
| [] -> 0
| _::t -> 1 + getListLength t;;
*)
  let isVowel c = 
    match c with
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
  let rec getListLength list =
    match list with
    | [] -> 0
    | _::t -> 1 + getListLength t
(*exe8*)
(*
Write a function that:
• reverses a list using pattern matching
let rec reverseList list = match list with
| [] -> []
| h::t -> reverseList t @ [h];;
• returns the last element for a list of integers and 0 for an emtpy list
let rec getLastInteger l =
match l with
| [] -> 0
| [e] -> e
| h::t -> getLastInteger t;;
*)
let rec reverseList list =
  match list with
  | [] -> []
  | h::t -> reverseList t @ [h]
let rec getLastInteger l =
  match l with
  | [] -> 0
  | [e] -> e
  | _::t -> getLastInteger t