function x -> x + 1;;
fun x -> x + 1;;
fun x -> x +. 1.;;
fun x -> x;;
(fun x -> x) 2;;
let f = fun x -> x + 1;;
(f) 2;;
f 2;;
f(-1);;
fun (a,b) -> (a,b);;
fun a b -> a + b;;
let sum a b = a + b;;
sum 2 7;;
sum 2;;
let sumWith2 = sum 2;;
sumWith2 9;;
((sum) 2) 9;;

fun x -> x*x;;
(fun x -> x*x) 5;;
let f (a,b) = [a;b];;
let max a b = if a > b then a else b;;
max 5 10;;
max 5 9;;
let f (a, b) = (a, b);;

let rec sumToN n = if (n = 0) then 0 else n + sumToN (n - 1);;
sumToN 5;;
let rec createList n = if (n = 1) then [1] else createList (n - 1) @ [n];;
createList 20;;
List.hd[1; 2; 3];;
List.tl[1; 2; 3];;
List.hd[1];;
List.tl[1];;
let rec sumList 1 = if (l = []) then 0 else List.hd l + sumList (List.tl l);;
let rec f n = if n = 0 then 1 else (f (n-1)) * n;;
f 5;;
let rec reverseL l = if l = []  then [] else (reverseL (List.tl)) @ [List.hd l];;
reverseL [5; 4; 3; 2; 1];;

match 1 with 
| 1 -> true
| 2 -> false;;

let x = 10;;
match x with
| 1 -> 1
| 2 -> 2
| _ -> 10;;

let isEmptyList l =
  match l with
  | [] -> true
  | _ -> false;;
isEmptyList [];;

match 'd' with 
| 'a' | 'b' | 'c' -. true;;
| _ -> false;;

let sumList l =
  match l
  | _ -> 0
  | h::t -> h + sumList t;;

let isVowel c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false;;

let rec len l =
  match l with
  | [] -> 0
  | _::t -> 1 + len t;; 

let rec reversaL l =
  match l with
  | [] -> []
  | h::t -> (reversaL t) @ [h];;
revreversaL [0, -1, -2];;
