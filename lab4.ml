let x = 5;;
match x with
| 1 -> 1
| 4 -> 4
| a -> a;;
match [1 ; 2; 3] with
| h::t -> t
| _ -> [];;

let l = [5; 6; 7];;
match l with
| h1::(h2::t2) -> t2
| [] -> [];;
match l with
| h1::(h2::t2) -> t2
| [a] -> [a]
| [] -> [];;

let sameElements = (1, 1);;
match sameElements with
| (x, x) -> true
| _ -> false;;

let isBetween0and5 n = 
  match n with 
| x when (-1 < x && x < 6) -> true
| _ -> false;;
isBetween0and5 0;; isBetween0and5 6;;

let sameElements x =
match x with
| (x, y) when x = y -> true
| _ -> false;;
sameElements (1, 1);; sameElements (1, 2);;

let f x = 
match x with
| h1::(h2::(h3::t3)) when (h1 = h3) -> true
| _ -> false;;
f [1; 2; 1];;
f [1; 2; 2];;

let x = (1, 2);;
match x with
| (a, b) as twoTuple -> if (a = b) then twoTuple (a, b) else (0, 0);;

let rec compress l =
match l with
| h1::(h2::t2) as t1 -> 
  if (h1 = h2) then compress t1
  else h1 :: compress t1
| a -> a;;

let rec myMap f l = 
match l with 
| h::t -> (f h) :: myMap f t 
| [] -> [];;
myMap (fun x -> x*x) [10; 15; 20];;
List.map (fun x -> x > 0) [10; 15; 20];;
List.for_all (fun x -> x > 0) [5; 10; 2];;
List.filter (fun x -> x > 0) [-5; 10; 7];;

let f l n =
List.map (fun x -> x*n) l;;
f [1; 2; 3] 5;;

let f l n =
List.filter (fun x -> x > n) l;;
f [1; 2; 3] 2;;
