let implies a b =
  match (a, b) with 
  | (true, false) -> false
  | _ -> true;;
  implies true false;; implies true true;;
  List.fold_left implies false [true; true; true];;
  List.fold_right implies [true; true; true] false;;
  let y = "0123456789";;
  y ^ y ^ y ^ y;;

  let z = "0123456789" in z ^ z ^ z ^ z;;

  let x = 10 in let y = 5 in x + y;;

  let powerOf4 x = x * x * x * x in powerOf4 12;;

  let a = 10 in let b = 2 in let c = 5 in a*b + a*c + b*c;;

  let powerOf8 x = let square y = y*y in square(square(square(x)));;
  powerOf8 2;; square;;
  
  let x = 5;;
  let rec r n = 
    match n with
    | 0 -> ""
    | _ -> "_" ^ r (n - 1);;

    r 5;;

    let s n =
      match n with
      | 0 -> "I"
      | _ -> (s (n -1)) ^ (r n) ^ (s (n - 1));;

let rec s n =
  match n with
  | 0 -> ""
  | _ -> "_" ^ r (n - 1);;

let rec s n =
  match n with
  | 0 -> "|"
  | _ let a = (s(n - 1)) in a ^ (r n) ^ a;;

let x = ref 10;;
!x;;
x := 20;; !x;;

let a = true;;
let b = ref a;;

let a = 5;;
let b = a;;
!a = 10;;
b := 10;;
!a = 10;;

let a = ref 10;;
let b = ref 10;;
let c = a;;
a = b;; a = c;; b = c;;
a == b;; a == c;; b == c;;
a := 20;;
!b;; !c;;

let squareRef = ref (fun x -> x * x);;
let squareFun x = x * x;;
let squareRef_ = ref squareFun;;

let rec f n = 
  match n with
  | 0 -> 1
  | _ -> n * f(n - 1);;

let fr = ref (fun x -> x);;
let f_ n =
  match n with
| 0 -> 1
| _ -> n * !fr (n - 1);;
fr := f_;;
f_ 5;;

let r = ref (fun x -> x);;
let sumToN n = 
  match n with
  | 0 -> 0
  | _ -> n * !r (n - 1);;
  sumToN 5;;
