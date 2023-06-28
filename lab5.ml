(*exe1*)
(*
Use both fold functions separately to apply logical implication to a
given list. Apply both functions on the list [true;true;true] and an initial
value of false.   
*)
let logical_implication a b =
  (not a) || b

let apply_logical_implication_left lst =
  List.fold_left logical_implication false lst

let apply_logical_implication_right lst =
  List.fold_right logical_implication lst false

let lst = [true; true; true]

let result_left = apply_logical_implication_left lst
let result_right = apply_logical_implication_right lst

print_endline (string_of_bool result_left);;    
print_endline (string_of_bool result_right);;   
(*exe2*)
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
let apply_logical_implication_left lst =
  List.fold_left implies false lst
let apply_logical_implication_right lst =
  List.fold_right implies lst false
let lst = [true; true; true]
let result_left = apply_logical_implication_left lst
let result_right = apply_logical_implication_right lst
print_endline (string_of_bool result_left);;    
print_endline (string_of_bool result_right);;   
(*exe3*)
(*
Declare local variables a, b, and c and return a*b + a*c - b*c
• Declare a global function powerOf8, that applies a local function square to
an integer parameter three times
*)
let calculate_expression () =
  let a = 5 in
  let b = 3 in
  let c = 2 in
  a * b + a * c - b * c
let result = calculate_expression ()
print_endline (string_of_int result);;
(*exe4*)
(*
Declare local variables a, b, and c and return a*b + a*c - b*c
let a = 5 in let b = 2 in let c = 4 in a*b + a*c - b*c;;
• Declare a global function powerOf8, that applies a local function square to
an integer parameter three times
let powerOf8 x = let square x = x*x in square (square ( square (x)));;   
*)
let calculate_expression () =
  let a = 5 in
  let b = 2 in
  let c = 4 in
  a * b + a * c - b * c
let result = calculate_expression ()
print_endline (string_of_int result);;
(*exe5*)
(*
We have the following variables:
let a = ref 10;;
let b = ref 10;;
let c = a;;
• Test each pair of variables for structural equality ("=")
• Test each pair of variables for physical equality ("==")
• See if assigning (“ := “) a new value to a changes b and c   
*)
let a = ref 10
let b = ref 10
let c = a
let test_structural_equality () =
  if !a = !b then
    print_endline "a and b are structurally equal"
  else
    print_endline "a and b are not structurally equal"
  if !a = !c then
    print_endline "a and c are structurally equal"
  else
    print_endline "a and c are not structurally equal"
let test_physical_equality () =
  if !a == !b then
    print_endline "a and b are physically equal"
  else
    print_endline "a and b are not physically equal"
  if a == c then
    print_endline "a and c are physically equal"
  else
    print_endline "a and c are not physically equal"
let assign_new_value () =
  a := 20;
  print_endline "After assigning a new value to a:"
  print_endline ("a: " ^ string_of_int !a)
  print_endline ("b: " ^ string_of_int !b)
  print_endline ("c: " ^ string_of_int !c)
test_structural_equality ()
test_physical_equality ()
assign_new_value ()
(*exe6*)
(*
Test each pair of variables for structural equality ("=")
a = b;; a = c;; b = c;;
• Test each pair of variables for physical equality ("==")
a == b;; a == c;; b == c;;
• See if assigning (:=) a new value to a changes b and c
a := 5;;
!b;; !c;;   
*)
let a = ref 10
let b = ref 10
let c = a
let test_structural_equality () =
  if !a = !b then
    print_endline "a and b are structurally equal"
  else
    print_endline "a and b are not structurally equal"
  if !a = !c then
    print_endline "a and c are structurally equal"
  else
    print_endline "a and c are not structurally equal"
  if !b = !c then
    print_endline "b and c are structurally equal"
  else
    print_endline "b and c are not structurally equal"
let test_physical_equality () =
  if !a == !b then
    print_endline "a and b are physically equal"
  else
    print_endline "a and b are not physically equal"
  if a == c then
    print_endline "a and c are physically equal"
  else
    print_endline "a and c are not physically equal"
  if b == c then
    print_endline "b and c are physically equal"
  else
    print_endline "b and c are not physically equal"
let assign_new_value () =
  a := 5;
  print_endline "After assigning a new value to a:"
  print_endline ("a: " ^ string_of_int !a)
  print_endline ("b: " ^ string_of_int !b)
  print_endline ("c: " ^ string_of_int !c)
test_structural_equality ()
test_physical_equality ()
assign_new_value ()
(*exe7*)
(*
Recursive functions without using the keyword rec – factorial:
let refFun = ref (fun x -> x);;
let fact n =
match n with
| 0 -> 1
| _ -> n * !refFun (n-1);;
refFun := fact;;
fact 5;;
*)
let refFun = ref (fun x -> x)
let fact n =
  match n with
  | 0 -> 1
  | _ -> n * (!refFun (n - 1))
let () =
  refFun := fact
let result = fact 5
print_endline (string_of_int result);;
(*exe8*)
(*
Write a non-recursive function sumToN using function referencing,
equivalent to its recursive version:
let rec sumToN n =
match n with
| 0 -> 0
| _ -> n + sumToN (n - 1);;   
*)
let sumToN n =
  let refFun = ref (fun x -> x + !refFun (x - 1)) in
  let baseCase = ref (fun _ -> 0) in
  refFun := (fun x -> if x = 0 then !baseCase x else !refFun x);
  !refFun n
(*exe9*)
(*
Write a non-recursive function sumToN using function referencing,
equivalent to its recursive version:
let refFun = ref (fun x -> x);;
let sumToN_ n =
match n with
| 0 -> 0
| _ -> n + !refFun (n-1);;
refFun := sumToN_;;
*)
let refFun = ref (fun x -> x)
let sumToN_ n =
  match n with
  | 0 -> 0
  | _ -> n + (!refFun (n - 1))
let () =
  refFun := sumToN_
let result = sumToN_ 5
print_endline (string_of_int result)
(*exe10*)
(*
Define a variable counter initialized to 0:
let counter = ref 0;; .
Write a next_val function, with no parameters*, which in each call |
increases the value of counter by 1 and returns the new value. |
(*you can declare the function like "let next_val _ = ..." or "let next_val () = ..."*) 
*)
let counter = ref 0
let next_val () =
  counter := !counter + 1;
  !counter
(*exe11*)
(*
Define a variable counter initialized to 0:
let counter = ref 0;; .
Write a next_val function, with no parameters*, which in each call |
increases the value of counter by 1 and returns the new value. |
(*you can declare the function like "let next_val _ = ..." or "let next_val () = ..."*)
let counter = ref 0;;
let next_val _ =
counter := (!counter) + 1;
!counter;;   
*)
let counter = ref 0
let next_val () =
  counter := !counter + 1;
  !counter
(*exe12*)
(*
Write a next_val2 function, with no parameters, which in each call increases
the value of a variable counter2 by 1 and returns the new |value, with
counter2 accessible only inside the scope of next_val.
*)
let next_val2 =
  let counter2 = ref 0 in
  fun () ->
    counter2 := !counter2 + 1;
    !counter2
(*exe13*)
(*
Write a next_val2 function, with no parameters, which in each call increases
the value of a variable counter2 by 1 and returns the new |value, with
counter2 accessible only inside the scope of next_val.
let next_val_2 =
let counter_2 = ref 0 in
fun () ->
incr counter_2;
!counter_2;;   
*)
let next_val2 =
  let counter2 = ref 0 in
  fun () ->
    incr counter2;
    !counter2