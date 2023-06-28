(*exe1*)
(*
Define a variable counter initialized to 0:
let counter = ref 0;; .
Write a next_val function, with no parameters*, which in each call |
increases the value of counter by 1 and returns the new value. |
(*you can declare the function like "let next_val _ = ..." or "let next_val () = ..."*)
*)
let counter = ref 0;;
let next_val _ =
counter := (!counter) + 1;
!counter;;
(*exe2*)
(*
Write a next_val2 function, with no parameters, which in each call increases
the value of a variable counter2 by 1 and returns the new |value, with
counter2 accessible only inside the scope of next_val.
*)
let next_val_2 =
  let counter_2 = ref 0 in
  fun () ->
  incr counter_2;
  !counter_2;;
(*exe3*)
(*
a) Use the for loop to write a function that prints all divisors of a given
natural number.
b) Solve the problem by using the while loop instead.
c) For each of the following shapes (square, sandHour, upperTriangle) write
a function that takes a parameter n and draws the given shape
(examples are for n = 5).
*****     *****     *****
*   *      * *       *  *
*   *       *         * *
*   *      * *         **
*****     *****         *
*)
(a) let divisors n =
  for i = 1 to n do
  if n mod i == 0 then (
  print_int i;
  print_string " "
  )
  done;;
(b) let divisors2 n =
  let i = ref 1 in
  while !i <= n do
  if (n mod !i = 0) then
  (
  print_int !i;
  print_string \n );
  incr i
  done;;
(c) let square n =
  for i=0 to n-1 do
  for j=0 to n-1 do
  if (i=0 || i=n-1 || j=0 || j=n-1) then
  print_string "*"
  else
  print_string " "
  done;
  print_string \n
  done;;
  square:
  (i=0 || i=n-1 || j=0 || j=n-1)
  sandHour:
  (i = 0 || i = n-1 || i = j || i = n - 1 - j)
  upperTriangle
  (i = 0 || i = j || j = n - 1)
(*exe4*)
(*
Write a function that returns an array containing the integers from 1 to n.
*)
let array1toN n =
  let a = Array.make n 1 in
  for i = 1 to n do
  a.(i - 1) <- I
  done;
  a;;
(*exe5*)
(*
Create an array a_1 containing integers 1 and 2, and an array
a_2 = [| a_1; a_1; a_1|]. Change the first element of the first element of a_2.
What is now the value of a_2?
*)
let a_1 = [| 1; 2 |] in
let a_2 = [| a_1; a_1; a_1 |] in
a_2.(0).(0) <- 3;
(*arrays and references*)
let x = 0;;
let y = [|x; x; x;|];;
y.(0) <- 1;;
y;;
x;;
let a_1 = [|1; 2|];;
let a_2 = [|a_1; a_1; a_1;|];;
a_2.(0).(0) <- 5;;
a_2;;
a_1;;
a_1.(1) <- 3;;
a_2;;
let a = Array.make 2 1;;
let b = a;;
b.(0) <- 5;;
b;;
a;;
(*exe6*)
(*
Write a function that applies a given function f over the diagonal elements of
matrix m.
*)
let diag f m =
  for i = 0 to ((Array.length m) - 1) do
  m.(i).(i) <- f m.(i).(i)
  done;
  m;;
(*exe7*)  
(*
Let m_1 and m_2 be matrices of integers and matrices, respectively:
let m_1 = Array.make_matrix 2 2 5;;
let m_2 = Array.make_matrix 2 2 (Array.make_matrix 2 2 3);;
a) What is the value of m_1 when you change the element at position (0, 0)
to 1?
b) What is the value of m_2 when you change the element at position (0, 0)
to (Array.make_matrix 2 2 1)?
c) Without doing b), what is the value of m_2 when you change the element
at position (0, 0, 0, 0) to 1?
*)
let m_1 = Array.make_matrix 2 2 5;;
m_1.(0).(0) <- 1;;
let m_2 = Array.make_matrix 2 2 (Array.make_matrix 2 2 3);;
m_2.(0).(0) <- Array.make_matrix 2 2 1;;
[|  [| [| 3; 3 |]; [| 3; 3 |] |];
  [| [| 3; 3 |]; [| 3; 3 |] |] |]
(*exe8*)
(*Write a function that multiplies a matrix m with a vector v.*)
let mult_mat_vec m v =
  let vec = Array.make (Array.length v) 0 in
  for i = 0 to ((Array.length m) - 1) do
  for j = 0 to ((Array.length m.(i)) - 1) do
  vec.(i) <- vec.(i) + m.(i).(j)*v.(j)
  done
  done;
  vec;;
