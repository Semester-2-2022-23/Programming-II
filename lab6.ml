print_string "Hello World! \n"
for i = 0 to 5 do
  print_string "Hello World! \n"
done;;

for i = 0 to 5 do
  print_string "Hello";
  print_string " World! \n"
done;;

for i = 0 to 6 do
  print_int i;
  print_string "\n"
done;;

for i = 10 downto 5
print_int i;
print_string "\n"
done;;

let i = ref 5;;
while !i < 10 do
  print_int !i;
  print_string "\n";
  i := !i + 1
done;;
let a = 5;;

let divisors n =
  for i = 1 to n do
    if (n mod i = 0) then (
      print_int i;
      print_string "\n"
    )
done;;
divisors 15;;

let divisors_2 n =
  let i = ref 1 in
  while !i <= n do
    if (n mod !i = 0) then (
      print_int !i;
      print_string "\n"
    );
    i := i + 1
  done;;
;;

let square n = 
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if (i = 0 || i = n - 1 || j = 0 || j = n - 1) then
        print_string "*"
      else 
        print_string " "
      done;
      print_string "\n"
    done;;

let hourGlass n = 
    for i = 0 to n - 1 do
     for j = 0 to n - 1 do
        if (i = 0 || i = n - 1 || j = i || j = n - 1 - i) then
          print_string "*"
        else 
          print_string " "
        done;
        print_string "\n"
      done;;
      hourGlass 5;;

let upperTriangle n = 
    for i = 0 to n - 1 do
     for j = 0 to n - 1 do
        if (i = 0 || j = i || j = n - 1) then
          print_string "*"
        else 
          print_string " "
        done;
        print_string "\n"
      done;;
;;
upperTriangle 5;;

[|1; 2; 3|];;
let a = [| 'a'; "b"|];;
let b = Array.make 5 'B';;
b;;
b.(0);;
b.(4) <- 'A';;
b;;
b.(Array.lenght b) <- 'C';;


let createArray n =
  let a = Array.make n 0 in
  for i = 0 to n - 1 do
    a.(i) <- i + 1
  done;;
  createarray 10;;

let x = 0;;
let y = [| x; x |];;
y.(0) <- 5;;
y;;
x;;

let a_1 = [| 1; 2 |];;
let a_2 = [| a_1; a_1; a_1 |];;
a_2.(0).(0) <- 7;;
a_2;;
a_1;;

let a = [| 1; 2 |];;
let f a = 
  a.(0) <- 5;;
  f a;;
  a;;

let m = Array.make_matrix 3 2 1;;
Array.lenght m.(0);;
m.(0).(0);;
m.(0).(0) <- 3;;

let diagonal f m =
  for i = 0 to Array.lenght m - 1 do
      m.(i).(i) <- (f m.(i).(i))
  done;
  m
;;
  applyF (fun x -> x*x) (Array.make_matrix 3 2 2);;

(Array.make_matrix 3 2 2).(0).(0);;
(fun x -> x*x) 5;;

let sum m =
  let s = ref 0 in
  for i = 0 to Array.lenght m - 1 do
    for j = 0 to Array.lenght m.(0) - 1 do
s := m.(i).(j)
    done
  done;
  !s
   ;;
   sum (applyF (fun x -> x*x) (Array.make_matrix 3 2 2));;

let m_1 = Array.make_matrix 2 2 5;;
let m_2 = Array.make_matrix 2 2 (Array.make_matrix 2 2 3);;
m_1.(0).(0) <- (Array.make_matrix 2 2 3);;
m_1;;
m_2.(0).(0).(0).(0) <- (Array.make_matrix 2 2 1);;
m_2.(0).(0).(0).(0) <- 1;;
m_2;;
let m_1 = Array.make_matrix 2 2 5;;
let m_2 = Array.make_matrix 2 2 (Array.make_matrix 2 2 3);;
