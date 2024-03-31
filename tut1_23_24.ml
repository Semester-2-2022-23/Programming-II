(*int*)
4;;
2 + 5;;
max_int + 1;;
(*float*)
4.0;;
4.;;
2. +. 5.;;
(*char*)
'a';;
(*string*)
"Hello World";;
"Hello" ^ "World!"
(*operations*)
2 * 5 mod 10;;
0.2 +. 0.1;;
4. -. 6.5;;
-4 / 3;;
-4. /. 3.;;
3.0 ** 2.;;
true;;
false;;
not false;;
(3>4) || (3.0>2.5);;
false && false;;
false && false || true;;
not(1=2)&&1.0<2.0
(not(1=2)||1=2)||2.0<1.0&&1.0<2.0
3 = 3;;
3 == 4;;
3 <> 3;;
4 != 5;;
3.=3.0;;
"asdas"<"aban";;
float_of_int 1;;
int_of_float 1.;;
string_of_int (1+1);;
int_of_char '1';;
char_of_int 98;;

let x = 10;;
let x = x < 20;;

(*multiple variable declaration*)
let c = 18 and d = 2 and e = 7;;
c + d -e;;
c - d +e;;
c + d *e;;
c - d *e;;
c + d / e;;

let i = 1;;
let j = i+2;;

let c=4 in c+3;;
c;;

let a=10;;
let a=40 in a+2;;
a;;

let d=10 and e=2 in d+e+10;;
(let c=10 and d=20 in c+d)>50;;
((let c=10 and d =20 in c+d))>20;;
if (3>4) then "asd1" else "2";;
let a=4 and b=3 in if (a>b) then a else b;; (*here we have 2 expressions*)

(*n-tuples*)
(1,2);;
("abd", 45, 'c', true);;
fst(3, 23);;
snd(3, 23);;

(*lists*)
[1; 2; 3; 4; 5];;
["mango"; "banana"];;
[(1,"as");(10,"banana")];;
[1,"a";10,"koper";100, "hundred"];;
[1, "a"];
(*head is the first element of the list and tail is the remaining elements of the list*)
List.hd[1;2;3;4];;
List.tl[1;2;3;4];;
List.hd(List.tl[1;2;3;4]);; (*this will give us the first element of the tail*)
(*add element 1 to [2;3;4]*)
1::[2;3;4;];;
1::2::[3;4];;
"b"::"a"::[];;
"b"::["a"];;
(*Concatenation: @*)
[1.2] @ [2.;3.4;-1.];;
[]@[1;2;3];;
[1] @ [2;3] @ [4;5;6];;
["dad"] @ ["mom"];;
