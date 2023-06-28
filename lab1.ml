(*basic types*)
(*integers*)
4;;
2 + 5;;
(*operators: +, -, *, /*)
2 * 5 mod 10;;
(*max_int / min_int*)
max_int + 1;;
(*float*)
4.0;;
4.;;
2. +. 5.;;
(*boolean*)
bool = true
bool = false
(*operators: not, &&, ||*)
not true || true;;
false && false || true;;
(*char*)
'a';;
char = '4'
(*string*)
"Hello world!";;
(*CANNOT BE COMPARED -> ERROR*)
'a' = "a";;
(*type conversion*)
float_of_int 1;;
int_of_float 1.;;
string_of_int (1+1);;
int_of_char '1';;
char_of_int 98;;

(*global variables*)
(*command let*)
let x = 10;;
let x = x < 20;;
(*command and*)
int = 18
int = 2
int = 7
(*N-tuples*)
imt * imt = (1, 2)
string * int * char * bool = ("abc", 45, 'c', true)
(*first and second elements of 2-tuple*)
int = 3 #
int = 23
(*for larger tuples this do not work!*)