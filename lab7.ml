type 'a someType = { intVal:int; aVa1:'a };;
let b = { intVal=5; aVa1=true };;
let c = { intVal=0; aVa1='f' };;
let d = { intVal=0; aVa1='f' };;
let e = { intVal=0; aVa1='h' };;

c = d;;
d = e;;
(* b = c;; *)
type vehicle =
  { typo:string; color:string; manufactureYear:int };;

let v1 = { typo="Car"; color="Green"; manufactureYear=1997 };;
let v2 = { color="Black"; manufactureYear=2010; typo="JetSki"};;

v1.typo;;
v2.typo;;
v2.manufactureYear;;

(* we declare a list from type vehicle *)
[v1; v2];;
type person = 
  { name : string; surname : string; age : int; occ : string };;
let pA = { name = "Iztok"; surname = "Savnik"; age=50; occ="Proffessor"};;
pA.name;;
pA.age;;


let youngerPerson p =
  { name = p.name; occ = p.occ; age = p.age-20; surname = p.surname };;

youngerPerson pA;;
pA.age;;


type mutablePerson = 
  { name : string; surname : string; mutable age : int; occ : string };;
let mpA = { name = "Iztok"; surname = "Savnik"; age=55; occ="Proffessor"};;
mpA.age;;
(* mpA.age <- 35;; *)
mpA.age;; 
type _2Dpoint =
  { mutable x:float; mutable y:float };;

let point = { x = 3.5; y = 5.3 };;
point;;

let swap p =
  let temp = p.x in 
  p.x <- p.y;
  p.y <- temp;;
;;
swap point;;
point;;
type floatOrBool = F of float | B of bool;;

let a = F 5.0;;
let b = F 5.0;;
let c = B true;;

a = b;;
b = c;;
type listOr2tuple = L of int list | T of int * int;;
let a = L [2; 3];;
T (4, 5);;

(* in case the type is unknown, this is the way to check *)
(true, 5);;
[[(Array.make 5 1)]];;
type color = Red | Green | Blue | White;;
type color2 = 
  | Red
  | Green 
  | Blue 
  | White
  | RGB of int * int * int;;

let a = Red;;
let b = Blue;;
let c = RGB (0, 0, 0);;
type color = Red | Green | Blue | White;;
type color2 = 
  | Red
  | Green 
  | Blue 
  | White
  | RGB of int * int * int;;

let a = Red;;
let b = Blue;;
let c = RGB (0, 0, 0);;

c;;

match c with
  | RGB (r, g, b) -> (r, g, b)
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | White -> (255, 255, 255);;
  type color2 = 
  | Red
  | Green 
  | Blue 
  | White
  | RGB of int * int * int;;

let a = Red;;
let b = Green;;
let c = RGB (25, 254, 107);;
c;;

let rgb c =
  match c with 
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 0)
  | White -> (255, 255, 255)
  | RGB (r, g, b) -> (r, g, b);;

rgb a;;
rgb b;;
rgb c;;
type ('a, 'b) someType = V1 of int | V2 of 'a | V3 of 'b;;
let a = V2 "b";;
let b = V3 false;;
let c = V2 0;;

[a; b];;
[b; c];;
(* [a; c];; *)
type 'a aType = Int of int | B of 'a;;
let n = Int 10;;
let m = B "true";;


type ('a, 'b) gORh = G of 'b | H of 'a;;
G true;;
H false;;
(* first type types were called product types *)
(* these are called sum types*)

type myInt = A of int;;
let b = A 5;;

type myString = S of string;;
S "hello, world!";;

type intOrFloat = G of int | F of float;;
let a = G 10;;
let b = F 10.0;;
a = b;;
type integerList = Empty | IList of int * integerList ;;
let emptyList = Empty;;
let oneElementList = IList (7, Empty);;
let twoElementList = IList (5, IList (6, Empty));;
let threeElementList = IList (8, twoElementList);;

threeElementList;;

let getT l =
  match l with
  | IList (_, t) -> t
  | Empty -> Empty;;    (* this line of code was written by me *)

getT threeElementList;;
(* custom types *)
let p1 = ("Zhivko", "Stoimchev", 20, "Student");;

(* find age of tuple custom *)
let p1 = ("Zhivko", "Stoimchev", 20, "Student");;
let getAge p =
  match p with
  | (_, _, age, _) -> age;;
getAge p1;;
getAge (false, false, false, false);;   (* we get false, it is problem, because it can be applied to every 4 tuple *)
(* now to fix it` *)

(* we define our own type *)
type person = 
  { name : string; surname : string; age : int; occ : string };;
let pA = { name = "Iztok"; surname = "Savnik"; age=50; occ="Proffessor"};;
pA.name;;
pA.age;;
