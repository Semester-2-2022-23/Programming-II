(*This is last lab for this semestar*)
(*Reuse*)
(*Organize*)
(*Abstarct*)

(*Task 1*)
module Module1 = 
struct
  let v1 = 5
  let v2 = false
  let square n = n*n
  type t = string
  module Module2 =
  struct
  let v2 = 5.0;;
end;;
Module1.v1;; 
Module2.v2;;
Module1.square 10;;
open Module1;;
v1;;
v2;;
open Module2;;
v1;;
v2;;
Module1.v2;;
open Module1;;
v1;;
let open Module2 in v2*.v2+.v2;;
v2;;

(*Task2*)
module type SIGNATURE1 =
sig
  val v1 : int
  val v2 : bool
  val square : int -> int
  type t
end;;

(*Task3*)
module type NATURALS = 
sig
  type t
  val one : t
  val next n : t -> t
  val prev n : t -> t
  val add : t -> t -> t
  val mult : t -> t -> t
  val comp : t -> t -> t
end;;
module IntNaturals =
struct
  type t = int
  val one = 1
  let next n = n + 1
  let prev n = if (n = 1) then 1 else n - 1
    let add n m = n + m
    let mult n m = n * m
    let comp n m = if (n > m) then n else m
end;;

(*Task4*)
module NatIntoInt (N : NATURALS) =
struct
include N
let zero = 0
let prev n = n - 1
let sub n m = n - m
end;;
module M = NatIntoInt (IntNaturals)

(*Task5*)
module type SET = 
sig
  type 'b t
  val empty : 'b t
  val member : 'b t -> t -> t -> bool
  val add : 'b t -> 'b -> 'b t
  val elements : 'b t -> 'b list
end;;

(*Task6*)
module Set =
struct
  type 'b t = 'b list
  let (empty : 'b list) = []
  let member 1 (e : 'b) =
    match l with 
    | [] -> false
    | h::t -> (h = e) || member t e 
end;;

(*Task7*)
module Set =
struct
  type 'b t = 'b list
  let (empty : 'b list) = []
  let (member : 'b -> 'b list -> bool) = List.mem
  let add l (e : 'b) if (member e l) then l else @ [e]
  let elements (l : 'b t) = l
end;;

(*Task8*)
module ExtendSet (S : SET) = 
struct
  include S 
  let rec union s1 s2 = 
    match s1 with 
    | [] -> S.elements s1
    | h::t -> 
      if (S.member s1 h)
        then union s1 t
        else union (S.add s1 h) t
end;;
