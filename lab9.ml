(*exe1*)
(*
Define an interface for the module Naturals. Then define its structure. Structure must
include the first natural number 1, functions next and previous, which take a natural
number and compute the next or previous natural number, functions add and mult which
add or multiply two natural numbers, and a function compare which returns the higher of
the two numbers.
*)
module type NATURALS = sig
  type t
  val one : t
  val next : t -> t
  val prev : t -> t
  val add : t -> t -> t
  val mult : t -> t -> t
  val compare : t -> t -> t
  end;;
  module Naturals : NATURALS = struct
  type t = int
  let one = 1
  let next n = n+1
  let prev n = if n=0 then 0 else n-1
  let add n m = n+m
  let mult n m = n*m
  let compare (n:int) (m:int) = if n>m then n else m
  end;;  
(*exe2*)
(*
Define a functor that will map Naturals into Integers.
*)
module MakeInt (N : NATURALS) = struct
  include N
  let zero = 0
  let prev n = n-1
  let sub n m = n-m
  end;;  
(*exe3*)
(*
Define an interface for the module Set. Elements of a set can be of any type and we must
be able to represent a set using any structure that can store multiple elements. The
signature should also contain the following definitions: empty (returns an empty set),
member (checks if an element is contained in the set), add (adds an element to the set),
and elements (returns the set as a list).
*)
module type SET =
sig
 type 'b s
 val empty : 'b s
 val member : 'b -> 'b s -> bool
 val add : 'b -> 'b s -> 'b s
 val elements : 'b s -> 'b list
end;;
(*exe4*)
(*
Define a module Set using a list structure to represent the set.
*)
module ListSet : SET =
struct
 type 'b s = 'b list
 let empty = []
 let member e u = List.mem e u
 let add e u = u @ [e]
 let elements u = u
end;;
Recursive implementation of member:
 let rec member u (e : 'b) =
 match u with
 | [] -> false
 | h::t -> (h = e) || member t e
(*exe5*)
(*
Define a functor that will produce a structure ExtendedSet from a Set, which will contain
all the definitions of a set together with the definitions for union and intersection of two
sets
*)
module MakeSet (M : SET) = struct
  include M
  let rec union s1 s2 =
  match s2 with
  | [] -> M.elements s1
  | h::t ->
  if M.member h s1
  then union s1 t
  else union (M.add h s1) t
  let intersection s1 s2 =
  let rec intersection2 s1 s2 s =
  match s1 with
  | [] -> s
  | h::t ->
  if M.member h s2
  then intersection2 t s2 (M.add h s)
  else intersection2 t s2 s
  in intersection2 s1 s2 M.empty
  end;;
  module ExtendedSet = MakeSet(Set);;