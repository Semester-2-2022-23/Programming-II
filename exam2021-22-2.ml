(*exe1*)
(*
a) Write a higher-order function
filter_map : ‘a list -> (‘a->bool) -> (‘a->’a) -> ‘a list,
that first filters the elements of input ‘a list using a function (‘a->bool). Each selected
element is further mapped using a function (‘a->’a).
Example:
# filter_map [1;2;3;4] (fun x -> x mod 2 = 0) (fun x -> x+1);;
- : int list = [3; 5]
b) How can we represent and compute with integer numbers in Lambda calculus?   
*)
let filter_map lst pred f =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs ->
      if pred x then
        let mapped_x = f x in
        aux (mapped_x :: acc) xs
      else
        aux acc xs
  in
  aux [] lst

  type church_numeral = (int -> int) -> int -> int

let zero : church_numeral = fun f x -> x
let succ (n : church_numeral) : church_numeral = fun f x -> f (n f x)
let add (n : church_numeral) (m : church_numeral) : church_numeral = fun f x -> n f (m f x)
let mult (n : church_numeral) (m : church_numeral) : church_numeral = fun f x -> n (m f) x
let to_int (n : church_numeral) : int = n (fun x -> x + 1) 0
(*exe2*)
(*
a) A parametrized type ‘a matrix is defined as follows.
type ‘a matrix = ‘a array array
Write a function
submatrix : ‘a matrix -> ‘a matrix -> (‘a->’a->bool) -> int*int list,
that is defined as follows. We name the first parameter a and the second parameter b.
Assume that the dimensions of matrix b are smaller or equal to the dimensions of a.
The third parameter is a function that tests the equality of two instances of a type ‘a.
The function submatrix finds all positions in a where b matches a. Each position is a
pair of integers representing the matching position of the left upper corner of b inside the
matrix a.
b) Describe the stack-based allocation of the space for subroutines. Detail the role
of the activation records.   
*)
type 'a matrix = 'a array array

let submatrix (a : 'a matrix) (b : 'a matrix) (eq : 'a -> 'a -> bool) : (int * int) list =
  let rows_a = Array.length a in
  let cols_a = Array.length a.(0) in
  let rows_b = Array.length b in
  let cols_b = Array.length b.(0) in
  let result = ref [] in
  for i = 0 to rows_a - rows_b do
    for j = 0 to cols_a - cols_b do
      let match_found = ref true in
      for x = 0 to rows_b - 1 do
        for y = 0 to cols_b - 1 do
          if not (eq a.(i + x).(j + y) b.(x).(y)) then
            match_found := false
        done
      done;
      if !match_found then
        result := (i, j) :: !result
    done
  done;
  List.rev !result
(*exe3*)
(*
a) Represent integer arithmetic expressions with a tree composed of objects that stand
for operations and constants. The abstract class expr is the base class for the available
types of arithmetic expressions: constant, addition, subtraction, multiplication, and
(integer) division.
class virtual expr =
 object
 method virtual eval : int
 end
All classes of arithmetic operations are defined as sub-classes of the class expr.
Implement the classes for the constants and one of the binary operations!
Pay attention to the following aspects. Binary operations must have references to subexpressions to be able to evaluate the sub-expressions. Evaluation of a constant
expression simply returns a value.
Define a tree of objects for the arithmetic expression 3*10+(5-1).
b) Present the concepts of subtyping and substitutivity.    
*)
class virtual expr =
  object
    method virtual eval : int
  end

class constant (value : int) =
  object
    inherit expr
    method eval = value
  end

class addition (left : expr) (right : expr) =
  object
    inherit expr
    method eval = left#eval + right#eval
  end

  let expr_tree =
    let constant3 = new constant 3 in
    let constant10 = new constant 10 in
    let constant5 = new constant 5 in
    let constant1 = new constant 1 in
    let mult = new multiplication constant3 constant10 in
    let sub = new subtraction constant5 constant1 in
    new addition mult sub
(*exe4*)
(*
a) Implement the structure of a module KVS that stores pairs composed of a key of
a type ‘a, and a value of type ‘b. The key-value pairs are stored in a list in the increasing
order of the key.
Define the abstract data type (‘a,’b) KVS.t that stands for a type of a module.
Implement a function create : (‘a->’a->cmp) -> (‘a,’b) KVS.t,
where (‘a->’a->cmp) is a type of a comparison function for keys. It returns an instance
of a type cmp = Less | Equal | Greater. Make sure that this function is accessible in
the module KVS!
b) Implement the function min : (‘a,’b) KVS.t -> ‘a*’b that returns the element
with the smallest key.
Sketch in two sentences the implementation of a function
add : (‘a,’b) KVS.t -> ‘a*’b -> (‘a,’b) KVS.t .
Define the signature of the module to include a hidden type (‘a,’b) KVS.t and functions
create, min and add.
c) Present the constructs of the Ocaml module language.    
*)
(a) module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

module KVS (KeyType : ORDERED_TYPE) (ValueType : sig type t end) =
  struct
    type key = KeyType.t
    type value = ValueType.t
    type pair = key * value
    type t = pair list

    let create cmp = []
  end
(b) module KVS (KeyType : ORDERED_TYPE) (ValueType : sig type t end) =
struct
  type key = KeyType.t
  type value = ValueType.t
  type pair = key * value
  type t = pair list

  let create cmp = []

  let min kvs =
    match kvs with
    | [] -> failwith "Empty KVS"
    | (k, v) :: _ -> (k, v)

  let add kvs (k, v) =
    ...
end
(c) module type MY_TYPE =
sig
  type t
  val print : t -> unit
end

module MyModule : MY_TYPE =
struct
  type t = int
  let print x = print_int x
end