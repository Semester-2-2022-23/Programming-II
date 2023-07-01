(*exe1*)
(*
Let the type ('a*'b) list describe a list of pairs where the first components of pairs 
are keys of type 'a and the second components are the values of type 'b. 
Write higher-order function 
meet : ('a*'b) list -> ('a*'b) list -> ('a*('b*'b)) list,
that joins two lists sorted by the value of the key of type 'a.The pairs from the input lists
are joined only if they agree in the keys. The value part of the pairs in the resulting list 
are composed of the pairs of values from both input lists. 
Example: 
meet [(1,2);(2,3);(4,5)] [(2,4);(4,6)] –→ [(2,(3,4));(4,(5,6))]
*)
let rec meet list1 list2 =
  match (list1, list2) with
  | [], _ | _, [] -> []
  | (key1, value1)::rest1, (key2, value2)::rest2 ->
    if key1 = key2 then
      (key1, (value1, value2)) :: meet rest1 rest2
    else if key1 < key2 then
      meet rest1 list2
    else
      meet list1 rest2
(*exe2*)
(*
Suppose we have a sorted array of integers. Define the function encode that returns an 
array of pairs where the first components of pairs are the elements of the input array and
the second components represent the number of occurrences of the elements in the 
input array. 
Example:
encode [|1;1;3;4;4;5|] → [|(1,2);(3,1);(4,2);(5,1)|]
*)
let encode arr =
  let rec count_occurrences count acc = function
    | [] -> acc
    | [x] -> (x, count + 1) :: acc
    | x :: (y :: _ as rest) ->
      if x = y then
        count_occurrences (count + 1) acc rest
      else
        count_occurrences 0 ((x, count + 1) :: acc) rest
  in
  Array.of_list (List.rev (count_occurrences 0 [] (Array.to_list arr)))
(*exe3*)
(*
The boolean expressions are represented by using the following type bool_exp. 
type bool_exp =
 | Val of bool
 | Not of bool_exp
 | And of bool_exp * bool_exp
 | Or of bool_exp * bool_exp;
Write function 
eval : bool_exp -> bool, 
that evaluates the parameter boolean expression and returns a boolean value. 
*)
type bool_exp =
  | Val of bool
  | Not of bool_exp
  | And of bool_exp * bool_exp
  | Or of bool_exp * bool_exp

let rec eval = function
  | Val b -> b
  | Not exp -> not (eval exp)
  | And (exp1, exp2) -> eval exp1 && eval exp2
  | Or (exp1, exp2) -> eval exp1 || eval exp2
(*exe4*)
(*
a) Define a class matrix that is used for the representation of matrices storing the 
elements of arbitrary type 'a. 
1. The parameters of the class should define the dimensions of matrix and the initial 
value of the elements.
2. The method set : int*int → 'a → unit, where the first parameter 
represents the indices of the element and the second parameter is the new value 
of the indexed element.
3. The method get : int*int → 'a, that returns an element with indices
defined with the first two parameters.
b) Define the class int_matrix as the subclass of the class matrix.
c) How would you define a method equals : int*int → int*int → bool, that 
compares two elements of matrix by using the equality relation.
*)
class ['a] matrix rows cols initial_value =
  object (self)
    val mutable matrix : 'a array array = Array.make_matrix rows cols initial_value

    method set row col value =
      matrix.(row).(col) <- value

    method get row col =
      matrix.(row).(col)
  end

class int_matrix rows cols initial_value =
  object
    inherit [int] matrix rows cols initial_value

    method equals row1 col1 row2 col2 =
      self#get row1 col1 = self#get row2 col2
  end;;