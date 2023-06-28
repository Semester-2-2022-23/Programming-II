(*exe1*)
(*
Suppose we have an ordered sequence of integer numbers. Write a function
count : int list -> int*int list,
that accepts an ordered list as the 1st parameter and returns a list of integer pairs. The
first component of a pair is the integer number k from the input list, and the 2nd
parameter is the number of occurrences of a given number k in the input list of integers.
Example:
count [2;2;3;3;3;4;5;5] ==> [(2,2);(3,3);(4,1);(5,2)]
*)
let count lst =
  let rec count_helper acc count = function
    | [] -> acc
    | [x] -> (x, count + 1) :: acc
    | x :: (y :: _ as ys) ->
        if x = y then count_helper acc (count + 1) ys
        else count_helper ((x, count + 1) :: acc) 0 ys
  in
  List.rev (count_helper [] 0 lst)

(*exe2*)
(*
Write a function
shift : ‘a array array -> int -> ‘a array array
that shifts the contents of all matrix cells n places to the right in the following way. You
can assume that n is smaller than the length of lines.
The first element in the first line is moved to the cell n, the second element to the cell
n+1, etc. The last n elements in the first line are moved to the beginning of the second
line, and the same for all other lines. The last n elements in the last line are moved to
the first n cells of the first line.
An example and a hint:
shift [|[|1;2;3|];[|4;5;6|]|] 1 ==> [|[|6;1;2|];[|3;4;5]|]
shift [|[|1;2;3|];[|4;5;6|]|] 2 ==> [|[|5;6;1|];[|2;3;4|]|]
*)
let shift matrix n =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let shifted = Array.make_matrix rows cols matrix.(0).(0) in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let new_i = (i + (j + n) / cols) mod rows in
      let new_j = (j + n) mod cols in
      shifted.(new_i).(new_j) <- matrix.(i).(j)
    done
  done;

  shifted
(*exe3*)
(*
A list of elements of an arbitrary type can be represented with the type ‘a t_list.
type ‘a t_list = Nil | Cons of ‘a*’a t_list
Write a higher-order function
increasing ‘a t_list -> (‘a->‘a->bool) -> bool,
that checks if the elements of the list are stored in the increasing order. The second
parameter is a function of type ‘a->‘a->bool, which compares two elements of type
‘a . The function returns true if the second parameter is larger or equal to the first, and
false otherwise.
*)
let rec increasing t_list compare_func =
  match t_list with
  | Nil -> true
  | Cons (x, Nil) -> true
  | Cons (x, Cons (y, rest)) ->
      if compare_func x y then
        increasing (Cons (y, rest)) compare_func
      else
        false
(*exe4*)
(*
Implement a module Sequence that manages an increasing sequence of integer
numbers.
a) Define the abstract type Sequence.t to represent an increasing sequence of integer
numbers. Any data structure can be used.
b) Implement one of the following two methods. Describe briefly the second method (in
English).
add : Sequence.t -> int -> Sequence.t, and
min : Sequence.t -> int.
Function add adds an element (the second parameter) to the sequence (the first
paramter), and returns the updated sequence. The function min returns the smallest
element from the sequence.
c) Define the signature Priority_queue as the interface of the module Sequence
that hides the abstract type Sequence.t and provides access to all functions.
d) Create an instance of the module Sequence with the signature Priority_queue.    
*)
module Sequence : sig
  type t
  val empty : t
  val add : t -> int -> t
  val min : t -> int
end = struct
  type t = int list

  let empty = []

  let rec add sequence x =
    match sequence with
    | [] -> [x]
    | hd :: tl ->
      if x <= hd then
        x :: sequence
      else
        hd :: add tl x

  let min sequence =
    match sequence with
    | [] -> failwith "Empty sequence"
    | hd :: _ -> hd
end
