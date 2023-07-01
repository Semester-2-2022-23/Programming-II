(*exe1*)
(*
Black-and-white picture is represented using the following type. 
type picture = array array int
Integer values represent the intensity of the pixels. Write the function 
match : picture -> picture -> (int*int)
that finds in the picture (1st parameter) the position (x,y) where 2D-pattern represented 
as small picture (2nd parameter) is the closest match to the pattern in the picture.
*)
type picture = int array array

let match_pattern picture small_picture =
  let rows = Array.length picture in
  let cols = Array.length picture.(0) in
  let small_rows = Array.length small_picture in
  let small_cols = Array.length small_picture.(0) in
  let min_diff = ref max_int in
  let min_pos = ref (0, 0) in

  for i = 0 to rows - small_rows do
    for j = 0 to cols - small_cols do
      let diff = ref 0 in
      for k = 0 to small_rows - 1 do
        for l = 0 to small_cols - 1 do
          diff := !diff + abs (picture.(i + k).(j + l) - small_picture.(k).(l))
        done;
      done;

      if !diff < !min_diff then begin
        min_diff := !diff;
        min_pos := (i, j)
      end
    done;
  done;

  !min_pos
(*exe2*)
(*
The distance between two lists is the minimum number of basic single-element 
operations required to transform one list into the other. The basic single-element edits 
are defined as:
• deleting one element from the list,
• inserting an element into the list, and
• replacing an element with a different one.
For given two lists of the same type, create a function 
edit_distance : 'a list -> 'a list -> int, 
that outputs the distance of the given lists.
*)
let edit_distance list1 list2 =
  let m = List.length list1 in
  let n = List.length list2 in
  let dp = Array.make_matrix (m+1) (n+1) 0 in

  for i = 0 to m do
    dp.(i).(0) <- i;
  done;

  for j = 0 to n do
    dp.(0).(j) <- j;
  done;

  for i = 1 to m do
    for j = 1 to n do
      if list1.(i-1) = list2.(j-1) then
        dp.(i).(j) <- dp.(i-1).(j-1)
      else
        dp.(i).(j) <- 1 + min (min dp.(i-1).(j) dp.(i).(j-1)) dp.(i-1).(j-1)
    done;
  done;

  dp.(m).(n)
(*exe3*)
(*
Expressions of simple language TP can include integer values and operations PLUS 
and TIMES. Both operations are left associative and TIMES has higher priority than 
PLUS. The following expression:
1 PLUS 2 TIMES 3 TIMES 4,
corresponds to arithmetic expression 1 + ((2 * 3) * 4). Expressions of language 
TP are defined by using the following types. 
# type operation = PLUS | TIMES;; 
type operation = PLUS | TIMES 
# type element = Val of int | Op of operation;; 
type element = Val of int | Op of operation
# type expr = list element;;
type expr = list element
Write the function calc : expr -> int, that calculates the value of given 
expression.
*)
type operation = PLUS | TIMES
type element = Val of int | Op of operation
type expr = element list

let calc expr =
  let rec evaluate stack = function
    | [] -> List.hd stack
    | Val n :: tail -> evaluate (n :: stack) tail
    | Op op :: tail ->
      match stack with
      | b :: a :: rest ->
        let result =
          match op with
          | PLUS -> a + b
          | TIMES -> a * b
        in
        evaluate (result :: rest) tail
      | _ -> failwith "Invalid expression"
  in

  evaluate [] expr
(*exe4*)
(*
Define a class Array, that contains an array of fixed size n with elements of type 'a. The
class should contain the following methods:
size : int (size of array)
get : int -> 'a (return i-th element of array) 
replace : int -> 'a -> unit (replace i-th element with instance of 'a)
Define a class Hash as a subclasses of Array, where we have 
'a = (int * 'b) list. 
Define the appropriate methods for Hash, where instance of int is a key and instance 
of 'b is value. The class should have the following methods.
hashfunction : int -> int (surjective function f : Z -> [0..n-1]) 
insert : int -> 'b -> unit (inserts instance of 'b using key i) 
get : int -> 'b (return instance of 'b with key i)
replace : int -> 'b -> unit (replace record with key i with new value
*)
class ['a] array_class n =
  object (self)
    val mutable arr = Array.make n (Obj.magic 0)

    method size = n

    method get i = arr.(i)

    method replace i value = arr.(i) <- value
  end;;

class ['b] hash_class n =
  object (self)
    inherit ['b list] array_class n

    method private hash_function key = key mod n

    method insert key value =
      let index = self#hash_function key in
      let current_list = self#get index in
      let updated_list = (key, value) :: current_list in
      self#replace index updated_list

    method get key =
      let index = self#hash_function key in
      let current_list = self#get index in
      let rec find_value = function
        | [] -> raise Not_found
        | (k, v) :: tail -> if k = key then v else find_value tail
      in
      find_value current_list

    method replace key value =
      let index = self#hash_function key in
      let current_list = self#get index in
      let rec update_value = function
        | [] -> raise Not_found
        | (k, v) :: tail -> if k = key then (key, value) :: tail else (k, v) :: update_value tail
      in
      let updated_list = update_value current_list in
      self#replace index updated_list
  end;;