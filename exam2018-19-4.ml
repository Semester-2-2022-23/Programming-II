(*exe1*)
(*
a) Define a type date as a triple where the first component is a year represented as an
integer number, the second component is an integer number from the interval [1..12]
representing the month, and the last component represents a day with an integer
number.
b) Implement a function is_before : date -> date -> bool that takes two dates
as input and evaluates to true or false. It evaluates to true if the first argument is a
date that comes before the second argument. (If the two dates are the same, the result
is false.)
*)
type date = int * int * int

let is_before (date1 : date) (date2 : date) : bool =
  match date1, date2 with
  | (year1, month1, day1), (year2, month2, day2) ->
    if year1 < year2 then true
    else if year1 > year2 then false
    else if month1 < month2 then true
    else if month1 > month2 then false
    else day1 < day2
(*exe2*)
(*
a) Define a polymorphic type kv_store that stores pairs composed of a key of the type
int, and a value of a type ‘a . Pairs are stored in an array of pairs that are not ordered
in any particular way.
b) Write a function max_key : ‘a kv_store -> ‘a, that returns the value of a pair
with the largest key.
*)
type 'a kv_pair = int * 'a
type 'a kv_store = 'a kv_pair array

let max_key (store : 'a kv_store) : 'a =
  let max_pair = ref store.(0) in
  for i = 1 to Array.length store - 1 do
    let key, _ = store.(i) in
    let max_key, _ = !max_pair in
    if key > max_key then
      max_pair := store.(i)
  done;
  let _, value = !max_pair in
  value
(*exe3*)
(*
   A bush is a tree such that a tree node can have either no children, one child or two
children nodes. Each tree node stores an instance of a type ‘a. The polymorphic type
‘a bush is defined as follows.
type ‘a bush = None
 | One of ‘a*‘a bush
 | Two of ‘a*‘a bush*’a bush
a) Create an instance of the type 'a bush, such that 'a=float. An instance should
contain at least three nodes.
b) Write a function print_level : 'a bush -> int -> unit that prints the keys
of bush nodes from the given level defined with the second parameter. 
*)
(a) let example_bush : float bush = Two (1.0, One (2.0, None), Two (3.0, None, One (4.0, None)))
(b) let rec print_level (bush : 'a bush) (level : int) : unit =
  match bush, level with
  | None, _ -> ()
  | One (key, child), 0 -> print_float key; print_string " "
  | One (_, child), _ -> print_level child (level - 1)
  | Two (key, left_child, right_child), 0 -> print_float key; print_string " "
  | Two (_, left_child, right_child), _ ->
      print_level left_child (level - 1);
      print_level right_child (level - 1)
(*exe4*)
(*
The display of some device is represented by a matrix of real numbers. Define a class
display with the following properties.
a) Use class parameters to provide the dimensions of the display. Write the code for the
initialization of the matrix. Initially, all cells are set to 0.0.
b) Write methods get:int->int->float and set:int->int->float->unit, that
either get or set the value of a matrix cell with indexes given as the first two parameters.
c) Write a method mat_map: (float->float)->unit, where the first parameter is a
function that is applied to the value of each matrix cell, and the results of the function are
assigned back to the given cells. 
*)
class display (rows : int) (cols : int) =
  object (self)
    val matrix : float array array = Array.make_matrix rows cols 0.0

    method get (row : int) (col : int) : float =
      matrix.(row).(col)

    method set (row : int) (col : int) (value : float) : unit =
      matrix.(row).(col) <- value

    method mat_map (f : float -> float) : unit =
      for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
          matrix.(i).(j) <- f matrix.(i).(j)
        done
      done
  end