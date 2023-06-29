(*exe1*)
(*
Define a function that takes an integer d and string m as input and returns true just
when d and m form a valid date. Here, a valid date has a month that is one of the
following abbreviations: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec.
And the day must be a number that is between 1 and the minimum number of days in
that month, inclusive. For example, if the month is Jan, then the day is between 1 and
31, inclusive, whereas if the month is Feb, then the day is between 1 and 28, inclusive.
How terse (i.e., few and short lines of code) can you make your function? You can
definitely do this in fewer than 12 lines.
*)
let is_valid_date d m =
  let days_in_month = function
    | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 31
    | "Apr" | "Jun" | "Sep" | "Nov" -> 30
    | "Feb" -> 28
    | _ -> invalid_arg "is_valid_date: invalid month"
  in
  match m with
  | "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec" ->
      d >= 1 && d <= days_in_month m
  | _ -> false
(*exe2*)
(*
Write a function is_unimodal : int list -> bool that takes an integer list and
returns whether that list is unimodal. A unimodal list is a list that monotonically increases
to some maximum value then monotonically decreases after that value. Either or both
segments (increasing or decreasing) may be empty. A constant list is unimodal, as is the
empty list.
*)
let is_unimodal lst =
  let rec is_increasing = function
    | [] | [_] -> true
    | x :: y :: rest -> x <= y && is_increasing (y :: rest)
  in
  let rec is_decreasing = function
    | [] | [_] -> true
    | x :: y :: rest -> x >= y && is_decreasing (y :: rest)
  in
  match lst with
  | [] -> true
  | [_] -> true
  | x :: rest -> is_increasing (x :: rest) || is_decreasing (x :: rest)
(*exe3*)
(*
a) Write a function multiples : int -> int -> int array, where the first
parameter is a number k and the second parameter is a number n. The array returned
by the function includes n multiplies of k (starting with k).
Example:
# mutiples 4 5;;
- : int array = [|4;8;12;16;20|]
b) Suppose that the input arrays contain multiples of the first element. Write a function
lcm : int array -> int array -> int, that computes the least common
multiple form the two input arrays of the same length. The function returns 0 if LCM does
not exist.
Use arrays, not lists!
*)
(a) let multiples k n =
  Array.init n (fun i -> k * (i + 1))
(b) let lcm arr1 arr2 =
  let length = Array.length arr1 in
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
  in
  let lcm_helper a b =
    if a = 0 || b = 0 then 0
    else (a * b) / (gcd a b)
  in
  let rec find_lcm i lcm_val =
    if i = length then lcm_val
    else find_lcm (i + 1) (lcm_helper lcm_val arr2.(i))
  in
  find_lcm 0 arr1.(0)
(*exe4*)
(*
The display of some device is represented by a matrix of real numbers. Define a class
display with the following properties.
a) Use class parameters to provide the dimensions of the display. Write the code for the
initialization of the matrix. Initially all cells are set to 0.0.
b) Write methods get:int->int->float and set:int->int->float->unit, that
either get or set the value of a matrix cell with indexes given as the first two parameters.
c) Write a method clip:float->float->unit, where the first parameter is the
minimal value and the second parameter is a maximal value of matrix elements. The
method clip clips the values of all cells to the given interval.   
*)
class display (rows : int) (cols : int) =
  object (self)
    val mutable matrix : float array array = Array.make_matrix rows cols 0.0

    method get (row : int) (col : int) : float =
      matrix.(row).(col)

    method set (row : int) (col : int) (value : float) : unit =
      matrix.(row).(col) <- value

    method clip (min_val : float) (max_val : float) : unit =
      for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
          matrix.(i).(j) <- max min_val (min max_val matrix.(i).(j))
        done
      done
  end;;