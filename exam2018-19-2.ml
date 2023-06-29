(*exe1*)
(*
Given the following function clip, write a function cliplist that clips every integer in
its input list.
let clip n =
if n < 0 then 0
else if n > 10 then 10
else n
Write two version of cliplist: one that uses map, and another that is a direct
recursive implementation.   
*)
let clip n =
  if n < 0 then 0
  else if n > 10 then 10
  else n

let cliplist_map lst =
  List.map clip lst
(*exe2*)
(*
A mathematical matrix can be represented with lists. For example, the matrix
[[1;1;1];[9;8;7]] has the first row [1;1;1] and the second row [9;8;7].
A valid matrix is int list list that has at least one row, at least one column, and in which
every column has the same number of rows. There are many values of type int list
list that are invalid, for example,
[]
[[1;2];[3]]
Implement a function is_valid_matrix: int list list -> bool that returns
whether the input matrix is valid. 
*)
let is_valid_matrix matrix =
  match matrix with
  | [] -> false
  | firstRow :: _ ->
    let columnCount = List.length firstRow in
    List.for_all (fun row -> List.length row = columnCount) matrix
(*exe3*)
(*
a) Define a parameterized type association array (‘a,’b) assoc_array that is an
array of records, such that
• the first components, named key, represent the keys, and,
• the second components, named value, represent values.
b) Write a function keys : (‘a,’b) assoc-array -> ‘a array, that returns an
array of all different! keys from the association array.
*)
type ('a, 'b) assoc_array = { key: 'a; value: 'b } array

let keys assoc_array =
  let unique_keys = ref [] in
  Array.iter (fun { key; _ } ->
    if not (List.mem key !unique_keys) then
      unique_keys := key :: !unique_keys
  ) assoc_array;
  Array.of_list (List.rev !unique_keys)
(*exe4*)
(*
Write a module that implements the following Fraction module type.
module type Fraction = sig
 (* A fraction is a rational number p/q, where q != 0.*)
 type t
 (* [make n d] is n/d. Requires d != 0. *)
 val make : int -> int -> t
 val numerator : t -> int
 val denominator : t -> int
 val toString : t -> string
 val toReal : t -> float
 val add : t -> t -> t
 val mul : t -> t -> t
end
*)
module type Fraction = sig
  type t
  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val toString : t -> string
  val toReal : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end

module FractionImpl : Fraction = struct
  type t = { numerator: int; denominator: int }

  let make numerator denominator =
    if denominator = 0 then
      invalid_arg "Fraction.make: denominator cannot be zero"
    else
      { numerator; denominator }

  let numerator fraction = fraction.numerator

  let denominator fraction = fraction.denominator

  let toString fraction =
    let num_str = string_of_int fraction.numerator in
    let denom_str = string_of_int fraction.denominator in
    num_str ^ "/" ^ denom_str

  let toReal fraction =
    float_of_int fraction.numerator /. float_of_int fraction.denominator

  let add fraction1 fraction2 =
    let new_numerator =
      (fraction1.numerator * fraction2.denominator) +
      (fraction2.numerator * fraction1.denominator)
    in
    let new_denominator = fraction1.denominator * fraction2.denominator in
    { numerator = new_numerator; denominator = new_denominator }

  let mul fraction1 fraction2 =
    let new_numerator = fraction1.numerator * fraction2.numerator in
    let new_denominator = fraction1.denominator * fraction2.denominator in
    { numerator = new_numerator; denominator = new_denominator }
end