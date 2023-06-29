(*exe1*)
(*
Write the function count : int list -> int list, such that the first parameter is
a list of integers. The result of the function count is a list of integer numbers where the
i-th element of a list stores the sum of all predecessing list elements.   
*)
let count (lst : int list) : int list =
  let rec sum_prefix acc lst =
    match lst with
    | [] -> []
    | x :: xs ->
        let sum = acc + x in
        sum :: sum_prefix sum xs
  in
  sum_prefix 0 lst
(*exe2*)
(*
Let 'a kv_array be a parameterized type defined as an array of key-value pairs. The
first component of a pair is a key of type int. The second component of a pair is a
value of type 'a.
type 'a kv_array = int*'a array
Write a polymorphic function,
join : ('a->'a->'a) -> 'a kv_array -> 'a kv_array -> 'a kv_array,
that joins the two input arrays of key-value pairs in the following way. The keys of the
two input arrays are unordered !
Let us name the first parameter f, the second a1, the third a2, and the result a3. For
every element (k,v) from the array a3, there exists at least one pair (k,v1) from a1
and one pair (k,v2) from a2, such that v = f v1 v2.
Explanation: The array a3 (result) includes pairs with the keys that appear in the pairs of
the array a1, and in a2 (in both!). The function f is used for merging the values of pairs
from the arrays a1 and a2, that have the same keys.    
*)
let join (f : 'a -> 'a -> 'a) (a1 : 'a kv_array) (a2 : 'a kv_array) : 'a kv_array =
  let combine_values v1 v2 = f v1 v2 in
  let (k1, arr1) = a1 in
  let (k2, arr2) = a2 in
  let merged_keys = Array.append arr1 arr2 in
  let unique_keys = Array.sort_uniq compare merged_keys in
  let merge_values key =
    let v1 = Array.fold_left (fun acc (k, v) -> if k = key then f acc v else acc) (List.assoc key arr1) a1 in
    let v2 = Array.fold_left (fun acc (k, v) -> if k = key then f acc v else acc) (List.assoc key arr2) a2 in
    (key, combine_values v1 v2)
  in
  let merged_values = Array.map merge_values unique_keys in
  (k1, merged_values)
(*exe3*)
(*
a) Define a parameterized type of a tree node named 'a bush. The nodes can have
no branches (no children), one branch (like a list) or two branches (two children). Every
node stores a key of type 'a.
Use the union type for the definition of the type 'a bush.
b) Create an instance of the type 'a bush, such that 'a=string. An instance should
contain at least three nodes.
c) Write a function print : 'a bush -> unit, that prints the keys of tree nodes by
using the depth-first ordering of tree nodes.    
*)
(a) type 'a bush =
| Leaf
| OneBranch of 'a * 'a bush
| TwoBranches of 'a * 'a bush * 'a bush
(b) let example_tree : string bush =
  TwoBranches ("root",
    OneBranch ("left", Leaf),
    OneBranch ("right", OneBranch ("sub-right", Leaf))
  )
(c) let rec print (tree : 'a bush) : unit =
  match tree with
  | Leaf -> ()
  | OneBranch (key, branch) ->
      print_string key;
      print branch
  | TwoBranches (key, branch1, branch2) ->
      print_string key;
      print branch1;
      print branch2
(*exe4*)
(*
Define a class kv_class, that implements a key-value store. The keys are of type int
and the values are of type string.
The class kv_class has two methods.
• put : int*string -> unit; enters a new key-value pair in kv_class.
• get : int -> string; returns a value for a given key.
You can suppose that the entered pairs are never deleted.
Hint: Use a simple data structure for storing key-value pairs. For example, you can use
an unordered list of key-value pairs.    
*)
class kv_class =
  object(self)
    val mutable data : (int * string) list = []

    method put (key, value) =
      data <- (key, value) :: data

    method get key =
      match List.assoc_opt key data with
      | Some value -> value
      | None -> raise Not_found
  end