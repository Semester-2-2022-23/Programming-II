(*exe1*)
(*
Suppose that we have a function inc : int -> int that increments a given
parameter of type integer by one, but we do not have a function for adding two integers.
Implement a function
is_sum int -> int -> int -> bool,
that checks if the third parameter is a sum of the first two. The result is a boolean value.
Examples:
is_sum 5 3 8 ==> true
is_sum 10 1 8 ==> false
*)
let is_sum a b c =
  let sum = a + b in
  if sum = c then
    true
  else
    false
(*exe2*)
(*
A DNA sequence includes the symbols C, G, A, and T.
a) Define a type dna_syn by using the union type. Represent the symbols as
constructors of union type.
b) Define a type dna_array as a one-dimensional array of the instances of the type
dna_syn.
c) Write a function
longest_subseq : dna_array -> dna_syn -> int,
that returns the length of the longest sub-sequence of repeating symbols specified as
the second parameter in a DNA sequence specified as the first parameter.
Example:
longest_subseq [C;G;C;A;A;T;C;C;A;A;A;T] A ==> 3
*)
let longest_subseq dna_arr symbol =
  let n = Array.length dna_arr in
  let rec find_longest_subseq idx count max_count =
    if idx >= n then
      max_count
    else if dna_arr.(idx) = symbol then
      let new_count = count + 1 in
      let new_max_count = max count max_count in
      find_longest_subseq (idx + 1) new_count new_max_count
    else
      let new_max_count = max count max_count in
      find_longest_subseq (idx + 1) 0 new_max_count
  in
  find_longest_subseq 0 0 0
(*exe3*)
(*
A binary tree btree is defined in the following way.
type 'a node = {left: 'a btree; key: int; right: 'a btree}
and 'a btree = Nil | Node of 'a node ;;
a) Create a binary tree such that the key of the root is 7, the key of the left child is 3 and
the key of the right child is 5. There are no other nodes.
b) Write a function
is_heap 'a btree -> bool,
that checks if the tree given as the parameter is a heap. In a heap, the key of any node
is smaller than all keys from the sub-trees of a given node. 
*)
(a) let create_binary_tree () =
  let left_child = Node { left = Nil; key = 3; right = Nil } in
  let right_child = Node { left = Nil; key = 5; right = Nil } in
  Node { left = left_child; key = 7; right = right_child }
(b) let rec is_heap btree =
  let rec is_less_than_all key lst =
    match lst with
    | Nil -> true
    | Node { left; key = k; right } -> key < k && is_less_than_all key left && is_less_than_all key right
  in
  match btree with
  | Nil -> true
  | Node { left; key; right } -> is_less_than_all key left && is_less_than_all key right && is_heap left && is_heap right
(*exe4*)
(*
Define a module KVS for managing a key-value store. Suppose that the keys are of the
type string and the values are of a type 'a.
a) Define the type KVS.t as a list of key-value pairs!
b) Define the module KVS structure by implementing the functions
add : KVS.t -> string*'a -> unit, and
get : KVS.t -> string -> 'a
c) Define the module signature that hides the data type KVS.t and provides solely the
function get as an interface.
*)
module KVS : sig
  type 'a t
  val add : 'a t -> string * 'a -> unit
  val get : 'a t -> string -> 'a
end = struct
  type 'a t = (string * 'a) list

  let add kvs (key, value) =
    let rec loop kvs acc =
      match kvs with
      | [] -> (key, value) :: acc
      | (k, v) :: rest ->
          if k = key then
            (key, value) :: acc @ rest
          else
            loop rest ((k, v) :: acc)
    in
    let updated_kvs = loop kvs [] in
    updated_kvs

  let get kvs key =
    let rec loop kvs =
      match kvs with
      | [] -> raise Not_found
      | (k, v) :: _ when k = key -> v
      | _ :: rest -> loop rest
    in
    loop kvs
end;;
