(*exe1*)
(*
Let the type ('a*'b) list describe a list of pairs where the first components of pairs 
are keys of type 'a and the second components are the values of type 'b. 
Write a function 
diff : ('a*'b) list -> ('a*'b) list -> ('a*'b) list,
that computes the difference of the two lists in the following way.
1) The input lists are sorted by the value of the key of type 'a.
2) The result includes the pairs from the first list with the keys that are not included in
 the second list. 
Example: 
diff [(1,2);(2,3);(4,5)];(5,6)] [(2,4);(4,6)] → [(1,2);(5,6)]
*)
let rec diff list1 list2 =
  match list1, list2 with
  | [], _ -> []
  | _, [] -> list1
  | (key1, _)::rest1, (key2, _)::rest2 ->
  if key1 = key2 then diff rest1 rest2
  else (key1, List.assoc key1 list1) :: diff rest1 list2
(*exe2*)
(*
The type text defines the representation of text in text editor. The instance of the type 
text is composed of lines that are composed of words. 
type text = Eot | Line of line * text
and line = Eol | Word of string * line
Write a function search : text → line → bool, that returns true if a sequence of
words from the second parameter is found in the text given as the first parameter of the function.
*)

2. ```ocaml
type line = Eol | Word of string * line
type text = Eot | Line of line * text

let rec search text pattern =
  match text, pattern with
  | _, Eol -> true
  | Eot, _ -> false
  | Line (line, rest), Word (word, next) ->
      match line, word with
      | Eol, _ -> search rest pattern
      | Word (w, rest'), _ ->
          if w = word then search (Line (rest', rest)) next
          else search (Line (rest', rest)) pattern
(*exe3*)
(*
a) Define a parametric type 'a tree where the nodes of the tree are represented as 
records ! The first component of the node is a key of type 'a.The second component of 
a node represents a list of sub-trees of type 'a tree. 
b) Write the function tree_apply : 'a tree → ('a → 'b) → 'b tree. The 
keys of nodes from the input tree are replaced by the value of the function that is 
*)
type 'a tree = { key: 'a; sub_trees: 'a tree list }

let rec tree_apply tree f =
{ key = f tree.key; sub_trees = List.map (fun t -> tree_apply t f) tree.sub_trees }
(*exe4*)
(*
Reverse Polish calculator uses a stack to store the values. The operations of the 
calculator are plus, minus, mult and divide. Each of the operations takes the top 
two values from the stack, evaluates the operation, and, returns the value to the top of 
the stack as well as to the caller. 
a) Define an abstract class rpc that defines a generic reverse Polish calculator. The 
values manipulated by the calculator are of arbitrary type 'a. The abstract class rpc
implements, besides the arithmetic operations, also the operations push and pop. 
b) Define a concrete class float_rpc as the implementation of the class rpc for the 
concrete type of values float.
*)
(a) 
class ['a] rpc =
  object
    val mutable stack : 'a list = []
    method push (x: 'a) = stack <- x :: stack
    method pop () =
      match stack with
      | [] -> failwith "Stack underflow"
      | x::xs -> stack <- xs; x
    method plus () =
      let x = this#pop () in
      let y = this#pop () in
      this#push (x +. y)
    method minus () =
      let x = this#pop () in
      let y = this#pop () in
      this#push (y -. x)
    method mult () =
      let x = this#pop () in
      let y = this#pop () in
      this#push (x *. y)
    method divide () =
      let x = this#pop () in
      let y = this#pop () in
      this#push (y /. x)
  end
(b) class float_rpc =
object
inherit [float] rpc
end;;