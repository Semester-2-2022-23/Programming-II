(*exe1*)
(*
a) Define a type plist that represents lists of pairs. The first component of the pair 
contains a key of type int and the second component contains a value of type string.
b) Write a function vapply : plist → (string → string) → plist, that 
applies a function of the type string → string (2nd parameter) to the second 
components of the input pairs. The result is a list of pairs where the second component 
of input pairs is replaced with the values of the function string → string.
*)
(a) type plist = (int * string) list
(b) let vapply plist f =
  List.map (fun (key, value) -> (key, f value)) plist
(*exe2*)
(*
The text of an editor program is represented as an array of lists that include words. 
type text = string list array;;
 
Define a function find_replace : text → string → string → text, that 
replaces all instances of a given string (2nd parameter) with a string specified as 3rd
parameter in the text given as 1st parameter.
*)
type text = string list array

let find_replace text find replace =
  Array.map (fun line ->
    List.map (fun word ->
      if word = find then replace else word
    ) line
  ) text
(*exe3*)
(*
A tree a_tree is defined as follows. 
type 'a a_tree = { mutable key:'a; mutable trees: a_tree list}
Write a higher-order function 
tree_filter : 'a a_tree -> ('a → bool) -> 'a list.
The function returns a list of all keys for which the function given as the 2nd parameter 
returns the value true.
*)
type 'a a_tree = { mutable key: 'a; mutable trees: 'a a_tree list }

let rec tree_filter tree pred =
  let filtered_keys = if pred tree.key then [tree.key] else [] in
  let child_keys = List.concat (List.map (fun t -> tree_filter t pred) tree.trees) in
  filtered_keys @ child_keys
(*exe4*)
(*
Define a class queue that implements a common queue by using an array. Let the 
queue include the elements of the type int. The class has the following methods. 
enqueue: Elements are added starting at the beginning of an array. In the case we 
 come to the end of an array we continue at the beginning. 
 Check if the queue is full. 
dequeue: Elements are taken from the beginning of an array. When we come to the end
 of the array then we start from the beginning. Check if the queue is empty. 
The size of queue is a parameter of the class that should be rounded to the size in kilobytes (KB).
The class initializer enters 10 zeros just after the queue is created.
*)
module Queue = struct
  type t = {
    mutable size: int;
    mutable queue: int array;
    mutable front: int;
    mutable rear: int;
  }

  let create size =
    let size_bytes = size * 1024 in
    let queue = Array.make 10 0 in
    { size = size_bytes; queue; front = 0; rear = 0 }

  let enqueue queue element =
    if ((queue.rear + 1) mod Array.length queue.queue) = queue.front then
      raise (Failure "Queue is full");

    queue.queue.(queue.rear) <- element;
    queue.rear <- (queue.rear + 1) mod Array.length queue.queue

  let dequeue queue =
    if queue.front = queue.rear then
      raise (Failure "Queue is empty");

    let element = queue.queue.(queue.front) in
    queue.front <- (queue.front + 1) mod Array.length queue.queue;
    element

  let is_empty queue =
    queue.front = queue.rear

  let is_full queue =
    ((queue.rear + 1) mod Array.length queue.queue) = queue.front
end;;