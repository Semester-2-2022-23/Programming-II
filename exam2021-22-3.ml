(*exe1*)
(*
We have the following combinators.
I = λx.x
K = λx.(λy.x)
K
*
 = λx.(λy.y)
Ω = (λx.x x)(λx.x x)
B = λf.λg.λx.f(g x)
a) Reduce the expression (B (K I Ω) I) to a value.
b) Which evaluation strategy did you use in (a)? What happens if you use the
strategy call-by-value?
c) How are the logical values represented in Lambda calculus?
*)
(a) (B (K I Ω) I)
=> ((λf.λg.λx.f(g x)) (λx.(λy.x)) Ω) I -- Substitute B, K, and I
=> (λg.λx.(λx.(λy.x)) (g x)) Ω I -- Apply B
=> (λg.λx.(λy.x)) (Ω x) I -- Apply (λx.(λy.x)) to (g x)
=> (λg.λx.(λy.x)) ((λx.x x) x) I -- Substitute Ω
=> (λg.λx.(λy.x)) (x x) I -- Apply (λx.x x) to x
=> (λg.λx.(λy.x)) x I -- Apply (x x) to x
=> (λg.λx.x) I -- Apply (λy.x) to x
=> λx.x -- Apply (λg.λx.x) to I
(c) type 'a church_bool = 'a -> 'a -> 'a

let church_true : 'a church_bool = fun x y -> x
let church_false : 'a church_bool = fun x y -> y

let church_and : 'a church_bool -> 'a church_bool -> 'a church_bool = fun p q -> p q church_false
let church_or : 'a church_bool -> 'a church_bool -> 'a church_bool = fun p q -> p church_true q
let church_not : 'a church_bool -> 'a church_bool = fun p -> p church_false church_true

let ocaml_true = church_true
let ocaml_false = church_false

let ocaml_and p q = church_and p q
let ocaml_or p q = church_or p q
let ocaml_not p = church_not p
(*exe2*)
(*
The list of pairs, including a key of type 'a and a value of type 'b is represented by the
following type.
type ('a,'b) kv_list = ('a*'b) list
Suppose the lists that we use are ordered by the increasing value of the key. We also
have the following type defined.
type cmp = Less | Equal | Greater
a) Write the function
merge : ('a,'b) kv_list -> ('a,'b) kv_list ->
 ('a->'a->cmp) -> ('a,'b) kv_list,
where the first and the second parameters are the lists of the arbitrary size ordered by
the increasing value of the key. The third parameter is the comparison function for the
instances of a type 'a.
The function merge merges sorted lists to obtain the sorted elements from both lists as
a result.
b) Describe the representation of arrays and matrices in the main memory. 
*)
(a) type cmp = Less | Equal | Greater

let rec merge lst1 lst2 cmp_func =
  match (lst1, lst2) with
  | [], _ -> lst2
  | _, [] -> lst1
  | (k1, v1) :: t1, (k2, v2) :: t2 ->
    (match cmp_func k1 k2 with
     | Less -> (k1, v1) :: merge t1 lst2 cmp_func
     | Equal -> (k1, v1) :: merge t1 t2 cmp_func
     | Greater -> (k2, v2) :: merge lst1 t2 cmp_func)

let example_cmp_func x y =
  if x < y then Less
  else if x = y then Equal
  else Greater
(b) type cmp = Less | Equal | Greater

let rec merge lst1 lst2 cmp_func =
  match (lst1, lst2) with
  | [], _ -> lst2
  | _, [] -> lst1
  | (k1, v1) :: t1, (k2, v2) :: t2 ->
    (match cmp_func k1 k2 with
     | Less -> (k1, v1) :: merge t1 lst2 cmp_func
     | Equal -> (k1, v1) :: merge t1 t2 cmp_func
     | Greater -> (k2, v2) :: merge lst1 t2 cmp_func)

let example_cmp_func x y =
  if x < y then Less
  else if x = y then Equal
  else Greater
(*exe3*)
(*
A list data structure can be defined in the following way.
type 'a q_list = {mutable first:'a qlist; mutable last:'a qlist}
and 'a qlist = Nil |
 Cons of {head:'a; mutable next:'a qlist}
The list is represented by a record of the type 'a q_list, which includes a reference
to the first element in the list (first) and a reference to the last element of the list
(last). Elements of a list are the instances of 'a qlist. The last element of a list has
the component next set to Nil.
a) Write the functions
push_front : 'a q_list -> 'a -> 'a q_list and
push_end : 'a q_list -> 'a -> 'a q_list.
The function push_front adds an element to the beginning of the list and the function
push_end adds an element to the end of the list.
b) The function pop_end : 'a q_list -> 'a removes the last element from
a list and returns it as a result. Sketch the implementation of pop_end in a few
sentences.
c) Describe the heap storage of a programming language environment. 
*)
(a) type 'a q_list = { mutable first: 'a qlist; mutable last: 'a qlist }
and 'a qlist = Nil | Cons of { head: 'a; mutable next: 'a qlist }

let push_front q_list element =
  let new_node = Cons { head = element; next = q_list.first } in
  q_list.first <- new_node;
  if q_list.last = Nil then q_list.last <- new_node;
  q_list

let push_end q_list element =
  let new_node = Cons { head = element; next = Nil } in
  match q_list.last with
  | Nil ->
    q_list.first <- new_node;
    q_list.last <- new_node;
    q_list
  | Cons last_node ->
    last_node.next <- new_node;
    q_list.last <- new_node;
    q_list
(b) let pop_end q_list =
  match q_list.first with
  | Nil -> failwith "Empty list"
  | Cons node ->
    match node.next with
    | Nil ->
      q_list.first <- Nil;
      q_list.last <- Nil;
      node.head
    | _ ->
      let rec traverse prev_node current_node =
        match current_node.next with
        | Nil ->
          prev_node.next <- Nil;
          q_list.last <- prev_node;
          current_node.head
        | Cons next_node ->
          traverse current_node next_node
      in
      traverse node node.next
(*exe4*)
(*
Suppose we have a class stack defined in the library of a programming language.
class ['a] stack
 object
 method pop : 'a
 method push : 'a -> unit
 method reverse : unit
 end
The above stack type includes solely the methods needed for this exercise. Note that
the method reverse reverses the elements stored on the stack.
a) Implement a class queue of integer numbers by using two instances of a class
stack. The first stack is used for storing the inserted new integer numbers. The second
stack is used for retrieving the elements from the queue. If the second stack is empty
and the first is not, then the first is reversed and stored as the second.
The class queue has the following type.
class queue
 object
 method enqueue : int -> unit
 method dequeue : int
 end
The method enqueue inserts an element to the beginning of a queue, and the method
dequeue returns an element from the end of the queue.
b) Describe the dynamic binding in object-oriented programming languages.    
*)
(a) class queue =
object
  val mutable in_stack = new stack
  val mutable out_stack = new stack

  method enqueue (x : int) =
    in_stack#push x

  method dequeue : int =
    match out_stack#pop with
    | x -> x
    | exception _ ->
        in_stack#reverse;
        out_stack <- in_stack;
        in_stack <- new stack;
        out_stack#pop
end
(b) class parent =
object
  method virtual greet : unit -> unit
end

class child =
object
  inherit parent
  method greet () = print_endline "Hello from child!"
end

let obj : parent = new child in
obj#greet () 