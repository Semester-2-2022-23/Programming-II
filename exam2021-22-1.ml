(*exe1*)
(*
(a) Logical values and operators can be expressed in the lambda calculus in the
following way.
true ≡ λp.λq.p
false ≡ λp.λq.q
AND ≡ λp.λq.p q p
OR ≡ λp.λq.p p q
NOT ≡ λp.p false true
IF ≡ λp.λa.λb.p a b
Use the above combinators to write a λ-expression that computes the operation XOR
presented in the following table.
a b (XOR a b)
----------------------
0 0 0
1 0 1
0 1 1
1 1 0
Present the reduction of the expression XOR true false.
(b)  Present the important language properties of lambda calculus. 
*)
XOR true false
= (λa.λb.((a (NOT b)) (NOT (a b)))) true false
= (λb.((true (NOT b)) (NOT (true b)))) false
= ((true (NOT false)) (NOT (true false)))
= ((true true) (NOT (true false)))
= (true (NOT (true false)))
= (true (NOT true))
= (true false)
= λp.λq.q
(*exe2*)
(*
a) Define a parametric type pmatrix that describes a matrix (array of arrays) of
elements of a type ‘a.
Write a higher-order function
map_wind : ‘a pmatrix -> int*int -> int*int -> (‘a -> ‘a) -> ‘a pmatrix,
that maps the elements from the window, specified by the upper left corner (2nd
argument) and the lower right corner (3rd argument), using a function of a type (‘a -> ‘a).
Create a matrix of the size 5x5 that has all elements initialized to 1.0. Map the elements
in the window specified with the points (2,2) and (4,4) by using a function that adds one
to the argument value.
c) Describe the concept of a pointer in imperative programming languages.   
*)
type 'a pmatrix = 'a list list

let map_wind matrix upper_left lower_right func =
  let start_row, start_col = upper_left in
  let end_row, end_col = lower_right in

  let rec modify_elements row col =
    if row > end_row then
      matrix
    else if col > end_col then
      modify_elements (row + 1) start_col
    else
      let modified_element = func (List.nth (List.nth matrix row) col) in
      let modified_row = List.mapi (fun i el -> if i = col then modified_element else el) (List.nth matrix row) in
      let modified_matrix = List.mapi (fun i row -> if i = row then modified_row else row) matrix in
      modify_elements row (col + 1)
  in

  modify_elements start_row start_col

let matrix = List.init 5 (fun _ -> List.init 5 (fun _ -> 1.0))

let window_upper_left = (2, 2)
let window_lower_right = (4, 4)

let add_one x = x +. 1.0

let result = map_wind matrix window_upper_left window_lower_right add_one

let () =
  List.iter (fun row -> List.iter (fun el -> print_float el; print_string " ") row; print_endline "") result
(*exe3*)
(*
a) A parameterized class stack is provided by a library. It has the following type.
class ['a] stack :
 int ->
 object
 method empty : bool
 method full : bool
 method pop : 'a
 method push : 'a -> unit
 end
Using a parameterized class stack, define a class stack2 that works as a standard
stack but is implemented with a stack of stacks.
Implement the class stack2 to include the currently active stack and a stack of stacks.
When the currently active stack is full, it is stored in the stack of stacks. When the
currently active stack is empty, it is replaced by the stack from the top of the stack of
stacks.
b) Present the problems and solutions that appear with the multiple inheritance.    
*)
class ['a] stack2 stack_size =
  object (self)
    val mutable active_stack = new stack stack_size
    val stack_of_stacks = new stack stack_size

    method private switch_stack () =
      if active_stack#empty then
        begin
          if not stack_of_stacks#empty then
            active_stack <- stack_of_stacks#pop
          else
            active_stack <- new stack stack_size
        end

    method empty : bool =
      active_stack#empty

    method full : bool =
      active_stack#full

    method pop : 'a =
      let value = active_stack#pop in
      self#switch_stack ();
      value

    method push (value : 'a) : unit =
      if active_stack#full then
        begin
          stack_of_stacks#push active_stack;
          active_stack <- new stack stack_size;
        end;
      active_stack#push value
  end
(*exe4*)
(*
a) We would like to implement a text editor as an Ocaml module named Editor.
First, define the structure part of the module Editor. Specify an abstract data type
Editor.t of the edited text. Represent the edited text as a list of lines. Further, represent
a line by a list of words of type string.
Implement a function delete : Editor.t -> string -> Editor.t that deletes all
instances of a word from the complete text.
Define the signature of the module Editor to hide the abstract data type and to include
the signatures of public functions. Finally, define also an instance of module Editor by
specifying the previously defined structure and signature.
c) Describe parameterized modules (functors). Give an example of a
parameterized module.   
*)
module type EDITOR = sig
  type t
  val delete : t -> string -> t
end

module Editor : EDITOR = struct
  type line = string list
  type t = line list

  let delete (text : t) (word : string) : t =
    let remove_word_from_line (line : line) : line =
      List.filter (fun w -> w <> word) line
    in
    List.map remove_word_from_line text
end

module type Container = sig
  type 'a t
  val create : unit -> 'a t
  val add : 'a -> 'a t -> unit
  val size : 'a t -> int
end

module ListContainer : Container = struct
  type 'a t = 'a list ref

  let create () = ref []

  let add item container = container := item :: !container

  let size container = List.length !container
end

module SetContainer : Container = struct
  type 'a t = 'a list ref

  let create () = ref []

  let add item container =
    if not (List.mem item !container) then
      container := item :: !container

  let size container = List.length !container
end
