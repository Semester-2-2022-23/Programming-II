(*exe1*)
(*
a) Write a function
substitute : ‘a list -> ‘a -> ‘a list -> ‘a list.
Suppose we name the first, second, and third arguments of the function substitute
as a, b and c, respectively. The function replaces the first appearance of b within a with
c.
Example:
# substitute [1,2,3,6,7] 3 [3;4;5];;
- : int list = [1;2;3;4;5;6;7]
b) Describe the use of parameterized (polymorphic) types. What is the relationship
between the parametric polymorphism and the parameterized types?
*)
let rec substitute lst target replacement =
  match lst with
  | [] -> []
  | x :: xs ->
      if x = target then
        replacement @ xs
      else
        x :: substitute xs target replacement
(*exe2*)
(*
a) A computer display is represented by a two-dimensional array. Each element
of the array is a pixel composed of three values, namely the intensities for the red,
green, and blue colors of the pixel. The computer display is defined in the following way.
type pixel = int*int*int
and display = pixel array array
b) Write a function submat that searches for a pattern in a display. A pattern is
value of type display, but it has smaller dimensions.
submat : display -> display -> (pixel->pixel->bool) -> int*int
The first parameter of submat is a display, the second is a pattern, and the third
parameter is the function that compares two pixels returning a result of type bool. The
function submat returns a pair of integers that stand for the position of the upper-left
corner of the pattern in the display. If the pattern does not exist, the function returns a
pair (-1,-1).
c) Describe the implementation of the one and two-dimensional arrays in the
memory managed by a programming language environment.
*)
type pixel = int * int * int
type display = pixel array array

let submat display pattern compare_fn =
  let display_rows = Array.length display in
  let display_cols = Array.length display.(0) in
  let pattern_rows = Array.length pattern in
  let pattern_cols = Array.length pattern.(0) in

  for i = 0 to display_rows - pattern_rows do
    for j = 0 to display_cols - pattern_cols do
      let found = ref true in
      for x = 0 to pattern_rows - 1 do
        for y = 0 to pattern_cols - 1 do
          let display_pixel = display.(i + x).(j + y) in
          let pattern_pixel = pattern.(x).(y) in
          if not (compare_fn display_pixel pattern_pixel) then
            found := false
        done;
      done;
      if !found then
        return (i, j)
    done;
  done;
  (-1, -1)
(*exe3*)
(*
A part of the faculty information system manages the data about persons, students,
teachers and assistants. A person is described with a name and some personal
identifier. A teacher is a person that teaches a list of courses. A student is a person that
is enrolled in a class and has an average grade. Finally, an assistant is a teacher and a
student. In addition, every assistant has a mentor.
a) Define the classes person, student, teacher and assistant. Further,
each class must include a public method display that prints the properties of the class
instances. Optimize the implementation of display by printing one property (data
member) of a class only once.
b) Describe the binding of the method display to the code for the instances of
different classes defined in (a). Which problem do we encounter with the data members
of the class person?
c) Explain the genericity of parameterized classes. When to use parameterized
classes?
*)
class person name id =
  object
    val name : string = name
    val id : int = id

    method display =
      print_endline ("Name: " ^ name);
      print_endline ("ID: " ^ string_of_int id)
  end

class student name id class_name average_grade =
  object
    inherit person name id

    val class_name : string = class_name
    val average_grade : float = average_grade

    method display =
      super#display;
      print_endline ("Class: " ^ class_name);
      print_endline ("Average Grade: " ^ string_of_float average_grade)
  end

class teacher name id courses =
  object
    inherit person name id

    val courses : string list = courses

    method display =
      super#display;
      print_endline "Courses Taught:";
      List.iter print_endline courses
  end

class assistant name id class_name average_grade mentor =
  object
    inherit student name id class_name average_grade
    inherit teacher name id []

    val mentor : string = mentor

    method display =
      super#display;
      print_endline ("Mentor: " ^ mentor)
  end
(*exe4*)
(*
A calculator that uses the reverse Polish notation works as follows. The calculator uses
a stack of values. The operation enter inserts a value to the top of the stack. The
operation plus, minus, times and divide take the top two operands from the stack,
compute the operation and put the result back to the stack. Here is a short session with
the calculator.
> enter 1
1
> enter 2
2
> plus
3
> enter 1
1
> minus
2
a) Define a module Calculator that implements a reverse Polish notation
calculator. Determine the abstract data type Calculator.t that represents the
module. Implement the operations enter and one of the binary operations plus,
minus, times or delete. You can assume that all the values are integer numbers.
b) Define the signature of the module Calculator that hides the abstract data
type and allows the use of the operations enter, plus and minus.
c) Describe modules as compilation units as defined, for example, in the
programming language C.
*)
(a) module Calculator : sig
  type t
  val create : unit -> t
  val enter : t -> int -> unit
  val plus : t -> unit
  val minus : t -> unit
end = struct
  type t = int list

  let create () = []

  let enter stack value =
    stack := value :: !stack

  let plus stack =
    match !stack with
    | x :: y :: rest ->
      let result = x + y in
      stack := result :: rest
    | _ -> failwith "Insufficient operands"

  let minus stack =
    match !stack with
    | x :: y :: rest ->
      let result = y - x in
      stack := result :: rest
    | _ -> failwith "Insufficient operands"
end
(b) module type CalculatorSig = sig
  type t
  val create : unit -> t
  val enter : t -> int -> unit
  val plus : t -> unit
  val minus : t -> unit
end
