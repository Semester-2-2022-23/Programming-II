(*exe1*)
(*
a) In the following λ-expressions, make all parentheses explicit: 
• (λx.xa)ax 
• (λz.zxz)(λy.yx)z 
b) Find all free (unbound) variable occurrences in the following λ-expressions: 
• (λb.xba)xb 
• λx.zyλy.yx 
c) Write with as little parentheses as possible:
• ((xy)(λy.(λz.(z(λy.(xy)))x)y)) 
*)
(a) let expr1 = (λx.(x a)) a x;;
let expr2 = (λz.(z x z))(λy.(y x)) z;;
(b) let expr1_free_vars = ["x"; "a"; "b"];;
let expr2_free_vars = ["x"; "z"; "y"];;
(c) let expr3 = xy(λy.(λz.(z(λy.(xy)))x)y);;
(*exe2*)
(*
Write polymorphic function 
list_rapp : ('a->'a->'a) -> 'a list -> ('a->'a). 
Function applies the first parameter function of type ('a->'a->'a) to the second 
parameter list of type 'a list such that 
list_rapp f l = function x -> f l1 (f l2 ... (f ln x) ... ) ,
where l1 is the first element of l and ln is the last element of l.
Example: 
# let f a b = a+b;; 
val f : int -> int -> int = <fun>
# (list_rapp f [1;2;3;4]) 0;; 
- : int = 10 
Hint: 
You can use function compose: 
# let compose f g x = f (g x) ;; 
val compose : (’a -> ’b) -> (’c -> ’a) -> ’c -> ’b = <fun>
*)
let list_rapp f lst =
  let compose g h x = g (h x) in
  let rec apply acc = function
    | [] -> acc
    | hd :: tl -> apply (compose f acc hd) tl
  in
  apply (fun x -> x) lst
(*exe3*)
(*
Let S={1,2,...,n}. The permutation on S is a bijection from S to itself. In this exercise
permutations are represented by a sequence of integers from S and we use an 
int list to store them. 
The cipher is one of most common operations in cryptography. Given a string and a 
permutation (both of the same length), the permutation cipher changes the order of 
letters in the given string according to the given permutation. 
Write a function encrypt : int list -> string -> string that, for a given 
input permutation (1st parameter) and string (2nd parameter) 
• checks if given list is a proper permutation and
• returns the encrypted string. 
*)
let encrypt permutation str =
  let n = List.length permutation in
  let validate_permutation lst =
    let sorted = List.sort_uniq compare lst in
    sorted = List.init n (fun i -> i + 1)
  in
  let rec encrypt_helper chars acc =
    match chars with
    | [] -> acc
    | c :: cs ->
        let index = c - 1 in
        let encrypted_char = String.make 1 (String.get str index) in
        encrypt_helper cs (encrypted_char :: acc)
  in
  if not (validate_permutation permutation) then
    failwith "Invalid permutation: not a proper bijection."
  else
    String.concat "" (List.rev (encrypt_helper permutation []))
(*exe4*)
(*
a) Define module Matrix that includes abstract data type Matrix.t = 'a matrix
of dimension n x m using the module language of OCaml. Module functions are:
new : int * int -> 'a -> Matrix.t
get : Matrix.t -> int * int -> 'a 
set : Matrix.t -> int * int -> 'a -> 'Matrix.t 
dim : Matrix.t -> int * int 
sub : Matrix.t -> Matrix.t -> bool 
equ : Matrix.t -> Matrix.t -> bool 
Operation new is a constructor that creates a matrix of dimensions specified with 1st
parameter and default value defined with the 2nd parameter. Operations get and set
are usual getter and setter. Operation dim returns matrix dimensions. Operation sub
checks if the first matrix is subsumed by the second matrix. Operation equ checks if all 
the components of matrices are equal. 
b) Define two signatures: the first one allows users to see abstract type Matrix.t and 
all the functions, but the second one hides Matrix.t as well as the functions sub and 
equ.
*)
module Matrix : sig
  type 'a t
  val new_matrix : int * int -> 'a -> 'a t
  val get : 'a t -> int * int -> 'a
  val set : 'a t -> int * int -> 'a -> 'a t
  val dim : 'a t -> int * int
  val sub : 'a t -> 'a t -> bool
  val equ : 'a t -> 'a t -> bool
end = struct
  type 'a t = 'a array array

  let new_matrix (n, m) default =
    Array.make_matrix n m default

  let get matrix (i, j) =
    matrix.(i).(j)

  let set matrix (i, j) value =
    matrix.(i).(j) <- value;
    matrix

  let dim matrix =
    let n = Array.length matrix in
    let m = Array.length matrix.(0) in
    (n, m)

  let sub matrix1 matrix2 =
    let (n1, m1) = dim matrix1 in
    let (n2, m2) = dim matrix2 in
    n1 <= n2 && m1 <= m2

  let equ matrix1 matrix2 =
    let (n1, m1) = dim matrix1 in
    let (n2, m2) = dim matrix2 in
    n1 = n2 && m1 = m2 && matrix1 = matrix2
end

module type MatrixSig = sig
  type 'a t
  val new_matrix : int * int -> 'a -> 'a t
  val get : 'a t -> int * int -> 'a
  val set : 'a t -> int * int -> 'a -> 'a t
  val dim : 'a t -> int * int
end

module type PublicMatrixSig = sig
  include MatrixSig
  val sub : 'a t -> 'a t -> bool
  val equ : 'a t -> 'a t -> bool
end

module PublicMatrix : PublicMatrixSig = Matrix