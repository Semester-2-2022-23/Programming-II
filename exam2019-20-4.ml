(*exe1*)
(*
Suppose we have a list of integer numbers and a function of type int->bool that maps
integer numbers to boolean values. Write a function
longest_sublist int list -> (int->bool) -> int,
that determines the length of the longest sub-list composed of integer numbers for which
the function int->bool returns the value true.
Example:
# let is_even n = n mod 2 = 0;;
val is_even : int -> bool = <fun>
# longest_sublist [1;2;4;3;7;8;6;12] is_even;;
- : int = 3   
*)
let longest_sublist lst pred =
  let rec helper sublist current_max = function
    | [] -> current_max
    | x :: xs ->
        if pred x then
          let new_sublist = x :: sublist in
          helper new_sublist (max current_max (List.length new_sublist)) xs
        else
          helper [] current_max xs
  in
  helper [] 0 lst
(*exe2*)
(*
a) Define a type 'a screen to be used for the representation of a computer display.
• A screen is a two-dimensional array of pixels.
• A screen includes pixels that are represented with a type 'a.
• The dimension of a screen is given when the instance of the type 'a screen is
created.
b) Create an instance of a type 'a screen.
 Hint: Make a tiny screen, and do not forget to choose the color of pixels.
c) Write a function
screen_map : 'a screen -> ('a->'a) -> 'a screen,
that, given an instance of a screen, applies a function 'a->'a to each pixel, stores new
pixels back to the cells, and returns the transformed screen. 
*)
type 'a screen = 'a array array

let create_screen width height pixel =
  Array.make_matrix height width pixel

let screen_map screen f =
  let height = Array.length screen in
  let width = Array.length screen.(0) in
  let transformed_screen = create_screen width height None in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let original_pixel = screen.(i).(j) in
      let transformed_pixel = f original_pixel in
      transformed_screen.(i).(j) <- transformed_pixel
    done
  done;
  transformed_screen
(*exxe3*)
(*
A binary tree btree is either a leaf or a node composed of a key of type 'a, a left subtree and a right sub-tree.
type 'a btree =
| Leaf
| Node of 'a * 'a btree * 'a btree
Write a function
tree_map : 'a btree -> ('a->b') -> 'b btree,
that applies a function 'a->b' to the keys of the input tree nodes, stores the results
back to nodes, and, therefore, returns a tree of type 'b btree. 
*)
type 'a btree =
  | Leaf
  | Node of 'a * 'a btree * 'a btree

let rec tree_map tree f =
  match tree with
  | Leaf -> Leaf
  | Node (key, left, right) ->
    let transformed_key = f key in
    let transformed_left = tree_map left f in
    let transformed_right = tree_map right f in
    Node (transformed_key, transformed_left, transformed_right)

    let tree =
      Node (10,
        Node (5,
          Leaf,
          Leaf
        ),
        Node (15,
          Leaf,
          Leaf
        )
      )
    
    let transformed_tree = tree_map tree (fun x -> x * 2)
    
    (* Print the transformed tree *)
    let rec print_tree tree =
      match tree with
      | Leaf -> print_string "Leaf"
      | Node (key, left, right) ->
        print_string "(";
        print_tree left;
        print_string " ";
        print_int key;
        print_string " ";
        print_tree right;
        print_string ")"
    
    print_tree transformed_tree;
    print_newline ()
(*exe4*)
(*
A class point is defined as follows.
class point x y = object
 val mutable x = x
 val mutable y = y
 method display = print_string ("(" ^ (string_of_int x) ^ "," ^
 (string_of_int y) ^ ")")
end
a) Define classes circle and square by specializing the class point. In both classes,
 define a method display that overrides the method display from the class point.
b) Define a class round_square by inheriting from the classes circle and square.
Write a method display that alternates the calls of methods display from both
super-classes. In other words, the instance of round_square presents itself once as a
circle, and the next time as a square, and then a circle,
*)
class point x y = object
  val mutable x = x
  val mutable y = y

  method display =
    print_string ("(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")")
end

class circle x y r = object
  inherit point x y

  val mutable radius = r

  method display =
    super#display;
    print_string (" (circle, radius: " ^ string_of_int radius ^ ")")
end

class square x y s = object
  inherit point x y

  val mutable side = s

  method display =
    super#display;
    print_string (" (square, side: " ^ string_of_int side ^ ")")
end

class round_square x y r s = object
  inherit circle x y r
  inherit square x y s

  method display =
    if (x + y) mod 2 = 0 then
      super#display
    else
      (super :> square)#display
end;;
