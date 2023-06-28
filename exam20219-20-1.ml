(*exe1*)
(*
Write the polymorphic function shift : ‘a list -> int -> ‘a list, such that
the first parameter is a list of instances of type ‘a, and the second parameter, say n,
represents a number of cyclic shifts to the right. One cyclic shift to the right moves the
last element to the head of the list. The result of a function shift is a list obtained by
applying n cyclic shifts to the input list.    
*)
let shift lst n =
  let len = List.length lst in
  let n = n mod len in
  let rotate_right xs k =
    let rec rotate acc = function
      | 0 -> acc
      | n -> match acc with
             | [] -> []
             | hd :: tl -> rotate (tl @ [hd]) (n - 1)
    in
    rotate xs k
  in
  rotate_right lst n
(*exe2*)
(*
a) Represent a chess table as a matrix of 8x8 positions. Each position is either empty or
includes one of the figures, i.e., a king, a queen, a bishop, a knight, a rook or a pawn.
Define the types position and chess_table. Create an instance of a chess board
with empty positions.
b) Write a function
queen_attack : chess_table -> int*int -> boolean,
where the first parameter represents a chess table, the second parameter is a position.
The function returns true if a queen attacks the position specified as the second
parameter, and false otherwise. Suppose that there are no other figures on the table
but the queen.
*)
(a) type figure = King | Queen | Bishop | Knight | Rook | Pawn
type position = Empty | Occupied of figure
type chess_table = position array array

let empty_chess_table : chess_table =
  Array.make_matrix 8 8 Empty
(b) let queen_attack chess_table (row, col) =
  let is_queen = function Occupied Queen -> true | _ -> false in
  let check_direction dx dy =
    let rec check_pos r c =
      if r < 0 || r >= 8 || c < 0 || c >= 8 then false
      else match chess_table.(r).(c) with
           | Occupied Queen -> true
           | Empty -> check_pos (r + dx) (c + dy)
           | _ -> false
    in
    check_pos (row + dx) (col + dy)
  in
  let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
  List.exists (fun (dx, dy) -> check_direction dx dy) directions
(*exe3*)
(*
The polymorphic type bush is defined as follows.
type ‘a bush = { mutable key: ‘a; mutable subts: ‘a bush list }
Write a higher-order function
bush_map : (‘a -> ‘b) -> ‘a bush → ‘b bush,
where the first parameter is a function f : ‘a -> ‘b, the second parameter is a bush b1 of
type ‘a bush, and the result is a bush b2 of type ‘b bush. Each node from b1 with the
key k1 is mapped to a node of b2 with the key k2 = f k1. The structural parts of
mapped nodes are not altered.
*)
let rec bush_map f bush =
  let mapped = { key = f bush.key; subts = [] } in
  let rec map_subts subts =
    List.map (fun sub -> bush_map f sub) subts
  in
  mapped.subts <- map_subts bush.subts;
  mapped
(*exe4*)
(*
a) Define a parameterized class matrix that is used to create and manage twodimensional arrays of instances of an arbitrary type ‘a.
• Provide the code for the initialization of the class matrix instances based on the
matrix size and the initial value of the matrix elements.
• Provide two methods for reading and setting the matrix elements.
◦ get : int*int -> ‘a
◦ set : int*int -> ‘a -> unit
b) Define a class screen by using the class matrix to represent a colored screen of
some device.
• One element of a matrix represents one screen pixel. The color of a pixel is
represented in RBG format, i.e., by three integer numbers that stand for the
intensity of R (red), G (green), and B (blue) color.
• Provide the methods for reading and setting the colors for the particular pixel. The
types of methods for the color R, for example, are as follows.
◦ get_R : int*int -> int
◦ set_R : int*int -> int -> unit
*)
class ['a] matrix width height init_val =
  object (self)
    val mutable data : 'a array array =
      Array.make_matrix width height init_val

    method get (x, y) = data.(x).(y)

    method set (x, y) value = data.(x).(y) <- value
  end

class screen width height init_color =
  object
    inherit [int * int * int] matrix width height init_color

    method get_R (x, y) =
      let (r, _, _) = self#get (x, y) in
      r

    method set_R (x, y) r =
      let (_, g, b) = self#get (x, y) in
      self#set (x, y) (r, g, b)
  end
