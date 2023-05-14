(*Task 1*)
(*Flood*)
(*a. Write a function print_int_matrix : int array array -> unit = <
fun> That prints a rectangular integer matrix on screen.*)
(*b. Write a function flood : ’a array array -> ’a -> unit = <fun> that
changes the values of the input matrix according to the rules of the game
Flood: https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/
flood.html. In short, it changes the value of the top left element, along
with every element of the same value that is reachable from it, to the
specified ’a value. An element is reachable if there exist a path from the
top left element by going down, left, up, right along same value elements.*)
(*Solutions*)
(a) let print_int_matrix matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      Printf.printf "%d " matrix.(i).(j)
    done;
    Printf.printf "\n"
  done

(b) let flood matrix value =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let target = matrix.(0).(0) in
  let rec dfs i j =
    if i >= 0 && i < rows && j >= 0 && j < cols && matrix.(i).(j) = target then
      begin
        matrix.(i).(j) <- value;
        dfs (i+1) j; (* go down *)
        dfs i (j-1); (* go left *)
        dfs (i-1) j; (* go up *)
        dfs i (j+1) (* go right *)
      end
