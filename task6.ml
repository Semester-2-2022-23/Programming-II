(*Task 2*)
(*Screen*)
(*You are given the type
1 type pixel ={ r : int ; g : int ; b : int };;
representing a pixel and the intensity of red, green and blue, as integers between
0 and 255.*)
(*a.Write a function brightness : int -> pixel array array -> pixel
array array = <fun> that increases/decreases all integers in all pixels
in the input pixel array array by the provided integer. The resulting
integers should not go below 0 or above 255.*)
(*b.Write a function blur : pixel array array -> pixel array array
= <fun> that blurs the input pixel array array. Pixels on the border
are unchanged, all other pixels colors however are recomputed. The new
value is 60% of the original + 5% of every neighboring pixel (out of 8).
Round down at the very end.*)
(*Solutions*)
(a) let clamp value min_val max_val =
  max min_val (min value max_val)

let brightness delta pixels =
  let rows = Array.length pixels in
  let cols = Array.length pixels.(0) in
  let clamp_val = clamp delta (-255) 255 in
  let new_pixels = Array.make_matrix rows cols {r=0; g=0; b=0} in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let p = pixels.(i).(j) in
      let r' = clamp (p.r + clamp_val) 0 255 in
      let g' = clamp (p.g + clamp_val) 0 255 in
      let b' = clamp (p.b + clamp_val) 0 255 in
      new_pixels.(i).(j) <- {r=r'; g=g'; b=b'}
    done
  done;
  new_pixels

(b) let blur pixels =
  let rows = Array.length pixels in
  let cols = Array.length pixels.(0) in
  let new_pixels = Array.make_matrix rows cols {r=0; g=0; b=0} in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if i = 0 || j = 0 || i = rows - 1 || j = cols - 1 then
        new_pixels.(i).(j) <- pixels.(i).(j)
      else
        let p = pixels.(i).(j) in
        let r =
          (int_of_float (0.6 *. float_of_int p.r
                      +. 0.05 *. float_of_int pixels.(i-1).(j-1).r
                      +. 0.05 *. float_of_int pixels.(i-1).(j).r
                      +. 0.05 *. float_of_int pixels.(i-1).(j+1).r
                      +. 0.05 *. float_of_int pixels.(i).(j-1).r
                      +. 0.05 *. float_of_int pixels.(i).(j+1).r
                      +. 0.05 *. float_of_int pixels.(i+1).(j-1).r
                      +. 0.05 *. float_of_int pixels.(i+1).(j).r
                      +. 0.05 *. float_of_int pixels.(i+1).(j+1).r))
                      |> min 255 |> max 0 in
        let g =
          (int_of_float (0.6 *. float_of_int p.g
                      +. 0.05 *. float_of_int pixels.(i-1).(j-1).g
                      +. 0.05 *. float_of_int pixels.(i-1).(j).g
                      +. 0.05 *. float_of_int pixels.(i-1).(j+1).g
                      +. 0.05 *. float_of_int pixels.(i).(j-1).g
                      +. 0.05 *. float_of_int pixels.(i).(j+1).g
                      +. 0.05 *. float_of_int pixels.(i+1).(j-1).g
                      +. 0.05 *. float_of_int pixels.(i+1).(j).g
                      +. 0.05 *. float_of_int pixels.(i+1).(j+1).g))
                      |> min 255 |> max 0 in
        let b =
          (int_of_float (0.6 *. float_of_int p.b
                      +. 0.05 *. float_of_int pixels.(i-1).(j-1).b
                      +. 0.05 *. float_of_int pixels.(i-1).(j).b
                      +. 0.05 *. float_of_int pixels.(i-1).(j+1).b
                      +. 0.05 *. float_of_int pixels.(i).(j-1).b
                      +. 0.05 *. float_of_int pixels.(i).(j+1).b
                      +. 0.05 *. float_of_int pixels.(i+1).(j-1).b
                      +. 0.05 *. float_of_int pixels.(i+1).(j).b
                      +. 0.05 *. float_of_int pixels.(i+1).(j+1).b))
                      |> min 255 |>
