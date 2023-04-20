(*Write a function min_and_max : ’a list -> ’a list = <fun> that takes an
’a list, and returns an ’a list containing two elements: the minimum of
the input list and the maximum of the input list. The use of OCaml library
functions (e.g., for_all , filter, map, ...) or sorting (whether by a library
function or your own implementation) the list is not allowed.*)
let rec min_and_max list =                            
  match list with                                     
  | [] -> []                                          
  | hd :: tl ->                                                       
      let min_x, max_x = find_min_max hd hd tl in [min_x; max_x]      
      and find_min_max min_x max_x list =                             
        match list with                                               
          | [] -> (min_x, max_x)                                      
          | hd :: tl ->                                               
            let new_min_x = if hd < min_x then hd else min_x in       
              let new_max_x = if hd > max_x then hd else max_x in     
              find_min_max new_min_x new_max_x tl ;; 
