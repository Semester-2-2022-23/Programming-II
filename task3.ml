(*Write a function apply_to_each_sublist : ’a list list -> (’a -> ’a ->
’a) -> ’a -> ’a list = <fun> that takes an ’a list list, a function ’a
-> ’a -> ’a, a default value ’a and returns an ’a list where each empty
sublist is replaced with the default value, and each other sublist is replaced by
a single value, obtained by repeatedly replacing the first two elements of the list
with the result of the input function on them until a single value remains (see
example).*)
let rec apply_to_each_sublist list f defaultValue =                                     
  match list with                                                                       
  | [] -> []                                                                            
  | hd::tl when (hd=[])-> defaultValue :: (apply_to_each_sublist tl f defaultValue)     
  | hd::tl ->                                                                           
    let rec apply_to_list l =                                                           
      match l with                                                                      
      | [] -> defaultValue                                                              
      | [x] -> x                                                                        
      | x::y::tl -> apply_to_list ((f x y) :: tl)                                       
        in                                                                              
        (apply_to_list hd) :: (apply_to_each_sublist tl f defaultValue) ;;              
