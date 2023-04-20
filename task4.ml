(*Write a function holds_for_lists : ’a list -> ’b list -> (’a -> ’b ->
bool) -> bool =<fun> that takes an ’a list, an ’b list, a function f: ’
a -> ’b ->bool and returns true if and only if
• The two list are of the same length, and
• (f a b) is true for all pairs a, b such that a is from the first list, b is from
the second list, and a and b appear in the same position in their lists*)
let rec holds_for_lists lst1 lst2 f =   
  match lst1, lst2 with                 
  | [], [] -> true                      
  | hd1 :: tl1, hd2 :: tl2              
  -> f hd1 hd2                       
  && holds_for_lists tl1 tl2 f    
  | _, _ -> false ;
