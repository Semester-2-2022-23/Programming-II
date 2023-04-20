(*Write a function insert_and_merge : (’a * int) list -> ’a * int -> (’
a * int) list = <fun> that takes a (’a * int) list, sorted in increasing
order with respect to the first tuple value we call the key, and an element ’a
* int. The function inserts the element at the appropriate position so the list
remains sorted and outputs said list. Assume there are no duplicate keys and
thus if the new element has the same key as another element in the list, the
function sums their values instead. Sorting the list is not allowed.*)
let rec insert_and_merge list (key, value) =                        
  match list with                                                   
  | [] -> [(key, value)]                                       
  | (k, v)::tl ->                                              
      if key < k then (key, value)::list                       
      else if key = k then (k, v + value)::tl                  
      else (k, v)::(insert_and_merge tl (key, value)) 
