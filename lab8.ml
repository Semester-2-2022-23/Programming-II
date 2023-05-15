(*
type 'a alphaList = 
| EmptyalphaList
| AList of 'a * ('a alphaList);;
let l = AList (true, AList (false, Empty));;
*)

(*
let rec addElement aL = 
match al with
| Empty -> AList (e, Empty)
| AList (h, t) -> AList (h, addElement t e);;
addElement l true;;
*)

(*
type boolEx:
  | And of boolEx * boolEx
  | Or of boolEx * boolEx
  | Not of boolEx
  | Value of bool;;
  let beVal = And (0r (Value false, Value true), Not(Value true));;
*)

(*
let rec eval e = 
  match e with
  | Value X -> x
  | And (x, y) -> (eval x) && (eval y)
  | Or (x, y) -> (eval x) || (eval y)
  | Not x -> not (eval x);;
  eval beVal;;
*)

(*
type 'a rle =
| One of 'a
| Many of int * 'a;;
let encode l = 
  let rec encode2 l count =
  match l with
  | [] -> []
  | h1::(h2::t2 as t1) -> when (h1 = h2) -> encode2 l (count + 1)
  | h::t -> (Many (count + 1))::(encode2 t 0);;
  in encode l 0;;
  encode [1; 1; 1; 1; 2; 2; 2; 5; 1; 1; 1;];;
*)

(*
type sumIntList =
| Empty
| Node of recordNode
and
recordNode = {value: int;tail : sumIntList};;
let nodeVal = {value = 5; tail = Empty}
*)

(*
let 'a bTree = 
| Empty
| Node of 'a node
and
'a node = {value : 'a lT : bTree; rT : 'a bTree};;
*)

(*
type ('a, 'b) tree =
| Nil
| NodeA of 'a * ('a, 'b) tree list
| NodeB of 'b * ('a, 'b) tree list;;
let nA1 = NodeA (5, [Nil; Nil]);;
let nB1 = NodeB ('d', [Nil]);;
let nA2 = NodeA (2, [nA1; Nil; nB1]);;
let nB2 = NodeB ('c', [Nil]);;
let root = NodeA (3, [nA2; nB2]);;
*)

(*
let rec applySplit l f =
  match l with
  | [] -> [], []
  |h::t -> (fst (f h)) :: fst (applySplit t f), (snd (f h)) @ (snd (applySplit t f));;
  let rec split tr =
    match tr with
    | Nil -> [], []
    | NodeA (v, l) ->
      v :: (fst (applySplit l split)), snd (applySplit l split)
    | NodeB (v, l) ->  
      fst (applySplit l split), v :: (snd (applySplit l split));;
      split root;;
*)
