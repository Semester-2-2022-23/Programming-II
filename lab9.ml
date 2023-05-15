(*
class basket (initApples : int) (initPears : int) =
object (self)
  val mutable apples = initApples
  val mutable pears = initPears
  
  method getApples = apples
  method getPears = pears 
  method setApples numApples = apples <- numApples
  method setPears numPears = pears <- numPears
  method getFruit = self#getApples + self#getPears 
  method private getBasketInfo = "The basket contains" ^ string_of_int self#getFruit ^ "fruits.\n"
  method printInfo = print_string self#getBasketInfo
end;;
   let b = new basket 5 10
   b#getApples;; b#getPears;; b#setApples 4;; b#getApples;;
   b#getFruit;; 
*)
(*
class warehouse = 
object 
  val mutable baskets = ([] : basket list)

  method getBaskets = baskets
  method insertBaskets numApples numPears = 
    let b = new basket numApples numPears in
    baskets <- baskets @ [b]
  end;;
  let w = new warehouse;;
  w#getBaskets;;
  w#insertBaskets 7 5;;
  w#insertBaskets;;
  w#getBaskets;;

let numOfApples w =
  match w#getBaskets with
  | [] -> 0
  | h::t -> h#getApples + countA t
in countA w#getBaskets;;

let numOfPears w =
  match w#getPears with
  | [] -> 0
  | h::t -> h#getPears + countP t
in countP w#getBaskets;;

numOfApples w;;
numOfPears w;;
*)
