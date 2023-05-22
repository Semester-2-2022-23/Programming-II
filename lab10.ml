(*
class person pName pSurname =
object (self)
val mutable name = pName
val mutable surname = pSurname
method getName = name
method setName newName = name <- newName
method getName = surname
method setName newSurname = surname <- newSurname
method toString = print_string (name ^ " " ^ surname ^ "\n")
end;;
*)

(*
class mySup = 
object
  val field1 = 5
  val field2 = 0
  method getF1 = field1
end;;
class mySup = 
object
inherit mySup
method getF2 = field2
end;;
(*case1*)
let i1 = new mySup;; 
let i2 = new mySub;;
i1#getF1;; 
i2#getF2;;
i2#getF1;;
(*case2*)
let f a = a#getF1;;
f i1;;
f i2;;
*)

(*
class male pName pSurname (pAge : int) =
object 
inherit person pName pSurname as super
val multiple age = pAge
val sex : char = 'M'
method getAge = age
method setAge newAge = age <- newAge
method toString
super#toString;
print_int age;
orint_string "\n"
end;;
let m = new male "Bill" "Cosby" 80;;
m#toString;;
*)

(*
class female pName pSurname (pAge : int) =
object 
inherit person pName pSurname as super
val multiple age = pAge
val sex : char = 'F'
method getAge = age
method setAge newAge = age <- newAge
method toString
super#toString;
print_int age;
orint_string "\n"
end;;
let f = new female "Anastasija" "Ivona" 80;;
f#toString;;
*)

(*
class child pName pSurname pAge =
object
inherit male pName pSurname pAge as dad
inherit female pName pSurname pAge as mom
val sex = pSex
method getSex = sex
method toString =
  mom#toString;
  print_string "child\n"
  print_string (string_of_char sex)
end;;
let c = new child "Whatever" "Sur" 10 'F';;
c#getSex;;

type person = { name : string; mutable surname : string }
*)

(*
class family (m : male) (f : female) =
  object (self)
    val couple = (m, f)
    val mutable children = ([] : child list)
    method getCouple = couple
    method getChildren = children
    method addChild c = 
      c#setSurname (fst couple)#surname
      children <- c :: children
    initializer f#setSurname m#getSurname
  end;;
let f = new female "Oprah" "Winfrey" 70;;
let c = new child "C1" "C2" 50 'F';;
let fam = new family m f;;
f#getSurname;;
*)

(*
class virtual shape pColor pNumEdges =
object
  val color = pColor
  val numEdges = pNumEdges
  method virtual getArea : int
  method virtual circumference : int
end;;

class square sColor eSize =
object
inherit shape sColor 4
method getArea = eSize*eSize
method getCircumference = 4*eSize
end;;

let s = new shape "blue" 5;;
let sq = new square "red" 5;;
sq#getArea;;
sq#circumference;;
*)
