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
*)
