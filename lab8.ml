(*task1*)
(*Define a class person with two arguments for person name and surname,
two variables of type string that can be changed to store the two values for name and
surname, methods get and set and a method toString, that will print the information of
the person.*)
class person pName pSurname =
object
val mutable name:string = pName
val mutable surname:string = pSurname
method get_name = name
method get_surname = surname
method set_name pName = name <- pName
method set_surname pSurname = surname <- pSurname
method toString =
print_newline ();
print_string ("Name: " ^ name); print_newline ();
print_string ("Surname: " ^ surname); print_newline ()
end;;
(*task2*)
(*2. Define a class male that inherits the class person, with an additional parameter for age and a fixed value M for sex of type char. Then add methods get for the
new variables, and a method set for age. In the end override the method toString by
using the method of a superclass to print also the new information.*)
class male pName pSurname pAge =
object
inherit person pName pSurname as super
val mutable age:int = pAge
val sex:char = ’M’
method get_age = age
method get_sex = sex
method set_age pAge = age <- pAge
(*overriden method toString*)
method toString =
  super#toString;
  print_string ("Age: " ^ string_of_int age); print_newline ();
  print_string ("Sex: " ^ String.make 1 sex); print_newline ()
  end;;
(*task3*)
(*Follow the previous exercise to create a class female, with a fixed value for
sex F.*)
class female pName pSurname pAge =
object
inherit person pName pSurname as super
val mutable age:int = pAge
val sex:char = ’F’
method get_age = age
method get_sex = sex
method set_age pAge = age <- pAge
(*overriden method toString*)
method toString =
super#toString;
print_string ("Age: " ^ string_of_int age); print_newline ();
print_string ("Sex: " ^ String.make 1 sex); print_newline ()
end;;
(*task4*)
(*Define a class child that will inherit from both male and female class in
such a way, that the sex will always be M. Override the method toString by calling the
method of the female superclass. Then change the class so that the sex of the child will
be defined with an additional parameter.*)
class child pName pSurname pAge pSex =
object
inherit female pName pSurname pAge as mom
inherit male pName pSurname pAge as dad
(* overriden variable sex, otherwise it takes the last value,
* which is from the class male *)
(* val sex = pSex *)
(* overriden method toString *)
method toString =
mom#toString
end;;
(*task5*)
(*Define a class family that creates a family of male and female stored as
a pair, and an empty list of children. When creating a family, you need to change the
surname of the female to that of the male. Then add a method for adding a new child
to the family that will have surname equal to males surname.*)
class family (m:male) (f:female) =
object
val couple = (m,f)
val mutable children = ([] : child list)
method get_couple = couple
method get_children = children
method add_child c =
c#set_surname (fst couple)#get_surname;
children <- children@[c]
initializer f#set_surname m#get_surname
end;;
(*task6*)
(*Define an abstract class shape with the variables storing its color and the
number of edges. Add also the methods for calculating the area and circumference of the
shape that will be declared in the subclass. Then define a subclass square which takes
the color and the size of the edge as parameters, has an additional variable for the size
of the edge and inherits the class shape.*)
class virtual shape sColor num_edges =
object
val color = (sColor : string)
val edges = (num_edges : int)
method virtual area : int
method virtual circumference : int
end;;
class square sColor a =
object
inherit shape sColor 4
val a = (a : int)
method area = a * a
method circumference = 4 * a
end;