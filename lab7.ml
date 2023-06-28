(*exe1*)
(*Write a function that counts the numbers of apples in the warehouse. Do
the same for pears.*)
let count_apples wh =
  let basket_list = wh#get_baskets in
  let rec count list = match list with
  | [] -> 0
  | h::t -> h#get_apples + count t in
  count basket_list;;
  let count_pears wh =
  let basket_list = wh#get_baskets in
  let rec count list = match list with
  | [] -> 0
  | h::t -> h#get_pears + count t in
  count basket_list;;
(*exe2*)
(*Add a method to the class warehouse, that deletes a basket at position i.*)
method delete i =
  let rec delete i list = match list with
  | [] -> []
  | h::t when (i=0) -> t
  | h::t -> h :: delete (i-1) t in
  baskets <- delete i baskets
(*exe3*)
(*Add a method to the class warehouse that returns the basket with the most
fruit in it.*)
method most_fruit =
  let compare basket1 basket2 =
  if basket1#count > basket2#count then basket1
  else basket2 in
  let rec most list = match list with
  | [] -> failwith "no basket exists"
  | [h] -> h
  | h::t -> compare h (most t) in
  most baskets
(*task4*)
(*Add a method to the class warehouse that returns a pair consisting of two
lists of baskets, one where there are strictly more apples and the other one where there
are strictly more pears.*)
method more_apples_pears =
  let rec more list (a,p) = match list with
  | [] -> (a, p)
  | h::t -> if (h#get_apples > h#get_pears) then more t (a @ [h], p)
  else if (h#get_apples < h#get_pears) then more t (a, p @ [h])
  else more t (a, p)
  in
  more baskets ([], [])
(*task5*)
(*Declare a class exam exercise with the following properties:
• variables: total points, points (a student got), difficulty (1 − 3)
• methods get for each instance variable
• methods set for points and difficulty
• method to string that prints all the information of the exam exercise
then declare a class exam with the following properties:
• variables: exam name, exam time, exercises (storing a list of exercises)
• methods to set exam name and time
• methods that get exam name, time and a list of exercises
• method that returns a list of exercises of a given difficulty
• method that calculates the total number of points on the exam
• method that calculates the grade with percentages (50 − 59: 6, 60 − 69: 7, 70 − 79:
8, 80 − 89: 9, 90 − 100: 10 and 0 otherwise).
Then create an exam with at least 5 exam exercises and print the information of all the
exercises on the exam.*)
class exam_exercise (tPoints:int) =
object (self)
val total_points = tPoints
val mutable points = 0
val mutable difficulty = 0
method get_total_points = total_points
method get_points = points
method get_difficulty = difficulty
method set_points p = points <- p
method set_difficulty d = difficulty <- d
method toString = print_newline ();
print_string "----------------"; print_newline ();
print_string ("Total points: " ^ string_of_int total_points);
print_newline ();
print_string ("Points: " ^ string_of_int points);
print_newline ();
print_string ("Difficulty: " ^ string_of_int difficulty);
print_newline ();
print_string "----------------"; print_newline ()
end;;
class exam (eList : exam_exercise list) =
object (self)
val mutable exam_name = ""
val mutable exam_time = 0
val mutable exercises = eList
method get_name = exam_name
method get_time = exam_time
method get_exercises = exercises
method set_name name = exam_name <- name
method set_time time = exam_time <- time
method get_exercisesD d =
let rec byDifficulty eList d2 = match eList with
| [] -> []
| h::t when (h#get_difficulty = d2) -> h :: byDifficulty t d2
| h::t -> byDifficulty t d2 in
byDifficulty exercises d
method countPoints =
  let rec count eList = match eList with
  | [] -> 0
  | h::t -> h#get_total_points + count t in
  count exercises
  method private countObtainedPoints =
  let rec count eList = match eList with
  | [] -> 0
  | h::t -> h#get_points + count t in
  count exercises
  method get_grade =
  let grade = match (((float_of_int self#countObtainedPoints) /.
  (float_of_int self#countPoints))*. 10) with
  | x when (x < 5) -> 0
  | x when (x < 6) -> 6
  | x when (x < 7) -> 7
  | x when (x < 8) -> 8
  | x when (x < 9) -> 9
  | _ -> 10 in
  grade
  end;;