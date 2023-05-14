(*Task 3*)
(*Tressette*)
(*You are given the types:
1 type colore = Spade | Coppa | Bastoni | Denari ;;
2 type valore = Ace | Two | Three | Four | Five | Six |
Seven | Jack | Knight | King ;;
3 type mazzo = Null | Carta of carta and carta ={ c : colore ; v
: valore ; mutable tl : mazzo };;*)
(*a. Write a function points : mazzo -> int = <fun> The given a deck of
cards it calculates the point score of it. The scoring is as follows: Ace is
worth 1 point; Two, Three, King, Knight, Jack are each worth 1/3; and the 
rest do not contribute the the point total. The total point score is equal
to the sum of the individual points, rounded down at the end.*)
(*b. Write a function max_cards : mazzo -> mazzo = <fun> that takes a
deck of cards and filters out everything except the highest cards by value
(valore) of any color. The order of cards by value is as follows: Three >
Two > Ace > King > Knight > Jack > Seven > Six > Five > Four.
Due to the size of the output, lists like generate_cards 20 2023 are not shown,
but can be generated with the use of the attached function.*)
(a) type colore = Spade | Coppa | Bastoni | Denari ;;
type valore = Ace | Two | Three | Four | Five | Six | Seven | Jack | Knight | King ;;
type carta = { c : colore ; v : valore ; mutable tl : mazzo }
and mazzo = Null | Carta of carta ;;

let points deck =
  let rec points_helper acc curr =
    match curr with
    | Null -> acc
    | Carta { v = Ace; tl = next } ->
        points_helper (acc + 1) next
    | Carta { v = Two; tl = next } ->
        points_helper (acc + 1) next
    | Carta { v = Three; tl = next } ->
        points_helper (acc + 1) next
    | Carta { v = Four; tl = next } ->
        points_helper (acc + 1) next
    | Carta { v = Five; tl = next } ->
        points_helper (acc + 1) next
    | Carta { v = Six; tl = next } ->
        points_helper acc next
    | Carta { v = Seven; tl = next } ->
        points_helper (acc + 3) next
    | Carta { v = Jack; tl = next } ->
        points_helper (acc + 3) next
    | Carta { v = Knight; tl = next } ->
        points_helper (acc + 3) next
    | Carta { v = King; tl = next } ->
        points_helper (acc + 3) next
    | Carta { tl = next } ->
        points_helper acc next
  in
  let total_points = points_helper 0 deck in
  total_points / 3

(b) let max_cards deck =
  let rec find_max_value acc curr max_val =
    match curr with
    | Null -> acc
    | Carta { v = value; c = _; tl = next } ->
        if value > max_val then find_max_value [curr] next value
        else if value = max_val then find_max_value (curr :: acc) next max_val
        else find_max_value acc next max_val
  in
  let max_values = find_max_value [] deck Three in
  let rec filter_max_values acc curr max_values =
    match curr with
    | Null -> acc
    | Carta { v = value; c = _; tl = next } ->
        if List.mem curr max_values then
          filter_max_values (Carta { c = curr.c; v = curr.v; tl = acc } ) next max_values
        else
          filter_max_values acc next max_values
  in
  let filtered_deck = filter_max_values Null deck max_values in
  filtered_deck
(*Another solution*)
(*Task 3*)
(*Tressette*)
(*You are given the types:
1 type colore = Spade | Coppa | Bastoni | Denari ;;
2 type valore = Ace | Two | Three | Four | Five | Six |
Seven | Jack | Knight | King ;;
3 type mazzo = Null | Carta of carta and carta ={ c : colore ; v
: valore ; mutable tl : mazzo };;*)
(*a. Write a function points : mazzo -> int = <fun> The given a deck of
cards it calculates the point score of it. The scoring is as follows: Ace is
worth 1 point; Two, Three, King, Knight, Jack are each worth 1/3; and the 
rest do not contribute the the point total. The total point score is equal
to the sum of the individual points, rounded down at the end.*)
(*b. Write a function max_cards : mazzo -> mazzo = <fun> that takes a
deck of cards and filters out everything except the highest cards by value
(valore) of any color. The order of cards by value is as follows: Three >
Two > Ace > King > Knight > Jack > Seven > Six > Five > Four.
Due to the size of the output, lists like generate_cards 20 2023 are not shown,
but can be generated with the use of the attached function.*)
(*Solutions*)
(a) type colore = Spade | Coppa | Bastoni | Denari ;;
type valore = Ace | Two | Three | Four | Five | Six | Seven | Jack | Knight | King ;;
type carta = { c : colore ; v : valore ; mutable tl : mazzo }
and mazzo = Null | Carta of carta ;;

let points deck =
let rec points_helper acc curr =
match curr with
| Null -> acc
| Carta { v = Ace; tl = next } ->
points_helper (acc + 1) next
| Carta { v = Two; tl = next } ->
points_helper (acc + 1) next
| Carta { v = Three; tl = next } ->
points_helper (acc + 1) next
| Carta { v = King; tl = next } ->
points_helper (acc + 1) next
| Carta { v = Knight; tl = next } ->
points_helper (acc + 1) next
| Carta { v = Jack; tl = next } ->
points_helper (acc + 1) next
| Carta { v = Seven; tl = next } ->
points_helper (acc + 1) next
| Carta { tl = next } ->
points_helper acc next
in
let total_points = points_helper 0 deck in
let fractional_points = float_of_int total_points /. 3.0 in
int_of_float fractional_points

(b) type colore = Spade | Coppa | Bastoni | Denari ;;
type valore = Ace | Two | Three | Four | Five | Six | Seven | Jack | Knight | King ;;
type carta = { c : colore ; v : valore ; mutable tl : mazzo }
and mazzo = Null | Carta of carta ;;

let max_cards deck =
let card_value_order = [Three; Two; Ace; King; Knight; Jack; Seven; Six; Five; Four] in
let rec find_max_value acc curr max_val =
match curr with
| Null -> acc
| Carta { v = value; c = _; tl = next } ->
if List.mem value card_value_order && List.index_of value card_value_order > List.index_of max_val card_value_order then
find_max_value [curr] next value
else if List.mem value card_value_order && List.index_of value card_value_order = List.index_of max_val card_value_order then
find_max_value (curr :: acc) next max_val
else
find_max_value acc next max_val
in
let max_values = find_max_value [] deck Four in
let rec filter_max_values acc curr max_values =
match curr with
| Null -> acc
| Carta { v = value; c = _; tl = next } ->
if List.mem curr max_values then
filter_max_values (Carta { c = curr.c; v = curr.v; tl = acc } ) next max_values
else
filter_max_values acc next max_values
in
let filtered_deck = filter_max_values Null deck max_values in
filtered_deck
