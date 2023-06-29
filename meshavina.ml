(* 2016/17 - 1 *)
(* Task 1 *)
let rec meet = fun list1 list2 ->
  match list1, list2 with
  | [], [] -> []
  | hd::tl, [] -> []
  | [], hd::tl -> []
  | hd1::tl1, hd2::tl2 ->
  let (k1, v1) = hd1 and (k2, v2) = hd2 in
  if k1 = k2 then (k1, (v1, v2))::(meet tl1 tl2)
  else if k1 < k2 then meet tl1 list2
  else meet list1 tl1;;

(* Task 2 *)
(*with list*)
let rec encode = fun list ->
  match list with
  | [] -> []
  | hd::tl ->
  let etl = encode tl in
  match etl with
  | [] -> [(hd, 1)]
  | hd1::tl1 ->
  let (v, r) = hd1 in
  if hd = v then (v, r + 1)::tl1
  else (hd, 1)::etl;;

(*with array*)
let encode array = 
	let n=(Array.length array)-1 and 
	counter= ref 1 and 
	resultindex = ref 0 and 
	result= Array.make (let num= ref 1 in for i= 1 to ((Array.length array)-1) do if array.(i-1) <> array.(i) then num:=!num+1 done;!num) (0,0) in
		for i=1 to n do
			if array.(i-1) <> array.(i) then
				begin
  				result.(!resultindex) <- (array.(i-1),!counter);
  				resultindex:=!resultindex+1;
					counter:=1;
					if i=n then
						result.(!resultindex) <- (array.(i),!counter);
				end
			else
				begin
  				counter:=!counter+1;
  				if i=n then
  						result.(!resultindex) <- (array.(i),!counter);
				end
		done;
		if n=0 then
			[|(array.(0),1)|]
		else
			result;;

encode [|1;1;3;4;4;5|]

(* Task 3 *)
let rec eval = fun expression ->
  match expression with
  | Val value -> value
  | Not expr -> not (eval expr)
  | And (expr1, expr2) -> (eval expr1) && (eval expr2)
  | Or (expr1, expr2) -> (eval expr1) || (eval expr2);;

(* Task 4 *)
class ['a] matrix m n v =
object
  val data = Array.make_matrix m n (v : 'a)
  method set i j v = data.(i).(j) <- (v : 'a)
  method get i j = data.(i).(j)
end;;

class int_matrix m n =
object
  inherit [int] matrix m n 0
end;;

let equals = fun m1 m2 ->
  let equals_rows = fun row1 row2 ->
  Array.fold_left (fun i j -> i && j) true
  (Array.map2 (fun i j -> i = j) row1 row2)
  in
  Array.fold_left (fun i j -> i && j) true
  (Array.map2 (fun row1 row2 -> equals_rows row1 row2) m1 m2);;

(* 2015/16 - 2 *)
(* Task 1 *)
(*
a)
(\x.xa)ax = ((\x.(xa))(ax))
(\z.zxz)(\y.yx)z = (((\z.(zxz))(\y.(yx)))z)
b)
(\b.xba)xb : bound first b
(\z.zxz)(\y.yx)z : bound first z, first y
c)
((xy)(\y.(\z.(z(\y.(xy)))x)y)) = xy\y.(\z.z(\y.xy)x)y
*)
(* Task 2 *)
let rec list_rapp = fun f list ->
  fun x ->
  match list with
  | [] -> x
  | hd::tl -> f hd ((list_rapp f tl) x);;

(* Task 3 *)
let rec proper = fun list n ->
  match n with
  | 0 -> true
  | n -> (List.exists (fun x -> x = n) list) && (proper list (n - 1));;

let rec encrypt = fun l s ->
  let n = (String.length s) in
  if ((List.length l) != n) || (not (proper l n)) then "error"
  else
  match l with
  | [] -> ""
  | hd::tl ->
  let letter = (String.get s 0) and rest = (String.sub s 1 (n - 1)) and
  tl1 = List.map (fun x -> if x > hd then x - 1 else x) tl in
  let rest1 = (encrypt tl1 rest) in
  (String.sub rest1 0 (hd - 1)) ^
  (String.make 1 letter) ^
  (String.sub rest1 (hd - 1) (n - hd));;

(* Task 4 *)
module Matrix =
struct
  type 'a t = 'a array array;;
  let create = fun m n v -> Array.make_matrix m n v;;
  let get = fun m i j -> m.(i).(j);;
  let set = fun m i j v -> m.(i).(j) <- v;;
  let dim = fun m ->
  if Array.length m > 0 then
  (Array.length m, Array.length m.(0))
  else
  (0, 0);;
  let equ = fun m1 m2 ->
  let (m, n) = (dim m1) and result = ref true in
  if (m, n) = (dim m2) then
  begin
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        if m1.(i).(j) != m2.(i).(j) then
          result := false
      done
    done;
    !result
  end
  else
  false;;
end;;

module type sig_full =
sig
  type 'a t = 'a array array
  val create : int -> int -> 'a -> 'a t
  val get : 'a t -> int -> int -> 'a
  val set : 'a t -> int -> int -> 'a -> unit
  val dim : 'a t -> int * int
  val equ : 'a t -> 'a t -> bool
end;;

module type sig_abstract =
sig
  type 'a t
  val create : int -> int -> 'a -> 'a t
  val get : 'a t -> int -> int -> 'a
  val set : 'a t -> int -> int -> 'a -> unit
  val dim : 'a t -> int * int
end;;

module MatrixFull = (Matrix:sig_full);;
module MatrixAbstract = (Matrix:sig_abstract);;
MatrixFull.create 5 5 'a';;
MatrixAbstract.create 5 5 'a';;