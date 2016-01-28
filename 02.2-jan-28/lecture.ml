open ExtLib

let rec increment_all (l : int list) : int list =
  match l with
    | [] -> []
    | first::rest ->
      (first + 1)::(increment_all rest);;

let rec negate_all (l : int list) : int list =
  match l with
    | [] -> []
    | first::rest ->
      ((-1) * first)::(add_two_all rest);;

let rec do_to_all (l : int list) (f : int -> int) : int list =
  match l with
    | [] -> []
    | first::rest ->
      (f first)::(do_to_all rest f);;


