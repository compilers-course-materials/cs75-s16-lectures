(* ocaml *)

(* rlwrap ocaml *)

let rec increment_all (l : int list) : int list =
  match l with
    | [] -> []
    | first::rest ->
      (first + 1)::(increment_all rest);;

let rec negate_all (l : int list) : int list =
  match l with
    | [] -> []
    | first::rest ->
      ((-1) * first)::(negate_all rest);;

let rec update_all (l : 'a list) (f : 'a -> 'b) : 'b list =
  match l with
    | [] -> []
    | first::rest ->
      (f first)::(update_all rest f);;

let increment (x : int): int = x + 1;;

let increment_all (l : int list) : int list =
  update_all l increment;;

let increment_all2 (l : int list) : int list =
  update_all l (fun x -> (x + 1));;
  

let add_n_a (l : int list) (n : int) : int list =
  update_all l ((fun x y -> x + y) n);;

let add_n_b (l : int list) (n : int) : int list =
  update_all l (fun x -> x + n)

let flip (f : 'b -> 'a -> 'c) (x : 'a) (y : 'b) : 'c = f y x;;

(*
(add_n_a [4;5] (-10))
(add_n_b [4;5] (-10))


(add_n_a [4;5] 10)

=>

(update_all [4;5] ((fun x y -> x + y) 10))

=>

(update_all [4;5] (fun y -> 10 + y))




(add_n_b [4;5] 10)

=>

(update_all [4;5] (fun x -> x + 10))
*)

(*
(increment_all2 [1;2])

=>

update_all [1;2] (fun x -> (x + 1))

=>

  match [1;2] with
    | [] -> []
    | first::rest ->
      ((fun x -> (x + 1)) first)::(update_all rest (fun x -> (x + 1)));;

=> 


      ((fun x -> (x + 1)) 1)::(update_all [2] (fun x -> (x + 1)));;

=>

      (1 + 1)::(update_all [2] (fun x -> (x + 1)));;

=>

      2::(
          match [2] with
            | [] -> []
            | first::rest ->
              ((fun x -> (x + 1)) first)::(update_all rest (fun x -> (x + 1)));;
        )
*)


let f g x = g x;;

let rec length_all4 l =
  match l with
    | [] -> []
    | first::rest -> first::(f length_all4 rest);;


let rec length_all5 l =
  match l with
    | [] -> []
    | first::second::rest ->
      (first < second)::(length_all5 rest)
    | [first] ->
      [];;





let rec length_all (l : string list) : int list =
  match l with
    | [] -> []
    | first::rest ->
      (String.length first)::(length_all rest);;
  
let rec length_all3 l =
  match l with
    | [] -> []
    | first::rest -> first::(length_all3 rest);;


let rec length_all2 l =
  match l with
    | [] -> []
    | first::rest ->
      (String.length first)::(length_all2 rest);;
