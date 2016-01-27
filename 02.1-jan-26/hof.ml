open Printf

let some_list = ["abc"; "more"; "strings"; "etc"];;

let rec three_characters_long_strings (l:string list) : string list =
    match l with
    | [] -> []
    | first::rest -> if (String.length first = 3)
                     then first::(three_characters_long_strings rest)
                     else three_characters_long_strings rest
;;

let rec filter_strings (l:string list) (f:string -> bool) : string list =
    match l with
    | [] -> []
    | first::rest -> if f first
                     then first::(filter_strings rest f)
                     else filter_strings rest f
;;

let other_three_characters_long_strings (l:string list) : string list =
    filter_strings l (fun s -> String.length s = 3)
;;

let rec filter_names (f:string -> bool) : string list =
  filter_strings ["Alice";"Bob";"Charlie";"Dog"] f
;;

let rec filter (l:'a list) (f:'a -> bool) : 'a list =
    match l with
    | [] -> []
    | first::rest -> if f first
                     then first::(filter rest f)
                     else filter rest f
;;

let sum_list (l:int list) : int =
    let rec loop (lst:int list) (total:int) : int =
        match lst with
        | [] -> total
        | first::rest -> loop rest (first + total)
    in
    loop l 0
;;

let rec sum_list_2 (l:int list) : int =
    match l with
    | [] -> 0
    | first::rest -> first + sum_list_2 rest
;;

let rec fold_left (base:'a) (l:'b list) (op:'b -> 'a -> 'a) : 'a =
    match l with
    | [] -> base
    | first::rest -> op first (fold_left base rest op)
    (* The above recursive case is WRONG; please see test.ml for
       an example of its failure.

       Here's why: suppose we have a list [1;2;3] and a base value 0.  If
       we are moving in from the left, we expect to get
            (op (op (op 0 1) 2) 3)
       That is, we combine 0 and 1 first, we combine the result with 2, etc.

       In the above, we instead get
            (op 1 (op 2 (op 3 0)))
       That is, we combine 3 and 0, then 2 and the result, etc.

       In fact, what I have written above is fold_right.  A correct
       implementation of fold_left is below.
    *)
;;

let rec actual_fold_left (base:'a) (l:'b list) (op:'b -> 'a -> 'a) : 'a =
    match l with
    | [] -> base
    | first::rest ->
        (* This is the only part that had to change. *)
        actual_fold_left (op first base) rest op
;;

let sum_list_2_again (l:int list) : int =
    fold_left 0 l (fun x y -> x + y)
;;

(*
map : 'a list -> ('a -> 'b) -> 'b list
*)
