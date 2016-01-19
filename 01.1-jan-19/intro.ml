open Printf


(*
let message = "Hello world";;

(printf "%s\n" message)
*)

let max (n : int) (m : int) : int =
  if n > m then n
  else m;;

let rec sum_up (n : int) : int =
  if n < 0 then failwith "Can't do that for negative n"
  else if n == 0 then 0
  else
    (sum_up (n - 1)) + n;;

(printf "%d\n" (max 5 4));
(printf "%d\n" (sum_up 4));

