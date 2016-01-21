open Printf

type bst =
  | Node of string * bst * bst
  | Leaf
;;


let bst1 = Node("2",
     Node("1", Leaf, Leaf),
     Node("3", Leaf, Leaf));;

let bst2 = Node("5",
                Node("3",
                     Node("2", Leaf, Leaf),
                     Node("4", Leaf, Leaf)),
                Leaf);;

let rec string_of_bst (b : bst) : string =
  match b with
    | Leaf -> "Leaf"
    | Node(value, left, right) ->
      "Node(\"" ^ value ^ "\", " ^
        (string_of_bst left) ^
        ", " ^
        (string_of_bst right) ^
        ")"
;;


let rec insert (b : bst) (v : string) : bst =
  match b with
    | Leaf -> Node(v, Leaf, Leaf)
    | Node(value, left, right) ->
      if v < value then
        Node(value, (insert left v), right)
      else if v > value then
        Node(value, left, (insert right v))
      else
        failwith "Inserted duplicate elements";;



(*
(insert (Node("3", Node("1", Leaf, Leaf), Leaf)) "2")

==>

  match (Node("3", Node("1", Leaf, Leaf), Leaf)) with
    | Leaf -> Node("2", Leaf, Leaf)
    | Node(value, left, right) ->
      if "2" < value then
        (insert left "2")
      else if "2" > value then
        (insert right "2")
      else
        failwith "Inserted duplicate elements";;

==>

      if "2" < "3" then
        (insert Node("1", Leaf, Leaf) "2")
      else if "2" > "3" then
        (insert Leaf "2")
      else
        failwith "Inserted duplicate elements";;

==>
        (insert Node("1", Leaf, Leaf) "2")

      Node("3", (insert Node("1", Leaf, Leaf) "2"), Leaf)

==>
*)


(*
(string_of_bst bst1)

==>

match Node("2",
         Node("1", Leaf, Leaf),
         Node("3", Leaf, Leaf))
  with
    | Leaf -> "Leaf"
    | Node(value, left, right) -> "Node"

==>

"Node"



match Leaf with
  | Leaf -> "Leaf"
  | Node(value, left, right) -> "Node"
  
*)

