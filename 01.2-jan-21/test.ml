open OUnit2
open Printf
open Bst
open ExtLib





let t_str (name : string) (value : string) (expected : string) = name>::
  (fun _ -> assert_equal expected value);;

let t_int (name : string) (value : int) (expected : int) = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_int);;

let t_bst (name : string) (value : bst) (expected : bst) = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_bst)





let suite = "suite">:::[
  (t_str "s_o_b1" (string_of_bst bst1)
    "Node(\"2\", Node(\"1\", Leaf, Leaf), Node(\"3\", Leaf, Leaf))"
    );

  (t_str "s_o_b2" (string_of_bst Leaf) "Leaf");

  (t_bst "insert_7" (insert bst2 "7") ...);
  (t_bst "insert_1" (insert bst2 "1") ...);

  (t_bst "insert1" (insert Leaf "m")
    (Node("m", Leaf, Leaf)));

  (t_bst "insert2"
    (insert
      (Node("3", Node("1", Leaf, Leaf), Leaf))
      "2")
    (Node("3",
          Node("1", Leaf, Node("2", Leaf, Leaf)),
          Leaf)));



(*  (t_str "s_o_b1" (string_of_bst bst1)) "Node"; *)
];;

run_test_tt_main suite































