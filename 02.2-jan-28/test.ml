open ExtLib
open OUnit2
open Functions

let t_any name left right =
  name>::(fun _ -> (assert_equal right left ~printer:dump));;

let suite = "suite">:::[
  t_any "do_to_all" (do_to_all [1;2;3] (fun x -> x + 1)) [2;3;4];


  t_any "all_lengths1" (all_lengths ["a"; "hi"; "hello"]) [1;2;5]
     
      
];;

run_test_tt_main suite;;
  
