open Compile
open Runner
open Printf
open OUnit2

let t name program expected = name>::test_run program name expected;;
let tvg name program expected = name>::test_run_valgrind program name expected;;
let terr name program expected = name>::test_err program name expected;;

let pairs = [
  t "pair1" "fst((3, 4))" "3";
  t "pair2" "snd((3, 4))" "4";

  t "pair3" "snd((3, (4, 5)))" "(4, 5)";

  t "pair4" "fst(snd((3, (4, 5))))" "4";

  t "pair5" "((1, 2), (3, (4, 5)))"  "((1, 2), (3, (4, 5)))";

  t "pair6" "let p = (4, 5) in p == p" "true";

  t "pair7" "let p1 = (4, 5), p2 = (4, 5) in p1 == p2" "true";

  t "pair7" "let p1 = (4, (5, true)), p2 = (4, (5, true))
             in p1 == p2" "true";

  t "pair8" "let p1 = (4, 6), p2 = (4, 5) in p1 == p2" "false";

  t "pair9" "let p1 = (4, (7, true)), p2 = (4, (5, true))
             in p1 == p2" "false";

  t "pair10" "let p1 = (3, (5, true)), p2 = (4, (5, true))
             in p1 == p2" "false";

(*
  terr "epair1" "fst(4)" "non-pair";
*)
  
]

let suite =
"suite">:::pairs


let () =
  run_test_tt_main suite
;;

