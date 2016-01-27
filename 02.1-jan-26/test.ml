open OUnit2
open Printf
open Hof
open ExtLib

let t_str (name : string) (value : string) (expected : string) = name>::
  (fun _ -> assert_equal expected value);;

let t_int (name : string) (value : int) (expected : int) = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_int);;

let t_str_lst (name : string) (value : string list) (expected : string list) = name>::
  (fun _ -> assert_equal expected value);;

let t_lst (name : string) (value : 'a list) (expected : 'a list) = name >::
  (fun _ -> assert_equal expected value);;

let is_three_chars_long (s:string) : bool =
  String.length s = 3
;;

let suite = "suite">:::[
    t_str_lst
      "3-char-long empty list"
      (three_characters_long_strings [])
      []
    ;
    t_str_lst
      "3-char-long basic test"
      (three_characters_long_strings ["a";"abc"])
      ["abc"]
    ;
    t_str_lst
      "3-char-long filter basic test"
      (filter_strings ["a";"abc"] is_three_chars_long)
      ["abc"]
    ;
    t_lst
      "positive integer list filter"
      (filter [1;-1;2;-5;0] (fun x -> x>0))
      [1;2]
    ;
    (* For fold_left, this should be 1+3+5 *)
    t_int
      "sum list"
      (fold_left 0 [1;3;5] (fun x y -> x + y))
      9
    ;
    (* For fold_left, this should be "a"^"b"^"c" *)
    (* OCaml uses ^ for string concatenation.  Weird, huh? *)
    t_str
      "concat list"
      (fold_left "" ["a";"b";"c"] (fun x y -> x ^ y))
      "abc"
    ;
    (* Here's where something interesting happens.  We'll use a function which
       throws away the second (old) value and gives back the first (new) one.
       This means that we should get the last element in the list (since it will
       be newest). *)
    t_int
      "last of list"
      (fold_left 0 [1;2;3] (fun x y -> x))
      3
    (* With the implementation we discussed in lecture, we wind up getting 1 and
       not 3; we get the first value, not the last one.  See hof.ml for an
       explanation of why and how to fix it. *)
    ;
    (* Here's a test using the real fold_left. *)
    t_int
      "last of list"
      (actual_fold_left 0 [1;2;3] (fun x y -> x))
      3
 ];;

run_test_tt_main suite

