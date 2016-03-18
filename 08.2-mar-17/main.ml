open Compile
open Runner
open Printf
open Lexing

let () =
  let name = Sys.argv.(1) in
  let input_file = open_in name in
  try
    let program = compile_file_to_string name input_file in
    printf "%s\n" program
  with
    | Failure(f) ->
      eprintf "Compilation failed: \n%s\n" f;
      exit 1

