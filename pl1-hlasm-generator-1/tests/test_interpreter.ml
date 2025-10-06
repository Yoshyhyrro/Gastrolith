(* This file contains test cases for the interpreter of the PL/I to HLASM generator. *)

open OUnit2
open Interpreter

let test_interpreter _ =
  let pl1_code = "/* Sample PL/I code */" in
  let expected_output = "/* Expected output after interpreting the assembly code */" in
  let assembly_code = Codegen.generate_assembly pl1_code in
  let actual_output = Interpreter.execute assembly_code in
  assert_equal expected_output actual_output

let suite =
  "Interpreter Tests" >::: [
    "test_interpreter" >:: test_interpreter;
  ]

let () =
  run_test_tt_main suite