(* Entry point of the PL/I to HLASM generator and interpreter *)

let () =
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in

  (* Step 1: Lexical analysis *)
  let lexbuf = Lexing.from_channel (open_in input_file) in
  let tokens = Pl1_lexer.token lexbuf in

  (* Step 2: Parsing *)
  let ast = Pl1_parser.program tokens in

  (* Step 3: Code generation *)
  let assembly_code = Codegen.generate ast in

  (* Step 4: Write assembly code to output file *)
  let out_channel = open_out output_file in
  output_string out_channel assembly_code;
  close_out out_channel;

  (* Step 5: Interpretation of the generated assembly code *)
  Interpreter.run assembly_code; 

  print_endline "Execution completed."