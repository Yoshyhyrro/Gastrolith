(* hlasm_parser.ml - Main parser orchestration (Refactored) *)
open Angstrom
open Hlasm_ast

(* =============================================================================
   COMMAND LINE INTERFACE (Optional)
   ============================================================================= *)

(* Parse arguments and run parser *)
let main () =
  let args = Sys.argv in
  
  if Array.length args < 2 then begin
    Printf.printf "Usage: %s <file.asm> [options]\n" args.(0);
    Printf.printf "Options:\n";
    Printf.printf "  --test       Run test suite\n";
    Printf.printf "  --validate   Parse and validate\n";
    Printf.printf "  --symbols    Show symbol table\n";
    exit 1
  end;
  
  let filename = args.(1) in
  let run_validation = Array.mem "--validate" args in
  let show_symbols = Array.mem "--symbols" args in
  
  if filename = "--test" then begin
    run_tests ();
    exit 0
  end;
  
  match parse_file filename with
  | Ok prog ->
      Printf.printf "✓ Successfully parsed %s\n" filename;
      Printf.printf "  Statements: %d\n" (List.length prog.statements);
      
      if show_symbols then begin
        let symbols = Stmt.build_symbol_table prog in
        Printf.printf "  Symbols defined: %d\n" (List.length symbols);
        List.iter (Printf.printf "    - %s\n") symbols
      end;
      
      if run_validation then begin
        match Stmt.validate_program prog with
        | Ok () -> Printf.printf "✓ Validation passed\n"
        | Error msg -> Printf.printf "✗ Validation failed: %s\n" msg
      end
  
  | Error msg ->
      Printf.eprintf "✗ Parse error: %s\n" msg;
      exit 1

(* Entry point *)
let () =
  if !Sys.interactive then
    ()  (* Don't run main in interactive mode *)
  else
    main ()
MODULE DEPENDENCIES
   
   This refactored parser is organized into focused modules:
   
   - Utils:    Common utilities, whitespace, position tracking
   - Lex:      Low-level lexical parsing (strings, DBCS, identifiers)
   - Symbol:   Symbol parsing and classification
   - Term:     Self-defining terms and literals
   - Expr:     Expression parsing with left-to-right evaluation
   - Operand:  Operand parsing with register validation
   - Asm:      Assembler instruction parsing
   - Machine:  Machine instruction parsing
   - Stmt:     Statement-level parsing and validation
   
   ============================================================================= *)

(* =============================================================================
   FORWARD REFERENCE INITIALIZATION
   
   Due to mutual recursion between terms and expressions, we need to
   initialize forward references before parsing.
   ============================================================================= *)

let initialize_parsers () =
  (* Create the expression parser *)
  let expr_parser = Expr.init Term.term_parser in
  
  (* Create operand list parser *)
  let operand_list_parser = Operand.operand_list (expr_parser ()) in
  
  (expr_parser, operand_list_parser)

(* =============================================================================
   MAIN PARSING FUNCTIONS
   ============================================================================= *)

(* Parse a complete HLASM program *)
let parse_program input =
  let (expr_p, operand_list_p) = initialize_parsers () in
  let parser = Stmt.program (expr_p ()) operand_list_p in
  
  match parse_string ~consume:All parser input with
  | Ok result -> Ok result
  | Error msg -> Error ("HLASM parse error: " ^ msg)

(* Parse a single statement *)
let parse_statement input =
  let (expr_p, operand_list_p) = initialize_parsers () in
  let parser = Stmt.statement (expr_p ()) operand_list_p in
  
  match parse_string ~consume:All parser input with
  | Ok result -> Ok result
  | Error msg -> Error ("Statement parse error: " ^ msg)

(* Parse an expression *)
let parse_expression input =
  let (expr_p, _) = initialize_parsers () in
  
  match parse_string ~consume:All (expr_p ()) input with
  | Ok result -> Ok result
  | Error msg -> Error ("Expression parse error: " ^ msg)

(* Parse a symbol *)
let parse_symbol input =
  match parse_string ~consume:All Symbol.symbol_kind input with
  | Ok result -> Ok result
  | Error msg -> Error ("Symbol parse error: " ^ msg)

(* =============================================================================
   PARSING WITH VALIDATION
   ============================================================================= *)

(* Parse and validate a program *)
let parse_and_validate input =
  match parse_program input with
  | Error msg -> Error msg
  | Ok prog ->
      match Stmt.validate_program prog with
      | Ok () -> Ok prog
      | Error msg -> Error ("Validation error: " ^ msg)

(* =============================================================================
   ENHANCED ERROR REPORTING
   ============================================================================= *)

type parse_error = {
  message: string;
  line: int option;
  column: int option;
  context: string option;
}

(* Extract more detailed error information *)
let parse_with_context input =
  match parse_program input with
  | Ok prog -> Ok prog
  | Error msg ->
      (* Try to extract line/column info from error message *)
      let error = {
        message = msg;
        line = None;
        column = None;
        context = None;
      } in
      Error error

(* =============================================================================
   UTILITY FUNCTIONS
   ============================================================================= *)

(* Parse from file *)
let parse_file filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    parse_and_validate content
  with
  | Sys_error msg -> Error ("File error: " ^ msg)
  | e -> Error ("Unexpected error: " ^ Printexc.to_string e)

(* Parse multiple files *)
let parse_files filenames =
  List.map (fun fname ->
    (fname, parse_file fname)
  ) filenames

(* =============================================================================
   TESTING FRAMEWORK
   ============================================================================= *)

let test_cases = [
  (* Basic instructions *)
  ("Simple load", "L R1,DATA");
  ("Register copy", "LR R2,R3");
  ("Store", "ST R4,RESULT");
  
  (* Labels *)
  ("Labeled instruction", "LOOP L R1,COUNTER");
  ("Variable symbol", "&VAR SETA 5");
  
  (* DC/DS *)
  ("Character constant", "DC CL12'HELLO'");
  ("Multiple constants", "DC 3CL5'ABC'");
  ("Storage definition", "DS 10F");
  
  (* DBCS *)
  ("Graphic constant", "DC G'<SO>漢字<SI>'");
  ("Mixed constant", "DC C'混合<SO>DBCS<SI>文字'");
  
  (* Expressions *)
  ("Addition", "DC A(LABEL1+LABEL2)");
  ("Complex expression", "DC A((LABEL1+8)*2-4)");
  
  (* Literals *)
  ("Literal", "L R1,=F'123'");
  ("Character literal", "MVC DEST,=CL8'LITERAL'");
  
  (* Assembler directives *)
  ("CSECT", "MAIN CSECT");
  ("USING", "USING *,R12");
  ("EQU", "REG1 EQU 1");
  ("END", "END MAIN");
  
  (* Comments *)
  ("Full-line comment", "* This is a comment");
  ("Inline comment", "L R1,DATA Comment here");
  
  (* Indexed addressing *)
  ("Index and base", "L R1,OFFSET(R2,R3)");
  ("Base only", "L R1,OFFSET(,R3)");
  ("Index only", "L R1,OFFSET(R2)");
  
  (* Complex program *)
  ("Multi-line program", 
   "MAIN    CSECT\n\
   \        USING *,R12\n\
   \        L     R1,=F'100'\n\
   \        END   MAIN");
]

(* Run a single test case *)
let run_test (name, input) =
  Printf.printf "Testing: %s\n" name;
  Printf.printf "Input: %s\n" input;
  
  match parse_program input with
  | Ok prog ->
      Printf.printf "✓ OK - Parsed %d statement(s)\n" 
        (List.length prog.statements);
      (match Stmt.validate_program prog with
       | Ok () -> Printf.printf "✓ Validation passed\n"
       | Error msg -> Printf.printf "✗ Validation failed: %s\n" msg);
      true
  | Error msg ->
      Printf.printf "✗ ERROR: %s\n" msg;
      false

(* Run all test cases *)
let run_tests () =
  Printf.printf "======================\n";
  Printf.printf "HLASM Parser Test Suite\n";
  Printf.printf "======================\n\n";
  
  let results = List.map run_test test_cases in
  let passed = List.filter Fun.id results |> List.length in
  let total = List.length test_cases in
  
  Printf.printf "\n======================\n";
  Printf.printf "Results: %d/%d passed\n" passed total;
  Printf.printf "======================\n"

(* =============================================================================
   EXPORTS
   ============================================================================= *)

(* Main parsing function for external use *)
let parse = parse_and_validate

(* Convenience exports *)
let parse_stmt = parse_statement
let parse_expr = parse_expression
let parse_sym = parse_symbol

(* Analysis exports *)
module Analysis = struct
  let validate = Stmt.validate_program
  let build_symbol_table = Stmt.build_symbol_table
  let find_undefined = Stmt.find_undefined_symbols
  let format = Stmt.format_program
end

(* =============================================================================
   