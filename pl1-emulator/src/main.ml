(* Main module for PL/I Parser *)
open Ast
open Config

let print_ast ast =
  Printf.printf "=== Abstract Syntax Tree ===\n";
  Printf.printf "Procedures: %d\n" (List.length ast.procedures);
  Printf.printf "Main statements: %d\n" (List.length ast.main);
  Printf.printf "=============================\n"

let parse_file filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_fname <- filename;

    if !debug_mode then
      Printf.printf "Parsing file: %s\n" filename;

    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;

    if !output_ast then print_ast ast;

    Printf.printf "✅ Successfully parsed PL/I program\n";
    ast
  with
  | Lexer.LexError msg ->
      Printf.eprintf "❌ Lexical error: %s\n" msg;
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "❌ Parse error at line %d, character %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "❌ File error: %s\n" msg;
      exit 1

let main () =
  Printf.printf "🏗️  PL/I Parser v1.0\n";
  parse_args ();

  let ast = parse_file !input_file in

  if !debug_mode then
    Printf.printf "✨ Parsing completed successfully!\n"

let () = main ()
