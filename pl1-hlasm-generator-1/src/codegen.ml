(* codegen.ml *)

open Ast
open Hlasm_ast

let rec generate_assembly (ast : program) : Hlasm_ast.program =
  match ast with
  | [] -> []
  | stmt :: rest ->
      let assembly_stmt = match stmt with
        | Assignment (var, expr) -> generate_assignment var expr
        | Print expr -> generate_print expr
        | _ -> failwith "Unsupported statement"
      in
      assembly_stmt :: generate_assembly rest

and generate_assignment (var : string) (expr : expression) : Hlasm_ast.statement =
  let assembly_expr = generate_expression expr in
  Hlasm_ast.Assignment (var, assembly_expr)

and generate_print (expr : expression) : Hlasm_ast.statement =
  let assembly_expr = generate_expression expr in
  Hlasm_ast.Print assembly_expr

and generate_expression (expr : expression) : Hlasm_ast.expression =
  match expr with
  | IntLiteral n -> Hlasm_ast.IntLiteral n
  | Var v -> Hlasm_ast.Var v
  | _ -> failwith "Unsupported expression"

let generate (ast : program) : Hlasm_ast.program =
  generate_assembly ast