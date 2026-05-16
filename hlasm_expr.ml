(* expr.ml - Expression parsing with left-to-right evaluation *)
open Angstrom
open Hlasm_ast
open Utils

(* =============================================================================
   BINARY OPERATORS
   ============================================================================= *)

let binary_op = choice [
  char '+' *> return Add;
  char '-' *> return Sub;
  char '*' *> return Mul;
  char '/' *> return Div;
]

(* =============================================================================
   LEFT-TO-RIGHT EXPRESSION BUILDER
   ============================================================================= *)

(* Build binary expression tree with strict left-to-right association
   
   Example: A + B - C * D
   Parse order: ((A + B) - C) * D
   
   This matches HLASM's evaluation order where all operations have
   equal precedence and associate left-to-right.
*)
let rec make_binary_expr left ops =
  match ops with
  | [] -> left
  | (op, right) :: rest ->
      (* Accumulate left-to-right: (left op right) becomes new left *)
      make_binary_expr (BinaryOp (left, op, right)) rest

(* =============================================================================
   FACTOR PARSING (Atomic expressions with optional unary operators)
   ============================================================================= *)

(* Parse a unary operator followed by a term *)
let unary_factor term_p =
  let unary_plus = char '+' *> term_p >>| fun t -> UnaryPlus (Term t) in
  let unary_minus = char '-' *> term_p >>| fun t -> UnaryMinus (Term t) in
  let plain_term = term_p >>| fun t -> Term t in
  
  choice [
    unary_plus;
    unary_minus;
    plain_term;
  ]

(* =============================================================================
   EXPRESSION PARSER
   ============================================================================= *)

(* Main expression parser with left-to-right evaluation
   
   Grammar:
     expression := factor (binary_op factor)*
     factor     := ['+' | '-'] term
     term       := <see term.ml>
*)
let expression term_p =
  (* Parse the initial factor *)
  unary_factor term_p >>= fun left ->
  
  (* Parse zero or more (operator, factor) pairs *)
  many (
    binary_op >>= fun op ->
    unary_factor term_p >>| fun right ->
    (op, right)
  ) >>| fun ops ->
  
  (* Build the expression tree left-to-right *)
  make_binary_expr left ops

(* =============================================================================
   INITIALIZATION AND EXPORTS
   ============================================================================= *)

(* Initialize the expression parser with the term parser from term.ml *)
let init term_parser =
  let expr_parser () = expression (term_parser ()) in
  
  (* Update the forward reference in term.ml *)
  Term.expression_ref := expr_parser;
  
  expr_parser

(* Convenience function for parsing expressions *)
let parse_expression term_p input =
  parse_string ~consume:All (expression term_p) input

(* =============================================================================
   EXPRESSION VALIDATION
   ============================================================================= *)

(* Check if an expression is relocatable (contains symbols) *)
let rec is_relocatable = function
  | Term (Symbol _) -> true
  | Term (LocationCounter) -> true
  | UnaryPlus e | UnaryMinus e -> is_relocatable e
  | BinaryOp (e1, _, e2) -> is_relocatable e1 || is_relocatable e2
  | Term (Parenthesized e) -> is_relocatable e
  | Term _ -> false

(* Check if an expression is absolute (no symbols) *)
let is_absolute expr = not (is_relocatable expr)

(* Get all symbols referenced in an expression *)
let rec get_symbols = function
  | Term (Symbol s) -> [s]
  | Term (LocationCounter) -> []
  | UnaryPlus e | UnaryMinus e -> get_symbols e
  | BinaryOp (e1, _, e2) -> get_symbols e1 @ get_symbols e2
  | Term (Parenthesized e) -> get_symbols e
  | Term _ -> []

(* =============================================================================
   EXPRESSION SIMPLIFICATION (Optional)
   ============================================================================= *)

(* Simplify constant expressions at parse time *)
let rec simplify = function
  | BinaryOp (Term (SelfDefining (Decimal a)), Add, Term (SelfDefining (Decimal b))) ->
      Term (SelfDefining (Decimal (a + b)))
  | BinaryOp (Term (SelfDefining (Decimal a)), Sub, Term (SelfDefining (Decimal b))) ->
      Term (SelfDefining (Decimal (a - b)))
  | BinaryOp (Term (SelfDefining (Decimal a)), Mul, Term (SelfDefining (Decimal b))) ->
      Term (SelfDefining (Decimal (a * b)))
  | BinaryOp (Term (SelfDefining (Decimal a)), Div, Term (SelfDefining (Decimal b))) when b <> 0 ->
      Term (SelfDefining (Decimal (a / b)))
  | UnaryPlus (Term t) -> Term t  (* +x = x *)
  | UnaryMinus (Term (SelfDefining (Decimal n))) -> Term (SelfDefining (Decimal (-n)))
  | BinaryOp (e1, op, e2) -> BinaryOp (simplify e1, op, simplify e2)
  | e -> e  (* No simplification *)
