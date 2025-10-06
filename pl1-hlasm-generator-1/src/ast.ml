type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

type stmt =
  | Assign of string * expr
  | Print of expr
  | Seq of stmt list

type program = stmt list

let rec string_of_expr = function
  | Int n -> string_of_int n
  | Var v -> v
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Div (e1, e2) -> "(" ^ string_of_expr e1 ^ " / " ^ string_of_expr e2 ^ ")"

let rec string_of_stmt = function
  | Assign (v, e) -> v ^ " := " ^ string_of_expr e
  | Print e -> "print " ^ string_of_expr e
  | Seq stmts -> String.concat "; " (List.map string_of_stmt stmts)

let string_of_program prog =
  String.concat "\n" (List.map string_of_stmt prog)