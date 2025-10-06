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

val parse_program : string -> program
val eval_program : program -> unit