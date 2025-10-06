type instruction =
  | Add of string * string * string
  | Sub of string * string * string
  | Mul of string * string * string
  | Div of string * string * string
  | Load of string * string
  | Store of string * string
  | Jump of string
  | Label of string
  | NoOp

type program = instruction list

let string_of_instruction = function
  | Add (dest, src1, src2) -> Printf.sprintf "ADD %s, %s, %s" dest src1 src2
  | Sub (dest, src1, src2) -> Printf.sprintf "SUB %s, %s, %s" dest src1 src2
  | Mul (dest, src1, src2) -> Printf.sprintf "MUL %s, %s, %s" dest src1 src2
  | Div (dest, src1, src2) -> Printf.sprintf "DIV %s, %s, %s" dest src1 src2
  | Load (dest, src) -> Printf.sprintf "LOAD %s, %s" dest src
  | Store (dest, src) -> Printf.sprintf "STORE %s, %s" dest src
  | Jump label -> Printf.sprintf "JUMP %s" label
  | Label label -> Printf.sprintf "%s:" label
  | NoOp -> "NOP"

let string_of_program prog =
  String.concat "\n" (List.map string_of_instruction prog)