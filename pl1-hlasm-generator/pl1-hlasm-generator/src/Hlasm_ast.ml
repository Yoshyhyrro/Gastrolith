(* Minimal AST needed by hlasm_parser.ml *)

type pos = { line : int; column : int }

type symbol = { name : string; pos : pos }

type self_defining =
  | Decimal of int
  | Hexadecimal of string
  | Binary of string
  | Character of string
  | Graphic of string

type attribute = { attr_type : [`Length | `Type | `Scale | `Integer | `Count | `Number | `Defined | `OpCode]; symbol : symbol }

type literal = { lit_type : string; duplication_factor : int; value : string; modifiers : (string * string option) list }

type term =
  | LocationCounter
  | Symbol of symbol
  | SelfDefining of self_defining
  | AttributeRef of attribute
  | Literal of literal
  | Parenthesized of expression

and expression =
  | Term of term
  | UnaryPlus of term
  | UnaryMinus of term
  | BinaryOp of expression * [ `Add | `Sub | `Mul | `Div ] * expression

type operand =
  | SimpleOperand of expression
  | RegisterOperand of int
  | IndexedOperand of expression * int option * int option

type asm_tag = ASM | MAC

type machine_instr = { mnemonic : string; operands : operand list; format : asm_tag option }

(* Minimal top-level types for testing *)

type program = { instructions : machine_instr list }
