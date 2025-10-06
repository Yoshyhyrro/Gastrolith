(* Abstract Syntax Tree for PL/I *)

type identifier = string

(* Data types *)
type pl1_type =
  | Fixed of int option * int option  (* FIXED(precision, scale) *)
  | Float of int option               (* FLOAT(precision) *)
  | Char of int option               (* CHAR(length) *)
  | Bit of int option                (* BIT(length) *)
  | Binary of int option             (* BINARY(length) *)

(* Expressions *)
type expr =
  | Var of identifier
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BinaryOp of expr * string * expr
  | UnaryOp of string * expr
  | FuncCall of identifier * expr list

(* ON conditions captured in AST *)
type on_condition =
  | Zerodivide
  | Endfile of identifier
  | Key of identifier
  | Other_on of string

(* Statements *)
type stmt =
  | Assign of identifier * expr
  | Declare of identifier * pl1_type * expr option
  | If of expr * stmt * stmt option
  | Do of stmt list
  | DoLoop of identifier * expr * expr * stmt list (* control DO i = a TO b *)
  | Put of expr list
  | Get of identifier list
  | Call of identifier * expr list
  | Return of expr option
  | Label of identifier
  (* ON statement variants *)
  | OnGoto of on_condition * identifier
  | OnBlock of on_condition * stmt list
  | OnSystem of on_condition
  | Labeled of identifier * stmt

(* Procedures and programs *)
type procedure_decl = {
  name: identifier;
  params: (identifier * pl1_type) list;
  body: stmt list;
}

type program = {
  procedures: procedure_decl list;
  main: stmt list;
}
