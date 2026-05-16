type expr =
  | EInt of int
  | EIdent of string
  | EBinOp of expr * string * expr

type stmt =
  | SDecl of string list
  | SIf of expr * stmt list * stmt list option
  | SReturn of expr option

type program = stmt list
