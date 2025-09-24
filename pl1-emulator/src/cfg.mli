open Ast

type node_id = int

type basic_block = {
  id: node_id;
  stmts: stmt list;
}

type edge_kind =
  | Normal
  | Conditional of expr
  | OnJump of on_condition

type cfg

val from_ast : program -> cfg
val iter_nodes : cfg -> (basic_block -> unit) -> unit
val find_block_by_label : cfg -> identifier -> basic_block option
val add_on_edge : cfg -> node_id -> node_id -> unit
