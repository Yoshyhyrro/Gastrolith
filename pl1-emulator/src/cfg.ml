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

type cfg = {
  mutable nodes: (node_id, basic_block) Hashtbl.t;
  mutable edges: (node_id, (node_id * edge_kind) list) Hashtbl.t;
  mutable entry: node_id;
  mutable next_id: int;
}

let create_cfg () = {
  nodes = Hashtbl.create 97;
  edges = Hashtbl.create 97;
  entry = 0;
  next_id = 1;
}

let new_node cfg stmts =
  let id = cfg.next_id in
  cfg.next_id <- cfg.next_id + 1;
  let b = { id; stmts } in
  Hashtbl.add cfg.nodes id b;
  Hashtbl.add cfg.edges id [];
  b

let add_edge cfg from_id to_id kind =
  let lst = Hashtbl.find cfg.edges from_id in
  Hashtbl.replace cfg.edges from_id ((to_id, kind) :: lst)

let from_ast (_prog : program) : cfg =
  let cfg = create_cfg () in
  let all_stmts = _prog.main in
  let entry = new_node cfg all_stmts in
  cfg.entry <- entry.id;
  cfg

let iter_nodes cfg f =
  Hashtbl.iter (fun _ b -> f b) cfg.nodes

let find_block_by_label _cfg _label = None

let add_on_edge cfg from_id to_id =
  add_edge cfg from_id to_id (OnJump Zerodivide)
