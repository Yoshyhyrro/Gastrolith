open Ast
open Cfg

(* Simple map keyed by on_condition (compare uses structural compare) *)
module CondMap = Map.Make(struct
  type t = on_condition
  let compare = compare
end)

type handler_map = node_id option CondMap.t

let empty_handler_map = CondMap.empty

let compute_on_handlers (cfg : cfg) : unit =
  (* Placeholder: dataflow fixed-point computation to determine active ON handlers at each point *)
  ()

let analyze prog =
  let cfg = from_ast prog in
  compute_on_handlers cfg;
  cfg
