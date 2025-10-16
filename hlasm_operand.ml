(* operand.ml - Operand parsing with register validation *)
open Angstrom
open Hlasm_ast
open Utils
open Lex

(* =============================================================================
   REGISTER VALIDATION
   ============================================================================= *)

(* HLASM supports general-purpose registers 0-15 *)
let validate_register reg =
  validate_range ~min:0 ~max:15 "Register" reg

(* Parse a register number with validation *)
let register_number =
  decimal_digits >>= fun s ->
  validate_register (int_of_string s)

(* =============================================================================
   REGISTER OPERANDS
   ============================================================================= *)

(* Simple register operand: R1, R15, etc. *)
let register_operand = 
  register_number >>| fun reg -> RegisterOperand reg

(* =============================================================================
   INDEX AND BASE REGISTER PARSING
   ============================================================================= *)

(* Parse index and base registers: (index,base) or (,base) or (index) *)
let index_base_registers =
  char '(' *>
  (* Optional index register *)
  option None (register_number >>| Option.some) >>= fun index ->
  (* Optional base register (preceded by comma if index present) *)
  option None (
    char ',' *> register_number >>| Option.some
  ) >>= fun base ->
  char ')' *>
  return (index, base)

(* =============================================================================
   INDEXED OPERANDS
   ============================================================================= *)

(* Parse an indexed operand: D(X,B) or D(,B) or D(X) or D
   where D is a displacement expression *)
let indexed_operand expr_p =
  expr_p >>= fun displacement ->
  option None (index_base_registers >>| Option.some) >>| function
  | None -> 
      SimpleOperand displacement
  | Some (index, base) ->
      IndexedOperand (displacement, index, base)

(* =============================================================================
   SIMPLE OPERANDS
   ============================================================================= *)

(* Simple expression operand (no indexing) *)
let simple_operand expr_p =
  expr_p >>| fun e -> SimpleOperand e

(* =============================================================================
   UNIFIED OPERAND PARSER
   ============================================================================= *)

(* Parse any operand type with proper precedence:
   1. Try indexed operand first (most complex)
   2. Try register operand (simple but specific)
   3. Fall back to simple expression operand *)
let operand expr_p =
  choice [
    attempt (indexed_operand expr_p);
    attempt register_operand;
    simple_operand expr_p;
  ]

(* =============================================================================
   OPERAND LISTS
   ============================================================================= *)

(* Parse a comma-separated list of operands *)
let operand_list expr_p = 
  comma_sep (operand expr_p)

(* Parse a non-empty operand list *)
let operand_list1 expr_p = 
  comma_sep1 (operand expr_p)

(* =============================================================================
   OPERAND VALIDATION
   ============================================================================= *)

(* Validate an operand according to HLASM rules *)
let validate_operand = function
  | RegisterOperand reg ->
      if reg >= 0 && reg <= 15 then Ok ()
      else Error (Printf.sprintf "Invalid register number: %d" reg)
  
  | IndexedOperand (_, Some idx, _) when idx < 0 || idx > 15 ->
      Error (Printf.sprintf "Invalid index register: %d" idx)
  
  | IndexedOperand (_, _, Some base) when base < 0 || base > 15 ->
      Error (Printf.sprintf "Invalid base register: %d" base)
  
  | IndexedOperand (_, Some 0, _) ->
      Error "Register 0 cannot be used as index"
  
  | SimpleOperand _ | IndexedOperand _ ->
      Ok ()

(* Validate a list of operands *)
let validate_operands operands =
  let rec check = function
    | [] -> Ok ()
    | op :: rest ->
        match validate_operand op with
        | Ok () -> check rest
        | Error msg -> Error msg
  in
  check operands

(* =============================================================================
   OPERAND ANALYSIS
   ============================================================================= *)

(* Extract all registers used in an operand *)
let get_registers = function
  | RegisterOperand r -> [r]
  | IndexedOperand (_, idx, base) ->
      List.filter_map Fun.id [idx; base]
  | SimpleOperand _ -> []

(* Extract all registers from an operand list *)
let get_all_registers operands =
  List.concat_map get_registers operands

(* Check if an operand uses a specific register *)
let uses_register reg = function
  | RegisterOperand r -> r = reg
  | IndexedOperand (_, idx, base) ->
      (match idx with Some r when r = reg -> true | _ -> false) ||
      (match base with Some r when r = reg -> true | _ -> false)
  | SimpleOperand _ -> false

(* =============================================================================
   OPERAND FORMATTING (for debugging/pretty-printing)
   ============================================================================= *)

let format_operand = function
  | RegisterOperand r ->
      Printf.sprintf "R%d" r
  | SimpleOperand _ ->
      "expression"
  | IndexedOperand (_, idx, base) ->
      let idx_str = match idx with Some i -> string_of_int i | None -> "" in
      let base_str = match base with Some b -> string_of_int b | None -> "" in
      Printf.sprintf "D(%s,%s)" idx_str base_str

let format_operands operands =
  String.concat "," (List.map format_operand operands)
