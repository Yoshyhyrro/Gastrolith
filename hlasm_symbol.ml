(* symbol.ml - Symbol parsing and classification *)
open Angstrom
open Hlasm_ast
open Utils
open Lex

(* =============================================================================
   BASIC SYMBOL PARSING
   ============================================================================= *)

(* Parse an ordinary symbol (alphanumeric, starts with letter) *)
let ordinary_symbol =
  with_pos symbol_name >>| fun (name, pos) ->
  { name; pos }

(* Parse a variable symbol (starts with &) *)
let variable_symbol =
  char '&' *> with_pos symbol_name >>| fun (name, pos) ->
  { name = "&" ^ name; pos }

(* Parse a sequence symbol (starts with .) *)
let sequence_symbol =
  char '.' *> with_pos symbol_name >>| fun (name, pos) ->
  { name = "." ^ name; pos }

(* =============================================================================
   SYMBOL CLASSIFICATION
   ============================================================================= *)

(* Parse any symbol and classify it *)
let symbol_kind =
  choice [
    variable_symbol >>| (fun s -> Variable s);
    sequence_symbol >>| (fun s -> Sequence s);
    ordinary_symbol >>| (fun s -> Ordinary s);
  ]

(* Optional symbol (used for labels) *)
let optional_symbol = option None (symbol_kind <* ws1 >>| Option.some)

(* =============================================================================
   ATTRIBUTE REFERENCES
   ============================================================================= *)

let attribute_type = choice [
  char 'L' *> return Length;
  char 'T' *> return Type;
  char 'S' *> return Scale;
  char 'I' *> return Integer;
  char 'K' *> return Count;
  char 'N' *> return Number;
  char 'D' *> return Defined;
  char 'O' *> return OpCode;
]

let attribute_ref =
  attribute_type >>= fun attr_type ->
  char '\'' *> ordinary_symbol >>| fun symbol ->
  { attr_type; symbol }

(* =============================================================================
   SYMBOL VALIDATION
   ============================================================================= *)

(* Check if a symbol name is valid according to HLASM rules *)
let validate_symbol_name name =
  let len = String.length name in
  if len = 0 then
    Error "Empty symbol name"
  else if len > 63 then
    Error "Symbol name too long (max 63 characters)"
  else if not (is_alpha_start name.[0]) then
    Error "Symbol must start with a letter"
  else if not (String.for_all is_alphanum name) then
    Error "Symbol contains invalid characters"
  else
    Ok ()

(* Create a symbol with validation *)
let make_symbol name pos =
  match validate_symbol_name name with
  | Ok () -> Ok { name; pos }
  | Error msg -> Error msg
