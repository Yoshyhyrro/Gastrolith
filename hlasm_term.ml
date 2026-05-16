(* term.ml - Terms and self-defining constants *)
open Angstrom
open Hlasm_ast
open Utils
open Lex

(* =============================================================================
   SELF-DEFINING TERMS WITH VALIDATION
   ============================================================================= *)

let decimal_term = 
  decimal_digits >>= fun s ->
  try return (Decimal (int_of_string s))
  with _ -> fail "Invalid decimal number"

let hex_term = 
  string_ci "X'" *> hex_digits >>= fun s ->
  char '\'' *>
  (if String.length s mod 2 = 0 && String.length s > 0 then 
     return (Hexadecimal (String.uppercase_ascii s))
   else 
     fail "Hexadecimal string must have even, non-zero length")

let binary_term =
  string_ci "B'" *> binary_digits >>= fun s ->
  char '\'' *> return (Binary s)

(* Character constants - Mixed SBCS/DBCS allowed *)
let character_term =
  string_ci "C'" *> mixed_string <* char '\'' >>| fun s ->
  Character s

(* Graphic constants - DBCS only *)
let graphic_term =
  string_ci "G'" *> dbcs_string <* char '\'' >>| fun s ->
  Graphic s

(* Unified self-defining term parser *)
let self_defining_term = 
  choice [
    hex_term;
    binary_term; 
    character_term;
    graphic_term;
    decimal_term;  (* Must be last - most general *)
  ]

(* =============================================================================
   LITERALS WITH CORRECT HLASM SYNTAX
   ============================================================================= *)

let valid_literal_types = [
  "C"; "X"; "B"; "G"; "A"; "F"; "H"; "E"; "D"; "L"; "P"; "S"; "Y"; "Z"
]

(* Parse a literal modifier: (type) or (type(value)) *)
let literal_modifier =
  char '(' *>
  type_code >>= fun mod_type ->
  option None (char '(' *> skip_to_eol <* char ')' >>| Option.some) >>= fun mod_value ->
  char ')' *> return (mod_type, mod_value)

(* Parse a complete literal *)
let literal =
  char '=' *> 
  type_code >>= fun lit_type ->
  
  if List.mem lit_type valid_literal_types then
    (* Duplication factor (optional, default 1) *)
    option 1 parse_int >>= fun dup ->
    
    (* Modifiers (optional) *)
    option [] (many literal_modifier) >>= fun modifiers ->
    
    (* Value (quoted string) *)
    apostrophe_string >>| fun value ->
    { 
      lit_type; 
      duplication_factor = dup; 
      value; 
      modifiers 
    }
  else
    fail ("Invalid literal type: " ^ lit_type)

(* =============================================================================
   LOCATION COUNTER WITH DISAMBIGUATION
   ============================================================================= *)

(* The '*' symbol can mean:
   1. Location counter (standalone or in expressions)
   2. Start of a comment line
   3. Multiplication operator
   This parser handles location counter detection *)
let location_counter =
  char '*' *>
  peek_char_fail >>= function
  | ')' | '+' | '-' | '*' | '/' | ',' | ' ' | '\t' | '\n' -> 
      return LocationCounter
  | _ -> 
      fail "Ambiguous use of '*' - not a location counter here"

(* =============================================================================
   FORWARD REFERENCE FOR RECURSIVE PARSING
   ============================================================================= *)

(* Expression parser will be defined in expr.ml *)
let expression_ref : (unit -> expression t) ref = 
  ref (fun () -> fail "Expression parser not initialized")

(* =============================================================================
   TERM PARSER (Building block of expressions)
   ============================================================================= *)

let term = 
  choice [
    attempt location_counter;
    Symbol.symbol_kind >>| (fun s -> Symbol s);
    self_defining_term >>| (fun t -> SelfDefining t);
    Symbol.attribute_ref >>| (fun a -> AttributeRef a);
    literal >>| (fun l -> Literal l);
    (char '(' *> !expression_ref () <* char ')') >>| (fun e -> Parenthesized e);
  ]

(* Export for use in expression parser *)
let term_parser = ref (fun () -> term)

(* =============================================================================
   VALIDATION HELPERS
   ============================================================================= *)

(* Validate a self-defining term *)
let validate_self_defining = function
  | Decimal n when n < 0 -> 
      Error "Decimal constant cannot be negative"
  | Hexadecimal s when String.length s mod 2 <> 0 ->
      Error "Hexadecimal constant must have even length"
  | Binary s when String.length s > 32 ->
      Error "Binary constant too long (max 32 bits)"
  | Character s when String.length s > 256 ->
      Error "Character constant too long (max 256 characters)"
  | Graphic s when String.length s > 256 ->
      Error "Graphic constant too long (max 256 characters)"
  | _ -> Ok ()
