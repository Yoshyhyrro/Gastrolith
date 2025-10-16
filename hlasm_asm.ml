(* asm.ml - Assembler instruction parsing *)
open Angstrom
open Hlasm_ast
open Utils
open Lex

(* =============================================================================
   DATA CONSTANT SUBFIELD PARSER
   ============================================================================= *)

let valid_dc_types = [
  "A"; "F"; "H"; "D"; "E"; "L"; "P"; "Z"; "C"; "X"; "B"; "G"; "S"; "Y"
]

(* Parse a single data constant subfield *)
let data_constant_subfield expr_p =
  (* Duplication factor: optional, default is 1 *)
  option 1 parse_int >>= fun dup ->
  
  (* Type code: required *)
  type_code >>= fun dc_type ->
  
  if List.mem dc_type valid_dc_types then
    (* Type extension: optional (e.g., 'L' in CL8) *)
    option None (type_code >>| Option.some) >>= fun type_extension ->
    
    (* Program type: P(value) format *)
    option None (
      char 'P' *> char '(' *> expr_p <* char ')' >>| Option.some
    ) >>= fun program_type ->
    
    (* Length modifier: Lnn *)
    option None (char 'L' *> parse_int >>| Option.some) >>= fun length ->
    
    (* Scale modifier: Smm *)
    option None (char 'S' *> parse_int >>| Option.some) >>= fun scale ->
    
    (* Exponent modifier: Emm *)
    option None (char 'E' *> parse_int >>| Option.some) >>= fun exponent ->
    
    (* Nominal value: quoted string *)
    apostrophe_string >>| fun nominal_value ->
    {
      duplication_factor = dup;
      dc_type;
      type_extension;
      program_type;
      length;
      scale;
      exponent;
      nominal_value;
    }
  else
    fail ("Invalid DC/DS type: " ^ dc_type)

(* Parse a comma-separated list of DC/DS subfields *)
let data_constant_list expr_p =
  comma_sep1 (data_constant_subfield expr_p)

(* =============================================================================
   USING STATEMENT PARSER
   ============================================================================= *)

(* USING statement: USING base_expr[,base_expr...],reg[,reg...] *)
let using_parser expr_p =
  comma_sep1 expr_p >>= fun exprs ->
  char ',' *> comma_sep1 register_number >>= fun regs ->
  
  (* Check for duplicate registers *)
  let unique_regs = List.sort_uniq compare regs in
  if List.length unique_regs = List.length regs then
    return (USING (exprs, regs))
  else
    fail "Duplicate registers in USING statement"

(* =============================================================================
   EQU STATEMENT PARSER
   ============================================================================= *)

(* EQU statement: symbol EQU value[,length[,type]] *)
let equ_parser expr_p =
  expr_p >>= fun value ->
  option None (char ',' *> expr_p >>| Option.some) >>= fun length ->
  option None (char ',' *> expr_p >>| Option.some) >>| fun type_val ->
  EQU (value, length, type_val)

(* =============================================================================
   MNOTE STATEMENT PARSER
   ============================================================================= *)

(* MNOTE statement: MNOTE severity,'message' *)
let mnote_parser =
  parse_int >>= fun severity ->
  char ',' *> skip_to_eol >>| fun msg ->
  MNOTE (severity, msg)

(* =============================================================================
   OPSYN STATEMENT PARSER
   ============================================================================= *)

(* OPSYN statement: OPSYN new_op[,old_op] *)
let opsyn_parser =
  Symbol.ordinary_symbol >>= fun new_op ->
  option None (char ',' *> Symbol.ordinary_symbol >>| Option.some) >>| fun old_op ->
  OPSYN (new_op, old_op)

(* =============================================================================
   ASSEMBLER INSTRUCTION DISPATCHER
   ============================================================================= *)

(* Main assembler instruction parser *)
let assembler_instruction expr_p =
  choice [
    keyword_opt "CSECT" Symbol.ordinary_symbol >>| (fun name -> CSECT name);
    keyword_opt "DSECT" Symbol.ordinary_symbol >>| (fun name -> DSECT name);
    keyword_opt "START" expr_p >>| (fun addr -> START addr);
    keyword_opt "END" Symbol.ordinary_symbol >>| (fun sym -> END sym);
    
    keyword_req "DC" (data_constant_list expr_p) >>| (fun fields -> DC fields);
    keyword_req "DS" (data_constant_list expr_p) >>| (fun fields -> DS fields);
    
    keyword_req "EQU" (equ_parser expr_p);
    keyword_req "USING" (using_parser expr_p);
    
    keyword "LTORG" *> return LTORG;
    keyword_opt "SPACE" parse_int >>| (fun n -> SPACE n);
    keyword "EJECT" *> return EJECT;
    
    keyword_req "COPY" Symbol.ordinary_symbol >>| (fun sym -> COPY sym);
    
    keyword "MACRO" *> return MACRO;
    keyword "MEND" *> return MEND;
    keyword "MEXIT" *> return MEXIT;
    
    keyword_req "MNOTE" mnote_parser;
    keyword_req "OPSYN" opsyn_parser;
  ]

(* =============================================================================
   VALIDATION HELPERS
   ============================================================================= *)

(* Validate a DC/DS subfield *)
let validate_dc_subfield field =
  (* Check duplication factor *)
  if field.duplication_factor < 0 then
    Error "Duplication factor cannot be negative"
  else if field.duplication_factor > 65535 then
    Error "Duplication factor too large (max 65535)"
  
  (* Check length modifier *)
  else match field.length with
  | Some l when l <= 0 -> Error "Length must be positive"
  | Some l when l > 256 -> Error "Length too large (max 256)"
  
  (* Check scale and exponent (for numeric types) *)
  | _ when List.mem field.dc_type ["P"; "Z"] ->
      (match field.scale with
       | Some s when s < 0 || s > 14 -> Error "Scale out of range (0-14)"
       | _ -> Ok ())
  
  | _ -> Ok ()

(* Validate USING statement *)
let validate_using exprs regs =
  if List.length exprs = 0 then
    Error "USING requires at least one base expression"
  else if List.length regs = 0 then
    Error "USING requires at least one register"
  else if List.length exprs > 16 then
    Error "Too many base expressions in USING (max 16)"
  else
    (* Check for register 0 usage *)
    if List.mem 0 regs then
      Error "Register 0 cannot be used as base register"
    else
      Ok ()

(* Validate EQU statement *)
let validate_equ value length type_val =
  match length with
  | Some _ when type_val = None ->
      Error "EQU with length requires type parameter"
  | _ -> Ok ()

(* =============================================================================
   INSTRUCTION ANALYSIS
   ============================================================================= *)

(* Check if instruction defines a symbol *)
let defines_symbol = function
  | CSECT (Some _) | DSECT (Some _) -> true
  | EQU _ -> true
  | _ -> false

(* Check if instruction uses registers *)
let uses_registers = function
  | USING (_, regs) -> List.length regs > 0
  | _ -> false

(* Get all registers used by an instruction *)
let get_used_registers = function
  | USING (_, regs) -> regs
  | _ -> []

(* Check if instruction affects location counter *)
let affects_location_counter = function
  | DC _ | DS _ -> true
  | START _ -> true
  | CSECT _ | DSECT _ -> true
  | _ -> false