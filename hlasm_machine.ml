(* machine.ml - Machine instruction parsing *)
open Angstrom
open Hlasm_ast
open Utils
open Lex

(* =============================================================================
   MNEMONIC TAG PARSER
   ============================================================================= *)

(* Parse optional mnemonic tag: :ASM or :MAC *)
let mnemonic_tag = 
  ws *> choice [
    string ":ASM" *> return (Some ASM);
    string ":MAC" *> return (Some MAC);
    return None;
  ]

(* =============================================================================
   MACHINE INSTRUCTION PARSER
   ============================================================================= *)

(* Parse a machine instruction with operands *)
let machine_instruction operand_list_p =
  mnemonic >>= fun mnemonic ->
  mnemonic_tag >>= fun tag ->
  ws1 *> operand_list_p >>| fun operands ->
  { 
    mnemonic; 
    operands; 
    format = tag 
  }

(* =============================================================================
   INSTRUCTION FORMAT VALIDATION
   ============================================================================= *)

(* Common S/360 and S/370 instruction formats *)
type instruction_format =
  | RR    (* Register-Register: 2 registers *)
  | RX    (* Register-Indexed: reg, D(X,B) *)
  | RS    (* Register-Storage: reg, reg, D(B) *)
  | SI    (* Storage-Immediate: D(B), immediate *)
  | SS    (* Storage-Storage: D(L,B), D(B) *)
  | SSE   (* Storage-Storage Extended *)
  | RRE   (* Register-Register Extended *)
  | RXE   (* Register-Indexed Extended *)
  | Unknown

(* Known instruction format database (partial) *)
let instruction_formats = [
  (* Common RR format *)
  ("LR", RR); ("AR", RR); ("SR", RR); ("MR", RR); ("DR", RR);
  ("CR", RR); ("NR", RR); ("OR", RR); ("XR", RR);
  ("LTR", RR); ("LCR", RR); ("LNR", RR); ("LPR", RR);
  
  (* Common RX format *)
  ("L", RX); ("ST", RX); ("A", RX); ("S", RX); ("M", RX); ("D", RX);
  ("C", RX); ("N", RX); ("O", RX); ("X", RX);
  ("LA", RX); ("LH", RX); ("STH", RX);
  
  (* Common RS format *)
  ("BXH", RS); ("BXLE", RS); ("SLL", RS); ("SRL", RS); ("SLA", RS); ("SRA", RS);
  
  (* Common SI format *)
  ("CLI", SI); ("MVI", SI); ("NI", SI); ("OI", SI); ("XI", SI); ("TM", SI);
  
  (* Common SS format *)
  ("MVC", SS); ("CLC", SS); ("XC", SS); ("NC", SS); ("OC", SS);
  ("MVZ", SS); ("MVN", SS); ("MVO", SS);
  ("PACK", SS); ("UNPK", SS);
  ("AP", SS); ("SP", SS); ("MP", SS); ("DP", SS); ("CP", SS);
  ("ZAP", SS); ("ED", SS); ("EDMK", SS);
]

(* Look up instruction format *)
let get_instruction_format mnemonic =
  match List.assoc_opt mnemonic instruction_formats with
  | Some fmt -> fmt
  | None -> Unknown

(* =============================================================================
   OPERAND COUNT VALIDATION
   ============================================================================= *)

(* Validate operand count for known instruction formats *)
let validate_operand_count mnemonic operands =
  let count = List.length operands in
  match get_instruction_format mnemonic with
  | RR when count <> 2 -> 
      Error (Printf.sprintf "%s (RR format) requires 2 operands, got %d" mnemonic count)
  | RX when count <> 2 ->
      Error (Printf.sprintf "%s (RX format) requires 2 operands, got %d" mnemonic count)
  | RS when count < 2 || count > 3 ->
      Error (Printf.sprintf "%s (RS format) requires 2-3 operands, got %d" mnemonic count)
  | SI when count <> 2 ->
      Error (Printf.sprintf "%s (SI format) requires 2 operands, got %d" mnemonic count)
  | SS when count <> 2 ->
      Error (Printf.sprintf "%s (SS format) requires 2 operands, got %d" mnemonic count)
  | Unknown -> 
      Ok ()  (* Can't validate unknown instructions *)
  | _ -> 
      Ok ()

(* =============================================================================
   INSTRUCTION ANALYSIS
   ============================================================================= *)

(* Check if instruction is a branch *)
let is_branch_instruction mnemonic =
  let branch_prefixes = ["B"; "BC"; "BCR"; "BAS"; "BAL"; "BALR"; "BXH"; "BXLE"] in
  List.exists (fun prefix -> String.starts_with ~prefix mnemonic) branch_prefixes

(* Check if instruction modifies condition code *)
let modifies_condition_code mnemonic =
  let no_cc = ["L"; "ST"; "LA"; "LR"; "LM"; "STM"; "MVC"; "MVI"] in
  not (List.mem mnemonic no_cc)

(* Check if instruction is privileged *)
let is_privileged mnemonic =
  let privileged = [
    "LPSW"; "SSM"; "STNSM"; "STOSM"; "SIGP"; "MC"; "PTLB";
    "IPTE"; "IVSK"; "SSKE"; "RRBE"; "LCTL"; "STCTL"
  ] in
  List.mem mnemonic privileged

(* Get instruction length in bytes *)
let get_instruction_length mnemonic =
  match get_instruction_format mnemonic with
  | RR | RRE -> 2
  | RX | RS | SI | RXE -> 4
  | SS | SSE -> 6
  | Unknown -> 0  (* Unknown length *)

(* =============================================================================
   REGISTER USAGE ANALYSIS
   ============================================================================= *)

(* Extract register usage from machine instruction *)
let get_register_usage instruction =
  let regs = Operand.get_all_registers instruction.operands in
  {
    reads = regs;
    writes = [];  (* Would need more detailed analysis *)
    both = [];
  }

(* Check if instruction uses a specific register *)
let uses_register reg instruction =
  List.exists (Operand.uses_register reg) instruction.operands

(* =============================================================================
   PRETTY PRINTING
   ============================================================================= *)

let format_instruction inst =
  let operands_str = Operand.format_operands inst.operands in
  let tag_str = match inst.format with
    | Some ASM -> ":ASM"
    | Some MAC -> ":MAC"
    | None -> ""
  in
  Printf.sprintf "%s%s %s" inst.mnemonic tag_str operands_str