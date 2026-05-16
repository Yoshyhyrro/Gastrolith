(* stmt.ml - Statement-level parsing *)
open Angstrom
open Hlasm_ast
open Utils
open Lex

(* =============================================================================
   STATEMENT PARSER
   ============================================================================= *)

(* Parse a single HLASM statement with position tracking *)
let statement expr_p operand_list_p =
  with_pos (
    peek_char >>= function
    | Some '*' -> 
        (* Full-line comment *)
        comment_line >>| fun comment ->
        { 
          label = None; 
          stmt_type = Comment comment; 
          remarks = None; 
          pos = { line = 0; column = 0 } (* Will be replaced by with_pos *)
        }
    
    | _ ->
        (* Optional label *)
        Symbol.optional_symbol >>= fun label ->
        
        (* Instruction (assembler or machine) *)
        choice [
          Asm.assembler_instruction expr_p >>| (fun inst -> AssemblerInst inst);
          Machine.machine_instruction operand_list_p >>| (fun inst -> MachineInst inst);
        ] >>= fun stmt_type ->
        
        (* Optional remarks *)
        option None (ws1 *> skip_to_eol >>| Option.some) >>| fun remarks ->
        
        { 
          label; 
          stmt_type; 
          remarks; 
          pos = { line = 0; column = 0 } (* Will be replaced by with_pos *)
        }
  ) >>| fun (stmt, pos) ->
  { stmt with pos }

(* =============================================================================
   LINE PARSER (Handles blank lines and comments)
   ============================================================================= *)

(* Parse a single line - could be statement, comment, or blank *)
let line expr_p operand_list_p =
  choice [
    (* Statement line *)
    (ws *> statement expr_p operand_list_p >>| Option.some);
    
    (* Full-line comment (already handled by statement parser) *)
    (full_line_comment *> return None);
    
    (* Blank line *)
    (ws *> return None);
  ]

(* =============================================================================
   PROGRAM PARSER
   ============================================================================= *)

(* Parse a complete HLASM program *)
let program expr_p operand_list_p =
  sep_by eol (line expr_p operand_list_p) >>| fun results ->
  { statements = List.filter_map Fun.id results }

(* =============================================================================
   STATEMENT VALIDATION
   ============================================================================= *)

(* Validate a statement *)
let validate_statement stmt =
  (* Check label usage *)
  (match stmt.label, stmt.stmt_type with
   | Some _, Comment _ -> 
       Error "Comments cannot have labels"
   | Some (Variable _), _ ->
       Error "Variable symbols cannot be used as labels"
   | _ -> Ok ())
  
  |> Result.bind (fun () ->
    (* Validate instruction-specific rules *)
    match stmt.stmt_type with
    | AssemblerInst inst ->
        (match inst with
         | USING (exprs, regs) -> Asm.validate_using exprs regs
         | EQU (v, l, t) -> Asm.validate_equ v l t
         | DC fields | DS fields ->
             List.fold_left (fun acc field ->
               Result.bind acc (fun () -> Asm.validate_dc_subfield field)
             ) (Ok ()) fields
         | _ -> Ok ())
    
    | MachineInst inst ->
        Machine.validate_operand_count inst.mnemonic inst.operands
    
    | Comment _ -> Ok ()
  )

(* Validate all statements in a program *)
let validate_program prog =
  let rec validate_list stmts =
    match stmts with
    | [] -> Ok ()
    | stmt :: rest ->
        Result.bind (validate_statement stmt) (fun () ->
          validate_list rest
        )
  in
  validate_list prog.statements

(* =============================================================================
   STATEMENT ANALYSIS
   ============================================================================= *)

(* Check if statement defines a symbol *)
let defines_symbol stmt =
  match stmt.label with
  | Some (Ordinary _) -> true
  | _ -> 
      match stmt.stmt_type with
      | AssemblerInst inst -> Asm.defines_symbol inst
      | _ -> false

(* Get symbol defined by statement *)
let get_defined_symbol stmt =
  match stmt.label with
  | Some (Ordinary sym) -> Some sym.name
  | Some (Sequence sym) -> Some sym.name
  | _ ->
      match stmt.stmt_type with
      | AssemblerInst (CSECT (Some sym)) -> Some sym.name
      | AssemblerInst (DSECT (Some sym)) -> Some sym.name
      | _ -> None

(* Check if statement uses registers *)
let uses_registers stmt =
  match stmt.stmt_type with
  | AssemblerInst inst -> Asm.uses_registers inst
  | MachineInst inst -> List.length inst.operands > 0
  | Comment _ -> false

(* Get all registers used by statement *)
let get_used_registers stmt =
  match stmt.stmt_type with
  | AssemblerInst inst -> Asm.get_used_registers inst
  | MachineInst inst -> Operand.get_all_registers inst.operands
  | Comment _ -> []

(* Check if statement affects location counter *)
let affects_location_counter stmt =
  match stmt.stmt_type with
  | AssemblerInst inst -> Asm.affects_location_counter inst
  | MachineInst inst -> 
      Machine.get_instruction_length inst.mnemonic > 0
  | Comment _ -> false

(* =============================================================================
   PRETTY PRINTING
   ============================================================================= *)

let format_label = function
  | None -> ""
  | Some (Ordinary sym) -> sym.name
  | Some (Variable sym) -> sym.name
  | Some (Sequence sym) -> sym.name

let format_statement stmt =
  let label_str = format_label stmt.label in
  let label_field = 
    if label_str = "" then "        " 
    else Printf.sprintf "%-8s" label_str 
  in
  
  let inst_str = match stmt.stmt_type with
    | AssemblerInst _ -> "ASM"  (* Would need more detail *)
    | MachineInst inst -> Machine.format_instruction inst
    | Comment c -> "* " ^ c
  in
  
  let remarks_str = match stmt.remarks with
    | None -> ""
    | Some r -> " " ^ r
  in
  
  Printf.sprintf "%s %s%s" label_field inst_str remarks_str

let format_program prog =
  String.concat "\n" (List.map format_statement prog.statements)

(* =============================================================================
   SYMBOL TABLE CONSTRUCTION
   ============================================================================= *)

(* Build symbol table from program *)
let build_symbol_table prog =
  let rec build_table stmts acc =
    match stmts with
    | [] -> acc
    | stmt :: rest ->
        let new_acc = match get_defined_symbol stmt with
          | Some name -> name :: acc
          | None -> acc
        in
        build_table rest new_acc
  in
  build_table prog.statements []

(* Find all undefined symbols *)
let find_undefined_symbols prog =
  let defined = build_symbol_table prog in
  let rec collect_used stmts acc =
    match stmts with
    | [] -> acc
    | stmt :: rest ->
        let used = match stmt.stmt_type with
          | MachineInst _ -> []  (* Would need expression analysis *)
          | _ -> []
        in
        collect_used rest (used @ acc)
  in
  let used = collect_used prog.statements [] in
  List.filter (fun sym -> not (List.mem sym defined)) used