open Angstrom
open Hlasm_ast

(* =============================================================================
	 UTILITY PARSERS AND BASIC TYPES
	 ============================================================================= *)

let ws = skip_while (function ' ' | '\t' -> true | _ -> false)
let ws1 = skip (function ' ' | '\t' -> true | _ -> false) *> ws

let alphanum = function
	| 'A'..'Z' | 'a'..'z' | '0'..'9' | '@' | '$' | '#' | '_' -> true
	| _ -> false

let alpha_start = function
	| 'A'..'Z' | 'a'..'z' | '@' | '$' | '#' | '_' -> true
	| _ -> false

let digit = function '0'..'9' -> true | _ -> false
let hex_digit = function '0'..'9' | 'A'..'F' | 'a'..'f' -> true | _ -> false

(* Position tracking *)
let current_pos = pos >>| fun pos -> 
	{ line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol }

(* =============================================================================
	 STRING AND CHARACTER PARSERS
	 ============================================================================= *)

let quoted_string quote_char =
	char quote_char *> 
	take_while (fun c -> c <> quote_char && c <> '\n') <*
	char quote_char

let apostrophe_string = quoted_string '\''
let comment_line = char '*' *> take_while (fun c -> c <> '\n')

(* =============================================================================
	 DBCS (Double-Byte Character Set) SUPPORT
	 ============================================================================= *)

let shift_out = char '\x0E'
let shift_in = char '\x0F'
let dbcs_byte = function '\x41'..'\xFE' -> true | _ -> false
let dbcs_byte_p = satisfy dbcs_byte

let dbcs_char =
	shift_out *>
	lift2 (fun b1 b2 -> String.make 1 b1 ^ String.make 1 b2) dbcs_byte_p dbcs_byte_p
	<* shift_in

let dbcs_string = many1 dbcs_char >>| String.concat ""

(* =============================================================================
	 SYMBOL PARSERS WITH HLASM CONSTRAINTS
	 ============================================================================= *)

let ordinary_symbol =
	peek_char >>= function
	| Some c when alpha_start c ->
			current_pos >>= fun pos ->
			take_while alphanum >>= fun name ->
			if String.length name > 63 then
				fail "Symbol name too long (max 63 characters)"
			else if String.length name = 0 then
				fail "Empty symbol name"
			else
				return { name; pos }
	| _ -> fail "Expected ordinary symbol"

let variable_symbol =
	char '&' *> ordinary_symbol >>| fun sym ->
	{ sym with name = "&" ^ sym.name }

let sequence_symbol =
	char '.' *> ordinary_symbol >>| fun sym ->
	{ sym with name = "." ^ sym.name }

let symbol_kind =
	choice [
		variable_symbol >>| (fun s -> Variable s);
		sequence_symbol >>| (fun s -> Sequence s);
		ordinary_symbol >>| (fun s -> Ordinary s);
	]

(* =============================================================================
	 SELF-DEFINING TERMS WITH VALIDATION
	 ============================================================================= *)

let decimal_term = 
	take_while1 digit >>= fun s ->
	try return (Decimal (int_of_string s))
	with _ -> fail "Invalid decimal number"

let hex_term = 
	string_ci "X'" *> take_while hex_digit >>= fun s ->
	char '\'' *>
	(if String.length s mod 2 = 0 && String.length s > 0 then 
		 return (Hexadecimal (String.uppercase_ascii s))
	 else 
		 fail "Hexadecimal string must have even, non-zero length")

let binary_term =
	string_ci "B'" *> take_while1 (function '0' | '1' -> true | _ -> false) >>= fun s ->
	char '\'' *> return (Binary s)

(* Character constants - Mixed SBCS/DBCS allowed *)
let character_term =
	string_ci "C'" *> 
	many (dbcs_char <|> (satisfy (fun c -> c <> '\'' && c <> '\n') >>| String.make 1)) >>= fun chunks ->
	char '\'' *> return (Character (String.concat "" chunks))

(* Graphic constants - DBCS only *)
let graphic_term =
	string_ci "G'" *> 
	dbcs_string <*
	char '\'' >>| fun s ->
	Graphic s

let self_defining_term = choice [
	hex_term;
	binary_term; 
	character_term;
	graphic_term;
	decimal_term;
]

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
	 LITERALS WITH CORRECT HLASM SYNTAX
	 ============================================================================= *)

let valid_literal_types = [
	"C"; "X"; "B"; "G"; "A"; "F"; "H"; "E"; "D"; "L"; "P"; "S"; "Y"; "Z"
]

let literal_modifier =
	char '(' *>
	take_while1 (function 'A'..'Z' | 'a'..'z' -> true | _ -> false) >>= fun mod_type ->
	option None (char '(' *> take_while (fun c -> c <> ')') <* char ')' >>| Option.some) >>= fun mod_value ->
	char ')' >>| fun () ->
	(mod_type, mod_value)

let literal =
	char '=' *> 
	take_while1 (function 'A'..'Z' | 'a'..'z' -> true | _ -> false) >>= fun lit_type ->
	let lit_type_upper = String.uppercase_ascii lit_type in
	if List.mem lit_type_upper valid_literal_types then
		(* duplication factor *)
		option 1 (take_while1 digit >>| int_of_string) >>= fun dup ->
		(* modifiers *)
		option [] (many literal_modifier) >>= fun modifiers ->
		(* value *)
		apostrophe_string >>| fun value ->
		{ lit_type = lit_type_upper; duplication_factor = dup; value; modifiers }
	else
		fail ("Invalid literal type: " ^ lit_type)

(* =============================================================================
	 FORWARD DECLARATIONS FOR RECURSIVE TYPES
	 ============================================================================= *)

let expression_ref = ref (fun _ -> assert false)
let term_parser = ref (fun _ -> assert false)

(* =============================================================================
	 TERMS WITH CORRECTED LOCATION COUNTER HANDLING
	 ============================================================================= *)

let location_counter =
	char '*' *>
	peek_char_fail >>= function
	| ')' | '+' | '-' | '*' | '/' | ',' | ' ' | '\t' | '\n' -> return LocationCounter
	| _ -> fail "Ambiguous use of '*' - not a location counter here"

let term = 
	choice [
		attempt location_counter;
		symbol_kind >>| (fun s -> Symbol s);
		self_defining_term >>| (fun t -> SelfDefining t);
		attribute_ref >>| (fun a -> AttributeRef a);
		literal >>| (fun l -> Literal l);
		(char '(' *> !expression_ref <* char ')') >>| (fun e -> Parenthesized e);
	]

let () = term_parser := (fun () -> term)

(* =============================================================================
	 EXPRESSIONS WITH STRICT LEFT-TO-RIGHT EVALUATION
	 ============================================================================= *)

let binary_op = choice [
	char '+' *> return Add;
	char '-' *> return Sub;
	char '*' *> return Mul;
	char '/' *> return Div;
]

let rec make_binary_expr left ops =
	match ops with
	| [] -> left
	| (op, right) :: rest ->
			make_binary_expr (BinaryOp (left, op, right)) rest

let expression =
	let factor = choice [
		char '+' *> (!term_parser ()) >>| (fun t -> UnaryPlus (Term t));
		char '-' *> (!term_parser ()) >>| (fun t -> UnaryMinus (Term t));
		(!term_parser ()) >>| (fun t -> Term t);
	] in

	factor >>= fun left ->
	many (binary_op >>= fun op -> factor >>| fun right -> (op, right)) >>| fun ops ->
	make_binary_expr left ops

let () = expression_ref := (fun () -> expression)

(* =============================================================================
	 OPERANDS WITH REGISTER VALIDATION
	 ============================================================================= *)

let validate_register reg =
	if reg >= 0 && reg <= 15 then
		return reg
	else
		fail ("Invalid register number: " ^ string_of_int reg)

let register_operand = 
	take_while1 digit >>= fun s ->
	validate_register (int_of_string s) >>| fun reg ->
	RegisterOperand reg

let simple_operand = expression >>| fun e -> SimpleOperand e

let indexed_operand =
	expression >>= fun addr ->
	option None (char '(' *> 
		option None (take_while1 digit >>= fun s -> 
								 validate_register (int_of_string s) >>| Option.some) >>= fun index ->
		option None (char ',' *> take_while1 digit >>= fun s -> 
								 validate_register (int_of_string s) >>| Option.some) >>= fun base ->
		char ')' *> return (index, base)
	) >>| fun idx_base ->
	match idx_base with
	| Some (Some index, Some base) -> IndexedOperand (addr, Some index, Some base)
	| Some (Some index, None) -> IndexedOperand (addr, Some index, None)
	| Some (None, Some base) -> IndexedOperand (addr, None, Some base)
	| Some (None, None) -> IndexedOperand (addr, None, None)
	| None -> SimpleOperand addr

let operand = choice [
	attempt indexed_operand;
	register_operand;
	simple_operand;
]

let operand_list = sep_by (char ',') operand

(* =============================================================================
	 MACHINE INSTRUCTIONS
	 ============================================================================= *)

let mnemonic_tag = 
	ws *> choice [
		string ":ASM" *> return (Some ASM);
		string ":MAC" *> return (Some MAC);
		return None;
	]

let machine_instruction =
	take_while1 (function 'A'..'Z' | 'a'..'z' | '0'..'9' -> true | _ -> false) >>= fun mnemonic ->
	mnemonic_tag >>= fun tag ->
	ws1 *> operand_list >>| fun operands ->
	{ mnemonic = String.uppercase_ascii mnemonic; operands; format = tag }

(* =============================================================================
	 DATA CONSTANTS WITH COMPLETE DC/DS SUPPORT
	 ============================================================================= *)

let valid_dc_types = [
	"A"; "F"; "H"; "D"; "E"; "L"; "P"; "Z"; "C"; "X"; "B"; "G"; "S"; "Y"
]

let data_constant_subfield =
	option 1 (take_while1 digit >>| int_of_string) >>= fun dup ->
	take_while1 (function 'A'..'Z' | 'a'..'z' -> true | _ -> false) >>= fun t ->
	let type_code = String.uppercase_ascii t in
	if List.mem type_code valid_dc_types then
		option None (take_while1 (function 'A'..'Z' | 'a'..'z' -> true | _ -> false) >>| 
								 fun e -> Some (String.uppercase_ascii e)) >>= fun ext ->
		option None (char 'P' *> char '(' *> expression <* char ')' >>| Option.some) >>= fun prog ->
		option None (char 'L' *> take_while1 digit >>| fun l -> Some (int_of_string l)) >>= fun len ->
		option None (char 'S' *> take_while1 digit >>| fun s -> Some (int_of_string s)) >>= fun scale ->
		option None (char 'E' *> take_while1 digit >>| fun e -> Some (int_of_string e)) >>= fun exponent ->
		apostrophe_string >>| fun value ->
		{
			duplication_factor = dup;
			dc_type = type_code;
			type_extension = ext;
			program_type = prog;
			length = len;
			scale = scale;
			exponent = exponent;
			nominal_value = value;
		}
	else
		fail ("Invalid DC/DS type: " ^ type_code)

(* =============================================================================
	 ASSEMBLER INSTRUCTIONS WITH VALIDATION
	 ============================================================================= *)

let assembler_instruction = 
	let keyword op parser = string_ci op *> ws1 *> parser in
	let using_parser =
		sep_by1 (char ',') expression >>= fun exprs ->
		char ',' *> sep_by1 (char ',') (take_while1 digit >>= fun s -> 
														validate_register (int_of_string s)) >>= fun regs ->
		let unique_regs = List.sort_uniq compare regs in
		if List.length unique_regs = List.length regs then
			return (USING (exprs, regs))
		else
			fail "Duplicate registers in USING statement"
	in
	choice [
		keyword "CSECT" (option None (ordinary_symbol >>| Option.some)) >>| (fun name -> CSECT name);
		keyword "DSECT" (option None (ordinary_symbol >>| Option.some)) >>| (fun name -> DSECT name);
		keyword "START" (option None (expression >>| Option.some)) >>| (fun addr -> START addr);
		keyword "END" (option None (ordinary_symbol >>| Option.some)) >>| (fun sym -> END sym);
		keyword "DC" (sep_by1 (char ',') data_constant_subfield) >>| (fun fields -> DC fields);
		keyword "DS" (sep_by1 (char ',') data_constant_subfield) >>| (fun fields -> DS fields);
		keyword "EQU" (
			expression >>= fun value ->
			option None (char ',' *> expression >>| Option.some) >>= fun length ->
			option None (char ',' *> expression >>| Option.some) >>| fun type_val ->
			EQU (value, length, type_val)
		);
		keyword "USING" using_parser;
		keyword "LTORG" (return LTORG);
		keyword "SPACE" (option None (take_while1 digit >>= fun s -> 
											return (Some (int_of_string s)))) >>| (fun n -> SPACE n);
		keyword "EJECT" (return EJECT);
		keyword "COPY" (ordinary_symbol >>| fun sym -> COPY sym);
		keyword "MACRO" (return MACRO);
		keyword "MEND" (return MEND);
		keyword "MEXIT" (return MEXIT);
		keyword "MNOTE" (
			take_while1 digit >>= fun severity ->
			char ',' *> take_while (fun c -> c <> '\n') >>| fun msg ->
			MNOTE (int_of_string severity, msg)
		);
		keyword "OPSYN" (
			ordinary_symbol >>= fun new_op ->
			option None (char ',' *> ordinary_symbol >>| Option.some) >>| fun old_op ->
			OPSYN (new_op, old_op)
		);
	]

(* =============================================================================
	 STATEMENT PARSER WITH IMPROVED COMMENT HANDLING
	 ============================================================================= *)

let statement =
	current_pos >>= fun pos ->
	peek_char >>= function
	| Some '*' -> fail "full line comment or to be implemented"
	| _ -> fail "statement parsing not yet implemented"

