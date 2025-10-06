(* PL/I Lexer for OCamllex *)
{
  open Parser

  exception LexError of string

  let keywords = [
    ("DECLARE", DECLARE); ("DCL", DCL);
    ("FIXED", FIXED); ("FLOAT", FLOAT); ("CHAR", CHAR);
    ("BIT", BIT); ("BINARY", BINARY);
    ("IF", IF); ("THEN", THEN); ("ELSE", ELSE);
    ("DO", DO); ("END", END); ("TO", TO);
    ("PUT", PUT); ("GET", GET); ("CALL", CALL);
    ("RETURN", RETURN); ("PROCEDURE", PROCEDURE); ("PROC", PROC);
    ("LIST", LIST);
    (* ON related keywords *)
    ("ON", ON); ("ZERODIVIDE", ZERODIVIDE); ("ENDFILE", ENDFILE);
    ("KEY", KEY); ("SYSTEM", SYSTEM); ("GOTO", GOTO); ("BEGIN", BEGIN);
  ]

  let keyword_table = Hashtbl.create 32
  let _ = List.iter (fun (k, v) -> Hashtbl.add keyword_table k v) keywords

  let lookup_keyword s =
    try Hashtbl.find keyword_table (String.uppercase_ascii s)
    with Not_found -> IDENT s
}

(* Character classes *)
let whitespace = [' ' '\t' '\r']
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*

rule token = parse
  | whitespace+     { token lexbuf }
  | newline         { Lexing.new_line lexbuf; token lexbuf }
  | "/*"            { comment lexbuf }

  (* Numbers *)
  | digit+ as i     { INT (int_of_string i) }
  | digit+ '.' digit* as f { FLOAT (float_of_string f) }
  | '.' digit+ as f { FLOAT (float_of_string f) }

  (* Strings *)
  | '\'' [^ '\''']* '\'' as s
    { STRING (String.sub s 1 (String.length s - 2)) }

  (* Identifiers and keywords *)
  | identifier as id { lookup_keyword id }

  (* Operators *)
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { MULT }
  | '/'             { DIV }
  | '='             { EQ }
  | "¬=" | "^="     { NE }
  | '<'             { LT }
  | "<="            { LE }
  | '>'             { GT }
  | ">="            { GE }

  (* Delimiters *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | ';'             { SEMICOLON }
  | ':'             { COLON }

  | eof             { EOF }
  | _ as c          { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }

and comment = parse
  | "*/"            { token lexbuf }
  | newline         { Lexing.new_line lexbuf; comment lexbuf }
  | _               { comment lexbuf }
  | eof             { raise (LexError "Unterminated comment") }
