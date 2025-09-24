# PL/1 Parser Project Setup Script (Corrected Version)
# Creates directory structure and initial files for OCaml Menhir-based PL/1 parser

param(
    [string]$ProjectName = "pl1-emulator"
)

Write-Host "🚀 Setting up PL/1 Parser Project: $ProjectName" -ForegroundColor Green

# Create main project directory
if (Test-Path $ProjectName) {
    Write-Host "⚠️  Directory $ProjectName already exists. Continuing..." -ForegroundColor Yellow
} else {
    New-Item -ItemType Directory -Name $ProjectName | Out-Null
    Write-Host "📁 Created main directory: $ProjectName" -ForegroundColor Blue
}

Set-Location $ProjectName

# Create src directory
New-Item -ItemType Directory -Name "src" -Force | Out-Null
Write-Host "📁 Created src directory" -ForegroundColor Blue

# Create dune-project file
@"
(lang dune 3.0)

(package
 (name pl1-parser)
 (depends ocaml dune menhir)
 (synopsis "PL/I Parser using OCaml and Menhir")
 (description "A parser and interpreter for PL/I programming language"))
"@ | Out-File -FilePath "dune-project" -Encoding UTF8

Write-Host "📄 Created dune-project" -ForegroundColor Blue

# Copy VS Code settings if provided
$SettingsPath = Join-Path $PSScriptRoot "settings.json"
if (Test-Path $SettingsPath) {
    Copy-Item $SettingsPath -Destination ".vscode/settings.json" -Force
    Write-Host "⚙️  Copied VS Code settings" -ForegroundColor Blue
} else {
    # Create basic .vscode/settings.json with PL/1 support
    New-Item -ItemType Directory -Name ".vscode" -Force | Out-Null
    @"
{
    "editor.fontSize": 18,
    "files.associations": {
        "**/*.PL1*{,/*}": "pl1",
        "**/*.PLI*{,/*}": "pl1",
        "**/*.INC*{,/*}": "pl1",
        "**/*.INCLUDE*{,/*}": "pl1"
    },
    "[ocaml]": {
        "editor.tabSize": 2,
        "editor.insertSpaces": true
    }
}
"@ | Out-File -FilePath ".vscode/settings.json" -Encoding UTF8
    Write-Host "⚙️  Created basic VS Code settings" -ForegroundColor Blue
}

# Create src/dune file
@"
(executables
 (public_names pl1-parser)
 (name main)
 (libraries menhirLib))

(menhir
 (modules parser))

(rule
 (targets lexer.ml)
 (deps lexer.mll)
 (action
  (run ocamllex %{deps})))
"@ | Out-File -FilePath "src/dune" -Encoding UTF8

Write-Host "📄 Created src/dune" -ForegroundColor Blue

# Create AST definition (with ON support)
@"
(* Abstract Syntax Tree for PL/I *)

type identifier = string

(* Data types *)
type pl1_type =
  | Fixed of int option * int option  (* FIXED(precision, scale) *)
  | Float of int option               (* FLOAT(precision) *)
  | Char of int option               (* CHAR(length) *)
  | Bit of int option                (* BIT(length) *)
  | Binary of int option             (* BINARY(length) *)

(* Expressions *)
type expr =
  | Var of identifier
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BinaryOp of expr * string * expr
  | UnaryOp of string * expr
  | FuncCall of identifier * expr list

(* ON conditions captured in AST *)
type on_condition =
  | Zerodivide
  | Endfile of identifier
  | Key of identifier
  | Other_on of string

(* Statements *)
type stmt =
  | Assign of identifier * expr
  | Declare of identifier * pl1_type * expr option
  | If of expr * stmt * stmt option
  | Do of stmt list
  | DoLoop of identifier * expr * expr * stmt list (* control DO i = a TO b *)
  | Put of expr list
  | Get of identifier list
  | Call of identifier * expr list
  | Return of expr option
  | Label of identifier
  (* ON statement variants *)
  | OnGoto of on_condition * identifier
  | OnBlock of on_condition * stmt list
  | OnSystem of on_condition
  | Labeled of identifier * stmt

(* Procedures and programs *)
type procedure_decl = {
  name: identifier;
  params: (identifier * pl1_type) list;
  body: stmt list;
}

type program = {
  procedures: procedure_decl list;
  main: stmt list;
}
"@ | Out-File -FilePath "src/ast.ml" -Encoding UTF8

Write-Host "📄 Created src/ast.ml (AST definition with ON support)" -ForegroundColor Blue

# Create Menhir grammar file (with ON rules)
@"
(* PL/I Parser Grammar for Menhir *)
%{
open Ast
%}

(* Tokens *)
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT

(* Keywords *)
%token DECLARE DCL FIXED FLOAT CHAR BIT BINARY
%token IF THEN ELSE DO END TO
%token PUT GET CALL RETURN
%token PROCEDURE PROC
%token OPTIONS MAIN SKIP LIST
%token ON ZERODIVIDE ENDFILE KEY SYSTEM GOTO BEGIN

(* Operators *)
%token PLUS MINUS MULT DIV
%token EQ NE LT LE GT GE

(* Delimiters *)
%token LPAREN RPAREN
%token COMMA SEMICOLON COLON
%token EOF

(* Precedence and associativity *)
%left EQ NE LT LE GT GE
%left PLUS MINUS
%left MULT DIV
%nonassoc UMINUS

(* Start symbol *)
%start program
%type <Ast.program> program

%%

(* Program structure *)
program:
  | procedures = list(procedure_decl); main = list(statement); EOF
    { { procedures; main } }

procedure_decl:
  | name = IDENT; COLON; PROCEDURE; LPAREN; params = separated_list(COMMA, parameter); RPAREN; SEMICOLON;
    body = list(statement);
    END; name_end = IDENT; SEMICOLON
    { { name; params; body } }

parameter:
  | name = IDENT; typ = pl1_type
    { (name, typ) }

(* Data types *)
pl1_type:
  | FIXED; precision_scale = option(precision_scale_spec)
    { match precision_scale with
      | None -> Fixed (None, None)
      | Some (p, s) -> Fixed (Some p, s) }
  | FLOAT; precision = option(precision_spec)
    { Float precision }
  | CHAR; length = option(length_spec)
    { Char length }
  | BIT; length = option(length_spec)
    { Bit length }

precision_scale_spec:
  | LPAREN; p = INT; COMMA; s = INT; RPAREN
    { (p, Some s) }
  | LPAREN; p = INT; RPAREN
    { (p, None) }

precision_spec:
  | LPAREN; p = INT; RPAREN
    { Some p }

length_spec:
  | LPAREN; len = INT; RPAREN
    { Some len }

(* Statements *)
statement:
  | DECLARE; name = IDENT; typ = pl1_type; init = option(preceded(EQ, expression)); SEMICOLON
    { Declare (name, typ, init) }
  | target = IDENT; EQ; value = expression; SEMICOLON
    { Assign (target, value) }
  | IF; cond = expression; THEN; then_stmt = statement; else_part = option(preceded(ELSE, statement))
    { If (cond, then_stmt, else_part) }
  | DO; SEMICOLON; body = list(statement); END; SEMICOLON
    { Do body }
  | DO; var = IDENT; EQ; start = expression; TO; end_ = expression; SEMICOLON;
    body = list(statement); END; SEMICOLON
    { DoLoop (var, start, end_, body) }
  | PUT; LIST; LPAREN; args = separated_list(COMMA, expression); RPAREN; SEMICOLON
    { Put args }
  | GET; LIST; LPAREN; vars = separated_list(COMMA, IDENT); RPAREN; SEMICOLON
    { Get vars }
  | CALL; name = IDENT; LPAREN; args = separated_list(COMMA, expression); RPAREN; SEMICOLON
    { Call (name, args) }
  | RETURN; value = option(expression); SEMICOLON
    { Return value }
  (* ON statements: syntactic recognition only; semantics handled by analyzer *)
  | ON; cond = on_condition; GOTO; label = IDENT; SEMICOLON
    { OnGoto (cond, label) }
  | ON; cond = on_condition; SYSTEM; SEMICOLON
    { OnSystem cond }
  | ON; cond = on_condition; BEGIN; body = list(statement); END; SEMICOLON
    { OnBlock (cond, body) }
  | label = IDENT; COLON; s = statement
    { Labeled (label, s) }

(* ON condition nonterminal *)
on_condition:
  | ZERODIVIDE { Zerodivide }
  | ENDFILE; LPAREN; name = IDENT; RPAREN { Endfile name }
  | KEY; LPAREN; name = IDENT; RPAREN { Key name }
  | IDENT { Other_on (ident) }

(* Expressions *)
expression:
  | name = IDENT
    { Var name }
  | value = INT
    { IntLit value }
  | value = FLOAT
    { FloatLit value }
  | value = STRING
    { StringLit value }
  | left = expression; op = binary_op; right = expression
    { BinaryOp (left, op, right) }
  | op = unary_op; expr = expression %prec UMINUS
    { UnaryOp (op, expr) }
  | name = IDENT; LPAREN; args = separated_list(COMMA, expression); RPAREN
    { FuncCall (name, args) }
  | LPAREN; expr = expression; RPAREN
    { expr }

%inline binary_op:
  | PLUS { "+" }
  | MINUS { "-" }
  | MULT { "*" }
  | DIV { "/" }
  | EQ { "=" }
  | NE { "¬=" }
  | LT { "<" }
  | LE { "<=" }
  | GT { ">" }
  | GE { ">=" }

%inline unary_op:
  | MINUS { "-" }

%%
"@ | Out-File -FilePath "src/parser.mly" -Encoding UTF8

Write-Host "📄 Created src/parser.mly (Menhir grammar with ON support)" -ForegroundColor Blue

# Create OCamllex lexer (with ON keywords)
@"
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
"@ | Out-File -FilePath "src/lexer.mll" -Encoding UTF8

Write-Host "📄 Created src/lexer.mll (OCamllex lexer with ON keywords)" -ForegroundColor Blue

# Create CFG interface
@"
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

type cfg

val from_ast : program -> cfg
val iter_nodes : cfg -> (basic_block -> unit) -> unit
val find_block_by_label : cfg -> identifier -> basic_block option
val add_on_edge : cfg -> node_id -> node_id -> unit
"@ | Out-File -FilePath "src/cfg.mli" -Encoding UTF8

Write-Host "📄 Created src/cfg.mli" -ForegroundColor Blue

# Create CFG implementation (skeleton)
@"
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
"@ | Out-File -FilePath "src/cfg.ml" -Encoding UTF8

Write-Host "📄 Created src/cfg.ml (CFG skeleton)" -ForegroundColor Blue

# Create analyzer skeleton
@"
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
"@ | Out-File -FilePath "src/analyzer.ml" -Encoding UTF8

Write-Host "📄 Created src/analyzer.ml (analyzer skeleton)" -ForegroundColor Blue

# Create configuration module interface (no changes needed)
@"
(* Configuration module interface *)

val debug_mode : bool ref
val output_ast : bool ref
val input_file : string ref
val output_file : string ref

val parse_args : unit -> unit
val print_usage : unit -> unit
"@ | Out-File -FilePath "src/config.mli" -Encoding UTF8

Write-Host "📄 Created src/config.mli" -ForegroundColor Blue

# Create configuration module implementation (no changes needed)
@"
(* Configuration module implementation *)

let debug_mode = ref false
let output_ast = ref false
let input_file = ref ""
let output_file = ref ""

let usage_msg = "pl1-parser [options] <input_file>"

let spec_list = [
  ("-debug", Arg.Set debug_mode, " Enable debug output");
  ("-ast", Arg.Set output_ast, " Output AST representation");
  ("-o", Arg.Set_string output_file, " Set output file");
]

let set_input_file filename =
  if !input_file = "" then
    input_file := filename
  else
    failwith "Multiple input files specified"

let parse_args () =
  Arg.parse spec_list set_input_file usage_msg;
  if !input_file = "" then (
    print_usage ();
    exit 1
  )

let print_usage () =
  Arg.usage spec_list usage_msg
"@ | Out-File -FilePath "src/config.ml" -Encoding UTF8

Write-Host "📄 Created src/config.ml" -ForegroundColor Blue

# Create main module (no changes needed)
@"
(* Main module for PL/I Parser *)
open Ast
open Config

let print_ast ast =
  Printf.printf "=== Abstract Syntax Tree ===\n";
  Printf.printf "Procedures: %d\n" (List.length ast.procedures);
  Printf.printf "Main statements: %d\n" (List.length ast.main);
  Printf.printf "=============================\n"

let parse_file filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_fname <- filename;

    if !debug_mode then
      Printf.printf "Parsing file: %s\n" filename;

    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;

    if !output_ast then print_ast ast;

    Printf.printf "✅ Successfully parsed PL/I program\n";
    ast
  with
  | Lexer.LexError msg ->
      Printf.eprintf "❌ Lexical error: %s\n" msg;
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "❌ Parse error at line %d, character %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "❌ File error: %s\n" msg;
      exit 1

let main () =
  Printf.printf "🏗️  PL/I Parser v1.0\n";
  parse_args ();

  let ast = parse_file !input_file in

  if !debug_mode then
    Printf.printf "✨ Parsing completed successfully!\n"

let () = main ()
"@ | Out-File -FilePath "src/main.ml" -Encoding UTF8

Write-Host "📄 Created src/main.ml (main program)" -ForegroundColor Blue

# Create sample PL/I program for testing
@"
/* Sample PL/I Program */
SAMPLE_PROC: PROCEDURE(X FIXED, Y CHAR(10));
  DECLARE I FIXED;
  DECLARE MSG CHAR(20) = 'Hello, World!';

  PUT LIST('Starting procedure');

  DO I = 1 TO 10;
    PUT LIST('Iteration: ', I);
  END;

  RETURN;
END SAMPLE_PROC;

/* Main program */
DECLARE NUM FIXED = 42;
DECLARE NAME CHAR(20) = 'PL/I Parser';

PUT LIST('Program: ', NAME);
PUT LIST('Number: ', NUM);

CALL SAMPLE_PROC(NUM, 'test');
"@ | Out-File -FilePath "sample.pl1" -Encoding UTF8

Write-Host "📄 Created sample.pl1 (test file)" -ForegroundColor Blue

# Create build script
@"
#!/usr/bin/env bash
# Build script for PL/I Parser

echo "🔨 Building PL/I Parser..."

# Check if dune is installed
if ! command -v dune &> /dev/null; then
    echo "❌ Dune is not installed. Please install it with: opam install dune"
    exit 1
fi

# Check if menhir is installed
if ! command -v menhir &> /dev/null; then
    echo "❌ Menhir is not installed. Please install it with: opam install menhir"
    exit 1
fi

# Build the project
dune build

if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    echo "🚀 Run with: dune exec pl1-parser sample.pl1"
else
    echo "❌ Build failed!"
    exit 1
fi
"@ | Out-File -FilePath "build.sh" -Encoding UTF8

Write-Host "📄 Created build.sh" -ForegroundColor Blue

# Create Windows batch build script
@"
@echo off
echo 🔨 Building PL/I Parser...

REM Check if dune is installed
dune --version >nul 2>&1
if errorlevel 1 (
    echo ❌ Dune is not installed. Please install it with: opam install dune
    exit /b 1
)

REM Check if menhir is installed
menhir --version >nul 2>&1
if errorlevel 1 (
    echo ❌ Menhir is not installed. Please install it with: opam install menhir
    exit /b 1
)

REM Build the project
dune build

if errorlevel 1 (
    echo ❌ Build failed!
    exit /b 1
) else (
    echo ✅ Build successful!
    echo 🚀 Run with: dune exec pl1-parser sample.pl1
)
"@ | Out-File -FilePath "build.bat" -Encoding UTF8

Write-Host "📄 Created build.bat" -ForegroundColor Blue

# Create README.md
@"
# PL/I Parser

OCaml-based PL/I parser using Menhir for grammar definition and OCamllex for lexical analysis.

## Features

- ✅ PL/I lexical analysis (identifiers, keywords, operators, literals)
- ✅ Menhir-based grammar parser
- ✅ Abstract Syntax Tree (AST) generation
- ✅ Basic PL/I constructs support:
  - Variable declarations with data types (FIXED, FLOAT, CHAR, BIT)
  - Control structures (IF-THEN-ELSE, `DO I=...` loops)
  - Procedure definitions and calls
  - I/O statements (`PUT LIST`)

## Requirements

- OCaml (4.12+)
- Dune build system
- Menhir parser generator
- OCamllex lexer generator

## Installation

```bash
# Install dependencies via opam
opam install dune menhir

# Build the project
dune build

# Or use the provided scripts
./build.sh       # Unix/Linux/macOS
build.bat        # Windows
```

## Usage

```bash
# Parse a PL/I file
dune exec pl1-parser sample.pl1

# With debug output
dune exec pl1-parser -- -debug sample.pl1

# Output AST representation
dune exec pl1-parser -- -ast sample.pl1
```

## Project Structure

```
pl1-emulator/
 ├─ .gitignore
 ├─ .vscode/
 │   └─ settings.json    # VS Code settings
 ├─ dune-project         # Dune project configuration
 ├─ src/
 │   ├─ dune             # Build configuration
 │   ├─ main.ml          # Main program entry point
 │   ├─ config.ml(i)     # Command-line argument handling
 │   ├─ parser.mly       # Menhir grammar definition
 │   ├─ lexer.mll        # OCamllex lexer definition
 │   └─ ast.ml           # Abstract Syntax Tree types
 ├─ sample.pl1           # Sample PL/I program for testing
 └─ build.sh/.bat        # Build scripts
```
"@ | Out-File -FilePath "README.md" -Encoding UTF8

Write-Host "📄 Created README.md" -ForegroundColor Blue

# Create .gitignore
@"
# OCaml
*.cmi
*.cmo
*.cmx
*.cma
*.cmxa
*.a
*.o
*.so
*.dylib
*.dll
*.exe

# Dune
_build/
.merlin
*.install

# Generated files
src/lexer.ml
src/parser.ml
src/parser.mli

# IDE
.vscode/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db

# Logs
*.log
"@ | Out-File -FilePath ".gitignore" -Encoding UTF8

Write-Host "📄 Created .gitignore" -ForegroundColor Blue

Write-Host ""
Write-Host "✅ PL/1 Parser project setup complete! (Bugs fixed)" -ForegroundColor Green
Write-Host ""
Write-Host "📋 Next steps:" -ForegroundColor Yellow
Write-Host "  1. cd $ProjectName" -ForegroundColor White
Write-Host "  2. Install dependencies: opam install dune menhir" -ForegroundColor White
Write-Host "  3. Build: ./build.sh (Unix) or build.bat (Windows)" -ForegroundColor White
Write-Host "  4. Test: dune exec pl1-parser sample.pl1" -ForegroundColor White
Write-Host ""