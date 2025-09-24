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
