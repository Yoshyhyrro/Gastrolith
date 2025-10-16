type t =
  | IDENT of string
  | INT of int
  | STRING of string
  | DCL | IF | THEN | ELSE | DO | END | SELECT | WHEN | OTHERWISE
  | PLUS | MINUS | STAR | SLASH | EQ | LT | GT | CONCAT
  | SEMICOLON | COMMA | LPAREN | RPAREN
  | EOF

