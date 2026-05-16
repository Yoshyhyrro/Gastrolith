(* file: pl1_parser.ml *)
open Angstrom

(* -------- AST -------- *)
type ident = string

type ty =
  | TyFixed of int (* fixed bin(n) *)
  | TyChar of int
  | TyFile
  | TyStruct of (ident * ty) list
  | TyUnknown of string

type declaration =
  | Dcl of ident * ty
  | DclMulti of (ident * ty) list

type expr =
  | EIdent of ident
  | EInt of int
  | EString of string
  | EBinOp of expr * string * expr

type stmt =
  | SDecl of declaration
  | SAssign of ident * expr
  | SReturn of expr option
  | SDoForever
  | SDoFor of ident * expr * expr * (stmt list)
  | SSelect of (expr option * stmt list) list (* when clauses, otherwise as None *)
  | SIf of expr * (stmt list) * (stmt list option)
  | SOn of expr * (stmt list)      (* on <event> begin ... end *)
  | SFetch of ident * ident
  | SPut of string list
  | SGoto of ident
  | SOpen of ident * string
  | SClose of ident
  | SProcedure of ident * (ident * ty) list * (stmt list)
  | SUnknown of string

type top =
  | TStmt of stmt
  | TProc of stmt

(* -------- helpers: whitespace/comments/literals -------- *)

let is_ident_start = function
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let is_ident_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '$' -> true
  | _ -> false

let ws =
  (* skip spaces, tabs, newlines *)
  skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let comment =
  let rec star_comment () =
    (* consume until '*/' *)
    fix (fun loop ->
      take_while (function '*' -> false | _ -> true) >>= fun _ ->
      option None (char '*' *> option None (char '/' *> return (Some ()))) >>= function
      | Some () -> return ()
      | None -> any_char >>= fun _ -> loop
    )
  in
  string "/*" *> (star_comment ()) *> return ()

let junk =
  skip_many (choice [ws; comment])

let lex p = junk *> p <* junk

let keyword s = lex (string_ci s)

(* case-insensitive string *)
and string_ci s =
  let rec chars i =
    if i >= String.length s then return ()
    else
      let c = s.[i] in
      let p = satisfy (fun x -> Char.lowercase_ascii x = Char.lowercase_ascii c) in
      p *> chars (i+1)
  in
  chars 0 *> return s

(* identifier *)
let ident =
  lex (
    lift2 (fun c cs -> String.of_seq (List.to_seq (c::cs)))
      (satisfy is_ident_start)
      (many (satisfy is_ident_char))
  ) <?> "identifier"

let int_lit =
  lex (take_while1 (function '0'..'9' -> true | _ -> false) >>= fun s -> return (EInt (int_of_string s)))

let quoted_string =
  let qch =
    choice [
      (char '\'' *> return '\''); (* we'll support simple 'string' by keeping quotes out *)
      any_char
    ]
  in
  lex (
    char '\'' *>
    take_while (fun c -> c <> '\'') <* char '\'' >>= fun s ->
    return (EString s)
  )

(* basic expression: identifier | int | string | simple binary op a / b *)
let parens p = lex (char '(') *> p <* lex (char ')')

let rec expr_parser () =
  let term =
    choice [
      (ident >>= fun id -> return (EIdent id));
      int_lit;
      quoted_string;
      parens (fix (fun e -> expr_parser ()));
    ]
  in
  let binop =
    lift3 (fun a op b -> EBinOp (a, op, b))
      term
      (lex (choice (List.map string ["+";"-";"*";"/";"||";"=";"<>";"<";">"])) <?> "binop")
      (term)
  in
  choice [binop; term]

(* -------- type parsers -------- *)

let integer_arg =
  lex (char '(' *> take_while1 (function '0'..'9' -> true | _ -> false) <* char ')' >>= fun s -> return (int_of_string s))

let ty_parser =
  choice [
    (keyword "fixed" *> keyword "bin" *> integer_arg >>= fun n -> return (TyFixed n));
    (keyword "char" *> integer_arg >>= fun n -> return (TyChar n));
    (keyword "file" *> return TyFile);
    (* struct ( ... ) *)
    (keyword "struct" *> lex (char '(') *> (sep_by (lex (char ',')) (lift2 (fun id ty -> (id, ty)) ident ty_parser)) <* lex (char ')') >>= fun fields -> return (TyStruct fields));
    (take_while1 (function c -> c <> ' ' && c <> ',' && c <> ';' && c <> ')' -> true | _ -> false) >>= fun s -> return (TyUnknown s));
  ]

(* declaration: dcl a char(32), b fixed bin(31); *)
let declaration_parser =
  let single =
    lift2 (fun id ty -> (id, ty))
      ident
      ty_parser
  in
  keyword "dcl" *> sep_by1 (lex (char ',')) single >>= fun lst ->
  return (DclMulti lst)

(* -------- statements (simplified) -------- *)

let stmt_return =
  keyword "return" *> option None (expr_parser () >>| fun e -> Some e) >>= fun r -> lex (char ';' <|> return ';') *> return (SReturn r)

let stmt_assign =
  lift2 (fun id e -> SAssign (id, e))
    ident
    (lex (char '=') *> expr_parser ()) >>= fun s -> lex (char ';' <|> return ';') *> return s

let stmt_put =
  keyword "put" *> lex (choice [string "skip"; return "skip";]) *> keyword "list" *> (
    sep_by1 (lex (char ',')) (choice [
      (ident >>| fun x -> x);
      (take_while1 (function '\'' -> false | _ -> true) >>| fun s -> s)
    ])
  ) >>= fun items -> lex (char ';' <|> return ';') *> return (SPut items)

let stmt_fetch =
  keyword "fetch" *> keyword "file" *> ident >>= fun file ->
  keyword "into" *> ident >>= fun into ->
  lex (char ';' <|> return ';') *> return (SFetch (file, into))

let stmt_goto =
  keyword "goto" *> ident >>= fun lab -> lex (char ';' <|> return ';') *> return (SGoto lab)

let stmt_open =
  keyword "open" *> keyword "file" *> parens (return ()) <|> (
    (* simplified: open file(input) input --- we'll parse as open file ident *)
    keyword "open" *> keyword "file" *> ident >>= fun f -> keyword "input" <|> keyword "output" *> return (SOpen (f, ""))
  )

(* on unit: on error begin ... end; or on endfile(input) begin ... end; *)
let stmt_on =
  keyword "on" *> (choice [
    keyword "endfile" *> parens (ident >>| fun i -> i) >>| fun event -> `Event event;
    keyword "error" >>| fun () -> `Event "error";
    keyword "zerodivide" >>| fun () -> `Event "zerodivide";
  ]) >>= fun ev ->
  (keyword "begin" *> many (fix (fun _ -> statement_parser ())) <* keyword "end") >>= fun body ->
  return (SOn (EString ev, body))

and statement_parser () =
  lex (
    choice [
      try_ declaration_parser >>| fun d -> SDecl d;
      stmt_return;
      stmt_put;
      stmt_fetch;
      stmt_goto;
      (* procedure definitions *)
      (keyword "procedure" *> ident >>= fun name ->
         option [] (parens (sep_by (lex (char ',')) (lift2 (fun id ty -> (id, ty)) ident ty_parser))) >>= fun params ->
         option [] (keyword "returns" *> parens (ty_parser >>| fun t -> [(name, t)]) <|> return []) >>= fun _ ->
         keyword "options" *> (parens (take_while (fun _ -> true))) *> keyword ";" *> return (SUnknown ("proc-options " ^ name))
      ) ;
      (* fallback to unknown token: consume until semicolon *)
      (take_while1 (fun c -> c <> ';') >>= fun s -> lex (char ';' <|> return ';') *> return (SUnknown s))
    ]
  )

(* top-level: many statements or procedure blocks *)
let top_level =
  junk *> many (statement_parser ()) <* end_of_input

(* -------- parse driver -------- *)

let parse_string str =
  match parse_string ~consume:All top_level str with
  | Ok stmts -> Ok stmts
  | Error e -> Error e

(* -------- Example usage --------
   let sample = (* your PL/I sample as string *) in
   match parse_string sample with
   | Ok stmts -> (* analyze / lint *)
   | Error err -> prerr_endline err
*)


