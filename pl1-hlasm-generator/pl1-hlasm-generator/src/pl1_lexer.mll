{ 
  open Printf
}

let digit = ['0'-'9']
let ident = ['A'-'Z' 'a'-'z' '_' '@' '$' '#'] (['A'-'Z' 'a'-'z' '0'-'9' '_' '@' '$' '#'])*
let ws = [' ' '\t' '\r' '\n']+

rule token = parse
  | ws { token lexbuf } /* skip whitespace */
  | "PUT" { "PUT" }
  | "GET" { "GET" }
  | "CALL" { "CALL" }
  | "RETURN" { "RETURN" }
  | ident as id { id }
  | eof { "<EOF>" }
  | _ { "<UNK>" }

/* Simple entry generating a stream of token strings */

{ 
  let from_channel ic =
    let lexbuf = Lexing.from_channel ic in
    let rec next acc =
      let t = token lexbuf in
      if t = "<EOF>" then List.rev acc
      else next (t::acc)
    in
    next []
}
