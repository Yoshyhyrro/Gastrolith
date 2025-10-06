open Printf

(* Very small generator: map some PL/I tokens to HLASM sample lines *)

let token_to_hlasm = function
  | "PUT" -> "PUT LIST(OUTPUT)"
  | "GET" -> "GET LIST(INPUT)"
  | "CALL" -> "CALL PROC"
  | "RETURN" -> "RETURN"
  | s -> sprintf "; UNHANDLED(%s)" s

let generate_from_channel ic oc =
  let tokens = Pl1_lexer.from_channel ic in
  List.iter (fun t -> fprintf oc "%s\n" (token_to_hlasm t)) tokens
