open Ast
open Printf
open Angstrom

(* Simple parser using Angstrom *)
let identifier =
  let is_letter = function 'A'..'Z' | 'a'..'z' | '_' -> true | _ -> false in
  let is_digit = function '0'..'9' -> true | _ -> false in
  let is_idchar c = is_letter c || is_digit c || c = '$' in
  take_while1 is_letter >>= fun first ->
  take_while is_idchar >>= fun rest ->
  return (first ^ rest)

let integer = take_while1 (function '0'..'9' -> true | _ -> false)
let whitespace = take_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let parse_program =
  let* _ = whitespace in
  let* ids = many (identifier <* whitespace) in
  return (List.map (fun id -> SDecl [id]) ids)

let parse_from_string s _filename =
  let input = Preprocess.normalize_source s in
  match parse_string ~consume:All parse_program input with
  | Ok prog -> prog
  | Error msg -> failwith msg

let read_file fname =
  try
    let ic = open_in fname in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic; s
  with e ->
    eprintf "File error: %s\n" (Printexc.to_string e); exit 1

let () =
  if Array.length Sys.argv < 2 then begin
    eprintf "Usage: pl1_demo <file>\n"; exit 1
  end;
  let file = Sys.argv.(1) in
  let src = read_file file in
  let prog = parse_from_string src file in
  printf "Parsed %d statements\n" (List.length prog)
