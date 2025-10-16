open Printf

let print_warning ~file ~line ~col ~code ~message =
  let loc = match (line, col) with
    | (Some l, Some c) -> sprintf "%s:%d:%d" file l c
    | (Some l, None) -> sprintf "%s:%d" file l
    | _ -> file
  in
  printf "\x1b[1;33mwarning\x1b[0m[%s]: %s\n  \x1b[1;34m-->\x1b[0m %s\n" 
    code message loc

let analyze_file filename content =
  (* Try to parse using existing parser entrypoints *)
  let _prog = Pl1_core.Driver.parse_from_string content filename in
  (* Example simple lints: long lines *)
  let lines = String.split_on_char '\n' content in
  List.iteri (fun i ln ->
    if String.length ln > 72 then
      print_warning ~file:filename ~line:(Some (i+1)) ~col:(Some 73) ~code:"PL1001" ~message:"Line exceeds 72 columns (card body)"
  ) lines;
  Ok ()
  

let read_file fname =
  try
    let ic = open_in fname in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Ok content
  with
  | Sys_error e -> Error ("File error: " ^ e)
  | e -> Error (Printexc.to_string e)

let usage () =
  printf "Usage: pl1-lint <file.pl1>\n";
  exit 1

let () =
  if Array.length Sys.argv < 2 then usage ();
  let fname = Sys.argv.(1) in
  match read_file fname with
  | Error e -> eprintf "%s\n" e; exit 1
  | Ok content ->
      match analyze_file fname content with
      | Ok () -> exit 0
      | Error e -> eprintf "Parse error: %s\n" e; exit 2
